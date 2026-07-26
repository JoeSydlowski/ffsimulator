# Multi-year backtest gate for the dynasty transition model.
#
# For each holdout pair Y -> Y+1: train transition pools on pairs into seasons
# <= Y (max_transition_season = Y), then for every player in the season-Y
# dynasty rankings, given his ACTUAL season-Y quality, draw K transitions and
# score the predicted Y+1 positional-rank distribution (and exit probability)
# against the actual Y+1 rankings. One holdout year (~450 players) is too thin
# to detect feature effects of a few percent - metrics are reported per
# holdout AND pooled across all of them.
#
# Env:
#   FFS_DYN_FORMAT   csv of formats (default "1qb,superflex")
#   FFS_DYN_HOLDOUTS csv of holdout years Y (default 2019-2025 for 1qb,
#                    2022-2025 for superflex - history starts 2015 / 2020)
#   FFS_DYN_K        draws per player (default 300)
#   FFS_DYN_INT_AGE  =1 regress ages to integer birth-year arithmetic via a
#                    temp cache overlay (A/B for the decimal-age change)
#   FFS_DYN_FEAT     optional kernel feature to enable for this run:
#                    exp | exp_strict | draft | momentum | sd (default none)
#   FFS_DYN_TAG      label for output files (default "base"/"intage"/<feat>)
#
# Outputs: dev/validate_outputs/dynasty_backtest_multi_<tag>.csv (metrics),
#          dynasty_backtest_players_<tag>.csv (per player-holdout), printed
#          pooled verdicts.

library(data.table)
devtools::load_all(here::here(), quiet = TRUE)
set.seed(42)

out_dir <- here::here("dev", "validate_outputs")
K <- as.integer(Sys.getenv("FFS_DYN_K", "300"))
fmts <- strsplit(Sys.getenv("FFS_DYN_FORMAT", "1qb,superflex"), ",")[[1]]
int_age <- Sys.getenv("FFS_DYN_INT_AGE", "0") == "1"
feat <- Sys.getenv("FFS_DYN_FEAT", "")
tag <- Sys.getenv("FFS_DYN_TAG",
                  if (nzchar(feat)) feat else if (int_age) "intage" else "base")
default_holdouts <- function(fmt) if (fmt == "superflex") 2022:2025 else 2019:2025
env_holdouts <- Sys.getenv("FFS_DYN_HOLDOUTS", "")

# feature variant under test (bandwidths per the plan; sd is set per holdout)
if (feat == "exp")        options(ffsimulator.dyn_h_exp = 2)
if (feat == "exp_strict") options(ffsimulator.dyn_h_exp = 2, ffsimulator.dyn_rookie_strict = TRUE)
if (feat == "draft")      options(ffsimulator.dyn_h_draft = 2)
if (feat == "momentum")   options(ffsimulator.dyn_h_momentum = 8)
# exit shrinkage is ON by default in production (kappa = 10, gated in on the
# multi-year backtest below). Set it explicitly here so every run is
# unambiguous: sweep with FFS_EXIT_KAPPA, or FFS_EXIT_KAPPA=0 for the
# pre-shrinkage baseline.
options(ffsimulator.dyn_exit_shrink = as.numeric(Sys.getenv("FFS_EXIT_KAPPA", "10")))
# exit broad-window multipliers (empirical-Bayes prior): the rank window the
# broad exit rate is pooled over is h_rank*exit_broad_rank. Widen -> the prior is
# contaminated by deeper (higher-exit) players, inflating mid-tier exits; shrink
# to test that. Empty = shipped default (3).
if (nzchar(Sys.getenv("FFS_EXIT_BROAD_RANK", "")))
  options(ffsimulator.dyn_exit_broad_rank = as.numeric(Sys.getenv("FFS_EXIT_BROAD_RANK")))
if (nzchar(Sys.getenv("FFS_EXIT_BROAD_AGE", "")))
  options(ffsimulator.dyn_exit_broad_age = as.numeric(Sys.getenv("FFS_EXIT_BROAD_AGE")))
# per-position interval widening (QB/TE under-dispersion). Empty = the shipped
# default (widening ON, matches production); "off" = pre-widening baseline;
# else a spec like "QB=1.7;TE=1.4;WR=1.2;RB=1.1".
disp_env <- Sys.getenv("FFS_DISP_FACTOR", "")
if (identical(disp_env, "off")) {
  options(ffsimulator.dyn_disp_factor = numeric(0))  # no position matches -> no widening
} else if (nzchar(disp_env)) {
  kv <- strsplit(strsplit(disp_env, ";")[[1]], "=")
  options(ffsimulator.dyn_disp_factor =
            stats::setNames(as.numeric(vapply(kv, `[`, "", 2)),
                            vapply(kv, `[`, "", 1)))
}
# per-position point-prediction recalibration (see .ffs_draw_transition
# move_slope/move_bias): FFS_MOVE_SLOPE="QB=0.35;RB=0.5;WR=0.5;TE=0.5",
# FFS_MOVE_BIAS="RB=-1" (delta space: positive = rank number grows = decline).
# Empty = the shipped default (correction ON, matches production);
# "off" = pre-correction baseline.
parse_kv <- function(spec) {
  kv <- strsplit(strsplit(spec, ";")[[1]], "=")
  stats::setNames(as.numeric(vapply(kv, `[`, "", 2)), vapply(kv, `[`, "", 1))
}
for (nm in c("SLOPE", "BIAS")) {
  spec <- Sys.getenv(paste0("FFS_MOVE_", nm), "")
  opt <- paste0("ffsimulator.dyn_move_", tolower(nm))
  if (identical(spec, "off")) do.call(options, stats::setNames(list(numeric(0)), opt))
  else if (nzchar(spec)) do.call(options, stats::setNames(list(parse_kv(spec)), opt))
}
# coordinate the survivor move is transported in: "rank" (default, shipped),
# "log" or "probit" (latent, rank-only transforms). Latent runs need latent-fit
# move_slope/move_bias (pass via FFS_MOVE_SLOPE/BIAS, or "off" for the raw fit).
move_space_env <- Sys.getenv("FFS_MOVE_SPACE", "")
if (nzchar(move_space_env)) options(ffsimulator.dyn_move_space = move_space_env)
if (nzchar(feat)) cat("feature variant:", feat, "\n")
cat("move_space:", getOption("ffsimulator.dyn_move_space", "rank"), "\n")

scoring_history <- readRDS(file.path(out_dir, "scoring_history_2012_2025.rds"))
dp_id <- as.data.table(ffscrapr::dp_playerids())[
  !is.na(gsis_id) & !is.na(fantasypros_id), c("fantasypros_id", "gsis_id")
]
ids_feat <- as.data.table(ffscrapr::dp_playerids())
ids_feat <- unique(ids_feat[!is.na(ids_feat$fantasypros_id),
  list(fantasypros_id = as.character(fantasypros_id),
       draft_year = suppressWarnings(as.integer(draft_year)),
       draft_round = suppressWarnings(as.integer(draft_round)))],
  by = "fantasypros_id")

# A/B overlay: regress the bundled decimal ages back to integer birth-year
# arithmetic in a temp cache dir; .ffs_read_data prefers the cache per-file,
# everything else falls through to inst/pkgdata
if (int_age) {
  ids <- as.data.table(ffscrapr::dp_playerids())
  ids <- unique(ids[!is.na(ids$fantasypros_id) & !is.na(ids$birthdate),
                    list(fantasypros_id = as.character(fantasypros_id),
                         birth_year = as.integer(substr(birthdate, 1, 4)))],
                by = "fantasypros_id")
  dyn_mod <- readRDS(here::here("inst", "pkgdata", "fp_dynasty_history.rds"))
  by <- ids$birth_year[match(as.character(dyn_mod$fantasypros_id), ids$fantasypros_id)]
  dyn_mod$age <- ifelse(!is.na(by), dyn_mod$season - by, dyn_mod$age)
  overlay <- file.path(tempdir(), "ffs_intage_cache")
  dir.create(overlay, showWarnings = FALSE)
  saveRDS(dyn_mod, file.path(overlay, "fp_dynasty_history.rds"))
  options(ffsimulator.cache_directory = overlay)
  cat("A/B overlay: integer birth-year ages\n")
}

## ---- one holdout ---------------------------------------------------------------
run_holdout <- function(fmt, Y) {
  dynasty <- as.data.table(fp_dynasty_history())[format == fmt]
  # superflex borrows the deep 1qb movement pool by default (see
  # ffs_dynasty_outlook's superflex_from_1qb); FFS_SFLX_FROM_1QB=0 for baseline.
  # Targets (hold) and the cross-section stay `fmt`; only the pool changes.
  from_1qb <- Sys.getenv("FFS_SFLX_FROM_1QB", "1") != "0"
  pool_fmt <- if (from_1qb && fmt == "superflex") "1qb" else fmt
  pools <- ffsimulator:::.ffs_dynasty_transition_pools(
    scoring_history = scoring_history, format = pool_fmt, max_transition_season = Y
  )
  tr <- pools$transitions
  # native-format exit pool when movement is borrowed (exit stays format-specific)
  exit_tr <- if (pool_fmt != fmt) {
    ffsimulator:::.ffs_dynasty_transition_pools(
      scoring_history = scoring_history, format = fmt, max_transition_season = Y
    )$transitions
  } else NULL

  hold <- dynasty[season == Y]
  redraft <- as.data.table(fp_rankings_history())[
    season == Y, list(fantasypros_id, redraft_rank = rank)
  ]
  hold <- merge(hold, redraft, by = "fantasypros_id", all.x = TRUE)
  totals <- merge(
    as.data.table(scoring_history)[season == Y & week %in% 1:14 & !is.na(gsis_id),
                                   list(gsis_id, points)],
    dp_id, by = "gsis_id"
  )[, list(total = sum(points)), by = fantasypros_id]
  hold <- merge(hold, totals, by = "fantasypros_id", all.x = TRUE)
  hold[is.na(total), total := 0]
  hold[, q := ffsimulator:::.ffs_season_quality(pos, redraft_rank, total, pools$quality_pools)]

  actual <- dynasty[season == Y + 1, list(fantasypros_id, actual_rank = pos_rank)]
  hold <- merge(hold, actual, by = "fantasypros_id", all.x = TRUE)
  hold[, actual_exit := is.na(actual_rank)]

  # target features as known at prediction time (mirror the pools' definitions)
  hitH <- match(as.character(hold$fantasypros_id), ids_feat$fantasypros_id)
  hold[, `:=`(draft_year = ids_feat$draft_year[hitH],
              draft_round = ids_feat$draft_round[hitH])]
  fs <- dynasty[season <= Y, list(first_season = min(season)), by = fantasypros_id]
  hold <- merge(hold, fs, by = "fantasypros_id", all.x = TRUE)
  hold[, years_exp := fifelse(!is.na(draft_year), Y - draft_year, Y - first_season)]
  hold[years_exp < 0, years_exp := 0L]
  hold[!is.na(draft_year) & is.na(draft_round), draft_round := 8L]
  prevY <- dynasty[season == Y - 1, list(fantasypros_id, prev_pos_rank = pos_rank)]
  hold <- merge(hold, prevY, by = "fantasypros_id", all.x = TRUE)
  hold[, prev_delta := pos_rank - prev_pos_rank]
  h_sd_val <- if (feat == "sd") 2 * stats::median(hold$sd, na.rm = TRUE) else
    getOption("ffsimulator.dyn_h_sd", NA)

  idx <- rep(seq_len(nrow(hold)), each = K)
  draws <- ffsimulator:::.ffs_draw_transition(
    pos = hold$pos[idx], age = hold$age[idx],
    dyn_pos_rank = hold$pos_rank[idx], q = hold$q[idx],
    transitions = tr,
    years_exp = hold$years_exp[idx], draft_round = hold$draft_round[idx],
    prev_delta = hold$prev_delta[idx], ecr_sd = hold$sd[idx],
    h_sd = h_sd_val, exit_transitions = exit_tr
  )
  pred <- data.table(
    row = idx, next_rank = draws[[1]], exited = draws[[2]]
  )
  # diagnostic: inflate the survivor rank draws around each player's median to
  # test how much extra dispersion closes the cover80/pit_tail gap (1 = no-op)
  infl <- as.numeric(Sys.getenv("FFS_INTERVAL_INFLATE", "1"))
  if (infl != 1) {
    pred[!is.na(next_rank), next_rank := {
      m <- stats::median(next_rank); pmax(1, m + infl * (next_rank - m))
    }, by = row]
  }
  pred_sum <- pred[, list(
    p_exit = mean(exited),
    med_rank = stats::median(next_rank, na.rm = TRUE),
    q10 = stats::quantile(next_rank, .10, na.rm = TRUE),
    q90 = stats::quantile(next_rank, .90, na.rm = TRUE),
    n_surv_draws = sum(!is.na(next_rank)),
    pit = NA_real_
  ), by = row]
  # PIT per player against his actual rank (survivors only)
  surv_rows <- which(!hold$actual_exit)
  pit_map <- pred[row %in% surv_rows & !is.na(next_rank)][
    , list(pit = {
      a <- hold$actual_rank[row[1]]
      (sum(next_rank < a) + 0.5 * sum(next_rank == a)) / .N
    }), by = row]
  pred_sum[pit_map, pit := i.pit, on = "row"]

  # ---- value space: map ranks through the synthetic value curve on the
  # season-Y ladder (10000*exp(-0.023*overall_rank), the live default when no
  # FantasyCalc values exist - which is the case for historical seasons);
  # exits are worth 0. exp_change (val_mean/cur_val - 1) is what the live
  # tool reports, so calibration HERE is what users actually read.
  fns <- lapply(split(hold[, list(pos_rank, rank)], hold$pos), function(d) {
    d <- d[order(pos_rank)]
    list(f = stats::approxfun(d$pos_rank, d$rank, rule = 2),
         max_pos_rank = max(d$pos_rank), max_rank = max(d$rank))
  })
  p2o <- function(p, pr) {
    out <- numeric(length(pr))
    for (pp in unique(p)) {
      j <- which(p == pp)
      fn <- fns[[pp]]
      if (is.null(fn)) { out[j] <- pr[j] * 4; next }
      v <- fn$f(pmin(pr[j], fn$max_pos_rank))
      ext <- pr[j] > fn$max_pos_rank
      v[ext] <- fn$max_rank + (pr[j][ext] - fn$max_pos_rank) * 4
      out[j] <- v
    }
    out
  }
  vcurve <- function(r) 10000 * exp(-0.023 * r)
  hold[, cur_val := vcurve(rank)]
  hold[, actual_val := 0]
  hold[actual_exit == FALSE, actual_val := vcurve(p2o(pos, actual_rank))]
  pred[, val := 0]
  pred[!is.na(next_rank), val := vcurve(p2o(hold$pos[row], next_rank))]
  val_sum <- pred[, list(
    val_mean = mean(val),                        # incl exit zeros = live next_value_mean
    val_med  = stats::median(val),               # incl exit zeros = live next_value_med
    val_mean_surv = mean(val[!is.na(next_rank)]),
    val_q10 = stats::quantile(val, .10),
    val_q90 = stats::quantile(val, .90)
  ), by = row]
  pred_sum[val_sum, `:=`(val_mean = i.val_mean, val_med = i.val_med,
                         val_mean_surv = i.val_mean_surv,
                         val_q10 = i.val_q10, val_q90 = i.val_q90), on = "row"]
  pred_sum[, val_pit := NA_real_]
  vpit_map <- pred[row %in% surv_rows][
    , list(val_pit = {
      a <- hold$actual_val[row[1]]
      (sum(val < a) + 0.5 * sum(val == a)) / .N
    }), by = row]
  pred_sum[vpit_map, val_pit := i.val_pit, on = "row"]

  res <- cbind(hold, pred_sum[order(row), -"row"])
  res[, `:=`(format = fmt, holdout = Y)]
  res[]
}

## ---- run the grid ----------------------------------------------------------------
players_all <- list()
for (fmt in fmts) {
  hs <- if (nzchar(env_holdouts)) as.integer(strsplit(env_holdouts, ",")[[1]]) else default_holdouts(fmt)
  for (Y in hs) {
    t0 <- Sys.time()
    r <- run_holdout(fmt, Y)
    cat(sprintf("%s %d->%d: n=%d, exit share pred %.3f / actual %.3f (%.0fs)\n",
                fmt, Y, Y + 1, nrow(r), mean(r$p_exit), mean(r$actual_exit),
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    players_all[[paste(fmt, Y)]] <- r
  }
}
players <- rbindlist(players_all)
players[, `:=`(
  exp_change_pred = val_mean / cur_val - 1,
  exp_change_med_pred = val_med / cur_val - 1,       # median-based (win_now_value basis)
  exp_change_act = actual_val / cur_val - 1,
  val_cover80 = actual_val >= val_q10 & actual_val <= val_q90,
  cover80 = actual_rank >= q10 & actual_rank <= q90,
  abs_err = abs(med_rank - actual_rank),
  age_band = cut(age, c(0, 23, 26, 29, 99), labels = c("<=23", "24-26", "27-29", "30+")),
  q_band = cut(q, c(-0.01, .25, .5, .75, 1.01), labels = c("Q1(bad)", "Q2", "Q3", "Q4(good)")),
  p_exit_bin = cut(p_exit, c(-0.01, .1, .25, .5, 1), labels = c("<10%", "10-25%", "25-50%", ">50%"))
)]

## ---- pooled metrics ---------------------------------------------------------------
cat("\n==== POOLED exit calibration (", tag, ") ====\n", sep = "")
exit_cal <- players[, list(n = .N, predicted = round(mean(p_exit), 3),
                           actual = round(mean(actual_exit), 3)),
                    by = list(format, p_exit_bin)][order(format, p_exit_bin)]
print(exit_cal)

# exit calibration by age band: shows whether shrinkage tempers noisy per-age
# exit spikes (e.g. mid-20s QBs) without flattening the genuine age trend
cat("\n==== POOLED exit calibration by age band (", tag, ") ====\n", sep = "")
exit_age <- players[!is.na(age_band), list(n = .N,
                    predicted = round(mean(p_exit), 3),
                    actual = round(mean(actual_exit), 3)),
                    by = list(format, age_band)][order(format, age_band)]
print(exit_age)

surv <- players[actual_exit == FALSE & n_surv_draws > 10]
cat("\n==== POOLED survivors: rank calibration by pos ====\n")
by_pos <- surv[, list(n = .N, cover80 = round(mean(cover80), 3),
                      pit_mean = round(mean(pit), 3),
                      pit_tail = round(mean(pit < .1 | pit > .9), 3),
                      mae_rank = round(mean(abs_err), 1)), by = list(format, pos)]
print(by_pos)
cat("\n==== POOLED survivors: VALUE-space calibration by pos ====\n")
print(surv[, list(n = .N, val_cover80 = round(mean(val_cover80), 3),
                  val_pit_mean = round(mean(val_pit), 3),
                  val_pit_tail = round(mean(val_pit < .1 | val_pit > .9), 3),
                  med_exp_pred = round(stats::median(exp_change_pred), 3),
                  med_exp_act = round(stats::median(exp_change_act), 3)),
           by = list(format, pos)])
cat("\n==== POOLED survivors: by age band ====\n")
by_age <- surv[!is.na(age_band), list(n = .N, cover80 = round(mean(cover80), 3),
                      pit_mean = round(mean(pit), 3),
                      mae_rank = round(mean(abs_err), 1)), by = list(format, age_band)][
  order(format, age_band)]
print(by_age)
cat("\n==== POOLED survivors: Q dose-response ====\n")
by_q <- surv[!is.na(q_band), list(n = .N,
                mean_actual_delta = round(mean(actual_rank - pos_rank), 1),
                mean_pred_delta = round(mean(med_rank - pos_rank), 1)),
             by = list(format, q_band)][order(format, q_band)]
print(by_q)
cat("\n==== per-holdout stability (survivors cover80 / exit gap) ====\n")
by_hold <- players[, list(
  n = .N,
  cover80 = round(mean(cover80[actual_exit == FALSE], na.rm = TRUE), 3),
  pit_tail = round(mean(pit[actual_exit == FALSE] < .1 | pit[actual_exit == FALSE] > .9, na.rm = TRUE), 3),
  exit_gap = round(mean(p_exit) - mean(actual_exit), 3)
), by = list(format, holdout)][order(format, holdout)]
print(by_hold)

## ---- write ------------------------------------------------------------------------
metrics <- rbindlist(list(
  exit_cal[, list(format, group = as.character(p_exit_bin), metric = "exit_actual",
                  n, value = actual, ref = predicted)],
  by_pos[, list(format, group = pos, metric = "cover80", n, value = cover80, ref = 0.8)],
  by_pos[, list(format, group = pos, metric = "pit_tail", n, value = pit_tail, ref = 0.2)],
  by_pos[, list(format, group = pos, metric = "mae_rank", n, value = mae_rank, ref = NA_real_)],
  by_age[, list(format, group = as.character(age_band), metric = "cover80", n, value = cover80, ref = 0.8)],
  exit_age[, list(format, group = as.character(age_band), metric = "exit_actual_byage",
                  n, value = actual, ref = predicted)],
  by_hold[, list(format, group = as.character(holdout), metric = "cover80", n, value = cover80, ref = 0.8)],
  by_hold[, list(format, group = as.character(holdout), metric = "exit_gap", n, value = exit_gap, ref = 0)]
), use.names = TRUE)
metrics[, tag := tag]
fwrite(metrics, file.path(out_dir, paste0("dynasty_backtest_multi_", tag, ".csv")))
fwrite(players[, -c("ecr", "sd")], file.path(out_dir, paste0("dynasty_backtest_players_", tag, ".csv")))
cat("\nwrote dynasty_backtest_{multi,players}_", tag, ".csv\nDONE\n", sep = "")
