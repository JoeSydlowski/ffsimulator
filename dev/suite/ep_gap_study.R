# Does scoring below your OPPORTUNITY predict a dynasty re-rate? - gated study
#
#   Rscript dev/suite/ep_gap_study.R
#   FFS_EP_HORIZONS=2,4,8 FFS_EP_TIER=0.33 Rscript dev/suite/ep_gap_study.R
#
# dev/suite/leadlag_study.R showed the redraft BOARD leads the dynasty board.
# This asks whether the underlying FUNDAMENTAL leads it too, and whether it adds
# anything once the redraft board is already in hand.
#
#   gap = expected fantasy points/g - actual fantasy points/g, season to date
#         (nflreadr::load_ff_opportunity, weeks completed before the anchor)
#   + gap = scored BELOW his opportunity -> should regress UP if the market
#           eventually pays for opportunity rather than for realised points
#
# Outcome is the same as the lead-lag study: the player's dynasty percentile move
# over the next h weeks, on a common-universe re-rank, never crossing the
# September draft-board -> rest-of-season page switch.
#
# SIGN CONVENTION: `ic` is reported as -cor(gap, dyn_fwd), so a POSITIVE ic means
# the signal worked as hypothesised (scored below opportunity -> ranking improves).
# dyn_fwd itself is a percentile move where + = his ranking got WORSE.
#
# The test that matters is `ic_incr`: the gap's correlation with the forward move
# after partialling out BOTH the dynasty board's own momentum AND the redraft-ECR
# signal we already ship. If that is ~0, the fundamental is already in the board
# and there is nothing new to trade.
#
# Outputs to dev/validate_outputs/:
#   ep_gap_anchors_<tag>.csv, ep_gap_ageband_<tag>.csv, ep_gap_study_<tag>.txt

library(data.table)

ECR_ARCHIVE_SOURCE_ONLY <- TRUE
source(here::here("dev", "suite", "ecr_archive.R"))
suppressMessages(devtools::load_all(here::here(), quiet = TRUE))

config <- list(
  horizons = as.integer(strsplit(Sys.getenv("FFS_EP_HORIZONS", "2,4,8"), ",")[[1]]),
  tier = as.numeric(Sys.getenv("FFS_EP_TIER", "0.33")),
  tol_days = as.integer(Sys.getenv("FFS_EP_TOL", "5")),
  min_n = as.integer(Sys.getenv("FFS_EP_MIN_N", "40")),
  min_games = as.integer(Sys.getenv("FFS_EP_MIN_GAMES", "2")),
  boot = as.integer(Sys.getenv("FFS_EP_BOOT", "2000")),
  seasons = as.integer(strsplit(Sys.getenv("FFS_EP_SEASONS", "2020,2021,2022,2023,2024,2025,2026"), ",")[[1]]),
  tag = Sys.getenv("FFS_EP_TAG", format(Sys.Date()))
)
formats <- list(`1qb` = "dynasty-overall", superflex = "dynasty-op")
out_dir <- here::here("dev", "validate_outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("reading ecr panel @ ", Sys.time())
panel <- ecr_panel_read(pages = c("dynasty-overall", "dynasty-op", "redraft-overall"))

## ---- opportunity, and when each week finished -------------------------------
message("loading opportunity + schedules @ ", Sys.time())
sched <- data.table::as.data.table(nflreadr::load_schedules(config$seasons))
wk_end <- sched[!is.na(gameday), list(ended = max(as.Date(gameday))),
                by = list(season, week)]

opp <- data.table::rbindlist(lapply(config$seasons, function(s) {
  o <- tryCatch(data.table::as.data.table(
    nflreadr::load_ff_opportunity(s, stat_type = "weekly")), error = function(e) NULL)
  if (is.null(o) || !nrow(o)) return(NULL)
  o[, list(season = s, week, gsis_id = player_id, pos = position,
           ep = total_fantasy_points_exp, fp = total_fantasy_points)]
}), fill = TRUE)
opp <- opp[!is.na(ep) & pos %chin% c("QB", "RB", "WR", "TE")]

ids <- data.table::as.data.table(nflreadr::load_ff_playerids())
ids <- unique(ids[!is.na(fantasypros_id) & !is.na(gsis_id),
                  list(gsis_id = as.character(gsis_id),
                       fantasypros_id = as.character(fantasypros_id))], by = "gsis_id")
opp[, gsis_id := as.character(gsis_id)]

# THE DENOMINATOR. load_ff_opportunity only has rows for players with expected
# points, so a week of offensive snaps with no touch is simply absent (18% of
# snap-weeks in 2026). Averaging over its rows alone divides by the wrong number
# and inflates every per-game rate. Add those weeks back as real zeros.
snaps <- data.table::rbindlist(lapply(config$seasons, function(s) {
  x <- tryCatch(data.table::as.data.table(nflreadr::load_snap_counts(s)),
                error = function(e) NULL)
  if (is.null(x) || !nrow(x)) return(NULL)
  x[game_type == "REG" & offense_snaps > 0,
    list(season = s, week, pfr_id = as.character(pfr_player_id), position)]
}), fill = TRUE)
pfr_map <- data.table::as.data.table(nflreadr::load_ff_playerids())
pfr_map <- unique(pfr_map[!is.na(pfr_id) & !is.na(gsis_id),
                          list(pfr_id = as.character(pfr_id),
                               gsis_id = as.character(gsis_id))], by = "pfr_id")
act <- merge(snaps[position %chin% c("QB", "RB", "WR", "TE")], pfr_map, by = "pfr_id")
act <- unique(act[, list(season, week, gsis_id, pos = position)])
miss <- act[!unique(opp[, list(season, week, gsis_id)]), on = c("season", "week", "gsis_id")]
cat("zero-opportunity active weeks added:", nrow(miss), "\n")
opp <- rbind(opp, miss[, list(season, week, gsis_id, pos, ep = 0, fp = 0)], fill = TRUE)

opp <- merge(opp, ids, by = "gsis_id")
opp <- merge(opp, wk_end, by = c("season", "week"))
cat("opportunity rows:", nrow(opp), "| seasons:",
    paste(sort(unique(opp$season)), collapse = ","), "\n")

ages <- tryCatch({
  d <- data.table::as.data.table(ffscrapr::dp_playerids())
  unique(d[!is.na(fantasypros_id) & !is.na(birthdate),
           list(fantasypros_id = as.character(fantasypros_id),
                birthdate = as.Date(birthdate))], by = "fantasypros_id")
}, error = function(e) NULL)

## ---- helpers (same as the lead-lag study) -----------------------------------
.near <- function(dates, target, tol) {
  i <- which.min(abs(as.numeric(dates - target)))
  if (length(i) && abs(as.numeric(dates[i] - target)) <= tol) dates[i] else as.Date(NA)
}
.season_of <- function(d) as.integer(format(d, "%Y")) - (as.integer(format(d, "%m")) <= 2L)
.safe_cor <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 5 || stats::sd(x[ok]) == 0 || stats::sd(y[ok]) == 0) return(NA_real_)
  suppressWarnings(stats::cor(x[ok], y[ok], method = "spearman"))
}
.block_boot_t <- function(x, block, n_boot) {
  x <- x[is.finite(x)]; n <- length(x)
  if (n < 5) return(c(mean = mean(x), se = NA_real_, t = NA_real_))
  block <- max(1L, min(as.integer(block), n)); nb <- ceiling(n / block)
  starts <- seq_len(n - block + 1L)
  mus <- vapply(seq_len(n_boot), function(i) {
    idx <- unlist(lapply(sample(starts, nb, replace = TRUE), function(s) s:(s + block - 1L)))
    mean(x[idx[seq_len(n)]])
  }, numeric(1))
  se <- stats::sd(mus)
  c(mean = mean(x), se = se, t = if (is.finite(se) && se > 0) mean(x) / se else NA_real_)
}

## ---- per-anchor measurement --------------------------------------------------
measure <- function(dyn_page, h) {
  dyn <- panel[page_type == dyn_page,
               list(fantasypros_id, pos, dt = scrape_date, e = ecr)]
  rdr <- panel[page_type == "redraft-overall",
               list(fantasypros_id, dt = scrape_date, e = ecr, page = fp_page)]
  rdr_page <- unique(rdr[, list(dt, page)])
  dates <- sort(unique(dyn$dt))
  # in-season only: the gap does not exist in the off-season
  dates <- dates[as.integer(format(dates, "%m")) %in% 9:12]
  lag_days <- 7L * h

  anchors <- list(); ageband <- list()
  for (t0 in dates) {
    t0 <- as.Date(t0, origin = "1970-01-01")
    tm <- .near(dates, t0 - lag_days, config$tol_days)
    tp <- .near(dates, t0 + lag_days, config$tol_days)
    if (is.na(tm) || is.na(tp) || tm >= t0 || tp <= t0) next
    pg <- rdr_page[dt %in% c(tm, t0), unique(page)]
    if (length(pg) != 1L) next

    # opportunity through the last week that FINISHED before the anchor
    o <- opp[ended < t0 & season == .season_of(t0)]
    if (!nrow(o)) next
    o <- o[, list(games = .N, ep_g = mean(ep, na.rm = TRUE), fp_g = mean(fp, na.rm = TRUE)),
           by = fantasypros_id]
    o <- o[games >= config$min_games]
    o[, gap := ep_g - fp_g]
    if (nrow(o) < config$min_n) next

    ids_c <- Reduce(intersect, list(
      dyn[dt == t0, fantasypros_id], dyn[dt == tm, fantasypros_id],
      rdr[dt == t0, fantasypros_id], rdr[dt == tm, fantasypros_id], o$fantasypros_id))
    if (length(ids_c) < config$min_n) next
    pctize <- function(src, when, col) {
      x <- src[dt == when & fantasypros_id %chin% ids_c]
      x[, (col) := data.table::frank(e, ties.method = "average") / .N]
      x[, c("fantasypros_id", col), with = FALSE]
    }
    a <- merge(dyn[dt == t0 & fantasypros_id %chin% ids_c, list(fantasypros_id, pos)],
               pctize(dyn, t0, "d0"), by = "fantasypros_id")
    a <- merge(a, pctize(dyn, tm, "dm"), by = "fantasypros_id")
    a <- merge(a, pctize(rdr, t0, "r0"), by = "fantasypros_id")
    a <- merge(a, pctize(rdr, tm, "rm"), by = "fantasypros_id")
    fwd <- dyn[dt == tp & fantasypros_id %chin% ids_c]
    fwd[, dp := data.table::frank(e, ties.method = "average") / .N]
    a <- merge(a, fwd[, list(fantasypros_id, dp)], by = "fantasypros_id", all.x = TRUE)
    # falling off the board IS decline, so exits stay in as the worst outcome -
    # but they are also the one way this result could be mechanical (if
    # underperformers simply vanish), so the survivor-only IC is reported too
    a[, exited := is.na(dp)]
    a[is.na(dp), dp := 1.0]
    a <- merge(a, o[, list(fantasypros_id, gap, ep_g, fp_g, games)], by = "fantasypros_id")
    a <- a[d0 <= config$tier]
    if (nrow(a) < config$min_n) next

    a[, fast := r0 - rm][, dyn_past := d0 - dm][, dyn_fwd := dp - d0]

    # raw, then net of the dynasty board's own momentum, then ALSO net of the
    # redraft-ECR signal we already ship - that last one is the real question
    ry_mom <- stats::resid(stats::lm(rank(dyn_fwd) ~ rank(dyn_past), a))
    rg_mom <- stats::resid(stats::lm(rank(gap) ~ rank(dyn_past), a))
    ry_all <- stats::resid(stats::lm(rank(dyn_fwd) ~ rank(dyn_past) + rank(fast), a))
    rg_all <- stats::resid(stats::lm(rank(gap) ~ rank(dyn_past) + rank(fast), a))

    anchors[[length(anchors) + 1L]] <- data.table(
      t0 = t0, h = h, dyn_page = dyn_page, n = nrow(a), season = .season_of(t0),
      # sign flipped so positive = hypothesis held
      ic_raw = -.safe_cor(a$gap, a$dyn_fwd),
      ic_partial = -.safe_cor(rg_mom, ry_mom),
      ic_incr = -.safe_cor(rg_all, ry_all),
      ic_ecr = .safe_cor(a$fast, a$dyn_fwd),          # the known signal, for scale
      cor_gap_fast = .safe_cor(a$gap, a$fast),
      exit_rate = mean(a$exited),
      # same incremental test on SURVIVORS only
      ic_incr_surv = {
        s <- a[exited == FALSE]
        if (nrow(s) < config$min_n) NA_real_ else
          -.safe_cor(stats::resid(stats::lm(rank(gap) ~ rank(dyn_past) + rank(fast), s)),
                     stats::resid(stats::lm(rank(dyn_fwd) ~ rank(dyn_past) + rank(fast), s)))
      }
    )

    if (!is.null(ages)) {
      aa <- merge(a, ages, by = "fantasypros_id", all.x = TRUE)
      aa[, age := as.numeric(difftime(t0, birthdate, units = "days")) / 365.25]
      aa[, age_band := cut(age, c(-Inf, 23, 26, 29, Inf),
                           labels = c("<=23", "24-26", "27-29", "30+"))]
      ab <- aa[!is.na(age_band) & .N >= 20,
               list(t0 = t0, h = h, dyn_page = dyn_page, n = .N,
                    ic = -.safe_cor(gap, dyn_fwd)), by = age_band]
      if (nrow(ab)) ageband[[length(ageband) + 1L]] <- ab
    }
  }
  list(anchors = data.table::rbindlist(anchors),
       ageband = data.table::rbindlist(ageband))
}

message("measuring @ ", Sys.time())
res <- list()
for (f in names(formats)) for (hh in config$horizons) {
  message("  ", f, " h=", hh, "w @ ", Sys.time())
  m <- measure(formats[[f]], hh)
  if (!nrow(m$anchors)) next
  m$anchors[, fmt := f]
  if (nrow(m$ageband)) m$ageband[, fmt := f]
  res[[paste(f, hh)]] <- m
}
anchors <- data.table::rbindlist(lapply(res, `[[`, "anchors"))
ageband <- data.table::rbindlist(lapply(res, `[[`, "ageband"), fill = TRUE)
stopifnot(nrow(anchors) > 0)

## ---- verdict -----------------------------------------------------------------
sink_path <- file.path(out_dir, paste0("ep_gap_study_", config$tag, ".txt"))
con <- file(sink_path, open = "wt")
say <- function(...) { cat(..., "\n", sep = ""); cat(..., "\n", sep = "", file = con) }

say("expected-points gap study  ", as.character(Sys.Date()))
say("signal: EP/g - FP/g, season to date, weeks finished before the anchor")
say("positive ic = scored below opportunity -> dynasty ranking then IMPROVED")
say("universe: dynasty pct <= ", config$tier, ", in-season anchors only, >= ",
    config$min_games, " games played")
say("anchors: ", nrow(anchors), "   ", as.character(min(anchors$t0)), " .. ",
    as.character(max(anchors$t0)))

for (f in names(formats)) for (hh in config$horizons) {
  A <- anchors[fmt == f & h == hh]
  if (!nrow(A)) next
  b <- .block_boot_t(A$ic_partial, block = hh, n_boot = config$boot)
  bi <- .block_boot_t(A$ic_incr, block = hh, n_boot = config$boot)
  say("")
  say("== ", f, "  h=", hh, " weeks  (", nrow(A), " anchors, median n=", median(A$n), ")")
  say(sprintf("   IC raw                        %+.3f", mean(A$ic_raw, na.rm = TRUE)))
  say(sprintf("   IC net of dynasty momentum    %+.3f  block-boot t=%.1f", b[["mean"]], b[["t"]]))
  say(sprintf("   IC ALSO net of redraft ECR    %+.3f  block-boot t=%.1f   <- the real test",
              bi[["mean"]], bi[["t"]]))
  say(sprintf("   (redraft ECR signal itself    %+.3f)", mean(A$ic_ecr, na.rm = TRUE)))
  say(sprintf("   corr(gap, redraft ECR move)   %+.3f", mean(A$cor_gap_fast, na.rm = TRUE)))
  say(sprintf("   IC incremental, SURVIVORS only %+.3f   (exit rate %.3f)",
              mean(A$ic_incr_surv, na.rm = TRUE), mean(A$exit_rate, na.rm = TRUE)))
  per <- A[, list(ic = mean(ic_incr, na.rm = TRUE), k = .N), by = season][order(season)][k >= 3]
  say("   per season (incremental): ",
      paste(sprintf("%d %+.3f", per$season, per$ic), collapse = "  "))
  say("   seasons positive: ", sum(per$ic > 0, na.rm = TRUE), "/", nrow(per))
}

if (nrow(ageband)) {
  say("")
  say("== AGE SPLIT (raw ic, all horizons pooled)")
  ab <- ageband[, list(anchors = .N, n = as.integer(median(n)), ic = mean(ic, na.rm = TRUE)),
                by = list(fmt, age_band)][order(fmt, age_band)]
  for (i in seq_len(nrow(ab))) say(sprintf("   %-9s %-6s n~%3d  ic %+.3f",
                                           ab$fmt[i], ab$age_band[i], ab$n[i], ab$ic[i]))
}

say("")
say("== GATE")
gate <- anchors[, {
  per <- .SD[, list(ic = mean(ic_incr, na.rm = TRUE), k = .N), by = season][k >= 3]
  bb <- .block_boot_t(ic_incr, block = h[1], n_boot = config$boot)
  list(seasons = nrow(per), seasons_pos = sum(per$ic > 0, na.rm = TRUE),
       ic = bb[["mean"]], t = bb[["t"]])
}, by = list(fmt, h)]
gate[, pass := seasons_pos >= pmax(1L, seasons - 1L) & t > 2]
for (i in seq_len(nrow(gate))) {
  g <- gate[i]
  say(sprintf("   %-9s h=%dw  seasons %d/%d positive  incremental IC %+.3f (t=%.1f)  -> %s",
              g$fmt, g$h, g$seasons_pos, g$seasons, g$ic, g$t,
              if (isTRUE(g$pass)) "PASS" else "FAIL"))
}
say("")
say("gate = incremental IC (net of dynasty momentum AND the redraft board)")
say("positive in all but at most one season, block-boot t > 2.")

close(con)
data.table::fwrite(anchors, file.path(out_dir, paste0("ep_gap_anchors_", config$tag, ".csv")))
if (nrow(ageband)) data.table::fwrite(ageband, file.path(out_dir, paste0("ep_gap_ageband_", config$tag, ".csv")))
message("wrote ", sink_path, " @ ", Sys.time())
