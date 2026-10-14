# Everything the talk's charts need, boiled down to one small rds the deck reads.
# Inputs are the (gitignored) backtest outputs + ECR archive + the July JML sim, so
# point FFS_TALK_ROOT at the checkout that has them.
#   Rscript dev/presentation/talk_data.R
suppressPackageStartupMessages(library(data.table))
devtools::load_all(here::here(), quiet = TRUE)
ROOT <- Sys.getenv("FFS_TALK_ROOT", "C:/Github/ffsimulator")
S <- here::here("dev", "presentation")
V <- file.path(ROOT, "dev", "validate_outputs")
out <- list()

bt <- function(tag) {
  p <- fread(file.path(V, paste0("dynasty_backtest_players_", tag, ".csv")))
  p[, `:=`(pred_mv = med_rank - pos_rank, act_mv = actual_rank - pos_rank, tag = tag)]
  p[]
}
lc <- bt("log_corr"); sh <- bt("shipped"); rr <- bt("raw_rank"); rl <- bt("raw_log")

# ---- backtest: per-holdout coverage of the 80% band (survivors) -------------
out$holdout_cover <- lc[actual_exit == FALSE & !is.na(cover80),
  .(cover80 = mean(cover80), n = .N), by = .(format, holdout)][order(format, holdout)]

# ---- exit calibration: predicted p_exit vs what happened --------------------
lc[, exit_bin := cut(p_exit, c(-0.01, .1, .25, .5, 1.01), labels = c("<10%", "10-25%", "25-50%", ">50%"))]
out$exit_cal <- lc[, .(pred = mean(p_exit), actual = mean(actual_exit), n = .N), by = exit_bin][order(exit_bin)]

# ---- dose-response: season quality -> rank lost next year (survivors) -------
lc[, q_quart := cut(q, c(-0.01, .25, .5, .75, 1.01), labels = c("Bust\n(bottom 25%)", "Below par", "Above par", "Smash\n(top 25%)"))]
out$dose <- lc[actual_exit == FALSE & !is.na(q_quart),
  .(pred = mean(pred_mv), actual = mean(act_mv), n = .N), by = q_quart][order(q_quart)]

# ---- why 11 holdouts: one position's bias bounces around year to year -------
out$rb_bias <- rbind(bt("calib_1qb"), bt("calib_sflx"), fill = TRUE)[
  actual_exit == FALSE & pos == "RB" & cur_val >= 150,
  .(bias = mean(act_mv - pred_mv), n = .N), by = .(format, holdout)][order(format, holdout)]

# ---- features that didn't make it: per-holdout cover80 vs base --------------
arms <- c(base = "base", exp = "Years in league", draft = "Draft round",
          momentum = "Last year's move", sd = "Expert disagreement")
cov <- rbindlist(lapply(names(arms), function(a) {
  m <- fread(file.path(V, paste0("dynasty_backtest_multi_", a, ".csv")))
  m[metric == "cover80" & grepl("^20", group), .(format, holdout = group, cover80 = value, arm = a)]
}))
base <- cov[arm == "base", .(format, holdout, base = cover80)]
cov <- merge(cov[arm != "base"], base, by = c("format", "holdout"))
cov[, `:=`(diff = cover80 - base, label = arms[arm])]
out$features <- cov[]

# ---- the overdispersion fix -------------------------------------------------
# predicted vs actual move in log-rank units, binned (material survivors, raw kernels)
od <- rbind(rr, rl, fill = TRUE)[actual_exit == FALSE & cur_val >= 150 & !is.na(actual_rank)]
od[, `:=`(pred_l = log(med_rank / pos_rank), act_l = log(actual_rank / pos_rank))]
od[, bin := cut(frank(pred_l), 10, labels = FALSE), by = tag]
out$od_bins <- od[, .(pred = mean(pred_l), actual = mean(act_l), n = .N), by = .(tag, bin)]
out$od_fit <- od[, as.list(coef(lm(act_l ~ pred_l))), by = .(tag, pos)]
# elite WRs (positional rank 1-6): old rank-space patch vs log
ap <- rbind(sh, lc, fill = TRUE)[actual_exit == FALSE & pos == "WR" & pos_rank <= 6 & !is.na(cover80)]
out$apex <- ap[, .(cover80 = mean(cover80), pred_drop = mean(pred_mv), act_drop = mean(act_mv), n = .N), by = .(tag)]
out$apex_rows <- ap[format == "1qb", .(tag, holdout, player_name, pos_rank, med_rank, q10, q90, actual_rank, cover80)]
out$pooled <- rbind(sh, lc, fill = TRUE)[actual_exit == FALSE & cur_val >= 150 & !is.na(cover80),
  .(cover80 = mean(cover80), mae = mean(abs(actual_rank - med_rank)), n = .N), by = tag]

# ---- two markets: redraft leads dynasty (lead-lag study buckets) -------------
ll <- fread(file.path(V, "leadlag_buckets_2026-09-22.csv")); setnames(ll, 1, "Q")
out$leadlag <- ll[inseason == TRUE & h == 4 & fmt == "1qb",
  .(fwd = mean(fwd), n_anchor = .N), by = Q]

# ECR archive: within-snapshot re-rank on players common to both boards, so board
# size changes don't move anyone (the percentile-drift trap)
pan <- as.data.table(arrow::read_parquet(here::here("dev", "data", "ecr_panel.parquet"), mmap = FALSE))
out$panel_dyn <- pan[page_type == "dynasty-overall" & scrape_date >= as.Date("2025-06-01"),
  .(scrape_date, fantasypros_id, player, pos, ecr)]
out$panel_red <- pan[page_type == "redraft-overall" & scrape_date >= as.Date("2025-06-01"),
  .(scrape_date, fp_page, fantasypros_id, player, pos, ecr)]

# ---- season-quality pools + transition pool (for the technique slides) ------
sim <- readRDS(file.path(ROOT, "dev", "league_sims", "1326464763936403456", "2026-07-19", "simulation.rds"))
pools <- .ffs_dynasty_transition_pools(sim$scoring_history, format = "1qb")
out$quality_pools <- pools$quality_pools
out$transitions <- pools$transitions[, .(season, fantasypros_id,
                                         pos, age, pos_rank, next_pos_rank, exited, q, redraft_rank, total)]
out$sim_points <- as.data.table(sim$projected_scores)[
  , .(total = sum(projected_score, na.rm = TRUE)), by = .(fantasypros_id, sim_season = season)]
out$latest_rankings <- as.data.table(sim$latest_rankings)[, .(fantasypros_id, player_name = player, pos, ecr)]
out$latest_rankings[, redraft_rank := frank(ecr, ties.method = "first"), by = pos]
rm(sim); gc()

saveRDS(out, file.path(S, "talk_data_a.rds"))
cat("saved part A:", paste(names(out), collapse = ", "), "\n")
