# Backtest gate for the dynasty transition model.
#
# Train transition pools on 2018->2024 (max_transition_season = 2025 limits
# the dynasty data to seasons <= 2025, so observed pairs run through
# 2024->2025). Then for every player in the 2025 dynasty rankings, given his
# ACTUAL 2025 season quality, draw K next-year transitions and compare the
# predicted 2026 dynasty rank distribution (and exit probability) against the
# actual 2026 dynasty rankings.
#
# Outputs: dev/validate_outputs/dynasty_backtest_*.csv + printed verdict

library(data.table)
devtools::load_all(here::here(), quiet = TRUE)
set.seed(42)

out_dir <- here::here("dev", "validate_outputs")
K <- 300

scoring_history <- readRDS(file.path(out_dir, "scoring_history_2012_2025.rds"))

dynasty <- as.data.table(fp_dynasty_history())

# training pools: transitions through 2024->2025 only
pools <- ffsimulator:::.ffs_dynasty_transition_pools(
  scoring_history = scoring_history,
  max_transition_season = 2025
)
tr <- pools$transitions
cat("training transitions:", nrow(tr), "| seasons:", paste(range(tr$season), collapse = "-"),
    "| exit share:", round(mean(tr$exited), 3), "\n")

# holdout: 2025 dynasty players with their ACTUAL 2025 season quality
hold <- dynasty[season == 2025]
redraft <- as.data.table(fp_rankings_history())[
  season == 2025, list(fantasypros_id, redraft_rank = rank)
]
hold <- merge(hold, redraft, by = "fantasypros_id", all.x = TRUE)

dp_id <- as.data.table(ffscrapr::dp_playerids())[
  !is.na(gsis_id) & !is.na(fantasypros_id), c("fantasypros_id", "gsis_id")
]
totals25 <- merge(
  as.data.table(scoring_history)[season == 2025 & week %in% 1:14 & !is.na(gsis_id),
                                 list(gsis_id, points)],
  dp_id, by = "gsis_id"
)[, list(total = sum(points)), by = fantasypros_id]
hold <- merge(hold, totals25, by = "fantasypros_id", all.x = TRUE)
hold[is.na(total), total := 0]
hold[, q := ffsimulator:::.ffs_season_quality(pos, redraft_rank, total, pools$quality_pools)]

# actual 2026 outcomes
actual26 <- dynasty[season == 2026, list(fantasypros_id, actual_rank = pos_rank)]
hold <- merge(hold, actual26, by = "fantasypros_id", all.x = TRUE)
hold[, actual_exit := is.na(actual_rank)]

# K draws per player
idx <- rep(seq_len(nrow(hold)), each = K)
draws <- ffsimulator:::.ffs_draw_transition(
  pos = hold$pos[idx], age = hold$age[idx],
  dyn_pos_rank = hold$pos_rank[idx], q = hold$q[idx],
  transitions = tr
)
pred <- data.table(
  fantasypros_id = hold$fantasypros_id[idx],
  next_rank = draws[[1]], exited = draws[[2]]
)

pred_sum <- pred[, list(
  p_exit = mean(exited),
  med_rank = stats::median(next_rank, na.rm = TRUE),
  q10 = stats::quantile(next_rank, .10, na.rm = TRUE),
  q90 = stats::quantile(next_rank, .90, na.rm = TRUE),
  draws = list(next_rank[!is.na(next_rank)])
), by = fantasypros_id]
res <- merge(hold, pred_sum, by = "fantasypros_id")

## ---- exit calibration --------------------------------------------------------
res[, p_exit_bin := cut(p_exit, c(-0.01, .1, .25, .5, 1), labels = c("<10%", "10-25%", "25-50%", ">50%"))]
exit_cal <- res[, list(n = .N, predicted = mean(p_exit), actual = mean(actual_exit)), by = p_exit_bin][order(p_exit_bin)]
cat("\n==== exit calibration (predicted bucket vs actual exit rate) ====\n")
print(exit_cal[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---- rank calibration for players who survived --------------------------------
surv <- res[actual_exit == FALSE & lengths(draws) > 10]
surv[, pit := mapply(function(d, a) (sum(d < a) + 0.5 * sum(d == a)) / length(d), draws, actual_rank)]
surv[, `:=`(
  cover80 = actual_rank >= q10 & actual_rank <= q90,
  abs_err = abs(med_rank - actual_rank),
  age_band = cut(age, c(0, 23, 26, 29, 99), labels = c("<=23", "24-26", "27-29", "30+"))
)]

cat("\n==== survivors: rank calibration by pos ====\n")
print(surv[, list(n = .N, cover80 = round(mean(cover80), 3),
                  pit_mean = round(mean(pit), 3),
                  pit_tail = round(mean(pit < .1 | pit > .9), 3),
                  mae_rank = round(mean(abs_err), 1)), by = pos])
cat("\n==== survivors: rank calibration by age band ====\n")
print(surv[, list(n = .N, cover80 = round(mean(cover80), 3),
                  pit_mean = round(mean(pit), 3),
                  mae_rank = round(mean(abs_err), 1)), by = age_band][order(age_band)])
cat("\n==== survivors: by season-quality quartile (did Q do its job?) ====\n")
surv[, q_band := cut(q, c(-0.01, .25, .5, .75, 1.01), labels = c("Q1(bad)", "Q2", "Q3", "Q4(good)"))]
print(surv[!is.na(q_band), list(n = .N, mean_actual_delta = round(mean(actual_rank - pos_rank), 1),
                                mean_pred_delta = round(mean(med_rank - pos_rank), 1)), by = q_band][order(q_band)])

fwrite(res[, -"draws"], file.path(out_dir, "dynasty_backtest_players.csv"))
fwrite(exit_cal, file.path(out_dir, "dynasty_backtest_exit_cal.csv"))
cat("\nDONE\n")
