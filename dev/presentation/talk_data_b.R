# Part B of the talk data: the FantasyPros-only preseason forecasts (log + old rank
# space) boiled down to the example players and the live 2026 check.
#   Rscript dev/presentation/talk_data_b.R
suppressPackageStartupMessages(library(data.table))
devtools::load_all(here::here(), quiet = TRUE)
S <- here::here("dev", "presentation")
a <- readRDS(file.path(S, "talk_data_a.rds"))
lg <- readRDS(file.path(S, "forecast_fp_log.rds"))
rk <- readRDS(file.path(S, "forecast_fp_rank.rds"))
curve <- function(rank) 10000 * exp(-0.023 * rank)
out <- list()

o <- as.data.table(lg$outlook)
o[, fantasypros_id := as.character(fantasypros_id)]
o[, `:=`(win_later = next_value_med, win_now = cur_value - next_value_med)]
o_rank <- as.data.table(rk$outlook)[, .(fantasypros_id = as.character(fantasypros_id),
  win_later_old = next_value_med, mean_old = next_value_mean)]
o <- merge(o, o_rank, by = "fantasypros_id", all.x = TRUE)
o[, win_now_old := cur_value - win_later_old]
dh <- data.table::as.data.table(ffsimulator::fp_dynasty_history())
dh <- dh[dh$format == lg$fmt & season == max(season), .(fantasypros_id = as.character(fantasypros_id), dyn_pos_rank = pos_rank)]
o <- merge(o, unique(dh, by = "fantasypros_id"), by = "fantasypros_id", all.x = TRUE)
out$outlook <- o[, .(fantasypros_id, player_name, pos, age, redraft_rank, dyn_rank, dyn_pos_rank, cur_value,
                     next_value_mean, next_value_med, next_value_p10, next_value_p90, p_rise, p_exit,
                     win_now, win_later, win_now_old, win_later_old)]

# per-draw values for the top of the board only (keeps the rds small)
top <- o[dyn_rank <= 150]$fantasypros_id
dr <- as.data.table(lg$draws)[, fantasypros_id := as.character(fantasypros_id)][fantasypros_id %in% top]
out$draws <- dr[, .(fantasypros_id, sim_season, next_rank, exited, next_value)]
dr_old <- as.data.table(rk$draws)[, fantasypros_id := as.character(fantasypros_id)][fantasypros_id %in% top]
out$draws_old <- dr_old[, .(fantasypros_id, sim_season, next_rank, exited, next_value)]
out$sim_points <- a$sim_points[fantasypros_id %in% top]

# ---- live check: preseason forecast vs FantasyPros dynasty ranks today -------
pan <- a$panel_dyn
d <- sort(unique(pan$scrape_date))
start <- d[which.min(abs(d - as.Date("2026-07-17")))]
now <- max(d)
s0 <- pan[scrape_date == start]; s1 <- pan[scrape_date == now]
# rank today only among players who were on the July board, so new entrants
# (rookies added since) push nobody down; anyone who fell off goes to the bottom
s0b <- s0[, .(fantasypros_id, r0 = frank(ecr, ties.method = "first"))]
s1b <- s1[fantasypros_id %in% s0$fantasypros_id][, .(fantasypros_id, r1 = frank(ecr, ties.method = "first"))]
gone <- setdiff(s0$fantasypros_id, s1$fantasypros_id)
s1b <- rbind(s1b, data.table(fantasypros_id = gone, r1 = nrow(s1b) + 1L))
m <- merge(o[, .(fantasypros_id, player_name, pos, age, pred_chg = next_value_med / cur_value - 1,
                 pred_chg_old = win_later_old / cur_value - 1)],
           merge(s0b, s1b, by = "fantasypros_id"),
           by = "fantasypros_id")
m[, real_chg := curve(r1) / curve(r0) - 1]
m[, tier := cut(r0, c(0, 24, 60, 120, Inf), labels = c("1-24", "25-60", "61-120", "121+"))]
out$live <- m[]
out$live_meta <- list(start = start, now = now, n_gone_roster = sum(o$fantasypros_id %in% gone))

sp <- function(x, y) suppressWarnings(cor(x, y, method = "spearman", use = "complete.obs"))
set.seed(1)
boot <- function(dt, B = 2000) quantile(replicate(B, { i <- sample.int(nrow(dt), replace = TRUE); sp(dt$pred_chg[i], dt$real_chg[i]) }), c(.05, .95))
out$live_ic <- rbind(
  m[, .(tier = "All", n = .N, ic = sp(pred_chg, real_chg), ic_age = sp(-age, real_chg),
        lo = boot(.SD)[1], hi = boot(.SD)[2])],
  m[as.integer(tier) <= 3, .(tier = "Top 120", n = .N, ic = sp(pred_chg, real_chg), ic_age = sp(-age, real_chg),
        lo = boot(.SD)[1], hi = boot(.SD)[2])],
  m[, .(n = .N, ic = sp(pred_chg, real_chg), ic_age = sp(-age, real_chg), lo = NA_real_, hi = NA_real_), by = tier][order(tier)][, tier := as.character(tier)]
)
print(out$live_ic)
cat("live window:", format(start), "->", format(now), " players:", nrow(m), "\n")

saveRDS(out, file.path(S, "talk_data_b.rds"))
cat("saved part B\n")
