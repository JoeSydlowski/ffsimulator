# Calibrate the two behavioral knobs against empirical targets:
#   A. lineup_noise_sd: manager evaluation error such that mean emergent
#      lineup efficiency matches the empirically observed ~0.775
#   B. ffsimulator.v3_team_rho: team-week copula strength such that simulated
#      same-team weekly score correlation matches the historical value
#
# Outputs: printed curves + dev/validate_outputs/calibration_knobs.csv

library(magrittr)
library(data.table)

devtools::load_all(here::here(), quiet = TRUE)
out_dir <- here::here("dev", "validate_outputs")
set.seed(1234)

n_sim <- 50
sim_weeks <- 1:14
pos_filter <- c("QB", "RB", "WR", "TE")

scoring_history <- readRDS(file.path(out_dir, "scoring_history_2012_2025.rds"))
scoring_history <- as.data.table(scoring_history)

rank_season <- max(fp_rankings_history()$season)
latest_rankings <- as.data.table(fp_rankings_history())[
  season == rank_season & pos %in% pos_filter,
  list(player = player_name, pos, team, ecr, sd, fantasypros_id,
       bye = 0, scrape_date = as.Date(paste0(rank_season, "-08-01")))
]

ao <- ffs_adp_outcomes(scoring_history, gp_model = "simple",
                       pos_filter = pos_filter, version = "v3")

## generic 12-team league (same construction as war_prototype.R) -------------
league_size <- 12
roster_spec <- c(QB = 2, RB = 5, WR = 6, TE = 2)
lc <- data.table(pos = c("QB", "RB", "WR", "TE"), min = c(1, 2, 3, 1),
                 max = c(1, 4, 5, 2), offense_starters = 9, total_starters = 9)

snake_draft <- function(pool) {
  open <- matrix(rep(roster_spec, league_size), nrow = league_size, byrow = TRUE,
                 dimnames = list(NULL, names(roster_spec)))
  picks <- list(); pk <- 0L
  for (rd in seq_len(sum(roster_spec))) {
    for (tm in (if (rd %% 2) 1:league_size else league_size:1)) {
      av <- pool[pos %in% names(roster_spec)[open[tm, ] > 0]]
      if (!nrow(av)) next
      s <- av[1]; pk <- pk + 1L
      picks[[pk]] <- data.table(
        league_id = "x", franchise_id = sprintf("%02d", tm),
        franchise_name = paste0("t", tm), player_id = s$fantasypros_id,
        fantasypros_id = s$fantasypros_id, pos = s$pos
      )
      open[tm, s$pos] <- open[tm, s$pos] - 1L
      pool <- pool[fantasypros_id != s$fantasypros_id]
    }
  }
  rbindlist(picks)
}

ps <- as.data.table(ffs_generate_projections(
  ao, latest_rankings, n_seasons = n_sim, weeks = sim_weeks, version = "v3"
))
ps[is.na(projected_score), projected_score := 0]
rosters <- snake_draft(latest_rankings[fantasypros_id %in% unique(ps$fantasypros_id)][order(ecr)])
rs <- ffs_score_rosters(ps, rosters)

## A. lineup noise curve ------------------------------------------------------
cat("==== A. emergent efficiency by lineup_noise_sd ====\n")
noise_curve <- rbindlist(lapply(c(0, 3, 5, 7, 9, 12), function(ns) {
  o <- ffs_optimise_lineups(rs, lc, lineup_method = "rank", lineup_noise_sd = ns)
  data.table(lineup_noise_sd = ns,
             mean_efficiency = mean(o$lineup_efficiency),
             sd_efficiency = sd(o$lineup_efficiency))
}))
print(noise_curve[, lapply(.SD, round, 4)])

## B. team-week copula rho ----------------------------------------------------

# empirical target: mean pairwise weekly-score correlation among relevant
# same-team players (>= 8 games and >= 8 ppg in a season)
sh <- scoring_history[
  !is.na(gsis_id) & !is.na(team) & week <= 16 & pos %in% pos_filter
]
relevant <- sh[, list(g = .N, ppg = mean(points)), by = list(season, gsis_id)][g >= 8 & ppg >= 8]
shr <- sh[relevant[, list(season, gsis_id)], on = c("season", "gsis_id")]

pair_corr <- function(dt, id_col) {
  dt <- dt[, list(season, team, id = get(id_col), week, points)]
  pairs <- merge(dt, dt, by = c("season", "team", "week"), allow.cartesian = TRUE)[id.x < id.y]
  pairs[, list(corr = stats::cor(points.x, points.y)), by = list(season, team, id.x, id.y)][
    !is.na(corr), list(mean_corr = mean(corr), n_pairs = .N)]
}
emp <- pair_corr(shr, "gsis_id")
cat("\n==== B. empirical same-team weekly correlation (relevant players) ====\n")
print(emp)

cat("\n==== simulated same-team correlation by rho ====\n")
rho_curve <- rbindlist(lapply(c(0, 0.3, 0.5, 0.7), function(rho) {
  options(ffsimulator.v3_team_rho = rho)
  psr <- as.data.table(ffs_generate_projections(
    ao, latest_rankings, n_seasons = 20, weeks = sim_weeks, version = "v3"
  ))
  options(ffsimulator.v3_team_rho = 0)
  psr[is.na(projected_score), projected_score := 0]
  # relevant sim players: startable ecr, real team
  psr <- psr[ecr <= 36 & !is.na(team) & team != "FA"]
  psr[, id := fantasypros_id]
  # correlation across weeks within (sim season, team) pairs
  d <- psr[, list(season = paste(season), team, id, week, points = projected_score)]
  pairs <- merge(d, d, by = c("season", "team", "week"), allow.cartesian = TRUE)[id.x < id.y]
  pc <- pairs[, list(corr = stats::cor(points.x, points.y)),
              by = list(season, team, id.x, id.y)][!is.na(corr)]
  data.table(rho = rho, sim_mean_corr = mean(pc$corr), n_pairs = nrow(pc))
}))
print(rho_curve[, lapply(.SD, round, 4)])

fwrite(rbind(
  noise_curve[, list(knob = "lineup_noise_sd", value = lineup_noise_sd, metric = mean_efficiency)],
  rho_curve[, list(knob = "team_rho", value = rho, metric = sim_mean_corr)]
), file.path(out_dir, "calibration_knobs.csv"))

cat("\nDONE\n")
