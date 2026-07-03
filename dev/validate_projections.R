# Backtest v1 vs v2 projection methods against held-out seasons
#
# For each holdout season Y in 2019:2025:
#   - training data = seasons 2012:(Y-1) only. Rankings history is filtered
#     via the ffsimulator.cache_directory option so that the v2 draft->week
#     crosswalk (which reads fp_rankings_history* directly) cannot see season Y.
#     (Known mild leakage: fp_injury_table is fit on all seasons.)
#   - generate v1 and v2 projections from season Y's preseason rankings
#   - compare simulated distributions against actual weekly scoring
#
# Also quantifies, directly from historical data (no simulation):
#   - the bye-week distortion: actual points at weekly rank N conditioned on
#     how many teams are on bye that week
#   - crosswalk survivorship: preseason-ranked player-seasons that never
#     received a weekly ranking
#
# Outputs: dev/validate_outputs/*.csv (findings written up separately in
# dev/validate_projections.md)

library(magrittr)
library(data.table)

devtools::load_all(here::here(), quiet = TRUE)

out_dir <- here::here("dev", "validate_outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

holdouts <- 2019:2025
n_sim_seasons <- 200
sim_weeks <- 1:14
pos_filter <- c("QB", "RB", "WR", "TE")

set.seed(613)

## ---- shared data ----------------------------------------------------------

sh_cache <- file.path(out_dir, "scoring_history_2012_2025.rds")
if (file.exists(sh_cache)) {
  scoring_history <- readRDS(sh_cache)
} else {
  conn <- ffscrapr::mfl_connect(2021, 47747)
  scoring_history <- ffscrapr::ff_scoringhistory(conn, 2012:2025)
  saveRDS(scoring_history, sh_cache)
}
scoring_history <- as.data.table(scoring_history)

fp_draft_full <- as.data.table(fp_rankings_history())
fp_week_full <- as.data.table(fp_rankings_history_week())
injury_full <- fp_injury_table()

dp_id <- as.data.table(ffscrapr::dp_playerids())[
  !is.na(gsis_id) & !is.na(fantasypros_id), c("fantasypros_id", "gsis_id")
]

# team bye weeks per season from nflverse schedules (weeks 1:14 only)
schedules <- as.data.table(nflreadr::load_schedules(min(holdouts):max(holdouts)))[
  game_type == "REG" & week <= max(sim_weeks)
]
team_weeks <- unique(rbind(
  schedules[, list(season, week, team = home_team)],
  schedules[, list(season, week, team = away_team)]
))
team_weeks[, team := nflreadr::clean_team_abbrs(team)]
all_teams <- unique(team_weeks[, list(season, team)])
byes <- all_teams[
  , list(week = setdiff(seq_len(max(sim_weeks)), team_weeks[season == .BY$season & team == .BY$team, week])),
  by = list(season, team)
][, list(bye = week[1]), by = list(season, team)]

## ---- backtest loop --------------------------------------------------------

pit_of <- function(sim, actual) {
  # randomized PIT so ties (esp. all-zero sims) don't pile up at 0/1
  (sum(sim < actual) + stats::runif(1) * sum(sim == actual)) / length(sim)
}

player_results <- list()

for (Y in holdouts) {
  message("=== holdout season ", Y, " ===")

  # dependency-inject rankings filtered to seasons < Y through the cache dir
  cache_y <- file.path(tempdir(), paste0("ffs_holdout_", Y))
  dir.create(cache_y, recursive = TRUE, showWarnings = FALSE)
  saveRDS(fp_draft_full[season < Y], file.path(cache_y, "fp_rankings_history.rds"))
  saveRDS(fp_week_full[season < Y], file.path(cache_y, "fp_rankings_history_week.rds"))
  saveRDS(injury_full, file.path(cache_y, "fp_injury_table.rds"))
  options(ffsimulator.cache_directory = cache_y)

  sh_train <- scoring_history[season < Y]
  sh_test <- scoring_history[season == Y & week <= max(sim_weeks)]

  # season-Y preseason rankings -> latest_rankings shape
  latest_rankings <- fp_draft_full[
    season == Y & pos %in% pos_filter,
    list(
      player = player_name, pos,
      team = nflreadr::clean_team_abbrs(team),
      ecr, sd, fantasypros_id,
      scrape_date = as.Date(paste0(Y, "-08-01"))
    )
  ]
  latest_rankings <- merge(
    latest_rankings, byes[season == Y, list(team, bye)],
    by = "team", all.x = TRUE
  )
  latest_rankings[is.na(bye), bye := 0]

  # actual weekly points for season Y keyed by fantasypros_id
  actual <- merge(
    sh_test[!is.na(gsis_id), list(gsis_id, week, points)],
    dp_id, by = "gsis_id"
  )[
    , list(actual_total = sum(points), actual_weeks = .N), by = "fantasypros_id"
  ]

  for (v in c("v1", "v2")) {
    ao <- ffs_adp_outcomes(sh_train, gp_model = "simple",
                           pos_filter = pos_filter, version = v)
    ps <- ffs_generate_projections(
      adp_outcomes = ao,
      latest_rankings = latest_rankings,
      n_seasons = n_sim_seasons,
      weeks = sim_weeks,
      version = v
    )
    ps <- as.data.table(ps)
    ps[is.na(projected_score), projected_score := 0] # NA draw = not relevant that week

    sim_totals <- ps[
      , list(sim_total = sum(projected_score)),
      by = list(fantasypros_id, player, pos, ecr, sim_season = season)
    ]

    res <- sim_totals[
      , list(
        sim_mean = mean(sim_total),
        sim_sd = sd(sim_total),
        q10 = quantile(sim_total, .10),
        q25 = quantile(sim_total, .25),
        q75 = quantile(sim_total, .75),
        q90 = quantile(sim_total, .90),
        sim_draws = list(sim_total)
      ),
      by = list(fantasypros_id, player, pos, ecr)
    ]
    res <- merge(res, actual, by = "fantasypros_id", all.x = TRUE)
    # ranked player with no actual rows = played zero relevant weeks
    res[is.na(actual_total), `:=`(actual_total = 0, actual_weeks = 0)]
    res[, pit := mapply(pit_of, sim_draws, actual_total)]
    res[, `:=`(
      cover50 = actual_total >= q25 & actual_total <= q75,
      cover80 = actual_total >= q10 & actual_total <= q90,
      abs_err = abs(sim_mean - actual_total),
      sim_draws = NULL
    )]
    res[, `:=`(holdout = Y, version = v)]

    # how many ranked players never made it into the simulation (dropout)
    dropped <- latest_rankings[!fantasypros_id %in% unique(ps$fantasypros_id)]
    res_drop <- data.table(
      holdout = Y, version = v, pos = dropped$pos,
      fantasypros_id = dropped$fantasypros_id, player = dropped$player,
      ecr = dropped$ecr, dropped = TRUE
    )

    player_results[[paste(Y, v)]] <- rbind(res, res_drop, fill = TRUE)
    message("  ", v, ": ", nrow(res), " players simulated, ",
            nrow(dropped), " ranked players dropped")
  }

  options(ffsimulator.cache_directory = NULL)
}

player_results <- rbindlist(player_results, fill = TRUE)
player_results[is.na(dropped), dropped := FALSE]
fwrite(player_results, file.path(out_dir, "backtest_player_results.csv"))

## ---- summary metrics ------------------------------------------------------

rank_tier <- function(ecr) cut(ecr, c(0, 12, 24, 36, Inf),
                               labels = c("1-12", "13-24", "25-36", "37+"))

summary_metrics <- player_results[dropped == FALSE][
  , tier := rank_tier(ecr)
][
  , list(
    n = .N,
    cover50 = mean(cover50),
    cover80 = mean(cover80),
    mae = mean(abs_err),
    pit_mean = mean(pit),
    # under-dispersion shows up as PIT piling into the tails (U-shape):
    pit_tail_share = mean(pit < .1 | pit > .9), # ideal = 0.20
    spearman_by_pos = NA_real_
  ),
  by = list(version, pos, tier)
][order(version, pos, tier)]

spearman <- player_results[dropped == FALSE][
  , list(spearman = cor(sim_mean, actual_total, method = "spearman")),
  by = list(version, pos, holdout)
][, list(spearman = mean(spearman)), by = list(version, pos)]

fwrite(summary_metrics, file.path(out_dir, "backtest_summary.csv"))
fwrite(spearman, file.path(out_dir, "backtest_spearman.csv"))

cat("\n==== coverage / calibration by version x pos x preseason tier ====\n")
print(summary_metrics, nrows = 200)
cat("\n==== spearman(mean sim, actual) by version x pos ====\n")
print(spearman)

drop_summary <- player_results[
  , list(n_ranked = .N, n_dropped = sum(dropped)),
  by = list(version, pos, tier = rank_tier(ecr))
][order(version, pos, tier)]
fwrite(drop_summary, file.path(out_dir, "backtest_dropout.csv"))
cat("\n==== ranked players dropped from sim (crosswalk/merge losses) ====\n")
print(drop_summary, nrows = 100)

## ---- bye-week distortion (data-level, all seasons) ------------------------

sched_all <- as.data.table(nflreadr::load_schedules(2012:2025))[
  game_type == "REG" & week <= 16
]
teams_per_week <- unique(rbind(
  sched_all[, list(season, week, team = home_team)],
  sched_all[, list(season, week, team = away_team)]
))[, list(n_playing = .N), by = list(season, week)]
teams_per_season <- unique(rbind(
  sched_all[, list(season, team = home_team)],
  sched_all[, list(season, team = away_team)]
))[, list(n_teams = .N), by = season]
bye_counts <- merge(teams_per_week, teams_per_season, by = "season")
bye_counts[, n_on_bye := n_teams - n_playing]

wk_actual <- merge(
  fp_week_full[pos %in% pos_filter,
               list(season, week, fantasypros_id, pos, rank)],
  dp_id, by = "fantasypros_id"
)
wk_actual <- merge(
  wk_actual,
  scoring_history[!is.na(gsis_id), list(season, week, gsis_id, points)],
  by = c("season", "week", "gsis_id")
)
wk_actual <- merge(wk_actual, bye_counts[, list(season, week, n_on_bye)],
                   by = c("season", "week"))
wk_actual[, bye_bucket := cut(n_on_bye, c(-1, 0, 2, 4, 8),
                              labels = c("0", "1-2", "3-4", "5+"))]
wk_actual[, rank_band := cut(rank, c(0, 6, 12, 18, 24, 36, Inf),
                             labels = c("1-6", "7-12", "13-18", "19-24", "25-36", "37+"))]

bye_effect <- wk_actual[
  , list(n = .N, mean_points = mean(points), sd_points = sd(points)),
  by = list(pos, rank_band, bye_bucket)
][order(pos, rank_band, bye_bucket)]
fwrite(bye_effect, file.path(out_dir, "bye_week_effect.csv"))
cat("\n==== actual points by pos x weekly-rank band x teams-on-bye ====\n")
print(bye_effect, nrows = 200)

## ---- crosswalk survivorship (data-level) -----------------------------------

xwalk <- merge(
  fp_draft_full[pos %in% pos_filter,
                list(season, fantasypros_id, pos, draft_rank = rank, ecr)],
  unique(fp_week_full[, list(season, fantasypros_id, weekly = TRUE)]),
  by = c("season", "fantasypros_id"), all.x = TRUE
)
survivorship <- xwalk[
  , list(n = .N, pct_never_weekly = mean(is.na(weekly))),
  by = list(pos, tier = rank_tier(draft_rank))
][order(pos, tier)]
fwrite(survivorship, file.path(out_dir, "crosswalk_survivorship.csv"))
cat("\n==== preseason-ranked player-seasons with zero weekly rankings ====\n")
print(survivorship, nrows = 100)

cat("\nDONE - outputs in ", out_dir, "\n")
