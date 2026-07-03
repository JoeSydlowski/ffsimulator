library(tidyverse)
library(ffscrapr)
pkgload::load_all()

setwd(here::here())

mfl_conn <- ffscrapr::mfl_connect(season = 2021, league_id = 22627)

mfl_scoring_history <- ffscrapr::ff_scoringhistory(mfl_conn, 2019:2020) %>%
  select(season, week, gsis_id, mfl_id, player_name, pos, team, points)

mfl_franchises <- ffs_franchises(mfl_conn)

mfl_rosters <- ffs_rosters(mfl_conn) %>%
  select(-salary, -contract_years, -roster_status, -draft_year, -draft_round)

mfl_lineup_constraints <- ffscrapr::ff_starter_positions(mfl_conn)

saveRDS(mfl_conn, "inst/examples/mfl_conn.rds")
saveRDS(mfl_scoring_history, "inst/examples/mfl_scoring_history.rds")
saveRDS(mfl_franchises, "inst/examples/mfl_franchises.rds")
saveRDS(mfl_rosters, "inst/examples/mfl_rosters.rds")
saveRDS(mfl_lineup_constraints, "inst/examples/mfl_lineup_constraints.rds")

sleeper_conn <- ff_connect(platform = "sleeper", league_id = "652718526494253056", season = 2021)

sleeper_scoring_history <- ffscrapr::ff_scoringhistory(sleeper_conn, 2019:2020) %>%
  select(season, week, gsis_id, sleeper_id, player_name, pos, team, points)

sleeper_franchises <- ffs_franchises(sleeper_conn)
sleeper_rosters <- ffs_rosters(sleeper_conn)

sleeper_lineup_constraints <- ffscrapr::ff_starter_positions(sleeper_conn)

saveRDS(sleeper_conn, "inst/examples/sleeper_conn.rds")
saveRDS(sleeper_scoring_history, "inst/examples/sleeper_scoring_history.rds")
saveRDS(sleeper_franchises, "inst/examples/sleeper_franchises.rds")
saveRDS(sleeper_rosters, "inst/examples/sleeper_rosters.rds")
saveRDS(sleeper_lineup_constraints, "inst/examples/sleeper_lineup_constraints.rds")

fleaflicker_conn <- fleaflicker_connect(2020, 206154)

fleaflicker_scoring_history <- ffscrapr::ff_scoringhistory(fleaflicker_conn, 2019:2020) %>%
  select(season, week, gsis_id, player_name, pos, team, points)

fleaflicker_franchises <- ffs_franchises(fleaflicker_conn)
fleaflicker_rosters <- ffs_rosters(fleaflicker_conn)

fleaflicker_lineup_constraints <- ffscrapr::ff_starter_positions(fleaflicker_conn)

saveRDS(fleaflicker_conn, "inst/examples/fleaflicker_conn.rds")
saveRDS(fleaflicker_scoring_history, "inst/examples/fleaflicker_scoring_history.rds")
saveRDS(fleaflicker_franchises, "inst/examples/fleaflicker_franchises.rds")
saveRDS(fleaflicker_rosters, "inst/examples/fleaflicker_rosters.rds")
saveRDS(fleaflicker_lineup_constraints, "inst/examples/fleaflicker_lineup_constraints.rds")

espn_conn <- espn_connect(season = 2020, league_id = 899513)

espn_scoring_history <- ffscrapr::ff_scoringhistory(espn_conn, 2019:2020) %>%
  select(season, week, gsis_id, player_name, pos, team, points)

espn_franchises <- ffs_franchises(espn_conn)
espn_rosters <- ffs_rosters(espn_conn)

espn_lineup_constraints <- ffscrapr::ff_starter_positions(espn_conn)

saveRDS(espn_conn, "inst/examples/espn_conn.rds")
saveRDS(espn_scoring_history, "inst/examples/espn_scoring_history.rds")
saveRDS(espn_franchises, "inst/examples/espn_franchises.rds")
saveRDS(espn_rosters, "inst/examples/espn_rosters.rds")
saveRDS(espn_lineup_constraints, "inst/examples/espn_lineup_constraints.rds")

latest_rankings <- ffs_latest_rankings(type = "draft")
latest_rankings_week <- ffs_latest_rankings(type = "week")
saveRDS(latest_rankings, "inst/examples/latest_rankings.rds")
saveRDS(latest_rankings_week, "inst/examples/latest_rankings_week.rds")

adp_outcomes <- ffs_adp_outcomes(
  scoring_history = mfl_scoring_history,
  gp_model = "simple"
)
saveRDS(adp_outcomes, "inst/examples/adp_outcomes.rds")

adp_outcomes_week <- ffs_adp_outcomes_week(
  scoring_history = mfl_scoring_history,
  pos_filter = c("QB", "RB", "WR")
)
saveRDS(adp_outcomes_week, "inst/examples/adp_outcomes_week.rds")

projected_scores <- ffs_generate_projections(
  adp_outcomes = adp_outcomes,
  latest_rankings = latest_rankings,
  n_seasons = 2,
  weeks = 1:3,
  rosters = mfl_rosters
)
saveRDS(projected_scores, "inst/examples/projected_scores.rds")

projected_scores_week <- ffs_generate_projections_week(
  adp_outcomes = adp_outcomes_week,
  latest_rankings = latest_rankings_week,
  n = 5,
  rosters = mfl_rosters
)
saveRDS(projected_scores_week, "inst/examples/projected_scores_week.rds")

roster_scores <- ffs_score_rosters(
  projected_scores = projected_scores,
  rosters = mfl_rosters
)
saveRDS(roster_scores, "inst/examples/roster_scores.rds")

optimal_scores <- ffs_optimize_lineups(
  roster_scores = roster_scores,
  lineup_constraints = mfl_lineup_constraints,
  best_ball = FALSE
)
saveRDS(optimal_scores, "inst/examples/optimal_scores.rds")

schedules <- ffs_build_schedules(
  n_teams = NULL,
  n_seasons = 2,
  n_weeks = 3,
  franchises = mfl_franchises
)

saveRDS(schedules, "inst/examples/schedules.rds")

foureight <- mfl_connect(2021, 22627)
foureight_sim <- ff_simulate(foureight, n_seasons = 10)

saveRDS(foureight_sim, "inst/examples/foureight_sim.rds")

foureight_sim_week <- ff_simulate_week(foureight, n = 25)
saveRDS(foureight_sim_week, "inst/examples/foureight_sim_week.rds")
