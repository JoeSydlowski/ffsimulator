# Prototype: generic WAR (wins above replacement) for fantasy players
#
# League-connection-free: takes a league *format* (size, roster spec, lineup
# constraints) instead of a conn, and computes each player's expected wins
# added over the best freely-available replacement at his position, in a
# generic league of that format.
#
# Method:
#   1. v3 projections (whole-trajectory resampling) for every player in the
#      latest preseason rankings.
#   2. Build a generic league: ECR snake draft into `league_size` rosters.
#   3. Team weekly-total distribution: optimal (best-ball) lineups for the
#      generic rosters via ffs_optimise_lineups(); the ecdf of team totals
#      converts marginal points into win probability.
#   4. Replacement level per position = the best undrafted player by ECR
#      (his simulated weekly scores, not a constant).
#   5. Player-week wins added = F_opp(base + s_player - s_repl) - F_opp(base);
#      season WAR = sum over weeks, distribution taken across sim seasons.
#
# Outputs (dev/validate_outputs/):
#   war_players.csv - per-player WAR mean / p10 / p90 + preseason ECR
#   war_by_round.csv - mean WAR by position x snake-draft round (redraft
#                      positional strategy: what does a round-3 RB buy vs a
#                      round-3 WR?)

library(magrittr)
library(data.table)

devtools::load_all(here::here(), quiet = TRUE)

out_dir <- here::here("dev", "validate_outputs")
set.seed(2026)

n_sim <- 200
sim_weeks <- 1:14
pos_filter <- c("QB", "RB", "WR", "TE")
league_size <- 12
roster_spec <- c(QB = 2, RB = 5, WR = 6, TE = 2) # 15 rounds
lineup_constraints <- data.table(
  pos = c("QB", "RB", "WR", "TE"),
  min = c(1, 2, 3, 1),
  max = c(1, 4, 5, 2),
  offense_starters = 9, # 1QB 2RB 3WR 1TE + 2 flex
  total_starters = 9 # used by .ff_optimise_one_lineup but absent from its assert
)

## ---- projections -----------------------------------------------------------

scoring_history <- readRDS(file.path(out_dir, "scoring_history_2012_2025.rds"))

# latest completed preseason for reproducibility; swap in
# ffs_latest_rankings() for live in-season use
rank_season <- max(fp_rankings_history()$season)
latest_rankings <- data.table::as.data.table(fp_rankings_history())[
  season == rank_season & pos %in% pos_filter,
  list(
    player = player_name, pos, team, ecr, sd, fantasypros_id,
    bye = 0, scrape_date = as.Date(paste0(rank_season, "-08-01"))
  )
]

adp_outcomes <- ffs_adp_outcomes(
  scoring_history = scoring_history,
  gp_model = "simple",
  pos_filter = pos_filter,
  version = "v3"
)

ps <- ffs_generate_projections(
  adp_outcomes = adp_outcomes,
  latest_rankings = latest_rankings,
  n_seasons = n_sim,
  weeks = sim_weeks,
  version = "v3"
)
ps <- as.data.table(ps)
ps[is.na(projected_score), projected_score := 0]

## ---- generic league via ECR snake draft ------------------------------------

draft_pool <- latest_rankings[
  fantasypros_id %in% unique(ps$fantasypros_id)
][order(ecr)]

n_rounds <- sum(roster_spec)
rosters <- vector("list", n_rounds * league_size)
open_slots <- matrix(
  rep(roster_spec, league_size), nrow = league_size, byrow = TRUE,
  dimnames = list(NULL, names(roster_spec))
)
pool <- copy(draft_pool)
pick <- 0L
for (rd in seq_len(n_rounds)) {
  order_teams <- if (rd %% 2 == 1) seq_len(league_size) else rev(seq_len(league_size))
  for (tm in order_teams) {
    available <- pool[pos %in% names(roster_spec)[open_slots[tm, ] > 0]]
    if (nrow(available) == 0) next
    sel <- available[1]
    pick <- pick + 1L
    rosters[[pick]] <- data.table(
      league_id = "war", franchise_id = sprintf("%02d", tm),
      franchise_name = paste0("team_", sprintf("%02d", tm)),
      player_id = sel$fantasypros_id, fantasypros_id = sel$fantasypros_id,
      player = sel$player, pos = sel$pos, ecr = sel$ecr,
      round = rd, overall = pick
    )
    open_slots[tm, sel$pos] <- open_slots[tm, sel$pos] - 1L
    pool <- pool[fantasypros_id != sel$fantasypros_id]
  }
}
rosters <- rbindlist(rosters)

## ---- team-total distribution (win-probability curve) -----------------------

roster_scores <- ffs_score_rosters(
  projected_scores = ps,
  rosters = rosters[, list(league_id, franchise_id, franchise_name,
                           player_id, fantasypros_id, pos)]
)

# calibrated settings: rank-based start/sit (manager sees weekly rankings,
# not realized points); v3 defaults carry the tuned trajectory kernel and
# the team-week copula (rho 0.4)
lineups <- ffs_optimise_lineups(
  roster_scores = roster_scores,
  lineup_constraints = lineup_constraints,
  lineup_method = "rank",
  pos_filter = pos_filter
)
lineups <- as.data.table(lineups)

team_totals <- lineups$actual_score
F_opp <- stats::ecdf(team_totals)
base_score <- stats::median(team_totals)
cat("team weekly totals (rank-managed): median", round(base_score, 1),
    "IQR", paste(round(stats::quantile(team_totals, c(.25, .75)), 1), collapse = "-"),
    "| emergent efficiency:", round(mean(lineups$lineup_efficiency), 3), "\n")

## ---- replacement level ------------------------------------------------------

repl_players <- draft_pool[
  !fantasypros_id %in% rosters$fantasypros_id, .SD[1], by = pos
]
cat("replacement players:\n")
print(repl_players[, list(pos, player, ecr)])

repl_scores <- ps[
  fantasypros_id %in% repl_players$fantasypros_id,
  list(pos, season, week, repl_score = projected_score)
]

## ---- WAR --------------------------------------------------------------------

war_weekly <- merge(
  ps[, list(fantasypros_id, player, pos, ecr, season, week, projected_score)],
  repl_scores, by = c("pos", "season", "week")
)
war_weekly[, wins_added := F_opp(base_score + projected_score - repl_score) - F_opp(base_score)]

war_season <- war_weekly[
  , list(war = sum(wins_added)), by = list(fantasypros_id, player, pos, ecr, season)
]
war_players <- war_season[
  , list(
    war_mean = mean(war),
    war_p10 = quantile(war, .10),
    war_p90 = quantile(war, .90),
    war_sd = sd(war)
  ),
  by = list(fantasypros_id, player, pos, ecr)
][order(-war_mean)]

fwrite(war_players, file.path(out_dir, "war_players.csv"))

cat("\n==== top 25 by mean WAR ====\n")
print(war_players[1:25, list(player, pos, ecr, war_mean = round(war_mean, 2),
                             war_p10 = round(war_p10, 2), war_p90 = round(war_p90, 2))])

cat("\n==== top 10 most volatile (war_sd) among ECR top-100 ====\n")
print(war_players[ecr <= 100][order(-war_sd)][1:10, list(player, pos, ecr,
      war_mean = round(war_mean, 2), war_sd = round(war_sd, 2))])

## ---- WAR by draft round x position (redraft strategy) -----------------------

war_by_round <- merge(
  war_players, rosters[, list(fantasypros_id, round)], by = "fantasypros_id"
)[
  , list(n = .N, war_mean = round(mean(war_mean), 2)), by = list(round, pos)
][order(round, -war_mean)]

fwrite(war_by_round, file.path(out_dir, "war_by_round.csv"))
cat("\n==== mean WAR by snake round x position (rounds 1-8) ====\n")
print(dcast(war_by_round[round <= 8], round ~ pos, value.var = "war_mean"), nrows = 10)

cat("\nDONE - outputs in ", out_dir, "\n")
