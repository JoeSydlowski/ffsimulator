# 2025 redraft positional-build analysis: expected (preseason view) vs actual
#
# Question: which positional draft builds should we have expected to succeed
# in 2025 *before the season*, and how did those same builds actually do?
# Run per league format: FFS_FORMAT=oneqb (default) or superflex.
#
# Design:
#   - 6 build archetypes x 2 teams each = 12-team league, 15-round snake
#     draft from 2025 preseason ECR; build-to-slot assignment randomized over
#     n_perm drafts so no build owns the good picks.
#   - Within a single preferred position, teams draft the market's best
#     player (positional ECR). Across a multi-position candidate set, teams
#     draft the highest model-expected-points player ("board"), so e.g.
#     c("RB","WR") genuinely means best RB-or-WR, not RB-first.
#   - EXPECTED: v3 projections trained only on 2012-2024 (rankings history
#     filtered via the cache-dir mechanism - no 2025 leakage), calibrated
#     settings (tuned kernel, team copula, rank-based start/sit). Success =
#     allplay win% distribution across simulated seasons.
#   - ACTUAL: identical rosters scored with real 2025 weekly points, lineups
#     set from real 2025 weekly FantasyPros ranks through the same
#     rank-lineup machinery (manager knowledge only, no hindsight).
#
# Outputs: dev/validate_outputs/build_2025_{expected,actual,summary}_<fmt>.csv

library(magrittr)
library(data.table)

devtools::load_all(here::here(), quiet = TRUE)
out_dir <- here::here("dev", "validate_outputs")
set.seed(2025)

fmt <- Sys.getenv("FFS_FORMAT", "oneqb")
stopifnot(fmt %in% c("oneqb", "superflex"))

target_season <- 2025
n_perm <- 20 # random build-to-slot assignments
n_sim <- 20 # simulated seasons per permutation
sim_weeks <- 1:14
pos_filter <- c("QB", "RB", "WR", "TE")
league_size <- 12
n_rounds <- 15

if (fmt == "oneqb") {
  lc <- data.table(pos = c("QB", "RB", "WR", "TE"), min = c(1, 2, 3, 1),
                   max = c(1, 4, 5, 2), offense_starters = 9, total_starters = 9)
  caps <- c(QB = 2, RB = 7, WR = 8, TE = 2)
  mins <- c(QB = 2, RB = 4, WR = 5, TE = 2)
} else {
  # superflex: second startable QB slot
  lc <- data.table(pos = c("QB", "RB", "WR", "TE"), min = c(1, 2, 3, 1),
                   max = c(2, 4, 5, 2), offense_starters = 9, total_starters = 9)
  caps <- c(QB = 3, RB = 7, WR = 8, TE = 2)
  mins <- c(QB = 2, RB = 4, WR = 5, TE = 2)
}

## ---- leakage-controlled projections (trained 2012-2024) --------------------

scoring_history <- as.data.table(readRDS(file.path(out_dir, "scoring_history_2012_2025.rds")))
fp_draft_full <- as.data.table(fp_rankings_history())
fp_week_full <- as.data.table(fp_rankings_history_week())

cache_dir <- file.path(tempdir(), "ffs_build2025")
dir.create(cache_dir, showWarnings = FALSE)
saveRDS(fp_draft_full[season < target_season], file.path(cache_dir, "fp_rankings_history.rds"))
saveRDS(fp_week_full[season < target_season], file.path(cache_dir, "fp_rankings_history_week.rds"))
saveRDS(fp_injury_table(), file.path(cache_dir, "fp_injury_table.rds"))
options(ffsimulator.cache_directory = cache_dir)

sh_train <- scoring_history[season < target_season]

# 2025 preseason rankings + real bye weeks
sched <- as.data.table(nflreadr::load_schedules(target_season))[game_type == "REG" & week <= max(sim_weeks)]
tw <- unique(rbind(sched[, list(week, team = home_team)], sched[, list(week, team = away_team)]))
tw[, team := nflreadr::clean_team_abbrs(team)]
byes <- tw[, list(bye = setdiff(seq_len(max(sim_weeks)), week)[1]), by = team]
byes[is.na(bye), bye := 0]

latest_rankings <- fp_draft_full[
  season == target_season & pos %in% pos_filter,
  list(player = player_name, pos, team = nflreadr::clean_team_abbrs(team),
       ecr, sd, fantasypros_id,
       scrape_date = as.Date(paste0(target_season, "-08-01")))
]
latest_rankings <- merge(latest_rankings, byes, by = "team", all.x = TRUE)
latest_rankings[is.na(bye), bye := 0]

ao <- ffs_adp_outcomes(sh_train, gp_model = "simple", pos_filter = pos_filter, version = "v3")
ps <- as.data.table(ffs_generate_projections(
  ao, latest_rankings, n_seasons = n_perm * n_sim, weeks = sim_weeks, version = "v3"
))
ps[is.na(projected_score), projected_score := 0]

# cross-position draft board: model-expected points per week
board <- ps[, list(board = mean(projected_score)), by = fantasypros_id]

## ---- build archetypes -------------------------------------------------------

# each archetype: function(round) -> candidate positions for that round.
# single position = take the market's best (positional ECR); multiple
# positions = take the best model-expected-points player among them.
builds <- list(
  robust_rb = function(r) if (r <= 3) "RB" else if (r <= 6) "WR" else c("RB", "WR", "QB", "TE"),
  zero_rb = function(r) if (r <= 4) "WR" else if (r <= 7) "RB" else c("WR", "RB", "QB", "TE"),
  hero_rb = function(r) if (r == 1) "RB" else if (r <= 4) "WR" else if (r <= 6) "RB" else c("WR", "RB", "QB", "TE"),
  early_onesie = function(r) if (r == 1) "TE" else if (r == 2) "QB" else if (r == 3 && fmt == "superflex") "QB" else c("RB", "WR"),
  bpa = function(r) c("QB", "RB", "WR", "TE"),
  punt_onesie = function(r) if (r <= 8) c("RB", "WR") else c("QB", "TE", "RB", "WR")
)
build_names <- names(builds)

draft_league <- function(pool, slot_builds) {
  counts <- matrix(0L, nrow = league_size, ncol = 4, dimnames = list(NULL, names(caps)))
  picks <- vector("list", league_size * n_rounds)
  pk <- 0L
  for (rd in seq_len(n_rounds)) {
    for (tm in (if (rd %% 2) 1:league_size else league_size:1)) {
      remaining <- n_rounds - sum(counts[tm, ])
      need <- pmax(0L, mins - counts[tm, ])
      pref <- builds[[slot_builds[tm]]](rd)
      open <- names(caps)[counts[tm, ] < caps]
      cand <- intersect(pref, open)
      if (!length(cand)) cand <- open
      # forced picks: if every remaining pick is needed for minimums,
      # restrict to needed positions
      if (sum(need) >= remaining) cand <- intersect(names(need)[need > 0], open)
      av <- pool[pos %in% cand]
      if (!nrow(av)) av <- pool[pos %in% open]
      sel <- if (length(unique(av$pos)) == 1 || length(cand) == 1) {
        av[order(ecr)][1] # market's best at the position
      } else {
        av[order(-board)][1] # best value across positions
      }
      pk <- pk + 1L
      picks[[pk]] <- data.table(
        league_id = "b", franchise_id = sprintf("%02d", tm),
        franchise_name = slot_builds[tm], build = slot_builds[tm],
        player_id = sel$fantasypros_id, fantasypros_id = sel$fantasypros_id,
        pos = sel$pos, round = rd
      )
      counts[tm, sel$pos] <- counts[tm, sel$pos] + 1L
      pool <- pool[fantasypros_id != sel$fantasypros_id]
    }
  }
  rbindlist(picks)
}

## ---- actual-season scoring inputs ------------------------------------------

dp_id <- as.data.table(ffscrapr::dp_playerids())[
  !is.na(gsis_id) & !is.na(fantasypros_id), c("fantasypros_id", "gsis_id")
]
actual_pts <- merge(
  scoring_history[season == target_season & week <= max(sim_weeks) & !is.na(gsis_id),
                  list(gsis_id, week, points)],
  dp_id, by = "gsis_id"
)[, list(points = sum(points)), by = c("fantasypros_id", "week")]

# what a 2025 manager knew each week: real weekly FP rank -> expected points
# via the *pre-2025* trained pools (bye-adjusted, consistent with training)
wk_ranks_2025 <- .ff_bye_adjust_rank(fp_week_full)[
  season == target_season & week <= max(sim_weeks) & pos %in% pos_filter,
  list(fantasypros_id, week, pos, rank)
]
wk_ranks_2025 <- merge(
  wk_ranks_2025, ao[, list(pos, rank, avg_week)], by = c("pos", "rank"), all.x = TRUE
)
wk_ranks_2025[is.na(avg_week), avg_week := 0]

actual_ps_all <- merge(
  wk_ranks_2025[, list(fantasypros_id, week, avg_week)],
  actual_pts, by = c("fantasypros_id", "week"), all = TRUE
)
actual_ps_all[is.na(points), points := 0]
actual_ps_all[is.na(avg_week), avg_week := 0]
actual_ps_all <- merge(actual_ps_all,
                       latest_rankings[, list(fantasypros_id, ecr, scrape_date)],
                       by = "fantasypros_id")
actual_ps_all[, `:=`(season = 1L, projection = points, gp_model = 1L, projected_score = points)]

## ---- run permutations -------------------------------------------------------

pool0 <- merge(latest_rankings[fantasypros_id %in% unique(ps$fantasypros_id)],
               board, by = "fantasypros_id")[order(ecr)]

expected_res <- list()
actual_res <- list()

for (perm in seq_len(n_perm)) {
  slot_builds <- sample(rep(build_names, 2))
  rosters <- draft_league(pool0, slot_builds)
  franchises <- unique(rosters[, list(league_id, franchise_id, franchise_name, build)])

  sim_slice <- ((perm - 1) * n_sim + 1):(perm * n_sim)
  ps_perm <- ps[season %in% sim_slice]
  rs <- ffs_score_rosters(ps_perm, rosters[, list(league_id, franchise_id, franchise_name,
                                                  player_id, fantasypros_id, pos)])
  ol <- ffs_optimise_lineups(rs, lc, lineup_method = "rank", pos_filter = pos_filter)
  schedules <- ffs_build_schedules(n_seasons = n_sim, n_weeks = length(sim_weeks),
                                   franchises = franchises[, list(league_id, franchise_id, franchise_name)])
  ol <- as.data.table(ol)[, season := match(season, sim_slice)]
  sw <- ffs_summarise_week(optimal_scores = ol, schedules = schedules)
  ss <- ffs_summarise_season(summary_week = sw)
  ss <- merge(ss, franchises[, list(franchise_id, build)], by = "franchise_id")
  expected_res[[perm]] <- ss[, list(perm = perm, season, build, franchise_id,
                                    allplay_winpct, h2h_winpct, points_for)]

  rs_a <- ffs_score_rosters(actual_ps_all, rosters[, list(league_id, franchise_id, franchise_name,
                                                          player_id, fantasypros_id, pos)])
  ol_a <- ffs_optimise_lineups(rs_a, lc, lineup_method = "rank", pos_filter = pos_filter)
  sched_a <- ffs_build_schedules(n_seasons = 1, n_weeks = length(sim_weeks),
                                 franchises = franchises[, list(league_id, franchise_id, franchise_name)])
  sw_a <- ffs_summarise_week(optimal_scores = as.data.table(ol_a), schedules = sched_a)
  ss_a <- ffs_summarise_season(summary_week = sw_a)
  ss_a <- merge(ss_a, franchises[, list(franchise_id, build)], by = "franchise_id")
  actual_res[[perm]] <- ss_a[, list(perm = perm, build, franchise_id,
                                    allplay_winpct, h2h_winpct, points_for)]

  message("perm ", perm, "/", n_perm, " done")
}

expected_res <- rbindlist(expected_res)
actual_res <- rbindlist(actual_res)
fwrite(expected_res, file.path(out_dir, paste0("build_2025_expected_", fmt, ".csv")))
fwrite(actual_res, file.path(out_dir, paste0("build_2025_actual_", fmt, ".csv")))

## ---- summarise ---------------------------------------------------------------

expected_res[, top4 := frank(-allplay_winpct, ties.method = "random") <= 4, by = list(perm, season)]
exp_sum <- expected_res[
  , list(exp_allplay = mean(allplay_winpct), exp_p10 = quantile(allplay_winpct, .1),
         exp_p90 = quantile(allplay_winpct, .9), exp_top4 = mean(top4),
         exp_pf = mean(points_for)),
  by = build
]
actual_res[, top4 := frank(-allplay_winpct, ties.method = "random") <= 4, by = perm]
act_sum <- actual_res[
  , list(act_allplay = mean(allplay_winpct), act_sd = sd(allplay_winpct),
         act_top4 = mean(top4), act_pf = mean(points_for), n = .N),
  by = build
]

summary <- merge(exp_sum, act_sum, by = "build")[order(-act_allplay)]
fwrite(summary, file.path(out_dir, paste0("build_2025_summary_", fmt, ".csv")))

cat("\n==== 2025 builds [", fmt, "]: expected (preseason, no leakage) vs actual ====\n")
print(summary[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
cat("\nspearman(expected, actual) allplay:",
    round(cor(summary$exp_allplay, summary$act_allplay, method = "spearman"), 3), "\n")

options(ffsimulator.cache_directory = NULL)
cat("\nDONE\n")
