# ffsimulator league analysis suite
#
# One command produces a dated report folder for your league:
#   Rscript dev/suite/run_league_suite.R
# (or source it from a console; edit the CONFIG block first)
#
# Outputs land in dev/league_sims/<league_id>/<date>/ - see dev/suite/README.md

library(data.table)
library(ggplot2)
devtools::load_all(here::here(), quiet = TRUE)
# ffscrapr reads a dead nflverse asset, so ff_scoringhistory() returns ZERO rows
# for 2025+ and every simulation silently loses its most recent season.
source(here::here("dev", "suite", "scoring_history_shim.R")); install_shim()

## ---- CONFIG -----------------------------------------------------------------
# JML 1326464763936403456
# Jon 1359546500786434048
config <- list(
  league_id = Sys.getenv("FFS_LEAGUE_ID", "1326464763936403456"),
  platform = "sleeper",
  season = as.integer(Sys.getenv("FFS_SEASON", "2026")),
  my_team = Sys.getenv("FFS_MY_TEAM", "sox05syd"),
  n_sims = as.integer(Sys.getenv("FFS_NSEASONS", "2000")), # standings/odds; value_convergence.csv: values stable to +-0.01-0.02 by n=2000 (playoff SD ~1%). n=400 was ~2x noisier; fast valuation path makes 2000 affordable. Higher n OOMs ~4000 on 32GB.
  n_sims_war = 50L, # per-player WAR (leave-one-out is expensive)
  version = "v3",
  lineup_method = "rank",
  # deep dynasty league: no usable waiver wire, value vs your own bench
  replacement_level = FALSE,
  # TRUE = condition on your real (known) schedule; FALSE = average over
  # random schedules (use when the schedule isn't released yet)
  actual_schedule = as.logical(Sys.getenv("FFS_ACTUAL_SCHEDULE", "TRUE")),
  playoff_slots = 6L,
  # Positions the simulation starts. K is EXCLUDED by default: a league that
  # requires a K slot but has managers carrying no kicker in the offseason gets
  # a badly asymmetric sim - the LP simply leaves that slot empty all season, so
  # those teams are modelled as punting a starter every week. Measured on the
  # Goofball league (2 of 10 teams kicker-less at the time): including K moved
  # their playoff odds by -51 and -42 points and inflated everyone else by 3-18.
  # DEF is never simulable (no rows in scoring history), so it drops out for all
  # teams regardless; dropping K too leaves both non-skill slots uniform.
  pos_filter = strsplit(Sys.getenv("FFS_POS_FILTER", "QB,RB,WR,TE"), ",")[[1]],
  run_dynasty = TRUE,
  run_trade_intel = TRUE,
  # generic owner-context WAR for every rostered player (ff_wins_added).
  # Trade intelligence no longer uses it - the same leave-one-out signal is
  # computed per decision with ffs_player_value on the shared sim - so it is
  # off by default; flip on for a leaguewide war_players.csv.
  run_war = as.logical(Sys.getenv("FFS_RUN_WAR", "FALSE")),
  trade_top_n = as.integer(Sys.getenv("FFS_TRADE_TOP_N", "200"))
)

out <- here::here("dev", "league_sims", config$league_id, format(Sys.Date()))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
saveRDS(config, file.path(out, "config.rds"))

set.seed(config$season)
conn <- switch(config$platform,
  sleeper = ffscrapr::sleeper_connect(season = config$season, league_id = config$league_id),
  mfl = ffscrapr::mfl_connect(season = config$season, league_id = config$league_id)
)

# round numeric columns for readable sheets: money-scale cols (>=100) to whole
# numbers, everything else (wins, probabilities, ratios) to 3 decimals
round_sheet <- function(dt) {
  dt <- data.table::copy(data.table::as.data.table(dt))
  for (col in names(dt)) {
    v <- dt[[col]]
    if (is.numeric(v)) {
      digits <- if (max(abs(v), na.rm = TRUE) >= 100) 0L else 3L
      data.table::set(dt, j = col, value = round(v, digits))
    }
  }
  dt
}

## ---- 1. season simulation ----------------------------------------------------

message("simulating ", config$n_sims, " seasons @ ", Sys.time())
sim <- ff_simulate(
  conn,
  n_seasons = config$n_sims,
  version = config$version,
  lineup_method = config$lineup_method,
  replacement_level = config$replacement_level,
  actual_schedule = config$actual_schedule,
  pos_filter = config$pos_filter,
  return = "all"
)
if (is.null(sim$summary_season)) {
  stop("actual_schedule=TRUE but no unplayed weeks found - set FFS_ACTUAL_SCHEDULE=FALSE")
}
saveRDS(sim, file.path(out, "simulation.rds"))
fwrite(sim$summary_simulation, file.path(out, "summary_simulation.csv"))

ss <- as.data.table(sim$summary_season)
# wins then points-for, deterministic (matches .ffs_franchise_summary)
ss[, lg_rank := frank(list(-h2h_wins, -points_for), ties.method = "first"), by = season]
odds <- ss[, list(
  mean_wins = mean(h2h_wins),
  p25_wins = quantile(h2h_wins, .25),
  p75_wins = quantile(h2h_wins, .75),
  mean_pf = mean(points_for),
  playoff_pct = mean(lg_rank <= config$playoff_slots),
  top_seed_pct = mean(lg_rank == 1),
  last_pct = mean(lg_rank == max(lg_rank))
), by = franchise_name][order(-mean_wins)]
# championship odds from the deterministic playoff bracket (ceiling-aware; berth
# odds wash out week-to-week variance, titles do not) - reported for comparison
champ_tbl <- ffsimulator:::.ffs_champion_pct(as.data.table(sim$summary_week), ss)
champ_tbl <- merge(champ_tbl, unique(ss[, list(franchise_id, franchise_name)]),
                   by = "franchise_id")
odds <- merge(odds, champ_tbl[, list(franchise_name, champion_pct)],
              by = "franchise_name", all.x = TRUE)[order(-champion_pct)]
fwrite(odds, file.path(out, "playoff_odds.csv"))

for (t in c("wins", "rank", "points")) {
  p <- try(autoplot(sim, type = t), silent = TRUE)
  if (!inherits(p, "try-error")) {
    ggsave(file.path(out, paste0(t, ".png")), p, width = 10, height = 7.5, dpi = 150)
  }
}

fr <- as.data.table(sim$franchises)
me <- fr[franchise_name == config$my_team, franchise_id][1]
if (is.na(me)) stop("my_team '", config$my_team, "' not found in franchises")

## ---- 2. dynasty outlook --------------------------------------------------------

if (config$run_dynasty) {
  # real FantasyCalc market values anchor dynasty values (both formats scraped;
  # ffs_dynasty_outlook filters to the league's). Set FFS_FANTASYCALC=0 to skip
  # and fall back to the synthetic rank-decay curve.
  dyn_vals <- NULL
  if (Sys.getenv("FFS_FANTASYCALC", "1") != "0") {
    dyn_vals <- tryCatch(
      rbind(fc_dynasty_values(num_qbs = 1), fc_dynasty_values(num_qbs = 2)),
      error = function(e) { message("FantasyCalc unavailable: ", conditionMessage(e)); NULL })
    db_path <- here::here("dev", "data", "fantasycalc_values.parquet")
    if (!is.null(dyn_vals) && requireNamespace("arrow", quietly = TRUE)) {
      try(fc_snapshot_append(db_path, configs = list(list(num_qbs = 1), list(num_qbs = 2))), silent = TRUE)
    }
  }
  message("dynasty outlook @ ", Sys.time())
  dyn <- as.data.table(ffs_dynasty_outlook(sim, dynasty_values = dyn_vals))
  fwrite(dyn, file.path(out, "dynasty_outlook.csv"))

  team_dyn <- dyn[, list(
    cur_capital = sum(cur_value),
    next_capital_mean = sum(next_value_mean),
    n_ranked = .N
  ), by = franchise_name][order(-cur_capital)]
  fwrite(team_dyn, file.path(out, "dynasty_capital.csv"))
}

## ---- 3. trade intelligence -----------------------------------------------------
# roster verdicts + best buyers, buy targets (Pareto sweet spots), complete
# buy/sell deal packages, and the portfolio view. Valuations run on a dedicated
# fast sim with the real schedule (FFS_TRADE_NSIMS; see valuation_convergence.R).

if (config$run_trade_intel && config$run_dynasty) {
  message("trade intelligence @ ", Sys.time())
  source(here::here("dev", "suite", "trade_intel.R"))
}

## ---- 4. optional: leaguewide generic WAR ----------------------------------------

if (config$run_war) {
  # scope: "mine" (my roster + trade targets - fast) or "all" (every rostered
  # player - slow). This is owner-context leave-one-out (irreplaceability on
  # the CURRENT owner's roster), not acquisition value - see README caveats.
  war_scope <- Sys.getenv("FFS_WAR_SCOPE", "mine")
  war_players <- NULL
  if (war_scope == "mine") {
    mine_ids <- unique(as.data.table(sim$roster_scores)[
      franchise_id == me & !grepl("^(QB|RB|WR|TE|K)_\\d+$", player_id), player_id])
    tgt <- if (exists("targets")) as.character(as.data.table(targets)$player_id) else character(0)
    war_players <- unique(c(mine_ids, tgt))
  }
  message("WAR (", config$n_sims_war, " sims/player, scope=", war_scope,
          if (!is.null(war_players)) paste0(", ", length(war_players), " players") else "",
          ") @ ", Sys.time())
  wa <- ff_wins_added(
    conn,
    players = war_players,
    n_seasons = config$n_sims_war,
    version = config$version,
    lineup_method = config$lineup_method,
    replacement_level = config$replacement_level
  )
  fwrite(as.data.table(wa$war), file.path(out, "war_players.csv"))
}

message("DONE - outputs in ", out)
