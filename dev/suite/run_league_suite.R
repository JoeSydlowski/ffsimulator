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

## ---- CONFIG -----------------------------------------------------------------

config <- list(
  league_id = Sys.getenv("FFS_LEAGUE_ID", "1359546500786434048"),
  platform = "sleeper",
  season = as.integer(Sys.getenv("FFS_SEASON", "2026")),
  my_team = Sys.getenv("FFS_MY_TEAM", "sox05syd"),
  n_sims = as.integer(Sys.getenv("FFS_NSEASONS", "400")), # standings/odds; see convergence.csv
  n_sims_war = 50L, # per-player WAR (leave-one-out is expensive)
  version = "v3",
  lineup_method = "rank",
  # deep dynasty league: no usable waiver wire, value vs your own bench
  replacement_level = FALSE,
  # TRUE = condition on your real (known) schedule; FALSE = average over
  # random schedules (use when the schedule isn't released yet)
  actual_schedule = as.logical(Sys.getenv("FFS_ACTUAL_SCHEDULE", "TRUE")),
  playoff_slots = 6L,
  run_war = TRUE,
  run_trades = TRUE,
  run_dynasty = TRUE,
  trade_top_n = 20L
)

out <- here::here("dev", "league_sims", config$league_id, format(Sys.Date()))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
saveRDS(config, file.path(out, "config.rds"))

set.seed(config$season)
conn <- switch(config$platform,
  sleeper = ffscrapr::sleeper_connect(season = config$season, league_id = config$league_id),
  mfl = ffscrapr::mfl_connect(season = config$season, league_id = config$league_id)
)

## ---- 1. season simulation ----------------------------------------------------

message("simulating ", config$n_sims, " seasons @ ", Sys.time())
sim <- ff_simulate(
  conn,
  n_seasons = config$n_sims,
  version = config$version,
  lineup_method = config$lineup_method,
  replacement_level = config$replacement_level,
  actual_schedule = config$actual_schedule,
  return = "all"
)
if (is.null(sim$summary_season)) {
  stop("actual_schedule=TRUE but no unplayed weeks found - set FFS_ACTUAL_SCHEDULE=FALSE")
}
saveRDS(sim, file.path(out, "simulation.rds"))
fwrite(sim$summary_simulation, file.path(out, "summary_simulation.csv"))

ss <- as.data.table(sim$summary_season)
ss[, lg_rank := frank(-h2h_wins, ties.method = "random"), by = season]
odds <- ss[, list(
  mean_wins = mean(h2h_wins),
  p25_wins = quantile(h2h_wins, .25),
  p75_wins = quantile(h2h_wins, .75),
  mean_pf = mean(points_for),
  playoff_pct = mean(lg_rank <= config$playoff_slots),
  top_seed_pct = mean(lg_rank == 1),
  last_pct = mean(lg_rank == max(lg_rank))
), by = franchise_name][order(-mean_wins)]
fwrite(odds, file.path(out, "playoff_odds.csv"))

for (t in c("wins", "rank", "points")) {
  p <- try(autoplot(sim, type = t), silent = TRUE)
  if (!inherits(p, "try-error")) {
    ggsave(file.path(out, paste0(t, ".png")), p, width = 10, height = 7.5, dpi = 150)
  }
}

## ---- 2. roster drivers for my team -------------------------------------------

fr <- as.data.table(sim$franchises)
me <- fr[franchise_name == config$my_team, franchise_id][1]
if (is.na(me)) stop("my_team '", config$my_team, "' not found in franchises")

team <- ss[franchise_id == me, list(season, allplay_winpct, h2h_wins, lg_rank)]
os <- as.data.table(sim$optimal_scores)[franchise_id == me]
if ("starter_player_id" %in% names(os)) {
  st <- os[, list(player_id = unlist(starter_player_id)), by = list(season, week)][!is.na(player_id)]
} else {
  st <- os[, list(player_id = unlist(optimal_player_id)), by = list(season, week)][!is.na(player_id)]
}
rs_me <- as.data.table(sim$roster_scores)[franchise_id == me,
  list(season, week, player_id, player_name, pos, projected_score)]
started <- merge(st, rs_me, by = c("season", "week", "player_id"))
pl <- started[, list(pts = sum(projected_score), wk = .N), by = list(season, player_id, player_name, pos)]
players_all <- unique(rs_me[, list(player_id, player_name, pos)])
grid <- CJ(season = unique(team$season), player_id = players_all$player_id)
grid <- merge(grid, players_all, by = "player_id")
pl <- merge(grid, pl, by = c("season", "player_id", "player_name", "pos"), all.x = TRUE)
pl[is.na(pts), pts := 0][is.na(wk), wk := 0]
pl <- merge(pl, team, by = "season")
qs <- quantile(team$allplay_winpct, c(.25, .75))
drivers <- pl[, list(
  mean_pts = mean(pts), mean_weeks_started = mean(wk),
  corr_with_team = suppressWarnings(cor(pts, allplay_winpct)),
  swing = mean(pts[allplay_winpct >= qs[2]]) - mean(pts[allplay_winpct <= qs[1]])
), by = list(player_name, pos)][mean_weeks_started > 0.5][order(-swing)]
fwrite(drivers, file.path(out, "roster_drivers.csv"))

## ---- 3. trade intelligence (before WAR so WAR can scope to relevant players) ---

if (config$run_trades) {
  message("trade scan @ ", Sys.time())
  targets <- ffs_trade_targets(sim, me, top_n = config$trade_top_n)
  fwrite(targets, file.path(out, "trade_targets.csv"))

  # my offerable pieces: value of each of my players to my own roster
  my_players <- unique(as.data.table(sim$roster_scores)[
    franchise_id == me & !grepl("^(QB|RB|WR|TE|K)_\\d+$", player_id),
    list(player_id, player_name, pos)
  ])
  offers <- rbindlist(lapply(my_players$player_id, function(p) {
    v <- ffs_player_value(sim, p, me)
    data.table(player_id = p, value_to_me = v$h2h_wins, playoff_delta = v$playoff_pct)
  }))
  offers <- merge(my_players, offers, by = "player_id")[order(value_to_me)]
  fwrite(offers, file.path(out, "trade_offers.csv"))
}

## ---- 4. WAR -------------------------------------------------------------------

if (config$run_war) {
  # scope: "mine" (my roster + trade targets - fast, the decision-relevant
  # players) or "all" (every rostered player - slow). leave-one-out cost
  # scales with the number of players, so "mine" is ~10x faster.
  war_scope <- Sys.getenv("FFS_WAR_SCOPE", "mine")
  war_players <- NULL
  if (war_scope == "mine") {
    mine <- unique(as.data.table(sim$roster_scores)[
      franchise_id == me & !grepl("^(QB|RB|WR|TE|K)_\\d+$", player_id), player_id])
    tgt <- if (config$run_trades) as.data.table(targets)$player_id else character(0)
    war_players <- unique(c(mine, tgt))
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

## ---- 5. dynasty outlook ---------------------------------------------------------

if (config$run_dynasty) {
  message("dynasty outlook @ ", Sys.time())
  dyn <- as.data.table(ffs_dynasty_outlook(sim))
  fwrite(dyn, file.path(out, "dynasty_outlook.csv"))

  team_dyn <- dyn[, list(
    cur_capital = sum(cur_value),
    next_capital_mean = sum(next_value_mean),
    n_ranked = .N
  ), by = franchise_name][order(-cur_capital)]
  fwrite(team_dyn, file.path(out, "dynasty_capital.csv"))

  # dynasty-aware trade sheets
  if (config$run_trades) {
    tt <- merge(as.data.table(targets),
                dyn[, list(player_id, dyn_value = cur_value,
                           dyn_next_mean = next_value_mean, p_rise, p_exit)],
                by = "player_id", all.x = TRUE)
    fwrite(tt[order(-surplus)], file.path(out, "trade_targets.csv"))
    oo <- merge(offers,
                dyn[, list(player_id, dyn_value = cur_value,
                           dyn_next_mean = next_value_mean, p_rise, p_exit)],
                by = "player_id", all.x = TRUE)
    # sell candidates: low win value to you, high market value, value at risk
    oo[, sell_score := data.table::fifelse(
      is.na(dyn_value), NA_real_,
      dyn_value * (1 - p_rise) - 1500 * value_to_me
    )]
    fwrite(oo[order(-sell_score)], file.path(out, "trade_offers.csv"))
  }
}

message("DONE - outputs in ", out)
