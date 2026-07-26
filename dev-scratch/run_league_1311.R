# Trimmed suite run for a league that isn't Joe's:
# season sim + playoff odds + dynasty outlook. No trade intel, no WAR.
library(data.table)
library(ggplot2)
devtools::load_all(here::here(), quiet = TRUE)

config <- list(
  league_id = "1311821354563026944",
  platform = "sleeper",
  season = 2026L,
  n_sims = as.integer(Sys.getenv("FFS_NSEASONS", "2000")),
  version = "v3",
  lineup_method = "rank",
  replacement_level = FALSE,
  actual_schedule = TRUE,
  playoff_slots = 6L
)

out <- here::here("dev", "league_sims", config$league_id, format(Sys.Date()))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
saveRDS(config, file.path(out, "config.rds"))

set.seed(config$season)
conn <- ffscrapr::sleeper_connect(season = config$season, league_id = config$league_id)

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
champ_tbl <- ffsimulator:::.ffs_champion_pct(as.data.table(sim$summary_week), ss)
champ_tbl <- merge(champ_tbl, unique(ss[, list(franchise_id, franchise_name)]), by = "franchise_id")
odds <- merge(odds, champ_tbl[, list(franchise_name, champion_pct)],
              by = "franchise_name", all.x = TRUE)[order(-champion_pct)]
fwrite(odds, file.path(out, "playoff_odds.csv"))

for (t in c("wins", "rank", "points")) {
  p <- try(autoplot(sim, type = t), silent = TRUE)
  if (!inherits(p, "try-error")) {
    ggsave(file.path(out, paste0(t, ".png")), p, width = 10, height = 7.5, dpi = 150)
  }
}

# dynasty outlook with FantasyCalc market values (auto-detects league format)
dyn_vals <- tryCatch(
  rbind(fc_dynasty_values(num_qbs = 1), fc_dynasty_values(num_qbs = 2)),
  error = function(e) { message("FantasyCalc unavailable: ", conditionMessage(e)); NULL })
message("dynasty outlook @ ", Sys.time())
dyn <- as.data.table(ffs_dynasty_outlook(sim, dynasty_values = dyn_vals))
fwrite(dyn, file.path(out, "dynasty_outlook.csv"))
team_dyn <- dyn[, list(
  cur_capital = sum(cur_value),
  next_capital_mean = sum(next_value_mean),
  n_ranked = .N
), by = franchise_name][order(-cur_capital)]
fwrite(team_dyn, file.path(out, "dynasty_capital.csv"))

message("DONE - outputs in ", out)
