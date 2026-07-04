# Run a full season simulation for a Sleeper league and save outputs
# Usage: FFS_LEAGUE_ID=<id> FFS_SEASON=2026 Rscript dev/league_sims/run_league_sim.R

library(data.table)
devtools::load_all(here::here(), quiet = TRUE)

league_id <- Sys.getenv("FFS_LEAGUE_ID", "1359546500786434048")
season <- as.integer(Sys.getenv("FFS_SEASON", "2026"))
n_seasons <- as.integer(Sys.getenv("FFS_NSEASONS", "200"))

out <- here::here("dev", "league_sims", league_id)
dir.create(out, recursive = TRUE, showWarnings = FALSE)

set.seed(2026)
conn <- ffscrapr::sleeper_connect(season = season, league_id = league_id)

sim <- ff_simulate(
  conn,
  n_seasons = n_seasons,
  version = "v3", # trajectory resampling: tuned kernel + team copula defaults
  lineup_method = "rank", # start/sit from simulated weekly rankings
  return = "all"
)

saveRDS(sim, file.path(out, "simulation.rds"))
fwrite(sim$summary_simulation, file.path(out, "summary_simulation.csv"))
fwrite(sim$summary_season, file.path(out, "summary_season.csv"))

# playoff-odds style extras from the season-level table
ss <- as.data.table(sim$summary_season)
ss[, rank_season := frank(-h2h_wins, ties.method = "random"), by = season]
odds <- ss[, list(
  mean_wins = mean(h2h_wins),
  p25_wins = quantile(h2h_wins, .25),
  p75_wins = quantile(h2h_wins, .75),
  mean_pf = mean(points_for),
  playoff_pct = mean(rank_season <= 6), # top-6 make playoffs on sleeper default
  top_seed_pct = mean(rank_season == 1),
  last_pct = mean(rank_season == max(rank_season))
), by = franchise_name][order(-mean_wins)]
fwrite(odds, file.path(out, "playoff_odds.csv"))

library(ggplot2)
for (t in c("wins", "rank", "points")) {
  p <- try(autoplot(sim, type = t), silent = TRUE)
  if (!inherits(p, "try-error")) {
    ggsave(file.path(out, paste0(t, ".png")), p, width = 10, height = 7.5, dpi = 150)
  }
}

cat("\n==== projected standings (", n_seasons, "sims ) ====\n")
print(as.data.frame(odds[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)]))
cat("\nOutputs in:", out, "\n")
