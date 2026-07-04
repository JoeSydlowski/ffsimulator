# Wins-above-replacement for every rostered player in a Sleeper league
# Usage: FFS_LEAGUE_ID=<id> FFS_SEASON=2026 FFS_NSEASONS=40 Rscript dev/league_sims/run_league_war.R

library(data.table)
devtools::load_all(here::here(), quiet = TRUE)

league_id <- Sys.getenv("FFS_LEAGUE_ID", "1359546500786434048")
season <- as.integer(Sys.getenv("FFS_SEASON", "2026"))
n_seasons <- as.integer(Sys.getenv("FFS_NSEASONS", "40"))

out <- here::here("dev", "league_sims", league_id)
dir.create(out, recursive = TRUE, showWarnings = FALSE)

set.seed(2026)
conn <- ffscrapr::sleeper_connect(season = season, league_id = league_id)

wa <- ff_wins_added(
  conn,
  n_seasons = n_seasons,
  version = "v3",
  lineup_method = "rank"
)

saveRDS(wa, file.path(out, "wins_added.rds"))
war <- as.data.table(wa$war)
fwrite(war, file.path(out, "war_players.csv"))

cat("\n==== top 30 by allplay wins added ====\n")
print(as.data.frame(war[order(-allplay_winpct)][1:30,
      list(player_name, pos, franchise_name, allplay_winpct = round(allplay_winpct, 3),
           h2h_wins = round(h2h_wins, 2), points_for = round(points_for, 1))]))

cat("\n==== team WAR totals ====\n")
print(as.data.frame(war[, list(total_allplay_war = round(sum(allplay_winpct), 2)),
                        by = franchise_name][order(-total_allplay_war)]))
cat("\nDONE\n")
