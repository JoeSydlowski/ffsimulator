# Minimal, fast checks: pick valuation + trade_eval win-neutrality (no build_trades).
suppressMessages({library(data.table); pkgload::load_all(".", quiet=TRUE); library(ffscrapr)})
DIR  <- "dev/league_sims/1326464763936403456/2026-07-19"
sim  <- readRDS(file.path(DIR, "simulation.rds"))
conn <- ff_connect(platform="sleeper", league_id="1326464763936403456", season=2026)
dyn  <- fread(file.path(DIR, "dynasty_outlook.csv")); dyn[, player_id := as.character(player_id)]
pv   <- as.data.table(ffs_pick_values(sim, conn=conn))

joe  <- unique(as.data.table(sim$roster_scores)[franchise_name=="sox05syd"]$franchise_id)[1]
stud <- dyn[franchise_id==joe][order(-cur_value)][1]
solar <- pv[grepl("solarpool", franchise_name) & round==1][order(-cur_value)][1:4]
opp  <- solar$franchise_id[1]

# win-neutrality: eval with 4 picks in the package == eval with none
te_pick <- as.data.table(ffs_trade_eval(sim, joe, stud$player_id, opp, solar$player_id))
te_none <- as.data.table(ffs_trade_eval(sim, joe, stud$player_id, opp, character(0)))
cat("stud:", stud$player_name, "cur", round(stud$cur_value), "next", round(stud$next_value_mean),"\n")
cat("4 firsts cur", round(sum(solar$cur_value)), "next", round(sum(solar$next_value_mean)),"\n\n")
cat("== Puka-for-4-firsts (Joe row) ==\n")
print(te_pick[franchise_id==joe, .(h2h_delta=round(h2h_wins_delta,4), playoff_delta=round(playoff_pct_delta,4))])
cat("future_capital_delta:", round(sum(solar$next_value_mean) - stud$next_value_mean), "\n\n")
cat("win-neutral (picks stripped) — deltas identical with/without picks:\n")
cat("  h2h  identical:", isTRUE(all.equal(te_pick$h2h_wins_delta, te_none$h2h_wins_delta)), "\n")
cat("  playoff identical:", isTRUE(all.equal(te_pick$playoff_pct_delta, te_none$playoff_pct_delta)), "\n")
