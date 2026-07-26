# Fast end-to-end check of pick valuation + pick-inclusive trades.
# Uses the SAVED dynasty_outlook.csv (avoids the ~2min recompute).
suppressMessages({library(data.table); pkgload::load_all(".", quiet=TRUE); library(ffscrapr)})

DIR  <- "dev/league_sims/1326464763936403456/2026-07-19"
sim  <- readRDS(file.path(DIR, "simulation.rds"))
conn <- ff_connect(platform="sleeper", league_id="1326464763936403456", season=2026)
dyn  <- fread(file.path(DIR, "dynasty_outlook.csv"))
dyn[, player_id := as.character(player_id)]

pv <- as.data.table(ffs_pick_values(sim, conn=conn))
cat("== pick values ==  rows:", nrow(pv), "\n")

# unified asset table: players + picks (same schema)
common <- intersect(names(dyn), names(pv))
assets <- rbind(dyn[, ..common], pv[, ..common])
cat("unified assets:", nrow(assets), "(", nrow(pv), "picks )\n\n")

joe <- unique(as.data.table(sim$roster_scores)[franchise_name=="sox05syd"]$franchise_id)[1]
stud <- dyn[franchise_id==joe][order(-cur_value)][1]
cat("Joe:", joe, "| stud:", stud$player_name, "cur", round(stud$cur_value),
    "next", round(stud$next_value_mean), "\n")

solar <- pv[grepl("solarpool", franchise_name) & round==1][order(-cur_value)][1:4]
opp <- solar$franchise_id[1]
cat("4 firsts from solarpool cur sum:", round(sum(solar$cur_value)),
    "next sum:", round(sum(solar$next_value_mean)), "\n\n")

cat("== direct ffs_trade_eval (Puka-for-4-firsts; picks auto-stripped) ==\n")
te <- as.data.table(ffs_trade_eval(sim, joe, stud$player_id, opp, solar$player_id))
print(te[, .(franchise_id, h2h_delta=round(h2h_wins_delta,3),
             playoff_delta=round(playoff_pct_delta,3))])
cat("future_capital_delta:", round(sum(solar$next_value_mean) - stud$next_value_mean),
    "| value_gap:", round(sum(solar$cur_value) - stud$cur_value), "\n\n")

cat("== ffs_build_trades WITH picks (sell-for-picks path) ==\n")
tgt <- fread(file.path(DIR, "targets.csv")); tgt[, player_id := as.character(player_id)]
bt <- ffs_build_trades(sim, joe, targets=tgt, dynasty=dyn, picks=pv,
                       shapes=list(c(1,1),c(1,2),c(2,1)), future_weight=1,
                       must_send=stud$player_id, opponents=opp,
                       value_band=0.20, screen_n=20L, top_n=10L)
setDT(bt)
print(bt[, .(send, receive, my_playoff=round(my_playoff_delta,3),
             fut=round(future_capital_delta), score=round(score,2))])
