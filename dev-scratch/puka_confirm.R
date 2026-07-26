# Confirm the top Puka offers' playoff/champ deltas on the n=2000 standings sim.
suppressMessages({library(data.table); pkgload::load_all(".", quiet=TRUE)})
sim <- readRDS("dev/league_sims/1359546500786434048/2026-07-21/simulation.rds")
o   <- as.data.table(readRDS("dev-scratch/puka_offers.rds"))
setorder(o, -score)
sel <- head(o, 8)
joe <- o$franchise_id[1]  # not stored; derive below
# owner column is 'opponent'; my franchise = the one sending Puka -> from send_ids owner
rs  <- as.data.table(sim$roster_scores)
joe <- unique(rs[player_name == "Puka Nacua"]$franchise_id)[1]

res <- rbindlist(lapply(seq_len(nrow(sel)), function(i){
  d <- sel[i]
  te <- as.data.table(ffs_trade_eval(sim, joe, d$send_ids[[1]], d$opponent, d$recv_ids[[1]]))
  data.table(rank=i, opp=d$franchise_name, send=d$send, recv=d$receive,
    mine_pl = round(100*te[franchise_id==joe]$playoff_pct_delta,1),
    opp_pl  = round(100*te[franchise_id==d$opponent]$playoff_pct_delta,1),
    mine_ch = round(100*te[franchise_id==joe]$champion_pct_delta,1),
    opp_ch  = round(100*te[franchise_id==d$opponent]$champion_pct_delta,1),
    fut = round(d$future_capital_delta), ww_n2k = NA)
}))
res[, ww_n2k := mine_pl>0 & opp_pl>0]
cat("== top offers CONFIRMED on n=2000 (playoff & championship deltas) ==\n")
for(i in seq_len(nrow(res))){ r<-res[i]
  cat(sprintf("\n[%d] %s  mine %+.1f%%pl/%+.1f%%ch  opp %+.1f%%pl/%+.1f%%ch  fut %+d %s\n",
      r$rank, r$opp, r$mine_pl, r$opp_pl, r$mine_ch, r$opp_ch, r$fut,
      ifelse(r$ww_n2k,"WIN-WIN","")))
  cat("    send:", r$send, "\n    recv:", r$recv, "\n")
}
