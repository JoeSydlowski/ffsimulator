# Jon trade board v2: 2027 picks only; no filler (2nd send piece must be REAL, else
# Puka alone); dedicated QB-upgrade board that sends Daniel Jones back; prefer
# 1-for-2 / 2-for-2 over 2-for-3.
suppressMessages({library(data.table); pkgload::load_all(".", quiet=TRUE); library(ffscrapr)})
options(ffsimulator.verbose = FALSE)
BASE <- "dev/league_sims/1359546500786434048"
sim  <- readRDS("dev-scratch/jon_n60.rds")                 # cached n=60 search sim
dyn  <- fread(file.path(BASE, "2026-07-20", "dynasty_outlook.csv"))
dyn[, `:=`(player_id = as.character(player_id), franchise_id = as.character(franchise_id))]
joe    <- dyn[franchise_name == "sox05syd"]$franchise_id[1]
nacua  <- dyn[player_name == "Puka Nacua"]$player_id[1]
djones <- dyn[player_name == "Daniel Jones"]$player_id[1]
frs    <- unique(dyn[, .(franchise_id, franchise_name)])

# REAL players only: drop filler (cur_value < 2000) from the tradeable universe
# by NA-ing their value in the table build_trades reads (Puka/Jones always kept).
REAL <- 2000
dyn_real <- copy(dyn)
dyn_real[cur_value < REAL & !(player_id %in% c(nacua, djones)), cur_value := NA_real_]

# 2027 first-round picks only (near-term, least discounted)
picks_df <- CJ(season = 2027L, franchise_id = frs$franchise_id)[
  , `:=`(round = 1L, original_franchise_id = franchise_id)]
pv <- as.data.table(ffs_pick_values(sim, picks = picks_df,
        pick_curve = "dev/data/pick_value_curve.csv", format = "superflex"))

fmt_board <- function(b, tag){
  b <- as.data.table(b); if(!nrow(b)) return(b)
  b <- merge(b, frs, by.x="opponent", by.y="franchise_id", all.x=TRUE)
  b[, `:=`(src=tag, gap_pct=round(100*value_gap/recv_value,1),
    mine_pl=round(100*my_playoff_delta,1), opp_pl=round(100*opp_playoff_delta,1),
    fut=round(future_capital_delta), nS=lengths(send_ids), nR=lengths(recv_ids),
    uses_pick=vapply(seq_len(.N), function(i) any(grepl("^PICK_",
      c(send_ids[[i]], recv_ids[[i]]))), logical(1)))]
  b[, shape := paste0(nS,"-for-",nR)][]
}

# tighter fairness: uneven_shade 0.06 + no consolidation floor keeps the premium
# under ~8% on 1-2/2-3 too; max_opp_drop 0.10 stops deals that gut the other side.
# Board A: Puka-centric, real 2nd piece or Puka alone (1-2)
A <- fmt_board(ffs_build_trades(sim, joe, dynasty=dyn_real, picks=pv, must_send=nacua,
      shapes=list(c(1,2),c(2,2),c(2,3)), value_band=0.06, uneven_shade=0.06,
      consolidation_penalty=0, future_weight=1, min_future_delta=-750,
      max_opp_drop=0.10, winwin_bonus=0.5, screen_n=100L, top_n=250L), "puka")

# Board B: QB upgrade, must send Puka + Daniel Jones (QB back to the QB seller)
qb_owners <- unique(dyn[pos=="QB" & cur_value>=6000 & franchise_id!=joe]$franchise_id)
B <- fmt_board(ffs_build_trades(sim, joe, dynasty=dyn_real, picks=pv,
      must_send=c(nacua, djones), opponents=qb_owners, shapes=list(c(2,2),c(2,3)),
      value_band=0.06, uneven_shade=0.06, consolidation_penalty=0, future_weight=1,
      min_future_delta=-750, max_opp_drop=0.10, winwin_bonus=0.5, screen_n=60L, top_n=100L), "qb")

board <- rbind(A, B, fill=TRUE)
# hard fairness cap on top of the builder's band (belt and suspenders)
board <- board[gap_pct <= 8 & opp_pl >= -8]
keep <- c("franchise_name","src","shape","send","receive","send_value","recv_value",
          "gap_pct","mine_pl","opp_pl","fut","win_win","uses_pick","score")
out <- board[, ..keep]; setnames(out, "franchise_name", "team")
out[, `:=`(send_value=round(send_value), recv_value=round(recv_value), score=round(score,2))]
fwrite(out, file.path(BASE, "trade_board_v2.csv"))
cat("wrote", nrow(out), "ideas (A:", nrow(A), " B:", nrow(B), ") to trade_board_v2.csv\n\n")

# per-team best FAIR (gap 2-8, opp>=-8), preferring simpler shapes
board[, shape_rank := match(shape, c("1-for-2","2-for-2","2-for-3"))]
cat("== realistic offer per team (fair, simplest shape first) ==\n")
for(t in sort(setdiff(frs$franchise_name, "sox05syd"))){
  d <- board[franchise_name==t]   # board already capped to gap<=8 & opp>=-8
  if(nrow(d)){ r <- d[order(shape_rank, -score)][1]
    cat(sprintf("\n[%s] %s gap%+.1f%% you%+.1f%% opp%+.1f%% fut%+d %s%s [%s]\n",
        t, r$shape, r$gap_pct, r$mine_pl, r$opp_pl, r$fut,
        ifelse(r$win_win,"WW ",""), ifelse(r$uses_pick,"+2027pk",""), r$src))
    cat("   ", r$send, "  ->  ", r$receive, "\n", sep="")
  } else cat(sprintf("\n[%s] no fair deal\n", t))
}
