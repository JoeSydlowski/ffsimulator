# Posture-aware deals for the teams the sell-boards missed:
#  - mkbarz (REBUILDER): reverse deal - Joe GIVES future (young WRs + pick) for a
#    win-now piece; they sacrifice playoffs but must GAIN future value.
#  - NFC / CrashOutChris (contenders): standard Joe-sells-Puka, they want win-now.
suppressMessages({library(data.table); pkgload::load_all(".", quiet=TRUE); library(ffscrapr)})
options(ffsimulator.verbose=FALSE)
BASE <- "dev/league_sims/1359546500786434048"
sim  <- readRDS("dev-scratch/jon_n60.rds")
dyn  <- fread(file.path(BASE,"2026-07-20","dynasty_outlook.csv"))
dyn[, `:=`(player_id=as.character(player_id), franchise_id=as.character(franchise_id))]
joe <- dyn[franchise_name=="sox05syd"]$franchise_id[1]
frs <- unique(dyn[,.(franchise_id,franchise_name)])
fid <- function(n) frs[franchise_name==n]$franchise_id[1]
picks_df <- CJ(season=2027L, franchise_id=frs$franchise_id)[,`:=`(round=1L, original_franchise_id=franchise_id)]
pv <- as.data.table(ffs_pick_values(sim, picks=picks_df, pick_curve="dev/data/pick_value_curve.csv", format="superflex"))

fmt <- function(b,tag){ b<-as.data.table(b); if(!nrow(b)) return(b)
  b <- merge(b, frs, by.x="opponent", by.y="franchise_id", all.x=TRUE)
  b[, `:=`(tag=tag, gap=round(100*value_gap/recv_value,1), mine=round(100*my_playoff_delta,1),
     opp=round(100*opp_playoff_delta,1), fut=round(future_capital_delta),
     nS=lengths(send_ids), nR=lengths(recv_ids))]
  b[, `:=`(shape=paste0(nS,"-for-",nR), opp_future=-fut)][] }

# REVERSE: Joe buys win-now from mkbarz with youth+picks; mkbarz indifferent to
# playoffs (max_opp_drop=Inf) but must gain future (opp_future>=0).
R <- fmt(ffs_build_trades(sim, joe, dynasty=dyn, picks=pv, opponents=fid("mkbarz"),
      shapes=list(c(1,1),c(1,2),c(2,2)), value_band=0.08, uneven_shade=0.08,
      future_weight=0, min_future_delta=-Inf, max_opp_drop=Inf, screen_n=40L, top_n=30L), "reverse")

# CONTENDERS want win-now: Joe sells Puka to NFC / CrashOutChris
S <- fmt(ffs_build_trades(sim, joe, dynasty=dyn, picks=pv, must_send=dyn[player_name=="Puka Nacua"]$player_id[1],
      opponents=c(fid("NFC Nostalgia"), fid("CrashOutChris'Kickoff"), fid("mjmx3")),
      shapes=list(c(1,2),c(2,2),c(2,3)), value_band=0.08, uneven_shade=0.06, consolidation_penalty=0,
      future_weight=1, min_future_delta=-750, max_opp_drop=0.10, screen_n=60L, top_n=40L), "sell")

show <- function(b, accept_expr, lbl){
  cat("\n====", lbl, "====\n")
  if(!nrow(b)){cat("  (no deals)\n"); return()}
  b <- b[eval(accept_expr, b)]
  if(!nrow(b)){cat("  (none acceptable under posture rule)\n"); return()}
  for(t in unique(b$franchise_name)){ r <- b[franchise_name==t][order(-score)][1]
    cat(sprintf("\n[%s] %s gap%+.1f%% you%+.1f%% opp_pl%+.1f%% opp_fut%+d\n   %s -> %s\n",
      t, r$shape, r$gap, r$mine, r$opp, r$opp_future, r$send, r$receive)) }
}
show(R, quote(opp_future >= -100 & gap <= 8 & mine > 0), "REBUILDER (mkbarz): they gain future, you win now")
show(S, quote(gap <= 8 & opp >= -6), "CONTENDERS: they take Puka for win-now")
