# Fast end-to-end build_trades-with-picks check on a SMALL sim (n_seasons=4).
suppressMessages({library(data.table); pkgload::load_all(".", quiet=TRUE)})
conn <- mfl_connect(2021, 22627)
sim  <- ff_simulate(conn, n_seasons = 4, version = "v3", lineup_method = "rank", return = "all")
rs   <- as.data.table(sim$roster_scores)
real <- rs[!grepl("^(QB|RB|WR|TE|K)_\\d+$", player_id)]
me   <- real$franchise_id[[1]]
tt   <- ffs_trade_targets(sim, me, top_n = 8)
ids  <- unique(c(real$player_id, tt$player_id))
set.seed(1)
dyn  <- data.frame(player_id = ids, cur_value = runif(length(ids), 200, 3000))
dyn$next_value_mean <- dyn$cur_value * runif(length(ids), 0.8, 1.2)

opps <- setdiff(unique(real$franchise_id), me)[1:2]
picks <- data.frame(
  player_id   = c("PICK_2027_1_me","PICK_2027_1_o1","PICK_2027_2_o2"),
  player_name = c("2027 R1 (me)","2027 R1 (o1)","2027 R2 (o2)"),
  franchise_id= c(me, opps), pos = "PICK",
  cur_value   = c(2500, 3000, 1200),
  next_value_mean = c(2700, 3200, 1300), stringsAsFactors = FALSE)

sell_p <- real[franchise_id == me]$player_id[[1]]
sell_n <- real[player_id == sell_p]$player_name[[1]]

cat("== sell-for-picks: must_send", sell_n, "to o1 ==\n")
bt <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn, picks = picks,
                       value_band = 0.8, future_weight = 1, must_send = sell_p,
                       opponents = opps[[1]], shapes = list(c(1,1),c(1,2)), top_n = 15)
setDT(bt)
cat("rows:", nrow(bt), "\n")
if (nrow(bt)) print(bt[, .(send, receive, my_playoff=round(my_playoff_delta,3),
                           fut=round(future_capital_delta), score=round(score,2))])
got_pick <- if (nrow(bt)) grepl("R[12]", bt$receive) else logical(0)
cat("\ndeals receiving a pick:", sum(got_pick), "| all future_capital finite:",
    all(is.finite(bt$future_capital_delta)), "\n")
cat("OK\n")
