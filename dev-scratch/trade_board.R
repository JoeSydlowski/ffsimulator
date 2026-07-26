# Comprehensive Jon trade board: simpler shapes, fair band, QB deals allowed,
# >=1 idea per opponent (throw in their own 1st when no player-only deal fits).
suppressMessages({library(data.table); pkgload::load_all(".", quiet=TRUE); library(ffscrapr)})
options(ffsimulator.verbose = FALSE)
BASE <- "dev/league_sims/1359546500786434048"
cfg  <- readRDS(file.path(BASE, "2026-07-21", "config.rds"))
conn <- sleeper_connect(season = cfg$season, league_id = cfg$league_id)

simf <- "dev-scratch/jon_n60.rds"
if (file.exists(simf)) { sim <- readRDS(simf) } else {
  sim <- ff_simulate(conn, n_seasons = 60, version = "v3", lineup_method = "rank",
                     return = "all", actual_schedule = TRUE, replacement_level = FALSE)
  saveRDS(sim, simf)
}

dyn <- fread(file.path(BASE, "2026-07-20", "dynasty_outlook.csv"))
dyn[, player_id := as.character(player_id)]
dyn[, franchise_id := as.character(franchise_id)]
joe <- dyn[franchise_name == "sox05syd"]$franchise_id[1]
frs <- unique(dyn[, .(franchise_id, franchise_name)])

# each franchise's own next-three firsts, slotted by projected finish -> pick values
picks_df <- CJ(season = c(2027L, 2028L, 2029L), franchise_id = frs$franchise_id)[
  , `:=`(round = 1L, original_franchise_id = franchise_id)]
pv <- as.data.table(ffs_pick_values(sim, picks = picks_df,
        pick_curve = "dev/data/pick_value_curve.csv", format = "superflex"))
# make pick "player_name" readable per team
pv <- merge(pv, frs, by = "franchise_id", suffixes = c("", ".y"), all.x = TRUE)

# broad search: simpler shapes, wide-ish band so the BOARD has range to sort;
# report gap so 2-8% can be filtered. QB deals allowed (no must_send).
board <- ffs_build_trades(
  sim, joe, dynasty = dyn, picks = pv,
  shapes = list(c(1,1), c(1,2), c(2,1), c(2,2)),
  value_band = 0.12, uneven_shade = 0.10, consolidation_penalty = 0.03,
  future_weight = 1, min_future_delta = -750, max_opp_drop = 0.25,
  winwin_bonus = 0.5, screen_n = 120L, top_n = 400L)
setDT(board)
board <- merge(board, frs, by.x = "opponent", by.y = "franchise_id", all.x = TRUE)
board[, `:=`(
  gap_pct = round(100 * value_gap / recv_value, 1),
  mine_pl = round(100 * my_playoff_delta, 1),
  opp_pl  = round(100 * opp_playoff_delta, 1),
  fut     = round(future_capital_delta),
  nS = lengths(send_ids), nR = lengths(recv_ids),
  uses_pick = vapply(seq_len(.N), function(i)
    any(grepl("^PICK_", c(send_ids[[i]], recv_ids[[i]]))), logical(1)))]
board[, shape := paste0(nS, "-for-", nR)]
setorder(board, franchise_name, -score)

# save full board for sorting later
outcsv <- file.path(BASE, "trade_board.csv")
fwrite(board[, .(team = franchise_name, shape, send, receive,
  send_value = round(send_value), recv_value = round(recv_value), gap_pct,
  mine_pl, opp_pl, fut, win_win, uses_pick, score = round(score, 2))], outcsv)
cat("wrote", nrow(board), "ideas to", outcsv, "\n\n")

# per-team coverage: best fair (gap 2-8%, opp not badly hurt) idea per opponent
cat("== teams and whether a fair (gap 2-8%, opp>=-10%) idea exists ==\n")
cov <- board[gap_pct >= 2 & gap_pct <= 8 & opp_pl >= -10]
for (t in setdiff(frs$franchise_name, "sox05syd")) {
  d <- cov[franchise_name == t][order(-score)]
  if (nrow(d)) { r <- d[1]
    cat(sprintf("\n[%s] %s  gap %+.1f%%  you %+.1f%%  opp %+.1f%%  fut %+d %s%s\n",
        t, r$shape, r$gap_pct, r$mine_pl, r$opp_pl, r$fut,
        ifelse(r$win_win,"WIN-WIN ",""), ifelse(r$uses_pick,"(+pick)","")))
    cat("   send:", r$send, "\n   recv:", r$receive, "\n")
  } else cat(sprintf("\n[%s] no fair player/pick deal in band\n", t))
}
