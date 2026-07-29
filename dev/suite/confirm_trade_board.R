# Confirm a subset of a player_trade_board.R scan on the n=2000 standings sim.
#
# player_trade_board.R enumerates + ranks packages on a fast search sim and saves
# the full board object (<Player>_board.rds, with send_ids/recv_ids/opponent list-
# cols). This script re-prices a CHOSEN subset of those deals on the big standings
# sim via ffs_trade_eval - the search sim's playoff deltas carry ~+-5% run-to-run
# noise (the whole reason n=60 "gives no real idea"), so the numbers a decision
# rides on come from the n=2000 sim.
#
# Subset gate (Joe's rule, 2026-07-26): keep board deals that are a MODEST OVERPAY
# and win-win on the search sim -> 0 < gap_pct < FFS_CONFIRM_GAP_MAX (8) AND both
# search playoff deltas > 0. Each survivor is then confirmed on n=2000; `pass` =
# the win-win holds up on the big sim (both confirmed playoff deltas > 0).
#
# Usage:
#   FFS_LEAGUE_ID=1359546500786434048 FFS_MOVE_PLAYER="Puka Nacua" \
#     Rscript dev/suite/confirm_trade_board.R
# Knobs: FFS_MY_TEAM, FFS_CONFIRM_GAP_MIN (0), FFS_CONFIRM_GAP_MAX (8),
#   FFS_CONFIRM_REQUIRE_WINWIN (1 = require both search deltas > 0).

suppressMessages({
  library(data.table)
  devtools::load_all(here::here(), quiet = TRUE)
})
options(ffsimulator.verbose = FALSE)

league_id  <- Sys.getenv("FFS_LEAGUE_ID", "1359546500786434048")
league_dir <- here::here("dev", "league_sims", league_id)
move_name  <- Sys.getenv("FFS_MOVE_PLAYER", "Puka Nacua")
safe       <- gsub("[^A-Za-z0-9]+", "_", move_name)

# newest folder that has BOTH the board object and the standings sim
board_rds <- Sys.glob(file.path(league_dir, "*", paste0(safe, "_board.rds")))
stopifnot("no <player>_board.rds found - run player_trade_board.R first" = length(board_rds) > 0)
out   <- dirname(board_rds[order(file.info(board_rds)$mtime, decreasing = TRUE)][1])
board <- as.data.table(readRDS(file.path(out, paste0(safe, "_board.rds"))))
sim   <- readRDS(file.path(out, "simulation.rds"))
config <- readRDS(file.path(out, "config.rds"))
n2k   <- length(unique(as.data.table(sim$summary_season)$season))
message("board: ", file.path(out, paste0(safe, "_board.rds")), " (", nrow(board), " deals)")
message("confirming on n=", n2k, " standings sim")

# my franchise = the one that rosters the move player in the standings sim
rs <- as.data.table(sim$roster_scores)
me <- unique(rs[player_name == move_name]$franchise_id)[1]
stopifnot("move player not found on any roster in the standings sim" = !is.na(me))

## ---- per-position reliability + smooth haircut (matches ffs_build_trades) -------
dyn <- fread(file.path(out, "dynasty_outlook.csv"),
             colClasses = list(character = c("player_id", "fantasypros_id")))
nv_by_id  <- setNames(dyn$next_value_mean, as.character(dyn$player_id))
cur_by_id <- setNames(dyn$cur_value, as.character(dyn$player_id))
pos_by_id <- setNames(dyn$pos, as.character(dyn$player_id))
fmt <- tryCatch(as.character(ffsimulator:::.ffs_detect_qb_format(sim$lineup_constraints)),
                error = function(e) "superflex")
# value-space calibration slopes capped into [0,1] - keep in sync with
# ffs_build_trades()'s future_reliability default (refit 2026-07-28 against the
# log-space transition model; the old set was fit pre-1bc4128 and went stale)
rel_pos <- if (identical(fmt, "1qb")) c(QB=0.651, RB=0.679, TE=1.000, WR=1.000) else
                                      c(QB=0.551, RB=1.000, TE=0.835, WR=1.000)
PICK_REL <- 0.55
rel_ids <- function(ids) { r <- rel_pos[pos_by_id[ids]]; r[is.na(r)] <- 0.905; r }
# reliability discounts the projected MOVE (next-cur), not the known current level
reliable_next <- function(ids) { ids <- ids[!grepl("^PICK_", ids)]; if (!length(ids)) return(0)
  cu <- cur_by_id[ids]; nx <- nv_by_id[ids]; rl <- rel_ids(ids); cu[is.na(cu)] <- 0; nx[is.na(nx)] <- 0
  sum(cu + rl * (nx - cu)) }
adj_future <- function(sids, rids, fcd) {   # reliability-adjusted future gain to ME
  praw <- sum(nv_by_id[rids[!grepl("^PICK_",rids)]], na.rm=TRUE) - sum(nv_by_id[sids[!grepl("^PICK_",sids)]], na.rm=TRUE)
  (reliable_next(rids) - reliable_next(sids)) + PICK_REL * (fcd - praw)
}
hc <- function(p) pmin(pmax(1.30 - p, 0.60), 1.00)     # smooth win-now haircut
grade_of <- function(s) fifelse(s>=18,"A",fifelse(s>=10,"B",fifelse(s>=4,"C",fifelse(s>=0,"D","F"))))

## ---- subset gate --------------------------------------------------------------
gap_min <- as.numeric(Sys.getenv("FFS_CONFIRM_GAP_MIN", "0"))
gap_max <- as.numeric(Sys.getenv("FFS_CONFIRM_GAP_MAX", "8"))
need_ww <- as.integer(Sys.getenv("FFS_CONFIRM_REQUIRE_WINWIN", "1")) == 1L
sub <- board[gap_pct > gap_min & gap_pct < gap_max]
if (need_ww) sub <- sub[mine_pl > 0 & opp_pl > 0]
setorder(sub, -score)
message("subset to confirm: ", nrow(sub), " deals (gap ", gap_min, "-", gap_max,
        "%", if (need_ww) ", search win-win" else "", ")")
if (!nrow(sub)) { message("nothing matched the gate - widen FFS_CONFIRM_GAP_MAX"); quit(save = "no") }

## ---- confirm each on the n=2000 sim -------------------------------------------
# Checkpoint the CSV every FFS_CONFIRM_CHECKPOINT deals so a long overnight run
# survives an interruption near the end (partial results are always on disk).
csv <- file.path(out, paste0(safe, "_shortlist_confirmed.csv"))
eval_one <- function(i) {
  d  <- sub[i]
  te <- tryCatch(
    as.data.table(ffs_trade_eval(sim, me, d$send_ids[[1]], d$opponent, d$recv_ids[[1]])),
    error = function(e) { message("  eval failed for ", d$send, " -> ", d$receive,
                                  ": ", conditionMessage(e)); NULL })
  if (is.null(te)) return(NULL)
  mrow <- te[franchise_id == me]; orow <- te[franchise_id == d$opponent]
  message(sprintf("  [%3d/%3d] %-22s %s -> %s", i, nrow(sub), d$franchise_name, d$send, d$receive))
  data.table(
    team = d$franchise_name, shape = d$shape, send = d$send, receive = d$receive,
    gap_pct = d$gap_pct, fut = d$fut,
    s_you_pl = d$mine_pl, s_opp_pl = d$opp_pl,                       # search-sim (n=240)
    you_pl  = round(100 * mrow$playoff_pct_delta, 1),               # confirmed (n=2000)
    opp_pl  = round(100 * orow$playoff_pct_delta, 1),
    you_h2h = round(mrow$h2h_wins_delta, 2),
    you_champ = round(100 * mrow$champion_pct_delta, 1),
    opp_champ = round(100 * orow$champion_pct_delta, 1),
    b_you = round(100 * mrow$playoff_pct_before, 1),                # baseline playoff odds
    b_opp = round(100 * orow$playoff_pct_before, 1),
    adj_fut = round(adj_future(d$send_ids[[1]], d$recv_ids[[1]], d$fut)))  # per-position reliability
}
# rank by the fixed-rate deal score (default): score_fw = you_pl% + fut/future_rate
# + winwin bonus, all in equivalent playoff % (same currency as ffs_build_trades).
# future_rate = playoff_value / (future_certainty * risk[posture]); posture from my
# baseline playoff odds. Legacy z-blend under FFS_TRADE_SCORE_MODE=zscore. Rates
# from dev/suite/exchange_rate_study.R.
score_mode       <- Sys.getenv("FFS_TRADE_SCORE_MODE", "rate")
playoff_value    <- as.numeric(Sys.getenv("FFS_TRADE_PLAYOFF_VALUE", "68"))
future_certainty <- as.numeric(Sys.getenv("FFS_TRADE_FUTURE_CERTAINTY", "0.905"))
fw  <- as.numeric(Sys.getenv("FFS_TRADE_FUTURE_WEIGHT", "3"))   # legacy zscore mode only
zsc <- function(x) { s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) rep(0, length(x)) else (x - mean(x, na.rm = TRUE)) / s }
write_partial <- function(acc) {
  r <- rbindlist(acc[!vapply(acc, is.null, logical(1))])
  if (nrow(r)) {
    r[, pass := you_pl > 0 & opp_pl > 0]
    if (score_mode == "rate") {
      # per-position reliability-adjusted future * smooth haircut / price, in
      # equivalent playoff %. opp_score = opponent's OWN valuation (their haircut)
      # -> gettable. Matches ffs_build_trades.
      r[, score_fw  := you_pl + adj_fut * hc(b_you/100) / playoff_value + 0.5 * as.numeric(pass)]
      r[, opp_score := opp_pl + (-adj_fut) * hc(b_opp/100) / playoff_value]
      r[, gettable  := opp_score >= -3]
      r[, grade     := grade_of(score_fw)]
    } else {
      r[, score_fw := zsc(you_pl) + fw * zsc(fut) + 0.5 * as.numeric(pass)]
    }
    setorder(r, -score_fw, -you_pl)
    fwrite(r, csv)
  }
  r
}
ckpt <- as.integer(Sys.getenv("FFS_CONFIRM_CHECKPOINT", "25"))
acc <- vector("list", nrow(sub))
for (i in seq_len(nrow(sub))) {
  acc[[i]] <- eval_one(i)
  if (i %% ckpt == 0 || i == nrow(sub)) {
    r <- write_partial(acc)
    message("  checkpoint @ ", Sys.time(), ": ", nrow(r), " rows on disk")
  }
}
res <- write_partial(acc)          # pass column set inside; win-win survives the big sim
message("\nwrote ", nrow(res), " confirmed deals (", sum(res$pass), " pass) to ", csv)

## ---- console summary ----------------------------------------------------------
cat("\n== ", move_name, " shortlist CONFIRMED on n=", n2k,
    " (search gate: 0<gap<", gap_max, "%, win-win) ==\n", sep = "")
cat("   s_* = search sim (n=240) • you_pl/opp_pl = confirmed (n=", n2k, ")\n\n", sep = "")
for (i in seq_len(nrow(res))) {
  r <- res[i]
  cat(sprintf("[%s]%s %-22s\n   send %-42s -> get %-42s\n   confirmed: you %+.1f%%pl %+.2fw %+.1f%%ch | opp %+.1f%%pl %+.1f%%ch | fut %+d | (search you %+.1f%% opp %+.1f%%)\n",
              if (r$pass) "PASS" else "  - ", "", r$team,
              r$send, r$receive,
              r$you_pl, r$you_h2h, r$you_champ, r$opp_pl, r$opp_champ, r$fut,
              r$s_you_pl, r$s_opp_pl))
}
