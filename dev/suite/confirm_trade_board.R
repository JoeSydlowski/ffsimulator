# Confirm a player_trade_board.R scan on the n=2000 standings sim.
#
# player_trade_board.R enumerates + ranks packages on a fast search sim and saves
# the full board object (<Player>_board.rds, with send_ids/recv_ids/opponent list-
# cols). This script re-prices those deals on the big standings sim via the
# batched trade engine - the search sim's playoff deltas carry ~+-3% run-to-run
# noise, so the numbers a decision rides on come from the n=2000 sim.
#
# Gate: confirm every deal the other side plausibly accepts (`gettable` on the
# market model) that also clears a my-side score floor. The old "0 < gap_pct < 8
# AND search win-win" gate is gone: win-win was measured at 1/318 survival on the
# n=2000 sim, i.e. pure noise on a stud sell, and gap_pct has been superseded by
# the shape-fair market edge.
#
# The run is resumable. Every FFS_CONFIRM_CHECKPOINT deals the CSV is rewritten,
# and re-running skips deals already present in it, so a multi-hour job survives
# an interruption or a dead parallel worker.
#
# Usage:
#   FFS_LEAGUE_ID=1359546500786434048 FFS_MOVE_PLAYER="Puka Nacua" \
#     Rscript dev/suite/confirm_trade_board.R
# Knobs: FFS_MY_TEAM, FFS_CONFIRM_MIN_SCORE (0), FFS_CONFIRM_REQUIRE_GETTABLE (1),
#   FFS_CONFIRM_MAX (Inf), FFS_CONFIRM_WORKERS (1), FFS_CONFIRM_CHECKPOINT (25),
#   FFS_CONFIRM_RESUME (1), FFS_OPP_EDGE_TOL, FFS_BOARD_MAX_OPP_DROP.

suppressMessages({
  library(data.table)
  devtools::load_all(here::here(), quiet = TRUE)
})
options(ffsimulator.verbose = FALSE)

league_id  <- Sys.getenv("FFS_LEAGUE_ID", "1359546500786434048")
league_dir <- here::here("dev", "league_sims", league_id)
move_name  <- Sys.getenv("FFS_MOVE_PLAYER", "Puka Nacua")
# FFS_MOVE_PLAYER="" is the board's whole-roster mode, which writes roster_board.rds
whole_roster <- !nzchar(trimws(move_name))
safe <- if (whole_roster) "roster" else gsub("[^A-Za-z0-9]+", "_", move_name)

# newest folder that has BOTH the board object and the standings sim
board_rds <- Sys.glob(file.path(league_dir, "*", paste0(safe, "_board.rds")))
stopifnot("no <player>_board.rds found - run player_trade_board.R first" = length(board_rds) > 0)
out   <- dirname(board_rds[order(file.info(board_rds)$mtime, decreasing = TRUE)][1])
board <- as.data.table(readRDS(file.path(out, paste0(safe, "_board.rds"))))
message("board: ", file.path(out, paste0(safe, "_board.rds")), " (", nrow(board), " deals)")

## ---- gate ----------------------------------------------------------------------
min_score   <- as.numeric(Sys.getenv("FFS_CONFIRM_MIN_SCORE", "0"))
need_get    <- as.integer(Sys.getenv("FFS_CONFIRM_REQUIRE_GETTABLE", "1")) == 1L
confirm_max <- Sys.getenv("FFS_CONFIRM_MAX", "Inf")
confirm_max <- if (confirm_max %in% c("Inf", "inf")) Inf else as.numeric(confirm_max)

sub <- board[score > min_score]
if (need_get && "gettable" %in% names(board)) sub <- sub[gettable == TRUE]
setorder(sub, -score)
if (is.finite(confirm_max) && nrow(sub) > confirm_max) {
  message("capping at FFS_CONFIRM_MAX = ", confirm_max, " (of ", nrow(sub), " eligible)")
  sub <- sub[seq_len(confirm_max)]
}
message("subset to confirm: ", nrow(sub), " deals (score > ", min_score,
        if (need_get) ", gettable" else "", ")")
if (!nrow(sub)) { message("nothing matched the gate - lower FFS_CONFIRM_MIN_SCORE"); quit(save = "no") }

## ---- resume ---------------------------------------------------------------------
csv <- file.path(out, paste0(safe, "_shortlist_confirmed.csv"))
sub[, deal_key := paste(franchise_name, send, receive, sep = " | ")]
done <- NULL
if (as.integer(Sys.getenv("FFS_CONFIRM_RESUME", "1")) == 1L && file.exists(csv)) {
  prev <- tryCatch(fread(csv), error = function(e) NULL)
  if (!is.null(prev) && nrow(prev) && all(c("team", "send", "receive") %in% names(prev))) {
    prev[, deal_key := paste(team, send, receive, sep = " | ")]
    done <- prev[deal_key %in% sub$deal_key]
    sub <- sub[!deal_key %in% done$deal_key]
    message("resuming: ", nrow(done), " already confirmed on disk, ", nrow(sub), " to go")
  }
}
if (!nrow(sub)) { message("everything in the gate is already confirmed"); quit(save = "no") }

## ---- load the standings sim + build the engine once -----------------------------
message("loading n=2000 standings sim @ ", Sys.time())
sim    <- readRDS(file.path(out, "simulation.rds"))
config <- readRDS(file.path(out, "config.rds"))
n2k    <- length(unique(as.data.table(sim$summary_season)$season))
message("confirming on n=", n2k, " standings sim")

# my franchise = the one that rosters the move player in the standings sim
rs <- as.data.table(sim$roster_scores)
# single-player mode: whoever rosters him. Whole-roster mode has no such anchor,
# so fall back to the team name the board ran for.
if (whole_roster) {
  my_team <- Sys.getenv("FFS_MY_TEAM", if (!is.null(config$my_team)) config$my_team else "")
  stopifnot("whole-roster mode needs FFS_MY_TEAM or config$my_team" = nzchar(my_team))
  me <- unique(rs[franchise_name == my_team]$franchise_id)[1]
} else {
  me <- unique(rs[player_name == move_name]$franchise_id)[1]
}
stopifnot("could not resolve my franchise in the standings sim" = !is.na(me))

workers <- as.integer(Sys.getenv("FFS_CONFIRM_WORKERS", "1"))
engine  <- if (workers <= 1L) ffs_trade_engine(sim) else NULL
if (workers <= 1L && is.null(engine))
  message("NOTE: engine unavailable, falling back to the (much slower) legacy path")

## ---- confirm, in resumable chunks ----------------------------------------------
dyn <- fread(file.path(out, "dynasty_outlook.csv"),
             colClasses = list(character = c("player_id", "fantasypros_id")))
dyn[, player_id := as.character(player_id)]
fmt <- tryCatch(as.character(ffsimulator:::.ffs_detect_qb_format(sim$lineup_constraints)),
                error = function(e) "superflex")

playoff_value <- as.numeric(Sys.getenv("FFS_TRADE_PLAYOFF_VALUE", "68"))
opp_edge_tol  <- as.numeric(Sys.getenv("FFS_OPP_EDGE_TOL", "0.05"))
max_opp_drop  <- as.numeric(Sys.getenv("FFS_BOARD_MAX_OPP_DROP", "0.15"))
ckpt <- as.integer(Sys.getenv("FFS_CONFIRM_CHECKPOINT", "25"))

eval_chunk <- function(d) {
  ev <- as.data.table(ffs_trade_eval_many(
    sim,
    data.table(franchise_a = me, gives_a = d$send_ids,
               franchise_b = d$opponent, gives_b = d$recv_ids),
    engine = engine, workers = workers))
  m  <- ev[franchise_id == me][order(deal_id)]
  op <- ev[franchise_id != me][order(deal_id)]
  r <- data.table(
    team = d$franchise_name, shape = d$shape, send = d$send, receive = d$receive,
    send_value = d$send_value, recv_value = d$recv_value,
    send_ids = d$send_ids, recv_ids = d$recv_ids,
    fut = d$fut, s_you_pl = d$mine_pl, s_opp_pl = d$opp_pl,   # search-sim estimates
    my_playoff_delta = m$playoff_pct_delta, my_champ_delta = m$champion_pct_delta,
    opp_playoff_delta = op$playoff_pct_delta, opp_champ_delta = op$champion_pct_delta,
    opp_playoff_before = op$playoff_pct_before,
    future_capital_delta = d$future_capital_delta,
    you_h2h = round(m$h2h_wins_delta, 2),
    b_you = round(100 * m$playoff_pct_before, 1),
    b_opp = round(100 * op$playoff_pct_before, 1))
  # score both sides with the SAME function the board used, on n=2000 numbers -
  # board score and confirm score finally live on one scale
  ffs_deal_scores(r, dyn, my_haircut = ffsimulator:::.ffs_haircut(m$playoff_pct_before[1]),
                  playoff_value = playoff_value, format = fmt,
                  opp_edge_tol = opp_edge_tol, max_opp_drop = max_opp_drop)
}

acc <- if (is.null(done)) list() else list(done)
chunks <- split(seq_len(nrow(sub)), ceiling(seq_len(nrow(sub)) / ckpt))
t0 <- Sys.time()
for (k in seq_along(chunks)) {
  ii <- chunks[[k]]
  acc[[length(acc) + 1L]] <- eval_chunk(sub[ii])
  r <- rbindlist(acc, fill = TRUE)
  setorder(r, -score)
  drop <- intersect(c("send_ids", "recv_ids", "deal_key"), names(r))
  flat <- r[, setdiff(names(r), drop), with = FALSE]
  fwrite(flat[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 4) else x)], csv)
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  message(sprintf("  [%d/%d chunks] %d rows on disk | %.1f s/deal | eta %.0f min",
                  k, length(chunks), nrow(r), el / max(1, max(ii)),
                  (el / max(1, max(ii))) * (nrow(sub) - max(ii)) / 60))
}
res <- rbindlist(acc, fill = TRUE)
setorder(res, -score)
message("\nwrote ", nrow(res), " confirmed deals to ", csv)

## ---- how much did the search sim actually tell us? ------------------------------
if (nrow(res) > 3 && "s_you_pl" %in% names(res)) {
  ok <- !is.na(res$s_you_pl) & !is.na(res$my_playoff_delta)
  if (sum(ok) > 3) cat(sprintf(
    "\nsearch vs confirmed: corr(you) = %.2f, corr(opp) = %.2f over %d deals\n",
    cor(res$s_you_pl[ok], 100 * res$my_playoff_delta[ok]),
    cor(res$s_opp_pl[ok], 100 * res$opp_playoff_delta[ok]), sum(ok)))
}

## ---- console summary ------------------------------------------------------------
cat("\n== ", move_name, " CONFIRMED on n=", n2k, " ==\n", sep = "")
cat("   s_* = search sim estimate • the rest is n=", n2k, "\n\n", sep = "")
for (i in seq_len(min(nrow(res), 40L))) {
  r <- res[i]
  cat(sprintf("[%s]%s %-22s  grade %s\n   send %-42s -> get %-42s\n   you %+.1f%%pl %+.2fw %+.1f%%ch | them %+.1f%%pl edge %+.1f%% surplus %+d | fut %+d | (search you %+.1f%%)\n",
              if (isTRUE(r$gettable)) "GET " else "  - ", "", r$team, r$grade,
              r$send, r$receive,
              100 * r$my_playoff_delta, r$you_h2h, 100 * r$my_champ_delta,
              100 * r$opp_playoff_delta, 100 * r$opp_edge, round(r$opp_surplus),
              round(r$future_capital_delta), r$s_you_pl))
}
