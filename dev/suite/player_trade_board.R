# Move-a-player trade board: given ONE player you want to move, scan every team
# for fair, realistic packages, and plot them so you can pick the best deal.
#
# The scan is EXHAUSTIVE by default: every package shape (1..3 pieces each way)
# against every team, with no per-team cap, and every deal inside the fair band
# gets an exact evaluation. What keeps that affordable is that the acceptance
# test is sim-free, so it prunes before the expensive step.
#
# The board rules:
#   * ONE value band, `FFS_FAIR_BAND` (default -5%..+5%), measured against the
#     SHAPE-FAIR price rather than a raw zero gap - a package must overpay the
#     stud it is traded for, so a 1-for-2 is fair at +`FFS_FAIR_PREMIUM` (5%),
#     not at 0. This replaces the old even/uneven band pair.
#   * The OTHER SIDE is priced on current market value, not on my dynasty
#     reliability model: they accept when their edge is within `FFS_OPP_EDGE_TOL`
#     of fair. Their playoff odds only VETO a deal (`FFS_BOARD_MAX_OPP_DROP`,
#     15%), and even then only flag `gettable` - an ordinary WR-for-WR+RB that
#     dings their odds is not something a real owner refuses.
#   * the second SEND piece must be a real player (min_piece_value), not filler;
#   * draft PICKS fill value gaps (nearest, least-discounted year);
#   * near-identical "player + filler / different pick year" ideas are deduped.
#
# Usage (env-configurable, defaults to Puka in the Jon superflex league):
#   FFS_LEAGUE_ID=1359546500786434048 FFS_MOVE_PLAYER="Puka Nacua" \
#     Rscript dev/suite/player_trade_board.R
#
# WHOLE-ROSTER MODE - set FFS_MOVE_PLAYER="" and every asset you own becomes a
# candidate headline piece instead of just one named player. Writes
# roster_trade_board.csv / roster_board.rds and adds a per-asset summary
# ("which of my players actually generate tradeable ideas"). Measured on the Jon
# league at production defaults: 42,246 evals / 1,571 ideas / 18 headline senders,
# ~4.5 h search on 14 workers - an overnight job once the n=2000 confirm follows.
#   FFS_MOVE_PLAYER="" Rscript dev/suite/player_trade_board.R
# Other knobs: FFS_MY_TEAM, FFS_TRADE_NSIMS (search sim n, default 60),
#   FFS_SEARCH_SIM (path to a cached search sim), FFS_PICK_SEASON (nearest pick
#   year, default = soonest future draft), FFS_FAIR_BAND ("lo,hi" or "off" for
#   the legacy bands), FFS_FAIR_PREMIUM, FFS_OPP_EDGE_TOL, FFS_BOARD_MAX_OPP_DROP,
#   FFS_BOARD_SHAPES, FFS_TARGET_N, FFS_TRADE_SCREEN_N / _PER_OPP ("Inf" =
#   exhaustive), FFS_TRADE_EVAL_MAX (safety cap, 40000), FFS_TRADE_WORKERS
#   (parallel exact evals), FFS_BOARD_GIVEBACK, FFS_MIN_PIECE_VALUE.

suppressMessages({
  library(data.table)
  devtools::load_all(here::here(), quiet = TRUE)
  library(ffscrapr)
})
# ffscrapr reads a dead nflverse asset, so ff_scoringhistory() returns ZERO rows
# for 2025+ and every simulation silently loses its most recent season.
source(here::here("dev", "suite", "scoring_history_shim.R")); install_shim()
options(ffsimulator.verbose = FALSE)

## ---- config: newest saved report for the league --------------------------------
league_id  <- Sys.getenv("FFS_LEAGUE_ID", "1359546500786434048")
league_dir <- here::here("dev", "league_sims", league_id)
sims <- Sys.glob(file.path(league_dir, "*", "simulation.rds"))
stopifnot("no saved simulation.rds found for this league" = length(sims) > 0)
out    <- dirname(sims[order(file.info(sims)$mtime, decreasing = TRUE)][1])
config <- readRDS(file.path(out, "config.rds"))
conn   <- ffscrapr::sleeper_connect(season = config$season, league_id = config$league_id)
my_team <- Sys.getenv("FFS_MY_TEAM", if (!is.null(config$my_team)) config$my_team else "")
stopifnot("set FFS_MY_TEAM or config$my_team" = nzchar(my_team))

## ---- clean dynasty outlook (skip any folder with merge-conflict markers) --------
dyn_dirs <- sort(unique(dirname(Sys.glob(file.path(league_dir, "*", "dynasty_outlook.csv")))),
                 decreasing = TRUE)
read_clean_dyn <- function(dirs) {
  for (d in dirs) {
    f <- file.path(d, "dynasty_outlook.csv")
    if (any(grepl("^(<<<<<<<|=======|>>>>>>>)", readLines(f, warn = FALSE)))) {
      message("skipping ", f, " (merge-conflict markers)"); next
    }
    message("dynasty outlook: ", f)
    return(fread(f, colClasses = list(character = c("player_id", "fantasypros_id"))))
  }
  stop("no clean dynasty_outlook.csv found")
}
dyn <- read_clean_dyn(dyn_dirs)
dyn[, `:=`(player_id = as.character(player_id), franchise_id = as.character(franchise_id))]

# trajectory (value move vs the position's incumbent drift), for the soft
# down-rank in ffs_build_trades: don't ship my risers / acquire their decliners.
# Same derivation as trade_intel.R (median-based, drift-relative).
if (!"next_value_med" %in% names(dyn)) dyn[, next_value_med := next_value_mean]
dyn[, exp_change := next_value_med / cur_value - 1]
pos_drift <- dyn[!is.na(cur_value) & cur_value > 0,
                 .(pos_drift = sum(next_value_med) / sum(cur_value) - 1), by = pos]
dyn <- merge(dyn, pos_drift, by = "pos", all.x = TRUE)
dyn[is.na(pos_drift), pos_drift := sum(dyn$next_value_med, na.rm = TRUE) /
      sum(dyn$cur_value, na.rm = TRUE) - 1]
dyn[, rel_change := (1 + exp_change) / (1 + pos_drift) - 1]

frs   <- unique(dyn[, .(franchise_id, franchise_name)])
me    <- frs[franchise_name == my_team]$franchise_id[1]
stopifnot("my_team not found in dynasty_outlook" = !is.na(me))

# WHOLE-ROSTER MODE: FFS_MOVE_PLAYER="" drops the must_send constraint, so every
# asset you own is a candidate headline piece rather than just one named player.
# MEASURED 2026-08-06 (Jon league, production defaults): Puka-only = 2,436 evals
# / 64 ideas / 1 headline sender; whole roster = 42,246 evals / 1,571 ideas / 18
# headline senders (~4.5 h search on 14 workers, so an overnight job with the
# n=2000 confirm). Your MID-VALUE assets generate more tradeable ideas than the
# stud does - a 2027 R1, Jordyn Tyson and Harold Fannin each beat Puka - because
# they fit inside more teams' fair-price bands.
move_name <- Sys.getenv("FFS_MOVE_PLAYER", "Puka Nacua")
whole_roster <- !nzchar(trimws(move_name))
if (whole_roster) {
  move_id <- NULL
  safe <- "roster"
  message("WHOLE-ROSTER mode: every asset is a candidate headline piece")
} else {
  move_id <- dyn[franchise_id == me & player_name == move_name]$player_id[1]
  if (is.na(move_id)) stop(sprintf("'%s' is not on %s's roster", move_name, my_team))
  safe <- gsub("[^A-Za-z0-9]+", "_", move_name)
}

## ---- fast SEARCH sim (cached) ---------------------------------------------------
n_trade <- as.integer(Sys.getenv("FFS_TRADE_NSIMS", "60"))
sim_cache <- Sys.getenv("FFS_SEARCH_SIM", file.path(league_dir, sprintf("search_n%d.rds", n_trade)))
if (file.exists(sim_cache)) {
  message("search sim: ", sim_cache)
  sim <- readRDS(sim_cache)
} else {
  message("building n=", n_trade, " search sim @ ", Sys.time())
  set.seed(config$season + 1L)
  sim <- ff_simulate(conn, n_seasons = n_trade, version = "v3", lineup_method = "rank",
                     return = "all", actual_schedule = TRUE, replacement_level = FALSE)
  saveRDS(sim, sim_cache)
}

## ---- nearest first-round picks per team (value-only gap fillers) -----------------
fmt <- ffsimulator:::.ffs_detect_qb_format(sim$lineup_constraints)
pick_season <- as.integer(Sys.getenv("FFS_PICK_SEASON", "0"))
if (pick_season == 0L) pick_season <- as.integer(config$season) + 1L  # soonest future draft
# real pick ownership (incl. traded picks) if ffs_draftpicks works, else each
# team's own nearest first-rounder (slotted from its projected finish)
picks_df <- tryCatch({
  dp <- as.data.table(ffs_draftpicks(conn))
  dp[season == min(dp$season) & round == 1L,
     .(season, round = 1L, franchise_id, original_franchise_id)]
}, error = function(e) {
  message("ffs_draftpicks unavailable (", conditionMessage(e),
          "); using each team's own R", 1, " ", pick_season)
  CJ(season = pick_season, franchise_id = frs$franchise_id)[
    , `:=`(round = 1L, original_franchise_id = franchise_id)]
})
pv <- tryCatch(as.data.table(ffs_pick_values(
        sim, picks = picks_df, pick_curve = here::here("dev", "data", "pick_value_curve.csv"),
        format = fmt)),
      error = function(e) { message("picks unavailable (skipping gap-fillers): ",
                                    conditionMessage(e)); NULL })

## ---- board knobs ----------------------------------------------------------------
# ONE band replaces the old even/uneven pair: my_edge is the deal's gap measured
# against the SHAPE-FAIR price (0 on an even swap, +fair_premium per extra piece
# the receiving side takes back, because a package must overpay the stud it is
# traded for). The upper edge is how far over fair I am willing to try to take;
# the lower edge stops enumerating deals where I overpay.
fair_premium <- as.numeric(Sys.getenv("FFS_FAIR_PREMIUM", "0.05"))
fair_band <- as.numeric(strsplit(Sys.getenv("FFS_FAIR_BAND", "-0.05,0.05"), ",")[[1]])
# How far below the shape-fair price the other owner will still deal. NOTE this
# defaults to the SAME 5% as the upper edge of FFS_FAIR_BAND - they are the same
# quantity seen from the two sides ("how much over fair I'll try to take" vs "how
# much under fair they'll swallow"). So every enumerated deal passes the market
# half of `gettable` by construction, and at board time the flag is effectively
# the playoff veto alone. Widen FFS_FAIR_BAND past this to enumerate near-misses
# and make the flag discriminate on value again.
opp_edge_tol <- as.numeric(Sys.getenv("FFS_OPP_EDGE_TOL", "0.05"))
# legacy bands, still used when FFS_FAIR_BAND is set to "off"
even_band <- as.numeric(Sys.getenv("FFS_BOARD_EVEN_BAND", "0.03"))
ug <- as.numeric(strsplit(Sys.getenv("FFS_BOARD_UNEVEN_GAP", "0.02,0.10"), ",")[[1]])
if (identical(Sys.getenv("FFS_FAIR_BAND"), "off")) fair_band <- NULL
# SEND floor at 1000: a real rotation player (Aaron Jones 1259, Tony Pollard 1582,
# Chris Rodriguez 1216) can be packaged with the stud, but true junk (Emanuel
# Wilson 553, Najee Harris 329, Taylen Green 474 - all <1000) stays out. The 2000
# floor Joe first tried treated Aaron Jones as filler, so "Aaron Jones + Puka"
# never enumerated (he only surfaced as an auto give-back).
min_piece <- as.numeric(Sys.getenv("FFS_MIN_PIECE_VALUE", "1000"))
# RECEIVED floor 1500 (Joe, 2026-08-06; was 2000). Once core_n is capping per
# idea the floor no longer buys runtime - it only decides which pieces are
# allowed to appear in a return package. 1500 lets smaller real pieces come back
# instead of being pre-excluded, and costs ~2 extra evals (measured: 1,049 ->
# 1,051 at core_n=20, despite the raw banded set growing 19,445 -> 32,263).
min_recv <- as.numeric(Sys.getenv("FFS_MIN_RECV_VALUE", "1500"))
# playoff VETO, not a filter: it now only flags `gettable`, and only fires on a
# large drop - the case where a deal genuinely guts them at a position. An
# ordinary WR-for-WR+RB that shaves a couple of points off their odds is not
# something a real owner refuses; their acceptance is a market decision.
max_opp_drop <- as.numeric(Sys.getenv("FFS_BOARD_MAX_OPP_DROP", "0.15"))
# legacy rebuilder branch, off by default (cur_value already prices their taste
# for youth and picks)
max_opp_future <- as.numeric(Sys.getenv("FFS_BOARD_MAX_OPP_FUTURE_DROP", "Inf"))
# soft trajectory down-rank: don't ship my risers / acquire their decliners
traj_weight <- as.numeric(Sys.getenv("FFS_BOARD_TRAJ_WEIGHT", "0.75"))
rise_cut <- as.numeric(Sys.getenv("FFS_BOARD_RISE_CUT", "0.075"))
fade_cut <- as.numeric(Sys.getenv("FFS_BOARD_FADE_CUT", "-0.075"))
# EXHAUSTIVE by default: every deal inside the fair band gets an exact
# evaluation, with no global or per-team cap. The screen then serves only as
# eval ordering, so a run stopped early (or capped by FFS_TRADE_EVAL_MAX) has
# still priced the most promising deals first. Set these to integers to go back
# to a budgeted scan.
num_or_inf <- function(v, d) { x <- Sys.getenv(v, d); if (x %in% c("Inf","inf")) Inf else as.numeric(x) }
screen_n <- num_or_inf("FFS_TRADE_SCREEN_N", "Inf")
screen_per_opp <- num_or_inf("FFS_TRADE_SCREEN_PER_OPP", "Inf")
# Cap per trade IDEA, not per team. MEASURED 2026-08-05: the ~19,400 banded deals
# on a full Puka board are only ~64 distinct (opponent, best sent, best received)
# cores - 74% are 3-for-3, and 58% carry a sent piece under 1500 - so ~300 of
# every 301 evaluations re-price the same idea with different filler. The largest
# single core ("send Puka, get Jayden Daniels") is expressed 1,151 ways, with the
# screen running from +52 down to -30.
#
# 50 rather than 20 (Joe, 2026-08-06): the core key only looks at the TOP piece
# each way, so "Daniels + Swift" and "Daniels + Nabers" share a core and compete
# for the same slots. 50 leaves room for genuinely different SECOND pieces to
# survive instead of being crowded out by variations on one combination.
core_n <- num_or_inf("FFS_TRADE_CORE_N", "50")
# safety net so a mis-set band cannot silently launch a multi-day run
eval_max <- num_or_inf("FFS_TRADE_EVAL_MAX", "40000")
# MEASURED 2026-08-05 (Jon league, n=240 search sim, all 9 shapes, target_n=400):
# the +-5% band admits ~19,400 deals, ~7,000 after dedupe. At ~3.8 s/deal that is
# 7+ hours sequentially, so the exhaustive default only makes sense in parallel -
# hence workers defaults to the box's physical cores less two. Set
# FFS_TRADE_WORKERS=1 to force in-process (and expect it to take all night).
workers <- as.integer(Sys.getenv("FFS_TRADE_WORKERS",
  as.character(max(1L, parallel::detectCores(logical = FALSE) - 2L))))
top_n    <- num_or_inf("FFS_TRADE_TOP_N", "Inf")
# the give-back loop costs up to giveback_try EXTRA exact evals per triggered
# deal, which on an exhaustive scan multiplies the budget - off by default here
giveback <- as.integer(Sys.getenv("FFS_BOARD_GIVEBACK", "0")) == 1L

# Build the target list HERE with a deep top_n instead of letting ffs_build_trades
# use its internal top_n=50 proxy screen. The proxy (mean points - positional
# baseline) under-ranks players at a position I'm deep in - Egbuka is a real 0.55-
# win target but ranks #76 on the proxy because my WR room is stacked, so top_n=50
# would drop him. A deep scan exact-values him and lets Egbuka+Barkley+pick build.
target_n <- as.integer(Sys.getenv("FFS_TARGET_N", "400"))
# ALL shapes by default (1..3 pieces each way). must_send forces the moved player
# into every send package, so 1-for-N is "just him" and 3-for-N packages him with
# two more pieces. FFS_BOARD_SHAPES="1,2;2,2" narrows it.
default_shapes <- paste(apply(expand.grid(1:3, 1:3), 1, paste, collapse = ","), collapse = ";")
board_shapes <- lapply(strsplit(Sys.getenv("FFS_BOARD_SHAPES", default_shapes), ";")[[1]],
                       function(s) as.integer(strsplit(trimws(s), ",")[[1]]))
# one engine for the whole run: the target scan, the roster valuation and every
# exact deal evaluation share it (each ffs_player_value call would otherwise
# re-summarise the league and re-copy roster_scores)
engine <- ffs_trade_engine(sim)
# NOTE this is the run's big fixed cost - roughly 2s per target on an n=240
# search sim, so ~13 min at target_n=400. sides="you" halves it by skipping the
# owner-side valuation, which the package builder never reads.
message("scanning ", target_n, " targets @ ", Sys.time())
targets <- data.table::as.data.table(
  ffs_trade_targets(sim, me, top_n = target_n, engine = engine, sides = "you"))

message("building board: ",
        if (whole_roster) "every asset" else paste("move", move_name),
        " across all teams @ ", Sys.time())
board <- as.data.table(ffs_build_trades(
  sim, me, dynasty = dyn, picks = pv, must_send = move_id, targets = targets,
  shapes = board_shapes,
  fair_premium = fair_premium, fair_band = fair_band, opp_edge_tol = opp_edge_tol,
  even_band = even_band, uneven_gap = ug,
  min_piece_value = min_piece, min_recv_value = min_recv,
  require_positive_target = as.integer(Sys.getenv("FFS_REQUIRE_POSITIVE_TARGET", "0")) == 1L,
  giveback = giveback, giveback_trigger_edge = 0, giveback_try = 3L,
  max_opp_drop = max_opp_drop, max_opp_future_drop = max_opp_future,
  traj_weight = traj_weight, rise_cut = rise_cut, fade_cut = fade_cut,
  score_mode = Sys.getenv("FFS_TRADE_SCORE_MODE", "rate"),
  opp_mode = Sys.getenv("FFS_TRADE_OPP_MODE", "market"),
  playoff_value = as.numeric(Sys.getenv("FFS_TRADE_PLAYOFF_VALUE", "68")),
  future_certainty = as.numeric(Sys.getenv("FFS_TRADE_FUTURE_CERTAINTY", "0.905")),
  win_to_playoff = as.numeric(Sys.getenv("FFS_TRADE_WIN_TO_PLAYOFF", "17.8")),
  future_weight = as.numeric(Sys.getenv("FFS_TRADE_FUTURE_WEIGHT", "3")),
  min_future_delta = -750, winwin_bonus = 0.5, verbose = TRUE, engine = engine,
  dedupe = TRUE, screen_n = screen_n, screen_per_opp = screen_per_opp,
  core_n = core_n, eval_max = eval_max, workers = workers, top_n = top_n))

## ---- format + write the ranked board --------------------------------------------
board <- merge(board, frs, by.x = "opponent", by.y = "franchise_id", all.x = TRUE)
board[, `:=`(
  gap_pct  = round(100 * value_gap / recv_value, 1),
  mine_pl  = round(100 * my_playoff_delta, 1),
  opp_pl   = round(100 * opp_playoff_delta, 1),
  fut      = round(future_capital_delta),
  nS = lengths(send_ids), nR = lengths(recv_ids),
  uses_pick = vapply(seq_len(.N), function(i)
    any(grepl("^PICK_", c(send_ids[[i]], recv_ids[[i]]))), logical(1)))]
board[, shape := paste0(nS, "-for-", nR)]
board[, headliner := tstrsplit(receive, " \\+ ", keep = 1)]
# the most valuable piece LEAVING - the whole-roster view's organising column
# (in single-player mode it is the moved player on every row)
cur_by_id <- setNames(dyn$cur_value, dyn$player_id)
if (!is.null(pv)) cur_by_id <- c(cur_by_id, setNames(pv$cur_value, pv$player_id))
send_nm <- setNames(dyn$player_name, dyn$player_id)
if (!is.null(pv)) send_nm <- c(send_nm, setNames(pv$player_name, pv$player_id))
board[, sending := vapply(send_ids, function(x) {
  v <- cur_by_id[x]; v[is.na(v)] <- 0; unname(send_nm[x[which.max(v)]])
}, character(1))]
setorder(board, -score)

outb <- board[, .(team = franchise_name, sending, shape, send, receive,
  send_value = round(send_value), recv_value = round(recv_value),
  gap_pct, my_edge = round(100 * my_edge, 1), opp_edge = round(100 * opp_edge, 1),
  mine_pl, opp_pl, fut, adj_fut = round(adj_future_capital),
  score = round(score, 1), grade,
  opp_surplus = round(opp_surplus), opp_score = round(opp_score, 1), gettable,
  win_win, give_back, uses_pick)]
board_csv <- file.path(out, paste0(safe, "_trade_board.csv"))
fwrite(outb, board_csv)
message("wrote ", nrow(outb), " deals to ", board_csv)

# also persist the full board object (with send_ids/recv_ids/opponent list-cols and
# the search-sim deltas) so the confirm step can re-price a chosen subset on the
# n=2000 standings sim via ffs_trade_eval - the readable CSV drops the id columns.
board_rds <- file.path(out, paste0(safe, "_board.rds"))
saveRDS(board, board_rds)
message("wrote board object ", board_rds)

## ---- per-team options: TARGETS, each with its SEND variations --------------------
# For each team show its best received-package TARGETS (Henderson+Olave+pick,
# Bucky+Pickens+pick, ...), and under each the several ways to SEND for it
# (Kittle+Nacua / Jones+Nacua / Simpson+Pollard+Nacua) - the theKmbl view Joe
# liked, generalised to every team. A target's own draft-pick YEAR is normalised
# so the same slot isn't shown as separate targets.
board[, shape_rank := match(shape, c("1-for-2", "1-for-3", "2-for-2", "2-for-3",
                                     "3-for-2", "3-for-3"))]
board[is.na(shape_rank), shape_rank := 99L]
norm_recv <- function(x) gsub("20[0-9]{2} R", "R", x)   # "2027 R1 (~1.07)" slots merge
board[, recv_key := vapply(receive, norm_recv, character(1))]
# the display pool is now the acceptance model itself: deals the other side
# plausibly takes (market edge within tolerance, no gutting playoff drop) that
# also help me
fair <- board[gettable == TRUE & mine_pl >= 0]
targets_per_team <- as.integer(Sys.getenv("FFS_TARGETS_PER_TEAM", "3"))
sends_per_target <- as.integer(Sys.getenv("FFS_SENDS_PER_TARGET", "3"))

deal_line <- function(r) sprintf("     %-8s %-46s edge%+.1f%% you%+.1f%% opp%+.1f%% fut%+d%s%s",
    r$shape, r$send, 100 * r$opp_edge, r$mine_pl, r$opp_pl, r$fut,
    if (isTRUE(r$win_win)) " WW" else "", if (isTRUE(r$give_back)) " +gb" else "")

show_team <- function(pool, label) {
  cat(sprintf("\n[%s]%s\n", t, label))
  tg_order <- pool[, .(best = max(score)), by = recv_key][order(-best)]
  for (k in utils::head(tg_order$recv_key, targets_per_team)) {
    tk <- pool[recv_key == k][order(-score)]
    cat("  ► get: ", tk$receive[1], "\n", sep = "")
    for (j in seq_len(min(nrow(tk), sends_per_target))) cat(deal_line(tk[j]), "\n", sep = "")
  }
}

## ---- coverage: how exhaustively did each team actually get scanned? -------------
cov <- merge(board[, .(deals = .N, gettable = sum(gettable), best = round(max(score), 1)),
                   by = franchise_name],
             board[gettable == TRUE, .(shapes = uniqueN(shape)), by = franchise_name],
             by = "franchise_name", all.x = TRUE)
setorder(cov, -best)
cat("\n== coverage per team (evaluated / gettable / shapes / best score) ==\n")
print(cov)
missing <- setdiff(setdiff(frs$franchise_name, my_team), board$franchise_name)
if (length(missing)) cat("NO deals enumerated for:", paste(missing, collapse = ", "), "\n")

## ---- whole-roster: which of MY assets actually generate tradeable ideas? --------
if (whole_roster) {
  snd <- board[, .(deals = .N, gettable = sum(gettable),
                   teams = uniqueN(franchise_name), best = round(max(score), 1)),
               by = sending][order(-best)]
  cat("\n== what you'd be shipping: ideas per headline asset (best first) ==\n")
  print(snd)
  cat("\n== best deal for each asset you could move ==\n")
  for (i in seq_len(nrow(snd))) {
    r <- board[sending == snd$sending[i]][order(-score)][1]
    cat(sprintf("  %-20s -> %-44s  [%s] you%+.1f%% them%+.1f%% score %.1f %s%s\n",
                substr(snd$sending[i], 1, 20), substr(r$receive, 1, 44),
                substr(r$franchise_name, 1, 14), r$mine_pl, r$opp_pl, r$score,
                r$grade, if (isTRUE(r$gettable)) "" else " (not gettable)"))
  }
}

cat("\n== options per team: each TARGET with its SEND variations (best first) ==\n")
for (t in sort(setdiff(frs$franchise_name, my_team))) {
  d <- fair[franchise_name == t]
  if (nrow(d)) {
    show_team(d, "")
  } else {
    # coverage fallback: no in-band deal -> show the closest available (stretch)
    s <- board[franchise_name == t][order(-score)]
    if (nrow(s)) show_team(utils::head(s, 8), "  (stretch - none fully in band)")
    else cat(sprintf("\n[%s] no value-matched package exists (nothing of theirs upgrades you at a fair price)\n", t))
  }
}

## ---- plot: win-now (x) vs future (y), colour = acceptability, label fair deals ---
if (requireNamespace("ggplot2", quietly = TRUE) && nrow(fair)) {
  library(ggplot2)
  has_repel <- requireNamespace("ggrepel", quietly = TRUE)
  pd <- copy(fair)
  pd[, deal_type := fifelse(give_back, "give-back (send filler back)", "straight swap")]
  # label only the best few deals per team so the map stays legible; every fair
  # deal is still plotted as a point, and the ranked CSV carries all the detail
  label_per_team <- as.integer(Sys.getenv("FFS_LABEL_PER_TEAM", "2"))
  # whole-roster mode has far too many points to label per team, and the useful
  # question there is "which of MY assets does this deal ship" - so label the
  # best few per outgoing asset instead
  if (whole_roster) {
    pd[, rk := frank(-score, ties.method = "first"), by = sending]
    pd[, lab := fifelse(rk <= label_per_team,
                        paste0(sending, " -> ", headliner), NA_character_)]
  } else {
    pd[, rk := frank(-score, ties.method = "first"), by = franchise_name]
    pd[, lab := fifelse(rk <= label_per_team,
                        paste0(franchise_name, ": ", headliner,
                               ifelse(uses_pick, " +pk", ""), " (", shape, ")"),
                        NA_character_)]
  }
  p <- ggplot(pd, aes(mine_pl, fut)) +
    geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.4) +
    geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.4) +
    geom_point(aes(colour = 100 * opp_edge, shape = deal_type), size = 4, stroke = 0.6) +
    scale_colour_gradient2(low = "#B45309", mid = "grey78", high = "#1D6D9C",
      midpoint = 0, name = "Their market edge\nvs fair (%)") +
    scale_shape_manual(values = c("give-back (send filler back)" = 17,
      "straight swap" = 16), name = "Deal type") +
    labs(
      title = if (whole_roster)
        sprintf("Gettable trades across %s's whole roster", my_team) else
        sprintf("Gettable trades to move %s (%s)", move_name, my_team),
      subtitle = "x = your win-now impact  •  y = future value banked  •  colour = their market edge vs the shape-fair price",
      x = "Your playoff-odds change (win-now)", y = "Future dynasty capital gained",
      caption = sprintf("n=%d search estimates • gettable = their edge >= -%.0f%% of fair and playoff drop < %.0f%%",
                        n_trade, 100 * opp_edge_tol, 100 * max_opp_drop)) +
    scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank(),
          plot.caption = element_text(colour = "grey55"))
  p <- if (has_repel) p + ggrepel::geom_text_repel(aes(label = lab), size = 3,
         colour = "grey20", na.rm = TRUE, max.overlaps = Inf, box.padding = 0.6,
         min.segment.length = 0, segment.color = "grey70", seed = 1)
       else p + geom_text(aes(label = lab), size = 2.8, colour = "grey20",
         vjust = -0.9, na.rm = TRUE)
  plot_png <- file.path(out, paste0(safe, "_trade_plot.png"))
  ggsave(plot_png, p, width = 12, height = 7.5, dpi = 150, bg = "white")
  message("wrote plot ", plot_png, " (", nrow(pd), " fair points, ",
          sum(!is.na(pd$lab)), " labelled)")
}
