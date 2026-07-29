# Move-a-player trade board: given ONE player you want to move, scan every team
# for fair, realistic packages, and plot them so you can pick the best deal.
#
# This formalises the dev-scratch Puka board (trade_board2.R + plot_trades.R) into
# a reusable, parameterised tool. The board rules encode what Joe converged on:
#   * even (same-count) trades within `FFS_BOARD_EVEN_BAND` (default 3%),
#     uneven consolidation within a `FFS_BOARD_UNEVEN_GAP` premium band (2-8%);
#   * the second SEND piece must be a real player (min_piece_value), not filler;
#   * draft PICKS fill value gaps (nearest, least-discounted year);
#   * give-back sweetener ON: when a deal drains the other team, the builder also
#     offers a variant that sends your cheapest player at that position BACK, so
#     the return of e.g. an RB to the depleted team shows up as a softer opp drop
#     (generalises "send Daniel Jones back on a QB buy");
#   * posture-aware acceptance: rebuilders take a playoff hit for future value;
#   * near-identical "player + filler / different pick year" ideas are deduped.
#
# Usage (env-configurable, defaults to Puka in the Jon superflex league):
#   FFS_LEAGUE_ID=1359546500786434048 FFS_MOVE_PLAYER="Puka Nacua" \
#     Rscript dev/suite/player_trade_board.R
# Other knobs: FFS_MY_TEAM, FFS_TRADE_NSIMS (search sim n, default 60),
#   FFS_SEARCH_SIM (path to a cached search sim), FFS_PICK_SEASON (nearest pick
#   year, default = soonest future draft), FFS_BOARD_EVEN_BAND, FFS_BOARD_UNEVEN_GAP
#   ("lo,hi"), FFS_BOARD_MAX_OPP_DROP, FFS_BOARD_MAX_OPP_FUTURE_DROP,
#   FFS_TRADE_SCREEN_N, FFS_TRADE_TOP_N, FFS_MIN_PIECE_VALUE.

suppressMessages({
  library(data.table)
  devtools::load_all(here::here(), quiet = TRUE)
  library(ffscrapr)
})
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

move_name <- Sys.getenv("FFS_MOVE_PLAYER", "Puka Nacua")
move_id   <- dyn[franchise_id == me & player_name == move_name]$player_id[1]
if (is.na(move_id)) stop(sprintf("'%s' is not on %s's roster", move_name, my_team))

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
even_band <- as.numeric(Sys.getenv("FFS_BOARD_EVEN_BAND", "0.03"))
# uneven (consolidation / fragment) premium band. A touch wider than the even
# band so fragment shapes (1-for-3: split a stud into 2 players + a pick) have
# room - two real pieces already sum near the stud, so the pick pushes the gap up.
ug <- as.numeric(strsplit(Sys.getenv("FFS_BOARD_UNEVEN_GAP", "0.02,0.10"), ",")[[1]])
# SEND floor at 1000: a real rotation player (Aaron Jones 1259, Tony Pollard 1582,
# Chris Rodriguez 1216) can be packaged with the stud, but true junk (Emanuel
# Wilson 553, Najee Harris 329, Taylen Green 474 - all <1000) stays out. The 2000
# floor Joe first tried treated Aaron Jones as filler, so "Aaron Jones + Puka"
# never enumerated (he only surfaced as an auto give-back).
min_piece <- as.numeric(Sys.getenv("FFS_MIN_PIECE_VALUE", "1000"))
# RECEIVED floor stays at 2000: real pieces coming back, no junk in the package.
min_recv <- as.numeric(Sys.getenv("FFS_MIN_RECV_VALUE", "2000"))
max_opp_drop <- as.numeric(Sys.getenv("FFS_BOARD_MAX_OPP_DROP", "0.10"))
max_opp_future <- as.numeric(Sys.getenv("FFS_BOARD_MAX_OPP_FUTURE_DROP", "750"))
# soft trajectory down-rank: don't ship my risers / acquire their decliners
traj_weight <- as.numeric(Sys.getenv("FFS_BOARD_TRAJ_WEIGHT", "0.75"))
rise_cut <- as.numeric(Sys.getenv("FFS_BOARD_RISE_CUT", "0.075"))
fade_cut <- as.numeric(Sys.getenv("FFS_BOARD_FADE_CUT", "-0.075"))
screen_n <- as.integer(Sys.getenv("FFS_TRADE_SCREEN_N", "120"))
# evaluate this many of each opponent's packages exactly (coverage). ffs_build_trades
# additionally reserves a per-opponent quota for pick-carrying packages, so
# future-banking deals (e.g. Puka+Kittle -> Egbuka+Barkley+pick, whose win-neutral
# pick deflates the win-gain screen) are not starved out of the eval budget.
screen_per_opp <- as.integer(Sys.getenv("FFS_TRADE_SCREEN_PER_OPP", "12"))
top_n    <- as.integer(Sys.getenv("FFS_TRADE_TOP_N", "400"))

# Build the target list HERE with a deep top_n instead of letting ffs_build_trades
# use its internal top_n=50 proxy screen. The proxy (mean points - positional
# baseline) under-ranks players at a position I'm deep in - Egbuka is a real 0.55-
# win target but ranks #76 on the proxy because my WR room is stacked, so top_n=50
# would drop him. A deep scan exact-values him and lets Egbuka+Barkley+pick build.
target_n <- as.integer(Sys.getenv("FFS_TARGET_N", "160"))
# package shapes to enumerate, e.g. FFS_BOARD_SHAPES="1,2;2,2" for send-1/2 only
board_shapes <- lapply(strsplit(Sys.getenv("FFS_BOARD_SHAPES", "1,2;2,2;2,3"), ";")[[1]],
                       function(s) as.integer(strsplit(trimws(s), ",")[[1]]))
message("scanning ", target_n, " targets @ ", Sys.time())
targets <- data.table::as.data.table(ffs_trade_targets(sim, me, top_n = target_n))

message("building board: move ", move_name, " across all teams @ ", Sys.time())
board <- as.data.table(ffs_build_trades(
  sim, me, dynasty = dyn, picks = pv, must_send = move_id, targets = targets,
  shapes = board_shapes,
  even_band = even_band, uneven_gap = ug, max_gap = ug[2],
  min_piece_value = min_piece, min_recv_value = min_recv,
  giveback = TRUE, giveback_trigger = 0, giveback_try = 3L,
  max_opp_drop = max_opp_drop, max_opp_future_drop = max_opp_future,
  traj_weight = traj_weight, rise_cut = rise_cut, fade_cut = fade_cut,
  score_mode = Sys.getenv("FFS_TRADE_SCORE_MODE", "rate"),
  playoff_value = as.numeric(Sys.getenv("FFS_TRADE_PLAYOFF_VALUE", "68")),
  future_certainty = as.numeric(Sys.getenv("FFS_TRADE_FUTURE_CERTAINTY", "0.905")),
  win_to_playoff = as.numeric(Sys.getenv("FFS_TRADE_WIN_TO_PLAYOFF", "17.8")),
  future_weight = as.numeric(Sys.getenv("FFS_TRADE_FUTURE_WEIGHT", "3")),
  min_future_delta = -750, winwin_bonus = 0.5,
  dedupe = TRUE, screen_n = screen_n, screen_per_opp = screen_per_opp, top_n = top_n))

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
setorder(board, -score)

board[, grade := data.table::fcase(score>=18,"A", score>=10,"B", score>=4,"C",
                                   score>=0,"D", default="F")]
outb <- board[, .(team = franchise_name, shape, send, receive,
  send_value = round(send_value), recv_value = round(recv_value),
  gap_pct, mine_pl, opp_pl, fut, adj_fut = round(adj_future_capital),
  score = round(score, 1), grade, opp_score = round(opp_score, 1), gettable,
  win_win, give_back, uses_pick)]
safe <- gsub("[^A-Za-z0-9]+", "_", move_name)
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
fair <- board[gap_pct <= 100 * ug[2] & opp_pl >= -8 & mine_pl >= -8]
targets_per_team <- as.integer(Sys.getenv("FFS_TARGETS_PER_TEAM", "3"))
sends_per_target <- as.integer(Sys.getenv("FFS_SENDS_PER_TARGET", "3"))

deal_line <- function(r) sprintf("     %-8s %-46s gap%+.1f%% you%+.1f%% opp%+.1f%% fut%+d%s%s",
    r$shape, r$send, r$gap_pct, r$mine_pl, r$opp_pl, r$fut,
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
  pd[, rk := frank(-score, ties.method = "first"), by = franchise_name]
  pd[, lab := fifelse(rk <= label_per_team,
                      paste0(franchise_name, ": ", headliner,
                             ifelse(uses_pick, " +pk", ""), " (", shape, ")"),
                      NA_character_)]
  p <- ggplot(pd, aes(mine_pl, fut)) +
    geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.4) +
    geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.4) +
    geom_point(aes(colour = opp_pl, shape = deal_type), size = 4, stroke = 0.6) +
    scale_colour_gradient2(low = "#B45309", mid = "grey78", high = "#1D6D9C",
      midpoint = 0, name = "Their playoffΔ\n(accept?)") +
    scale_shape_manual(values = c("give-back (send filler back)" = 17,
      "straight swap" = 16), name = "Deal type") +
    labs(
      title = sprintf("Fair trades to move %s (%s)", move_name, my_team),
      subtitle = "x = your win-now impact  •  y = future value banked  •  colour = will the other side accept",
      x = "Your playoff-odds change (win-now)", y = "Future dynasty capital gained",
      caption = sprintf("n=%d search estimates • fair to both: gap ≤ %.0f%% and neither side below -8%% playoff",
                        n_trade, 100 * ug[2])) +
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
