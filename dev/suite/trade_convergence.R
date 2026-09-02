# How many simulated seasons does a TRADE DELTA actually need?
#
# dev/suite/convergence.R answers a different question - the standard error of a
# franchise's ABSOLUTE playoff odds, which is ~1/sqrt(n). A trade delta is not
# that. It is a PAIRED difference: the same seasons, the same draws, one roster
# change. Paired differences cancel most of the common variance, so they can
# converge far faster than the absolute odds do - or, if a trade only matters in
# rare seasons, far slower. Guessing either way is how a board ends up quoting
# numbers it cannot support.
#
# The failure this exists to prevent: at n=60 a playoff delta can only ever be a
# multiple of 1/60 = 1.667%, so an entire board of "+1.7% / +3.3% / +5.0%" deals
# is really "this deal flipped 1, 2 or 3 seasons out of 60". Ranking on that is
# ranking on noise.
#
# DESIGN. Nested subsamples of one n=2000 simulation: the n=250 estimate is
# literally the first 250 seasons of the n=2000 estimate. That makes every
# comparison paired against the same truth and removes between-simulation
# variance from the answer, which a fresh ff_simulate() at each n would not.
#
# Usage:
#   FFS_LEAGUE_ID=1312145437842497536 Rscript dev/suite/trade_convergence.R
# Knobs: FFS_CONV_DEALS (40), FFS_CONV_NS ("60,125,250,500,1000,2000"),
#   FFS_CONV_WORKERS (1), FFS_MY_TEAM, FFS_WAIVER, FFS_OPP_CUT, FFS_BOARD_RDS.

suppressMessages({
  library(data.table)
  devtools::load_all(here::here(), quiet = TRUE)
})
options(ffsimulator.verbose = FALSE)

league_id  <- Sys.getenv("FFS_LEAGUE_ID", "1312145437842497536")
league_dir <- here::here("dev", "league_sims", league_id)
sims <- Sys.glob(file.path(league_dir, "*", "simulation.rds"))
stopifnot("no saved simulation.rds found for this league" = length(sims) > 0)
out    <- dirname(sims[order(file.info(sims)$mtime, decreasing = TRUE)][1])
config <- readRDS(file.path(out, "config.rds"))
my_team <- Sys.getenv("FFS_MY_TEAM", config$my_team)

ns <- as.integer(strsplit(Sys.getenv("FFS_CONV_NS", "60,125,250,500,1000,2000"), ",")[[1]])
n_deals <- as.integer(Sys.getenv("FFS_CONV_DEALS", "40"))
workers <- as.integer(Sys.getenv("FFS_CONV_WORKERS", "1"))
waiver_mode <- Sys.getenv("FFS_WAIVER", "me")
opp_cut <- as.integer(Sys.getenv("FFS_OPP_CUT", "1")) == 1L

message("loading n=2000 standings sim @ ", Sys.time())
sim <- readRDS(file.path(out, "simulation.rds"))
n_max <- uniqueN(as.data.table(sim$summary_season)$season)
ns <- sort(unique(pmin(ns, n_max)))
message("seasons available: ", n_max, " | testing n = ", paste(ns, collapse = ", "))

rs <- as.data.table(sim$roster_scores)
me <- unique(rs[franchise_name == my_team]$franchise_id)[1]
stopifnot("my_team not found in the standings sim" = !is.na(me))

if (waiver_mode != "none") {
  v_pos <- if (!is.null(config$pos_filter)) config$pos_filter else c("QB","RB","WR","TE","K")
  sim$waiver_scores <- ffs_waiver_proxies(
    sim, n_per_pos = as.integer(Sys.getenv("FFS_WAIVER_N", "4")),
    pos_filter = intersect(v_pos, c("QB", "RB", "WR", "TE")))
  sim$waiver_scores <- ffs_waiver_rank(sim, me)
}

## ---- the deals under test -------------------------------------------------------
# stratified across the board's own score range, not just the top: a study that
# only looks at the best deals cannot see whether the ORDERING is stable, which
# is the thing a board is actually asked to get right
board_rds <- Sys.getenv("FFS_BOARD_RDS", file.path(out, "roster_board.rds"))
stopifnot("no board object - run player_trade_board.R first" = file.exists(board_rds))
b <- as.data.table(readRDS(board_rds))[gettable == TRUE]
setorder(b, -score)
set.seed(20260902)
strata <- cut(seq_len(nrow(b)), breaks = min(n_deals, nrow(b)), labels = FALSE)
pick <- vapply(split(seq_len(nrow(b)), strata), function(i) i[[1]], integer(1))
deals <- b[pick]
message("deals under test: ", nrow(deals),
        " (board score ", round(min(deals$score), 1), " .. ", round(max(deals$score), 1), ")")

## ---- nested subsample: the first k seasons of the same simulation ---------------
subsim <- function(k) {
  keep <- function(x) { d <- as.data.table(x); if ("season" %in% names(d)) d[season <= k] else d }
  o <- list(
    roster_scores  = keep(sim$roster_scores),
    optimal_scores = keep(sim$optimal_scores),
    waiver_scores  = if (is.null(sim$waiver_scores)) NULL else keep(sim$waiver_scores),
    schedules      = keep(sim$schedules),
    lineup_constraints = sim$lineup_constraints,
    simulation_params  = modifyList(sim$simulation_params, list(n_seasons = k)))
  class(o) <- class(sim)
  o
}

res <- rbindlist(lapply(ns, function(k) {
  s <- subsim(k)
  eng <- ffs_trade_engine(s)
  t0 <- Sys.time()
  ev <- as.data.table(ffs_trade_eval_many(
    s, data.table(franchise_a = me, gives_a = deals$send_ids,
                  franchise_b = deals$opponent, gives_b = deals$recv_ids),
    engine = eng, workers = workers, waiver = waiver_mode, cut_to_size = opp_cut))
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  m <- ev[franchise_id == me][order(deal_id)]
  message(sprintf("  n=%-5d %5.1f s (%.2f s/deal)", k, el, el / nrow(deals)))
  data.table(n = k, deal = seq_len(nrow(m)),
             pl = 100 * m$playoff_pct_delta, win = m$h2h_wins_delta,
             sec_per_deal = el / nrow(deals))
}))

## ---- how wrong is a small n, against the n=2000 answer? -------------------------
truth <- res[n == max(ns), .(deal, pl_t = pl, win_t = win)]
cmp <- merge(res, truth, by = "deal")
top_k <- min(10L, nrow(deals))
best <- truth[order(-pl_t)]$deal[seq_len(top_k)]

summ <- cmp[, .(
  `s/deal`   = round(sec_per_deal[1], 2),
  grain      = round(100 / n[1], 2),          # smallest delta this n can express
  mae_pl     = round(mean(abs(pl - pl_t)), 2),
  p90_err    = round(stats::quantile(abs(pl - pl_t), 0.9), 2),
  sign_flips = sum(sign(pl) != sign(pl_t) & pl_t != 0),
  spearman   = round(stats::cor(pl, pl_t, method = "spearman"), 3),
  top10_kept = sum(order(-pl)[seq_len(top_k)] %in% best)
), by = n]

cat("\n== trade-delta convergence, against the n=", max(ns), " answer ==\n", sep = "")
cat("grain      = the smallest playoff delta this n can even express (100/n)\n")
cat("mae_pl     = mean |error| in playoff percentage points\n")
cat("sign_flips = deals whose delta has the WRONG SIGN at this n\n")
cat("top10_kept = how many of the true top 10 this n puts in its own top 10\n\n")
print(summ)

fwrite(cmp, file.path(out, "trade_convergence.csv"))
fwrite(summ, file.path(out, "trade_convergence_summary.csv"))
cat("\nwrote trade_convergence.csv + _summary.csv to ", out, "\n", sep = "")
