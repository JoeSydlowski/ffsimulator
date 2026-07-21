# Trade intelligence: roster verdicts, buy targets, and complete deals - one
# coherent pass, one valuation standard.
#
# Replaces the old roster_drivers / trade_targets / trade_offers / pareto_targets
# / trade_builder / market_edges sheets with three:
#   roster.csv  - my players: market + model outlook + win value TO MY TEAM,
#                 drivers, a team-coherent verdict, and (for SELLs) best buyers
#   targets.csv - players elsewhere: value TO MY TEAM vs value TO THEIR TEAM,
#                 market context, Pareto front + sweet-spot robustness
#   trades.csv  - complete value-matched packages, motive = "buy" or "sell <X>"
#                 (sell deals are forced to include the player, aimed at his
#                 best buyers, and must improve my team at similar price)
# plus portfolio.csv - team-level portfolio metrics (posture, concentration,
# capital trajectory with downside/upside, value-at-risk).
#
# Valuation semantics (consistent everywhere):
#   my player / incoming player  -> ffs_player_value(vsim, p, me)     (MY team)
#   their player / outgoing side -> ffs_player_value(vsim, p, owner) /
#                                   trade-eval opp deltas             (THEIR team)
#
# Two-stage valuation (both stages on the REAL schedule - taking a good player
# from a bubble team with an easy slate should show it):
#   SEARCH  - a dedicated fast sim (n = FFS_TRADE_NSIMS, default from
#             dev/suite/valuation_convergence.R: value RANKS plateau at n=60)
#             does the screening: buyer rankings, target scans, package
#             enumeration.
#   REPORT  - every number a decision rides on comes from the n=2000 standings
#             sim: roster values behind verdicts, the top targets' value/surplus
#             (value_convergence.csv: search-sim value_to_you carries ~5% noise -
#             a QB3 read 0.2 at n=60 vs ~0.05 at n=2000), and the deltas of every
#             reported deal (confirmed via ffs_trade_eval re-ranking the same
#             draws). All carry a `confirmed` flag; unconfirmed rows are search-
#             sim only. The fast valuation path (partial re-opt) makes this cheap.
#
# Usage: sourced by run_league_suite.R (sim/me/dyn/dyn_vals/out/config/conn in
# scope), or standalone: Rscript dev/suite/trade_intel.R (reads the newest
# saved report folder for the league).
# Env knobs: FFS_TRADE_NSIMS, FFS_TRADE_TOP_N, FFS_TRADE_VALUE_BAND,
# FFS_TRADE_UNEVEN_SHADE, FFS_TRADE_FUTURE_WEIGHT, FFS_TRADE_MIN_FUTURE

## ---- inputs: suite mode (objects in scope) or standalone ----------------------
standalone <- !exists("sim") || !exists("me") || !exists("out")
if (standalone) {
  library(data.table)
  devtools::load_all(here::here(), quiet = TRUE)
  options(ffsimulator.verbose = FALSE)
  league_id <- Sys.getenv("FFS_LEAGUE_ID", "1326464763936403456")
  league_dir <- here::here("dev", "league_sims", league_id)
  sims <- Sys.glob(file.path(league_dir, "*", "simulation.rds"))
  stopifnot(length(sims) > 0)
  out <- dirname(sims[order(file.info(sims)$mtime, decreasing = TRUE)][1])
  message("reading report ", out)
  sim <- readRDS(file.path(out, "simulation.rds"))
  config <- readRDS(file.path(out, "config.rds"))
  conn <- ffscrapr::sleeper_connect(season = config$season, league_id = config$league_id)
  fr0 <- data.table::as.data.table(sim$franchises)
  me <- fr0[fr0$franchise_name == config$my_team][["franchise_id"]][1]
  stopifnot(!is.na(me))
  dyn <- data.table::fread(file.path(out, "dynasty_outlook.csv"),
                           colClasses = list(character = c("player_id", "fantasypros_id")))
  dyn_vals <- NULL
  if (Sys.getenv("FFS_FANTASYCALC", "1") != "0") {
    dyn_vals <- tryCatch(
      rbind(fc_dynasty_values(num_qbs = 1), fc_dynasty_values(num_qbs = 2)),
      error = function(e) { message("FantasyCalc unavailable: ", conditionMessage(e)); NULL })
  }
}
if (!exists("round_sheet")) {
  round_sheet <- function(dt) {
    dt <- data.table::copy(data.table::as.data.table(dt))
    for (col in names(dt)) {
      v <- dt[[col]]
      if (is.numeric(v)) {
        digits <- if (max(abs(v), na.rm = TRUE) >= 100) 0L else 3L
        data.table::set(dt, j = col, value = round(v, digits))
      }
    }
    dt
  }
}

# knobs. n_trade default from dev/suite/valuation_convergence.R (2026-07-10):
# value-rank stability plateaus at n=60 (Spearman .82/.87 vs .86/.91 at double
# the cost); playoff-delta SD is pure 1/sqrt(n) (+-5.4% at 60) with no knee, so
# raise FFS_TRADE_NSIMS (e.g. 240 -> +-3.3%) only to confirm a specific deal.
n_trade      <- as.integer(Sys.getenv("FFS_TRADE_NSIMS", "60"))
# targets are exact-valued (~7s each on the n=60 search sim), so top_n trades
# breadth for time: 100 ~= +6 min over 50, 200 ~= +17 min. Frontier wants breadth.
trade_top_n  <- as.integer(Sys.getenv("FFS_TRADE_TOP_N", "100"))
confirm_n    <- as.integer(Sys.getenv("FFS_TRADE_CONFIRM_N", "15"))
# deal-shape defaults per Joe: weigh next-year value equally with win-now
# (future_weight=1), refuse deals bleeding >500 of future capital, and match
# values within 10% - pure win-now surfaced aging-star consolidations (CMC
# for young WRs) he would never accept
value_band   <- as.numeric(Sys.getenv("FFS_TRADE_VALUE_BAND", "0.10"))
uneven_shade <- as.numeric(Sys.getenv("FFS_TRADE_UNEVEN_SHADE", as.character(value_band)))
# consolidation realism: in an uneven trade the package (multi-player) side must
# OVERPAY the single player - a stud is worth more than a package of equal summed
# value, so you can't buy Jeanty with a cheaper Tucker+Lamb, and you shouldn't
# give up a stud for a package worth less. consolidation_penalty is the required
# overpay (both ways); 0.05 clears a proper consolidation like Loveland+Higgins
# (+6.9% over J.Love) while rejecting a discounted one. win_win is a soft ranking
# bump (winwin_bonus), NOT a hard filter. max_opp_drop still refuses deals that
# tank the other side's playoff odds, e.g. asking for a team's only startable QB.
consolidation <- as.numeric(Sys.getenv("FFS_TRADE_CONSOLIDATION", "0.05"))
winwin_bonus  <- as.numeric(Sys.getenv("FFS_TRADE_WINWIN_BONUS", "0.5"))
uneven_winwin <- as.logical(as.integer(Sys.getenv("FFS_TRADE_UNEVEN_WINWIN", "0")))
max_opp_drop <- as.numeric(Sys.getenv("FFS_TRADE_MAX_OPP_DROP", "0.20"))
future_weight <- as.numeric(Sys.getenv("FFS_TRADE_FUTURE_WEIGHT", "1"))
min_future   <- as.numeric(Sys.getenv("FFS_TRADE_MIN_FUTURE", "-750"))
playoff_slots <- if (!is.null(config$playoff_slots)) config$playoff_slots else 6L

# role thresholds (shared by roster labels and the console narrative)
TH <- list(
  depth_value = 150,   # below this, market value is noise: "depth"
  win_wins    = 0.10,  # leave-one-out wins to MY team that counts as "helping win"
                       # (n=400 run-to-run SD ~0.04, so this is a stable cut)
  chip_value  = 800,   # TRADE CHIP: parked capital worth moving ...
  # trajectory thresholds are RELATIVE to the position's incumbent drift: the
  # projection universe loses ~20%/yr by construction (rookie inflow takes rank
  # slots, exits go to zero), so absolute change conflates "fading" with
  # "average" - Tyler Warren read declining at -21% when TE drift was -16%
  fade        = -0.075, # "declining": >=7.5% drop BEYOND the position drift
  rise        = 0.075,  # "appreciating": >=7.5% gain vs the position drift ...
  rise_p      = 0.25,  # ... with decent P(rise) (absolute p_rise is already
                       # conservative under a -20% tide)
  bust_p      = 0.20,  # exit-risk flag
  edge_floor  = 300    # edge/robust rankings only above this value
)

## ---- valuation sim: fast, REAL schedule ----------------------------------------
actual_sched <- if (!is.null(config$actual_schedule)) config$actual_schedule else TRUE
message("valuation sim (n=", n_trade, ", actual_schedule=", actual_sched, ") @ ", Sys.time())
set.seed(config$season + 1L)
vsim <- ff_simulate(conn, n_seasons = n_trade, version = "v3",
                    lineup_method = "rank", replacement_level = FALSE,
                    actual_schedule = actual_sched, return = "all")

fr <- data.table::as.data.table(vsim$franchises)[, list(franchise_id, franchise_name)]
fr <- unique(fr)

## ---- market + model context per player ------------------------------------------
fmt <- ffsimulator:::.ffs_detect_qb_format(vsim$lineup_constraints)
d <- data.table::copy(data.table::as.data.table(dyn))  # don't mutate the suite's dyn
d[, `:=`(player_id = as.character(player_id), fantasypros_id = as.character(fantasypros_id))]
if (!is.null(dyn_vals)) {
  fcm <- data.table::as.data.table(dyn_vals)
  fcm <- fcm[fcm$format == fmt]
  fcm[, fantasypros_id := as.character(fantasypros_id)]
  d <- merge(d, fcm[, list(fantasypros_id, redraft_value, trend_30day, tier)],
             by = "fantasypros_id", all.x = TRUE)
} else {
  d[, `:=`(redraft_value = NA_real_, trend_30day = NA_real_, tier = NA_integer_)]
}
# trajectory reads use the MEDIAN simulated next value: the rank->value curve
# is convex, so the mean is Jensen-inflated by the draw spread (backtest: mean
# exp_change overstates realized change, QB worst ~+50pts, while the value
# distribution itself is calibrated -> median ~unbiased). The mean stays for
# additive capital totals (portfolio sums, trade packages, mkt_winnow).
if (!"next_value_med" %in% names(d)) d[, next_value_med := next_value_mean]
d[, `:=`(
  exp_change    = next_value_med / cur_value - 1,   # model's typical value move
  trend_pct     = trend_30day / cur_value,          # market's recent move
  redraft_ratio = redraft_value / cur_value         # market win-now $ per dynasty $
)]
# position-level incumbent drift (capital-weighted): the baseline every
# trajectory statement is measured against (median-based, matching exp_change)
pos_drift_tbl <- d[!is.na(cur_value) & cur_value > 0,
                   list(pos_drift = sum(next_value_med) / sum(cur_value) - 1), by = pos]
d <- merge(d, pos_drift_tbl, by = "pos", all.x = TRUE)
d[is.na(pos_drift), pos_drift := sum(d$next_value_med, na.rm = TRUE) /
    sum(d$cur_value, na.rm = TRUE) - 1]
d[, `:=`(
  rel_change = (1 + exp_change) / (1 + pos_drift) - 1,  # move vs position drift
  growth_abs = next_value_med - cur_value * (1 + pos_drift)  # $ added vs drift
)]

# end-of-roster shrinkage: wins-per-1k$ explodes at small denominators, so pull
# it toward the positional median with weight rising in market value - cheap
# players need extreme, repeated evidence to earn an extreme label
shrink_ratio <- function(raw, cur_value, pos) {
  med <- stats::ave(raw, pos, FUN = function(x) stats::median(x, na.rm = TRUE))
  w <- cur_value / (cur_value + 1000)
  out <- w * raw + (1 - w) * med
  out[is.na(raw)] <- NA_real_
  out
}

# value decomposition. cur_value is a 1-D projection of a 2-D asset: win-now
# production + future store. Split it:
#   mkt_winnow = cur_value - next_value_mean  = what the market charges for THIS
#     year's production. <=0 means a future/growth asset (a young riser the
#     market pays you to hold) - win-now analysis does not apply to him.
#   fit_residual = how much CHEAPER the market prices a win-now player than the
#     PLAYOFF ODDS he adds to MY team warrant. ONE market line (the league's
#     available players: mkt_winnow ~ playoff_delta over win-now targets) is fit,
#     then BOTH the targets and my roster are scored against it, fit_residual =
#     predicted - actual, so "priced-rich vs the market" means the same on both
#     sheets. Positive =
#     the market underprices his win-now relative to what he does for my playoff
#     odds (acquire); strongly negative on my own roster = win-now I am paying
#     for but cannot cash into playoff odds (surplus to sell). Playoff delta (not
#     raw wins) is the true objective - it bakes in leverage (a win at a 70%
#     bubble is worth more than at a 95% lock). It converges SLOWLY though, so
#     this is only trustworthy on rows valued at the standings n (roster always;
#     targets only where confirmed=TRUE - run FFS_TRADE_TARGET_CONFIRM_N=100).
#     This isolates the ONE component the market can't see - win-now FIT to my
#     team - from the future component it prices efficiently.
# Future assets (mkt_winnow<=0) get NA: judge them on retention/exp_change.
fit_residual_cols <- function(dt, valcol, coef) {
  d <- data.table::as.data.table(data.table::copy(dt))
  # deliberately MEAN-based: mkt_winnow is a capital decomposition (cur minus
  # expected future store), and the fit_residual regression absorbs any
  # systematic level shift across players anyway
  d[, mkt_winnow := cur_value - next_value_mean]
  fr <- rep(NA_real_, nrow(d))
  wn <- which(d$mkt_winnow > 0 & is.finite(d[[valcol]]))
  if (length(wn) && all(is.finite(coef)))
    fr[wn] <- (coef[[1]] + coef[[2]] * d[[valcol]][wn]) - d$mkt_winnow[wn]
  list(mkt_winnow = d$mkt_winnow, fit_residual = fr)
}

## ---- 1. roster.csv --------------------------------------------------------------
rs_v <- data.table::as.data.table(vsim$roster_scores)
my_ids <- unique(rs_v[franchise_id == me & !grepl("^(QB|RB|WR|TE|K)_\\d+$", player_id)][["player_id"]])
# verdicts ride on these numbers, so they come from the big standings sim
# (run-to-run SD roughly halves vs the search sim)
message("valuing my roster (", length(my_ids), " players, on the n=",
        length(unique(data.table::as.data.table(sim$summary_season)$season)),
        " standings sim) @ ", Sys.time())
# base standing is player-independent - compute my base summary once and reuse
# it for every player (ffs_player_value re-optimises only the affected weeks)
base_me <- ffsimulator:::.ffs_franchise_summary(sim, me)
roster <- data.table::rbindlist(lapply(my_ids, function(p) {
  v <- ffs_player_value(sim, p, me, base_summary = base_me)
  data.table::data.table(player_id = as.character(p),
                         value_to_me = v$h2h_wins, playoff_delta_me = v$playoff_pct)
}))
names_v <- unique(rs_v[, list(player_id = as.character(player_id), player_name, pos)])
roster <- merge(roster, names_v, by = "player_id", all.x = TRUE)
roster <- merge(
  roster,
  d[, list(player_id, age, cur_value, next_value_mean, next_value_med,
           next_value_p10, next_value_p90,
           p_rise, p_exit, exp_change, rel_change, pos_drift, trend_pct,
           redraft_ratio, tier)],
  by = "player_id", all.x = TRUE
)

# drivers: which players separate my playoff sims from my basement sims (needs
# the big standings sim's team-season spread, not the fast valuation sim)
drivers <- local({
  ss2 <- data.table::as.data.table(sim$summary_season)
  team <- ss2[franchise_id == me, list(season, allplay_winpct)]
  os <- data.table::as.data.table(sim$optimal_scores)[franchise_id == me]
  idcol <- if ("starter_player_id" %in% names(os)) "starter_player_id" else "optimal_player_id"
  st <- os[, list(player_id = unlist(get(idcol))), by = list(season, week)][!is.na(player_id)]
  rs_me <- data.table::as.data.table(sim$roster_scores)[
    franchise_id == me, list(season, week, player_id, projected_score)]
  started <- merge(st, rs_me, by = c("season", "week", "player_id"))
  pl <- started[, list(pts = sum(projected_score), wk = .N), by = list(season, player_id)]
  grid <- data.table::CJ(season = unique(team$season), player_id = unique(rs_me$player_id))
  pl <- merge(grid, pl, by = c("season", "player_id"), all.x = TRUE)
  pl[is.na(pts), pts := 0][is.na(wk), wk := 0]
  pl <- merge(pl, team, by = "season")
  qs <- stats::quantile(team$allplay_winpct, c(.25, .75))
  pl[, list(
    mean_weeks_started = mean(wk),
    swing = mean(pts[allplay_winpct >= qs[2]]) - mean(pts[allplay_winpct <= qs[1]])
  ), by = player_id][, player_id := as.character(player_id)]
})
roster <- merge(roster, drivers, by = "player_id", all.x = TRUE)

roster[, wins_per_1k := shrink_ratio(value_to_me / (cur_value / 1000), cur_value, pos)]
roster[, pos_depth_rank := data.table::frank(-value_to_me, ties.method = "first"), by = pos]

# dedicated starting slots per position: TRADE CHIP only applies to players
# buried behind a full room (starters + 1 insurance), otherwise a deep dynasty
# bench flags half the roster as movable - true but unactionable
lc_v <- data.table::as.data.table(vsim$lineup_constraints)
n_start <- stats::setNames(lc_v[["min"]], lc_v[["pos"]])
roster[, pos_starters := data.table::fifelse(pos %in% names(n_start),
                                             as.numeric(n_start[pos]), 1)]

# roles, not directives: two independent axes - does he help MY lineup win
# (leave-one-out wins), and what is his value doing (appreciating / holding /
# declining)? Surplus is a MODIFIER of low-wins holders, not a sell trigger:
# a TRADE CHIP's capital is parked and portable (convert only for a lineup
# upgrade), a SELL (fading)'s capital is actually leaving - urgency comes from
# the trajectory axis. Bust risk is a flag, not a role. (Redesigned after the
# Egbuka case: a young appreciating WR buried behind three starters read
# "SELL (redundant)" on a knife-edge wins-per-$ threshold.)
ge <- function(x, y) !is.na(x) & x >= y
le <- function(x, y) !is.na(x) & x <= y
roster[, wins_now := ge(value_to_me, TH$win_wins)]
roster[, trajectory := data.table::fifelse(
  le(rel_change, TH$fade), "declining",
  data.table::fifelse(ge(rel_change, TH$rise) & ge(p_rise, TH$rise_p),
                      "appreciating", "holding"))]
roster[, verdict := data.table::fifelse(
  is.na(cur_value) | cur_value < TH$depth_value, "depth",
  data.table::fifelse(
    wins_now & trajectory != "declining", "CORE (wins + value)",
    data.table::fifelse(
      wins_now, "RENTAL (win-now, fading)",
      data.table::fifelse(
        trajectory == "appreciating", "STASH (appreciating)",
        data.table::fifelse(
          trajectory == "declining", "SELL (fading)",
          data.table::fifelse(
            ge(cur_value, TH$chip_value) & pos_depth_rank > pos_starters + 1,
            "TRADE CHIP (surplus)", "hold")
        )
      )
    )
  )
)]
roster[, bust_flag := ge(p_exit, TH$bust_p)]

# best buyers for the biggest MOVABLE pieces - SELL (urgent), TRADE CHIP
# (parked surplus), RENTAL (melting win-now, sellable when not contending):
# the player's value to each OTHER franchise (11 valuation calls per player,
# so cap to the most capital-relevant ones)
sell_max <- as.integer(Sys.getenv("FFS_SELL_MAX", "4"))
others <- setdiff(fr$franchise_id, me)
sell_rows <- roster[grepl("SELL|TRADE CHIP|RENTAL", verdict)][order(-cur_value)][
  seq_len(min(.N, sell_max))]
buyer_tbl <- NULL
if (nrow(sell_rows)) {
  message("finding buyers for ", nrow(sell_rows), " movable players @ ", Sys.time())
  # cache each buyer franchise's base summary (player-independent) across the scan
  buyer_base <- lapply(stats::setNames(others, as.character(others)),
                       function(f) ffsimulator:::.ffs_franchise_summary(vsim, f))
  buyer_tbl <- data.table::rbindlist(lapply(sell_rows$player_id, function(p) {
    vb <- data.table::rbindlist(lapply(others, function(f) {
      v <- ffs_player_value(vsim, p, f, base_summary = buyer_base[[as.character(f)]])
      data.table::data.table(franchise_id = f, playoff = v$playoff_pct, wins = v$h2h_wins)
    }))
    # rank buyers by wins delta (converges much faster than playoff pct at the
    # search sim's n); playoff shown alongside as the noisier headline number
    vb <- merge(vb, fr, by = "franchise_id")[order(-wins)]
    data.table::data.table(
      player_id = p,
      buyer_ids = list(utils::head(vb$franchise_id, 3)),
      best_buyers = paste(sprintf("%s (%+.2fw/%+.0f%%)", utils::head(vb$franchise_name, 3),
                                  utils::head(vb$wins, 3), 100 * utils::head(vb$playoff, 3)),
                          collapse = "; ")
    )
  }))
  roster <- merge(roster, buyer_tbl[, list(player_id, best_buyers)],
                  by = "player_id", all.x = TRUE)
} else {
  roster[, best_buyers := NA_character_]
}

# reallocation view on my OWN roster (same two axes as targets): pareto_front =
# efficiency x trajectory front (1 = non-dominated); dominated_by names the
# rostermate(s) that give more wins-per-$ AND a better trajectory. This is a
# pure efficiency lens - it IGNORES concentration, so it will flag studs
# (Lamb/London are "dominated" by cheaper efficient players you can't replace
# in-slot). Read it as "who is efficiency-inferior to someone I already have"
# and judge replaceability (top-end value can't always be reassembled cheaply).
roster[, `:=`(pareto_front = NA_real_, dominated_by = NA_character_)]
rm_mat <- which(roster$cur_value >= TH$depth_value &
                is.finite(roster$wins_per_1k) & is.finite(roster$exp_change))
if (length(rm_mat) >= 2L) {
  rf <- roster[rm_mat]
  roster[rm_mat, pareto_front := ffs_pareto_front(
    rf[, list(wins_per_1k, exp_change)], maximize = c(TRUE, TRUE))]
  wp <- rf$wins_per_1k; ec <- rf$exp_change; nm <- rf$player_name
  dom <- vapply(seq_along(wp), function(i) {
    d <- which(wp >= wp[i] & ec >= ec[i] & (wp > wp[i] | ec > ec[i]))
    if (!length(d)) return(NA_character_)
    paste(nm[d[order(-wp[d])]][seq_len(min(3L, length(d)))], collapse = "; ")
  }, character(1))
  roster[rm_mat, dominated_by := dom]
}

# NOTE: roster.csv is written LATER (right after targets.csv) so its win-now
# decomposition (mkt_winnow / fit_residual) scores against the SAME market
# $/playoff-point line fit on the league's available players - see the targets
# block. roster stays fully computed here (verdict, dominated_by, best_buyers).

## ---- 2. targets.csv --------------------------------------------------------------
message("trade targets (top_n=", trade_top_n, ") @ ", Sys.time())
targets <- data.table::as.data.table(ffs_trade_targets(vsim, me, top_n = trade_top_n))
targets[, player_id := as.character(player_id)]
tg <- merge(
  targets,
  d[, list(player_id, age, franchise_name, cur_value, next_value_mean, next_value_med,
           p_rise, p_exit,
           exp_change, rel_change, trend_pct, redraft_ratio, growth_abs, tier)],
  by = "player_id", all.x = TRUE
)
tg <- tg[!is.na(cur_value) & cur_value > 0]
tg[, retention := next_value_med / cur_value]
tg[, wins_per_1k := shrink_ratio(value_to_you / (cur_value / 1000), cur_value, pos)]
tg[, gettable := surplus > 0]
tg[, fade_flag := !is.na(trend_pct) & le(rel_change, TH$fade) & trend_pct >= 0]
# reallocation frontier: return-on-capital (wins_per_1k) vs value trajectory
# (exp_change). value MAGNITUDE is fungible via trades, so it is folded into the
# wins_per_1k ratio rather than standing as its own axis (the old 3-axis
# value_to_you/cur_value/growth front was so broad ~half the pool was front-1).
# exp_change is the RAW expected % move, not drift-adjusted: drift-adjusting
# (rel_change) forgives structural RB decline while cheap RBs already win the
# wins_per_1k axis, double-counting them onto the front; Pareto keeps the
# corners either way, so raw is both honest capital and more position-balanced.
# Computed on the material subset (helps me, priced) so cheap fringe with noisy
# ratios do not crowd the front.
tg[, pareto_front := NA_real_]
mat <- which(tg$value_to_you > 0 & tg$cur_value >= TH$depth_value &
             is.finite(tg$wins_per_1k) & is.finite(tg$exp_change))
if (length(mat) >= 1L) {
  tg[mat, pareto_front := ffs_pareto_front(
    tg[mat, list(wins_per_1k, exp_change)], maximize = c(TRUE, TRUE))]
}

# buy edge + sweet-spot robustness, only where values are trustworthy
eligible <- which(tg$cur_value >= TH$edge_floor)
tg[, `:=`(edge_rk = NA_real_, robust_rank = NA_real_, sweet_spot = FALSE, tilt = "")]
if (length(eligible) >= 5) {
  e <- tg[eligible]
  # our wins-per-$ rank minus the market's redraft-value rank: positive = the
  # market prices him below what he does for MY lineup
  has_rr <- !is.na(e$redraft_ratio)
  if (sum(has_rr) >= 5) {
    erk <- data.table::frank(-e$redraft_ratio[has_rr]) -
      data.table::frank(-e$wins_per_1k[has_rr])
    tg[eligible[has_rr], edge_rk := erk]
  }
  # sweet-spot iteration: rank candidates under a grid of win-now/growth
  # weightings (cost always counts against); robust picks are top-k under most
  zs <- function(x) {
    s <- stats::sd(x, na.rm = TRUE)
    if (is.na(s) || s == 0) return(rep(0, length(x)))
    (x - mean(x, na.rm = TRUE)) / s
  }
  # same two axes as the frontier: wins_per_1k already nets cost (it is a ratio),
  # exp_change is the value trajectory - no separate cost term to subtract.
  zw <- zs(e$wins_per_1k); zg <- zs(e$exp_change)
  wgrid <- data.table::CJ(win_w = c(0.5, 1, 2), growth_w = c(0.5, 1, 2))
  rks <- vapply(seq_len(nrow(wgrid)), function(i) {
    data.table::frank(-(wgrid$win_w[i] * zw + wgrid$growth_w[i] * zg),
                      ties.method = "average")
  }, numeric(nrow(e)))
  top_k <- min(10L, nrow(e))
  tg[eligible, robust_rank := apply(rks, 1, stats::median)]
  tg[eligible, sweet_spot := rowMeans(rks <= top_k) >= 0.75]
  rw <- rks[, wgrid$win_w == 2 & wgrid$growth_w == 0.5]
  rg <- rks[, wgrid$win_w == 0.5 & wgrid$growth_w == 2]
  tg[eligible, tilt := data.table::fifelse(
    rw <= top_k & rg > top_k, "win-now pick",
    data.table::fifelse(rg <= top_k & rw > top_k, "growth pick", ""))]
}
# confirm the SHORTLIST's value on the n=2000 standings sim. The search sim
# ranks candidates fine (ranking is rank-stable at low n - the whole convergence
# finding), but the reported value_to_you carries ~5% run-to-run noise there: a
# QB3's rank-lineup optionality can read 0.2 at n=60 yet ~0.05 at n=2000. So
# frontier/sweet-spot/edge stay on the search sim, and only the top targets'
# value/surplus/playoff get re-priced on the standings sim (`confirmed` = TRUE).
# Cheap: ~15 players, not all top_n. FFS_TRADE_TARGET_CONFIRM_N=0 skips it.
tg[, confirmed := FALSE]
tconf_n <- as.integer(Sys.getenv("FFS_TRADE_TARGET_CONFIRM_N", "15"))
# confirm the frontier + sweet-spot picks AND the highest-value_to_you adds
# (the biggest headline numbers, and where search-sim optionality inflates most)
vy <- order(-tg$value_to_you)
conf_rows <- utils::head(unique(c(which(tg$pareto_front == 1),
                                  which(tg$sweet_spot %in% TRUE), vy)), tconf_n)
if (tconf_n > 0 && length(conf_rows) && exists("sim")) {
  message("confirming ", length(conf_rows), " top targets on the standings sim @ ", Sys.time())
  tbase <- new.env(parent = emptyenv())
  tbase_summ <- function(f) { k <- as.character(f); v <- tbase[[k]]
    if (is.null(v)) { v <- ffsimulator:::.ffs_franchise_summary(sim, f); tbase[[k]] <- v }; v }
  for (ri in conf_rows) {
    p <- tg$player_id[ri]; o <- tg$owner_id[ri]
    ty <- ffs_player_value(sim, p, me, base_summary = tbase_summ(me))
    to <- ffs_player_value(sim, p, o,  base_summary = tbase_summ(o))
    tg[ri, `:=`(value_to_you = ty$h2h_wins, playoff_delta_you = ty$playoff_pct,
                value_to_owner = to$h2h_wins, playoff_delta_owner = to$playoff_pct,
                surplus = ty$h2h_wins - to$h2h_wins, confirmed = TRUE)]
  }
  tg[, gettable := surplus > 0]                               # re-price on confirmed value
  tg[, wins_per_1k := shrink_ratio(value_to_you / (cur_value / 1000), cur_value, pos)]
}

# win-now/future decomposition. Fit the market's $/playoff-point line ONCE on the
# league's available players (targets, win-now assets, on the confirmed playoff
# deltas), then score the targets here and my roster below against the SAME line.
# mkt_winnow<=0 = future asset (NA fit_residual, judge on trajectory instead).
tg[, mkt_winnow := cur_value - next_value_mean]
mkt_wn <- tg[mkt_winnow > 0 & is.finite(playoff_delta_you)]
mkt_coef <- if (nrow(mkt_wn) >= 8L)
  stats::coef(stats::lm(mkt_winnow ~ playoff_delta_you, mkt_wn)) else c(NA_real_, NA_real_)
tg[, fit_residual := fit_residual_cols(tg, "playoff_delta_you", mkt_coef)$fit_residual]

# lead with confirmed win-now fit-bargains, then the search-sim ranking
data.table::setorder(tg, -confirmed, -fit_residual, robust_rank, -value_to_you, na.last = TRUE)
data.table::fwrite(round_sheet(tg[, list(
  player_name, pos, age, owner = franchise_name, cur_value, confirmed,
  value_to_you, playoff_delta_you, value_to_owner, playoff_delta_owner, surplus,
  mkt_winnow, fit_residual,
  gettable, retention, growth_abs, p_rise, p_exit, exp_change, rel_change, trend_pct,
  redraft_ratio, wins_per_1k, edge_rk, pareto_front, robust_rank, sweet_spot,
  tilt, fade_flag, player_id, owner_id)]), file.path(out, "targets.csv"))

# roster win-now decomposition vs the SAME market line, then write roster.csv
roster[, `:=`(mkt_winnow = cur_value - next_value_mean,
              fit_residual = fit_residual_cols(roster, "playoff_delta_me", mkt_coef)$fit_residual)]
data.table::setorder(roster, -cur_value, na.last = TRUE)
data.table::fwrite(round_sheet(roster[, list(
  player_name, pos, age, cur_value, exp_change, rel_change, pos_drift,
  trend_pct, p_rise, p_exit, value_to_me, playoff_delta_me, wins_per_1k,
  mkt_winnow, fit_residual, swing,
  mean_weeks_started, pos_depth_rank, trajectory, verdict, bust_flag,
  pareto_front, dominated_by, best_buyers, player_id)]),
  file.path(out, "roster.csv"))

# win-now value map: market's win-now charge (x) vs playoff odds to my team (y),
# BOTH price lines (my roster + league) on both panels, coloured by fit_residual
if (requireNamespace("ggplot2", quietly = TRUE)) {
  vm <- data.table::rbindlist(list(
    roster[, list(player_name, pos, cur_value, mkt_winnow, fit_residual,
                  pd = playoff_delta_me, confirmed = TRUE, panel = "My roster")],
    tg[, list(player_name, pos, cur_value, mkt_winnow, fit_residual,
              pd = playoff_delta_you, confirmed, panel = "Targets (acquire)")]))
  vm[, cat := data.table::fifelse(mkt_winnow <= 0, "future (judge on trajectory)",
        data.table::fifelse(fit_residual > 0, "win-now bargain (edge)", "win-now priced-rich"))]
  rwn <- roster[mkt_winnow > 0 & is.finite(playoff_delta_me)]
  rcoef <- if (nrow(rwn) >= 8L) stats::coef(stats::lm(mkt_winnow ~ playoff_delta_me, rwn)) else mkt_coef
  yv <- seq(0, max(vm$pd, na.rm = TRUE), length.out = 60)
  vmln <- data.table::rbindlist(list(
    data.table::data.table(pd = yv, mkt_winnow = rcoef[[1]] + rcoef[[2]] * yv, line = "my roster"),
    data.table::data.table(pd = yv, mkt_winnow = mkt_coef[[1]] + mkt_coef[[2]] * yv, line = "league (available)")))
  vmln <- rbind(cbind(data.table::copy(vmln), panel = "My roster"),
                cbind(data.table::copy(vmln), panel = "Targets (acquire)"))
  vmlab <- rbind(vm[panel == "My roster"], vm[panel == "Targets (acquire)" & confirmed == TRUE])
  vp <- ggplot2::ggplot(vm, ggplot2::aes(mkt_winnow, pd)) +
    ggplot2::geom_vline(xintercept = 0, color = "#dddddd", linewidth = 0.4) +
    ggplot2::geom_line(data = vmln, ggplot2::aes(linetype = line), color = "#333333", linewidth = 0.6) +
    ggplot2::geom_point(ggplot2::aes(color = cat, shape = confirmed, size = cur_value), alpha = 0.85) +
    ggplot2::facet_wrap(~panel, scales = "free") +
    ggplot2::scale_color_manual(values = c("win-now bargain (edge)" = "#1b9e77",
      "win-now priced-rich" = "#d95f02", "future (judge on trajectory)" = "#8c8c8c"), name = NULL) +
    ggplot2::scale_linetype_manual(values = c("my roster" = "solid",
      "league (available)" = "21"), name = "win-now price line") +
    ggplot2::scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1),
      labels = c(`TRUE` = "confirmed", `FALSE` = "search n=60"), name = NULL) +
    ggplot2::scale_size_continuous(range = c(1.4, 6), guide = "none") +
    ggplot2::labs(
      title = "Win-now value map: market's win-now charge vs playoff odds to your team",
      subtitle = "Above/left of a line = bargain vs that market. Gap between the lines = your reallocation edge. Left of 0 = future asset.",
      x = "mkt_winnow = market's charge for this-year production (cur - next; <0 = future)",
      y = "playoff odds added to YOUR team") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(color = "#555555", size = 9))
  if (requireNamespace("ggrepel", quietly = TRUE))
    vp <- vp + ggrepel::geom_text_repel(data = vmlab,
      ggplot2::aes(label = player_name, color = cat), size = 2.6, min.segment.length = 0,
      max.overlaps = 40, seed = 1, show.legend = FALSE, segment.color = "#bbbbbb", segment.size = 0.2)
  ggplot2::ggsave(file.path(out, "value_map.png"), vp, width = 13, height = 6.6, dpi = 150)
}

# pareto plot, highlighting the robust sweet-spot picks
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  p <- ggplot(tg[!is.na(pareto_front)], aes(x = wins_per_1k, y = exp_change)) +
    geom_point(aes(size = cur_value, color = sweet_spot,
                   shape = pareto_front == 1), alpha = 0.7) +
    scale_color_manual(values = c(`TRUE` = "#1b9e77", `FALSE` = "#999999"),
                       name = "sweet spot") +
    scale_shape_manual(values = c(`TRUE` = 17, `FALSE` = 16),
                       name = "on frontier") +
    scale_size_continuous(name = "acquisition cost ($)") +
    labs(title = "trade targets: return on capital vs value trajectory",
         subtitle = "triangles = Pareto frontier; green = robust across win-now/growth weightings",
         x = "wins added per $1k (return on capital)",
         y = "expected value change next year") +
    theme_minimal(base_size = 12)
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p + ggrepel::geom_text_repel(data = tg[sweet_spot == TRUE],
                                      aes(label = player_name), size = 3,
                                      max.overlaps = 20)
  }
  ggsave(file.path(out, "pareto.png"), p, width = 10, height = 7, dpi = 150)
}

## ---- 3. trades.csv ----------------------------------------------------------------
message("building buy-side trades @ ", Sys.time())
trades_buy <- data.table::as.data.table(ffs_build_trades(
  vsim, me, targets = targets, dynasty = dyn, value_band = value_band,
  uneven_shade = uneven_shade, consolidation_penalty = consolidation,
  max_opp_drop = max_opp_drop, winwin_bonus = winwin_bonus,
  uneven_require_winwin = uneven_winwin,
  future_weight = future_weight, min_future_delta = min_future))
if (nrow(trades_buy)) trades_buy[, motive := "buy"]

# sell matchmaker: for each SELL, deals that send HIM to his best buyers and
# bring back similar-priced pieces that improve my team. Incoming candidates
# come from the buyer's own priced roster (valued to me on the fly).
trades_sell <- list()
if (!is.null(buyer_tbl) && nrow(buyer_tbl)) {
  valued_cache <- new.env(parent = emptyenv())
  value_to_me_of <- function(p) {
    if (!is.null(valued_cache[[p]])) return(valued_cache[[p]])
    v <- ffs_player_value(vsim, p, me)$h2h_wins
    valued_cache[[p]] <- v
    v
  }
  for (i in seq_len(nrow(buyer_tbl))) {
    p <- buyer_tbl$player_id[i]
    p_name <- roster[player_id == p][["player_name"]][1]
    p_val <- roster[player_id == p][["cur_value"]][1]
    buyers <- buyer_tbl$buyer_ids[[i]]
    # buyer pieces near the price (wide enough for 1:2 combos to sum into band)
    pool <- d[franchise_id %in% buyers & !is.na(cur_value) &
                cur_value >= 0.15 * p_val & cur_value <= 1.4 * p_val &
                player_id %in% rs_v$player_id & player_id != p]
    pool <- pool[order(abs(cur_value - p_val)), utils::head(.SD, 10), by = franchise_id]
    if (!nrow(pool)) next
    message("sell matchmaker for ", p_name, ": valuing ", nrow(pool),
            " pieces from ", length(buyers), " buyers @ ", Sys.time())
    tmini <- unique(rbind(
      targets[owner_id %in% buyers, list(player_id, player_name, pos, owner_id, value_to_you)],
      pool[, list(player_id, player_name, pos, owner_id = franchise_id,
                  value_to_you = vapply(player_id, value_to_me_of, numeric(1)))]
    ), by = "player_id")
    deal <- data.table::as.data.table(ffs_build_trades(
      vsim, me, targets = tmini, dynasty = dyn, value_band = value_band,
      uneven_shade = uneven_shade, consolidation_penalty = consolidation,
      max_opp_drop = max_opp_drop, winwin_bonus = winwin_bonus,
      uneven_require_winwin = uneven_winwin,
      future_weight = future_weight, min_future_delta = min_future,
      must_send = p, opponents = buyers, screen_n = 15L, top_n = 5))
    if (nrow(deal)) {
      deal[, motive := paste0("sell ", p_name)]
      trades_sell[[length(trades_sell) + 1L]] <- deal
    }
  }
}
trades <- data.table::rbindlist(c(list(trades_buy), trades_sell), use.names = TRUE, fill = TRUE)
if (!"motive" %in% names(trades)) trades[, motive := character(nrow(trades))]
data.table::setcolorder(trades, "motive")
# the buy scan and sell matchmaker can construct the same package - keep one
# row, noting every motive that found it
if (nrow(trades)) {
  trades[, motive := paste(unique(motive), collapse = " & "),
         by = list(opponent, send, receive)]
  trades <- unique(trades, by = c("opponent", "send", "receive"))
}

# confirm reported deltas on the n=400 standings sim: the search sim's playoff
# deltas carry ~+-5% run-to-run SD, which is not converged enough to quote.
# ffs_trade_eval re-ranks the same 400 draws (paired), so confirmed deltas are
# standings-grade. Best deal per motive is always confirmed, then top buys.
trades[, confirmed := FALSE]
if (nrow(trades)) {
  trades[, rid := .I]
  # confirm the deals we would actually rank highest: best per motive, then by
  # the builder's future-blended score (NOT raw win-now delta - future_weight
  # must survive into what gets confirmed and shown)
  first_per_motive <- trades[, list(rid = rid[1]), by = motive][["rid"]]
  by_score <- setdiff(trades[order(-score)][["rid"]], first_per_motive)
  confirm_ids <- utils::head(c(first_per_motive, by_score), confirm_n)
  message("confirming ", length(confirm_ids), " deals on the standings sim @ ", Sys.time())
  for (rid_i in confirm_ids) {
    te <- data.table::as.data.table(ffs_trade_eval(
      sim, me, trades$send_ids[[rid_i]],
      trades$opponent[[rid_i]], trades$recv_ids[[rid_i]]))
    m <- te[te$franchise_id == me]
    op <- te[te$franchise_id != me]
    trades[rid == rid_i, `:=`(
      my_win_delta = m$h2h_wins_delta, my_playoff_delta = m$playoff_pct_delta,
      opp_win_delta = op$h2h_wins_delta, opp_playoff_delta = op$playoff_pct_delta,
      win_win = m$h2h_wins_delta > 0 & op$h2h_wins_delta > 0,
      confirmed = TRUE)]
  }
  # re-apply the acceptability gate on the CONFIRMED (n=400) opponent delta:
  # a deal that squeaked past the noisy search-sim value can confirm past the
  # threshold once the standings sim re-prices the other side's downside
  trades <- trades[is.na(opp_playoff_delta) | opp_playoff_delta >= -max_opp_drop]
  # final ranking re-blends win-now and future value from the CONFIRMED deltas
  zt <- function(x) {
    s <- stats::sd(x, na.rm = TRUE)
    if (is.na(s) || s == 0) return(rep(0, length(x)))
    (x - mean(x, na.rm = TRUE)) / s
  }
  trades[, score := zt(my_playoff_delta) + future_weight * zt(future_capital_delta) +
           winwin_bonus * as.numeric(win_win %in% TRUE)]
  data.table::setorder(trades, -confirmed, -score)
  trades[, rid := NULL]
}
# ids as readable strings for the sheet; list-columns don't survive fwrite
trades_out <- data.table::copy(trades)
if (nrow(trades_out)) {
  trades_out[, `:=`(
    send_ids = vapply(send_ids, paste, character(1), collapse = " + "),
    recv_ids = vapply(recv_ids, paste, character(1), collapse = " + "))]
}
data.table::fwrite(round_sheet(trades_out), file.path(out, "trades.csv"))

## ---- 4. portfolio.csv --------------------------------------------------------------
ss <- data.table::as.data.table(sim$summary_season)
# wins then points-for, deterministic (matches .ffs_franchise_summary)
ss[, lg_rank := data.table::frank(list(-h2h_wins, -points_for), ties.method = "first"),
   by = season]
my_playoff <- ss[franchise_id == me, mean(lg_rank <= playoff_slots)]
posture <- if (my_playoff >= 0.55) "contend" else if (my_playoff >= 0.35) "bubble" else "rebuild"

mine_d <- d[franchise_id == me & !is.na(cur_value)]
cap <- sum(mine_d$cur_value)
top3 <- sum(utils::head(sort(mine_d$cur_value, decreasing = TRUE), 3))
verdict_cap <- merge(
  roster[!is.na(cur_value), list(player_id, verdict, cur_value)],
  data.table::data.table(player_id = mine_d$player_id), by = "player_id"
)[, list(capital = sum(cur_value), n = .N), by = verdict][order(-capital)]

# positional allocation vs the league: my % of capital per position vs median team
pos_alloc <- d[!is.na(cur_value),
               list(share = sum(cur_value)), by = list(franchise_id, pos)]
pos_alloc[, share := share / sum(share), by = franchise_id]
lg_med <- pos_alloc[, list(league_median = stats::median(share)), by = pos]
my_alloc <- merge(pos_alloc[franchise_id == me, list(pos, my_share = share)],
                  lg_med, by = "pos")[order(-my_share)]

portfolio <- data.table::rbindlist(list(
  data.table::data.table(metric = "posture", group = posture, value = round(my_playoff, 3)),
  data.table::data.table(metric = "capital_now", group = "total", value = round(cap)),
  data.table::data.table(metric = "capital_next_mean", group = "total",
                         value = round(sum(mine_d$next_value_mean))),
  data.table::data.table(metric = "capital_next_p10", group = "downside",
                         value = round(sum(mine_d$next_value_p10))),
  data.table::data.table(metric = "capital_next_p90", group = "upside",
                         value = round(sum(mine_d$next_value_p90))),
  data.table::data.table(metric = "value_at_risk", group = "sum(cur*p_exit)",
                         value = round(sum(mine_d$cur_value * mine_d$p_exit))),
  data.table::data.table(metric = "top3_concentration", group = "share",
                         value = round(top3 / cap, 3)),
  data.table::data.table(metric = "age_weighted_capital", group = "years",
                         value = round(sum(mine_d$cur_value * mine_d$age, na.rm = TRUE) /
                                         sum(mine_d$cur_value[!is.na(mine_d$age)]), 1)),
  verdict_cap[, list(metric = "capital_by_verdict", group = verdict, value = capital)],
  my_alloc[, list(metric = "pos_allocation", group = pos,
                  value = round(my_share, 3))],
  my_alloc[, list(metric = "pos_allocation_league_median", group = pos,
                  value = round(league_median, 3))]
), use.names = TRUE)
data.table::fwrite(portfolio, file.path(out, "portfolio.csv"))

## ---- console narrative ---------------------------------------------------------------
fmtp <- function(x) ifelse(is.na(x), "  NA", sprintf("%+.0f%%", 100 * x))
cat("\n==== ", config$my_team, " trade intelligence (", fmt, ", valuation n=",
    n_trade, ") ====\n", sep = "")
cat("\nposture: ", posture, " (playoff odds ", sprintf("%.0f%%", 100 * my_playoff),
    " @ n=", length(unique(ss$season)), ")\n", sep = "")
cat("capital: ", round(cap), " now -> ", round(sum(mine_d$next_value_mean)),
    " expected (p10 ", round(sum(mine_d$next_value_p10)), " / p90 ",
    round(sum(mine_d$next_value_p90)), "); value-at-risk ",
    round(sum(mine_d$cur_value * mine_d$p_exit)), "\n", sep = "")
cat("concentration: top-3 assets hold ", sprintf("%.0f%%", 100 * top3 / cap),
    " of capital\n", sep = "")
cat("\n-- capital by verdict --\n")
print(verdict_cap)

cat("\n-- roster --\n")
print(round_sheet(roster[, list(player_name, pos, age, cur_value,
  model_chg = fmtp(exp_change), vs_drift = fmtp(rel_change), mkt_trend = fmtp(trend_pct),
  value_to_me, wins_per_1k, verdict)]))

if (!is.null(buyer_tbl) && nrow(buyer_tbl)) {
  cat("\n-- movable pieces (SELL = value leaving, urgent; TRADE CHIP = parked",
      "\n   surplus, no rush; RENTAL = win-now with melting value - keep while",
      "\n   contending): who buys, and what improves my team back --\n")
  for (i in seq_len(nrow(buyer_tbl))) {
    p <- buyer_tbl$player_id[i]
    p_name <- roster[player_id == p][["player_name"]][1]
    cat("\n", p_name, " -> buyers: ", buyer_tbl$best_buyers[i], "\n", sep = "")
    ret <- trades[motive == paste0("sell ", p_name)]
    if (nrow(ret)) {
      best <- ret[1]
      cat(sprintf("  best return: send %s -> get %s | me %+.2fw %+.1f%%pl | opp %+.2fw %+.1f%%pl%s\n",
                  best$send, best$receive, best$my_win_delta, 100 * best$my_playoff_delta,
                  best$opp_win_delta, 100 * best$opp_playoff_delta,
                  if (isTRUE(best$win_win)) " | WIN-WIN" else ""))
    } else {
      cat("  no value-matched return improves my team - hold or widen the band\n")
    }
  }
}

cat("\n-- top buys (sweet-spot picks first) --\n")
print(round_sheet(utils::head(tg[order(!sweet_spot, robust_rank, -value_to_you), list(
  player_name, pos, owner = franchise_name, cur_value, value_to_you,
  surplus, growth_abs, sweet_spot, tilt, fade_flag)], 12)))

# leaguewide fades outside the target list: don't buy these / their fragile assets
fades <- d[!is.na(trend_pct) & le(rel_change, TH$fade) & trend_pct >= 0 &
             cur_value >= 800 & !(player_id %in% tg$player_id) & franchise_id != me]
if (nrow(fades)) {
  cat("\n-- leaguewide fades (fading BEYOND position drift, market still bidding; not in targets) --\n")
  print(round_sheet(fades[order(rel_change), list(player_name, pos, age,
    owner = franchise_name, cur_value, vs_drift = fmtp(rel_change),
    mkt_trend = fmtp(trend_pct), p_exit)]))
}

cat("\n-- top deals (win-now + future value blend, future_weight=", future_weight,
    "; deltas confirmed on the n=400 standings sim, ~ = search-sim only) --\n", sep = "")
if (nrow(trades)) {
  # already sorted by -confirmed, -score; cap repeats of the same incoming
  # package so one popular target doesn't flood the list
  shown <- 0L; seen_recv <- c()
  for (i in seq_len(nrow(trades))) {
    dd <- trades[i]
    if (sum(seen_recv == dd$receive) >= 2) next
    seen_recv <- c(seen_recv, dd$receive)
    cat(sprintf("[%s]%s send %-28s -> get %-24s | me %+.2fw %+.1f%%pl | opp %+.2fw %+.1f%%pl | fut %+d%s\n",
                dd$motive, if (isTRUE(dd$confirmed)) "" else "~", dd$send, dd$receive,
                dd$my_win_delta, 100 * dd$my_playoff_delta,
                dd$opp_win_delta, 100 * dd$opp_playoff_delta,
                as.integer(round(dd$future_capital_delta)),
                if (isTRUE(dd$win_win)) " | WIN-WIN" else ""))
    shown <- shown + 1L
    if (shown >= 10L) break
  }
} else {
  cat("(no value-matched, net-positive packages found - widen FFS_TRADE_VALUE_BAND)\n")
}

cat("\nwrote roster.csv, targets.csv, trades.csv, portfolio.csv to ", out, "\n", sep = "")
