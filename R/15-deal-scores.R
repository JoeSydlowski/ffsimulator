# Shared trade-deal scoring -------------------------------------------------
#
# The reliability / haircut / acceptance math used to exist in three hand-synced
# copies (ffs_build_trades, dev/suite/confirm_trade_board.R and
# dev/suite/trade_intel.R), kept in agreement by comment alone. It now lives
# here and every caller goes through ffs_deal_scores().

#' Smooth win-now haircut from baseline playoff odds
#'
#' A contender values future value least. `clamp(intercept - playoff, floor, cap)`
#' - ~0.82 at a 48% bubble, ~0.65 near a 68% contender, 1.00 for a deep rebuilder.
#'
#' @param p baseline playoff odds on a 0-1 scale
#' @param intercept,floor,cap haircut schedule parameters
#' @keywords internal
.ffs_haircut <- function(p, intercept = 1.30, floor = 0.60, cap = 1.00) {
  pmin(pmax(intercept - p, floor), cap)
}

#' Default per-position future-value reliability
#'
#' The value-space calibration slope `lm(actual_move ~ predicted_move)` from
#' `dev/suite/dynasty_calibration.R`, capped into `[0, 1]`. A projected future
#' gain in QBs (~0.55 realized in superflex) is worth far less than one in WRs.
#'
#' Refit 2026-07-28 against the log-space transition model (commit 1bc4128).
#' Raw slopes: 1qb QB .651 RB .679 TE 1.362 WR 1.218; superflex QB .551 RB 1.105
#' TE .835 WR 1.046. Values above 1 would AMPLIFY projected moves, so they cap.
#'
#' @param format "1qb" or "superflex"
#' @keywords internal
.ffs_future_reliability <- function(format = "superflex") {
  if (identical(as.character(format), "1qb"))
    c(QB = 0.651, RB = 0.679, TE = 1.000, WR = 1.000) else
    c(QB = 0.551, RB = 1.000, TE = 0.835, WR = 1.000)
}

#' Build the reliability-adjusted future-capital function for a dynasty table
#'
#' Reliability discounts the projected **move** (`next - cur`), not the whole
#' level: a player's current market value is known, only next year's change is a
#' projection. Draft picks are pure speculation, so their future value is
#' level-discounted at `pick_reliability` - recovered as the residual of the raw
#' future delta that the real players do not explain.
#'
#' @param dynasty a dynasty outlook with player_id, cur_value, next_value_mean, pos
#' @param future_reliability named per-position vector; `NULL` uses [.ffs_future_reliability()]
#' @param pick_reliability level discount on draft-pick value
#' @param future_certainty fallback reliability for unknown positions
#' @param format QB format, used only when `future_reliability` is `NULL`
#'
#' @return a function `(send_ids, recv_ids, future_capital_delta)` returning the
#'   reliability-adjusted future gain to the side doing the receiving
#'
#' @keywords internal
.ffs_adj_future_fn <- function(dynasty, future_reliability = NULL,
                               pick_reliability = 0.55, future_certainty = 0.905,
                               format = "superflex", pos_lookup = NULL) {
  dyn <- data.table::as.data.table(dynasty)
  if (is.null(future_reliability)) future_reliability <- .ffs_future_reliability(format)
  ids <- as.character(dyn$player_id)
  nv_by_id <- stats::setNames(dyn$next_value_mean, ids)
  cur_by_id <- stats::setNames(dyn$cur_value, ids)
  pos_by_id <- if (!is.null(pos_lookup)) pos_lookup else
    stats::setNames(as.character(dyn$pos), ids)

  rel_ids <- function(x) {
    r <- future_reliability[pos_by_id[x]]
    r[is.na(r)] <- future_certainty
    r
  }
  reliable_next <- function(x) {
    x <- x[!grepl("^PICK_", x)]
    if (!length(x)) return(0)
    cu <- cur_by_id[x]; nx <- nv_by_id[x]; rl <- rel_ids(x)
    cu[is.na(cu)] <- 0; nx[is.na(nx)] <- 0
    sum(cu + rl * (nx - cu))
  }
  players_next <- function(x) {
    x <- x[!grepl("^PICK_", x)]
    sum(nv_by_id[x], na.rm = TRUE)
  }
  function(send_ids, recv_ids, future_capital_delta) {
    praw <- players_next(recv_ids) - players_next(send_ids)
    (reliable_next(recv_ids) - reliable_next(send_ids)) +
      pick_reliability * (future_capital_delta - praw)
  }
}

#' Market edge on a trade, relative to the shape-fair price
#'
#' A multi-player package must **overpay** the single better player it is traded
#' for: a premium player is worth more than a package of equal summed value, so
#' "fair" is not a zero value gap. The premium is measured against the **single**
#' side (the one with fewer pieces) - `(package - single) / single` - which is the
#' convention the legacy `uneven_gap` band used, and the only one under which
#' "the package must be worth 5% more" means what it says. Required premium is
#' `fair_premium` per extra piece.
#'
#' `my_edge > 0` means I am winning the value trade by that fraction of the
#' single side's value; `opp_edge` is its negation, and is what the other owner
#' is really deciding on. `single_value` is returned so the edge can be converted
#' back into dynasty value points.
#'
#' @param send_value,recv_value summed current market (dynasty) value each way
#' @param n_send,n_recv number of pieces each way
#' @param fair_premium required overpay per extra piece (default 0.05)
#'
#' @return a data.table with `prem`, `fair_gap`, `single_value`, `my_edge`, `opp_edge`
#'
#' @keywords internal
.ffs_market_edge <- function(send_value, recv_value, n_send, n_recv, fair_premium = 0.05) {
  # shapes may arrive as scalars (one shape for the whole batch) - recycle so
  # every branch below is elementwise
  len <- max(length(send_value), length(recv_value), length(n_send), length(n_recv))
  send_value <- rep_len(as.numeric(send_value), len)
  recv_value <- rep_len(as.numeric(recv_value), len)
  n_send <- rep_len(as.numeric(n_send), len)
  n_recv <- rep_len(as.numeric(n_recv), len)
  i_recv <- n_recv >= n_send                     # TRUE = I receive the package
  single <- data.table::fifelse(i_recv, send_value, recv_value)
  pkg    <- data.table::fifelse(i_recv, recv_value, send_value)
  single[!is.finite(single) | single <= 0] <- NA_real_
  prem <- (pkg - single) / single
  fair_gap <- fair_premium * abs(n_recv - n_send)
  # receiving the package, a bigger premium is mine to gain; sending it, it costs
  my_edge <- data.table::fifelse(i_recv, prem - fair_gap, fair_gap - prem)
  data.table::data.table(prem = prem, fair_gap = fair_gap, single_value = single,
                         my_edge = my_edge, opp_edge = -my_edge)
}

#' Score trade deals for both sides
#'
#' My side keeps the full weighted model - playoff odds plus reliability-adjusted
#' future capital at my win-now haircut, in equivalent playoff percentage points.
#'
#' The **opponent side does not**. Real owners price a deal off current market
#' value, not off my per-position dynasty-reliability slopes, and a routine deal
#' that shaves a couple of points off their playoff odds is not something they
#' refuse. So `opp_mode = "market"` (the default) scores them on the market value
#' they net, and lets their playoff odds veto only a large drop - the case where
#' a trade genuinely guts them at a position. `opp_mode = "mirror"` restores the
#' previous behaviour (their playoff delta plus the negated mirror of my
#' reliability-adjusted future at their haircut) for A/B comparison.
#'
#' @param deals a data.frame of deals. Required: `send_ids`, `recv_ids` (list
#'   columns), `send_value`, `recv_value`, `future_capital_delta`,
#'   `my_playoff_delta`, `opp_playoff_delta`. Optional: `my_champ_delta`,
#'   `traj_bad`, `win_win`, `opp_playoff_before` (needed by `opp_mode = "mirror"`).
#' @param dynasty a dynasty outlook (player_id, cur_value, next_value_mean, pos)
#' @param my_haircut my win-now haircut, from [.ffs_haircut()]
#' @param playoff_value dynasty value points per +1% playoff odds (`k_P`)
#' @param future_certainty fallback reliability for unknown positions
#' @param future_reliability named per-position reliability; `NULL` uses the
#'   format default
#' @param pick_reliability level discount on draft-pick future value
#' @param format QB format, for the `future_reliability` default
#' @param pos_lookup optional named position vector keyed by player_id, when the
#'   dynasty table lacks `pos`
#' @param fair_premium required overpay per extra piece (see [.ffs_market_edge()])
#' @param opp_edge_tol how far below the shape-fair price the opponent will still
#'   deal (0.05 = I may take up to 5% over fair)
#' @param max_opp_drop playoff-odds drop (0-1) that vetoes a deal outright
#' @param opp_mode "market" (default) or "mirror"
#' @param champ_weight,winwin_bonus,traj_weight my-side score weights
#'
#' @return `deals` with `adj_future_capital`, `prem`, `fair_gap`, `single_value`,
#'   `my_edge`, `opp_edge`, `opp_value_gain` (raw market value they net),
#'   `opp_surplus` (value above the shape-fair price, in dynasty points - what
#'   they judge on), `score`, `opp_score`, `gettable` and `grade`
#'
#' @export
ffs_deal_scores <- function(deals, dynasty,
                            my_haircut = 0.8, playoff_value = 68,
                            future_certainty = 0.905, future_reliability = NULL,
                            pick_reliability = 0.55, format = "superflex",
                            pos_lookup = NULL,
                            fair_premium = 0.05, opp_edge_tol = 0.05,
                            max_opp_drop = 0.15,
                            opp_mode = c("market", "mirror"),
                            champ_weight = 0, winwin_bonus = 0, traj_weight = 0) {
  opp_mode <- match.arg(opp_mode)
  d <- data.table::as.data.table(deals)
  if (!nrow(d)) return(d)
  checkmate::assert_names(names(d), must.include = c(
    "send_ids", "recv_ids", "send_value", "recv_value", "future_capital_delta",
    "my_playoff_delta", "opp_playoff_delta"))

  adj_future_capital <- opp_edge <- opp_playoff_delta <- opp_value_gain <-
    my_playoff_delta <- score <- opp_score <- gettable <- grade <-
    opp_surplus <- send_value <- recv_value <- send_ids <- recv_ids <-
    future_capital_delta <- opp_playoff_before <- single_value <- NULL

  adj_fn <- .ffs_adj_future_fn(dynasty, future_reliability, pick_reliability,
                               future_certainty, format, pos_lookup)
  d[, adj_future_capital := vapply(seq_len(.N), function(i)
    adj_fn(send_ids[[i]], recv_ids[[i]], future_capital_delta[i]), numeric(1))]

  edge <- .ffs_market_edge(d$send_value, d$recv_value,
                           lengths(d$send_ids), lengths(d$recv_ids), fair_premium)
  d[, names(edge) := edge]
  # raw market value they net, and - what they actually judge on - how far that
  # sits above or below the shape-fair price. A 2-for-3 hands them less summed
  # value BY DESIGN (the package overpays the stud), so raw gain would read every
  # fragment as a loss for them.
  d[, opp_value_gain := send_value - recv_value]
  d[, opp_surplus := opp_edge * single_value]

  # --- my side: the full weighted model, in equivalent playoff %
  champ <- if ("my_champ_delta" %in% names(d)) d$my_champ_delta else 0
  ww <- if ("win_win" %in% names(d)) as.numeric(d$win_win %in% TRUE) else 0
  traj <- if ("traj_bad" %in% names(d)) d$traj_bad else 0
  d[, score := 100 * my_playoff_delta +
      adj_future_capital * my_haircut / playoff_value +
      champ_weight * 100 * champ + winwin_bonus * ww - traj_weight * traj]

  # --- their side
  if (opp_mode == "market") {
    d[, opp_score := opp_surplus / playoff_value]
    d[, gettable := opp_edge >= -opp_edge_tol & opp_playoff_delta >= -max_opp_drop]
  } else {
    if (!"opp_playoff_before" %in% names(d))
      stop('opp_mode = "mirror" needs an opp_playoff_before column')
    d[, opp_score := 100 * opp_playoff_delta +
        (-adj_future_capital) * .ffs_haircut(opp_playoff_before) / playoff_value]
    d[, gettable := opp_score >= -3]
  }

  d[, grade := .ffs_grade(score)]
  d[]
}

#' Letter grade for a deal score
#'
#' @param s a score in equivalent playoff percentage points
#' @keywords internal
.ffs_grade <- function(s) {
  data.table::fcase(s >= 18, "A", s >= 10, "B", s >= 4, "C", s >= 0, "D",
                    default = "F")
}

#' Enumerate every package of the given sizes from a pool of assets
#'
#' Flattens `choose(nrow(pool), k)` combinations into one row per package with
#' its summed values pre-computed, so the caller can cross-join packages as plain
#' numeric vectors instead of looping.
#'
#' @param pool a data.table with player_id, player_name, cur_value,
#'   next_value_mean and the column named by `gain_col`
#' @param sizes package sizes to enumerate
#' @param must_include optional player_ids every package must contain
#' @param gain_col the additive win-value column to sum (`value_to_me` for the
#'   side sending, `value_to_you` for the side receiving)
#'
#' @return a data.table: `n`, `ids` (list), `label`, `value`, `next_value`,
#'   `gain`, `top`, `has_pick`
#'
#' @keywords internal
.ffs_packages <- function(pool, sizes, must_include = NULL, gain_col = "value_to_me") {
  pool <- data.table::as.data.table(pool)
  np <- nrow(pool)
  gain_v <- pool[[gain_col]]
  gain_v[is.na(gain_v)] <- 0
  is_pick <- grepl("^PICK_", pool$player_id)
  out <- list()
  for (k in sizes) {
    if (k > np || k < 1L) next
    idx <- utils::combn(np, k)                     # k x C(np, k)
    if (!is.null(must_include) && length(must_include)) {
      req <- which(pool$player_id %in% must_include)
      if (length(req) < length(must_include)) next
      ok <- apply(idx, 2L, function(cc) all(req %in% cc))
      idx <- idx[, ok, drop = FALSE]
    }
    if (!ncol(idx)) next
    cols <- split(idx, col(idx))                   # one entry per package
    out[[length(out) + 1L]] <- data.table::data.table(
      n = k,
      ids = lapply(cols, function(cc) pool$player_id[cc]),
      label = vapply(cols, function(cc) paste(pool$player_name[cc], collapse = " + "),
                     character(1)),
      value = vapply(cols, function(cc) sum(pool$cur_value[cc]), numeric(1)),
      next_value = vapply(cols, function(cc) sum(pool$next_value_mean[cc]), numeric(1)),
      gain = vapply(cols, function(cc) sum(gain_v[cc]), numeric(1)),
      top = vapply(cols, function(cc) max(gain_v[cc]), numeric(1)),
      has_pick = vapply(cols, function(cc) any(is_pick[cc]), logical(1)))
  }
  if (!length(out)) return(data.table::data.table(
    n = integer(), ids = list(), label = character(), value = numeric(),
    next_value = numeric(), gain = numeric(), top = numeric(), has_pick = logical()))
  data.table::rbindlist(out)
}

#' Vectorised value-band test for a set of candidate send/receive pairs
#'
#' `fair_band` (preferred) tests the deal's edge against the shape-fair price in
#' one symmetric band. Falling back, the legacy per-shape bands apply: a
#' symmetric `even_band` on same-count trades, and an overpay premium band on
#' uneven ones (the multi-player side must overpay the single player).
#'
#' @param send_value,recv_value vectors of summed value each way
#' @param n_send,n_recv the shape (scalars - one shape per call)
#' @param fair_premium,fair_band see [ffs_build_trades()]
#' @param even_band,uneven_gap,consolidation_penalty,uneven_shade legacy bands
#'
#' @return a logical vector, `TRUE` where the pair is inside the band
#'
#' @keywords internal
.ffs_band_ok <- function(send_value, recv_value, n_send, n_recv,
                         fair_premium = 0.05, fair_band = NULL,
                         even_band = 0.15, uneven_gap = NULL,
                         consolidation_penalty = 0, uneven_shade = even_band) {
  if (!is.null(fair_band)) {
    e <- .ffs_market_edge(send_value, recv_value, n_send, n_recv, fair_premium)
    return(!is.na(e$my_edge) & e$my_edge >= fair_band[[1]] & e$my_edge <= fair_band[[2]])
  }
  if (n_send == n_recv) return(abs(send_value - recv_value) / recv_value <= even_band)
  prem <- if (n_send > n_recv) (send_value - recv_value) / recv_value else
    (recv_value - send_value) / send_value
  if (!is.null(uneven_gap)) return(prem >= uneven_gap[[1]] & prem <= uneven_gap[[2]])
  floor_prem <- consolidation_penalty * abs(n_send - n_recv)
  prem >= floor_prem & prem <= floor_prem + uneven_shade
}
