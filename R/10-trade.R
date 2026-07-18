#' Roster-contextual player value
#'
#' (EXPERIMENTAL) Computes the marginal value of a specific player to a
#' specific franchise, in wins, using an existing simulation: the difference
#' in that franchise's simulated results with vs without the player, holding
#' every other franchise at its base-simulation lineups.
#'
#' Value is roster-contextual by construction: a QB is worth more to a
#' franchise with a weak QB room than to one with two studs, because the
#' lineups are re-optimized around the change.
#'
#' @param base_simulation an `ff_simulation` object from `ff_simulate(..., return = "all")`
#' @param player_id a player_id present in the simulation's rosters
#' @param franchise_id the franchise to value the player for (any franchise, not just the current owner)
#' @param base_summary optional precomputed base-sim summary for `franchise_id`
#'   (as returned by an internal call); pass it when valuing many players for the
#'   same franchise to avoid recomputing it each time
#' @param fast use the exact partial-reoptimisation fast path when the simulation
#'   used `lineup_method = "rank"` (default `TRUE`); `FALSE` forces the full
#'   whole-franchise re-optimisation (identical result, slower) and is mainly for
#'   validation. The full path is always used for efficiency/best-ball sims.
#'
#' @return a one-row dataframe: franchise_id, player_id, player_name, owner_id,
#'   and the deltas (h2h_wins, allplay_winpct, points_for, playoff_pct) of
#'   having the player vs not having him
#'
#' @seealso `ff_wins_added()` for league-wide leave-one-out values
#'
#' @export
ffs_player_value <- function(base_simulation, player_id, franchise_id,
                             base_summary = NULL, fast = TRUE) {
  checkmate::assert_class(base_simulation, "ff_simulation")

  pid <- player_id
  fid <- franchise_id
  projected_score <- avg_week <- pos_rank <- pos <- season <- week <- franchise_id <- NULL

  rs <- data.table::as.data.table(base_simulation$roster_scores)
  checkmate::assert_true(pid %in% rs$player_id)

  player_rows <- rs[rs$player_id == pid]
  owner_id <- player_rows$franchise_id[[1]]
  player_name <- if ("player_name" %in% names(player_rows)) player_rows$player_name[[1]] else NA_character_
  owned <- identical(owner_id, fid)

  params <- base_simulation$simulation_params
  use_fast <- isTRUE(fast) && identical(params$lineup_method, "rank") && !isTRUE(params$best_ball)

  # base (unmodified) summary for this franchise. Independent of the player, so
  # callers scanning many players for one franchise can pass it in once.
  if (is.null(base_summary)) base_summary <- .ffs_franchise_summary(base_simulation, fid)
  base_summary <- data.table::as.data.table(base_summary)

  if (use_fast) {
    # exact: reuse the base lineups, re-optimise only the weeks this player
    # actually changes, and skip the hindsight-optimal LP (unused here).
    base_opt <- data.table::as.data.table(base_simulation$optimal_scores)
    rows <- if (owned) .ffs_counterfactual_rows(base_simulation, fid, remove_ids = pid)
            else        .ffs_counterfactual_rows(base_simulation, fid, add_ids = pid)
    cf <- .ffs_summarise_optimal(base_simulation,
            rbind(base_opt[franchise_id != fid], rows, fill = TRUE), fid)
  } else {
    # full whole-franchise re-optimisation (efficiency/best-ball, or fast=FALSE)
    if (owned) {
      without_scores <- data.table::copy(rs[rs$franchise_id == fid])[
        player_id == pid, `:=`(projected_score = NA, avg_week = NA)]
      cf <- .ffs_franchise_summary(base_simulation, fid, without_scores)
    } else {
      incoming <- data.table::copy(player_rows)
      franchise_cols <- intersect(c("franchise_id", "franchise_name", "league_id"), names(incoming))
      template <- rs[rs$franchise_id == fid][1]
      for (col in franchise_cols) data.table::set(incoming, j = col, value = template[[col]])
      with_scores <- rbind(rs[rs$franchise_id == fid], incoming)
      cf <- .ffs_franchise_summary(base_simulation, fid, with_scores)
    }
    cf <- data.table::as.data.table(cf)
  }

  # owned:     value = with(base) - without(cf)
  # elsewhere: value = with(cf)   - without(base) = -(base - cf)
  s <- if (owned) 1 else -1
  out <- data.frame(
    franchise_id = fid,
    player_id = pid,
    player_name = player_name,
    owner_id = owner_id,
    h2h_wins = s * (base_summary$h2h_wins - cf$h2h_wins),
    allplay_winpct = s * (base_summary$allplay_winpct - cf$allplay_winpct),
    points_for = s * (base_summary$points_for - cf$points_for),
    playoff_pct = s * (base_summary$playoff_pct - cf$playoff_pct)
  )

  return(out)
}

#' Evaluate a proposed trade
#'
#' (EXPERIMENTAL) Swaps players between two franchises inside an existing
#' simulation, re-optimizes both franchises' lineups, and reports each side's
#' change in simulated results. Everything else in the league is held at the
#' base simulation.
#'
#' @param base_simulation an `ff_simulation` object from `ff_simulate(..., return = "all")`
#' @param franchise_a,franchise_b the two franchise_ids
#' @param gives_a player_ids franchise_a sends to franchise_b (can be empty)
#' @param gives_b player_ids franchise_b sends to franchise_a (can be empty)
#' @param fast use the exact partial-reoptimisation fast path when the simulation
#'   used `lineup_method = "rank"` (default `TRUE`); `FALSE` forces the full
#'   re-optimisation (identical result, slower). The full path is always used for
#'   efficiency/best-ball sims.
#'
#' @return a two-row dataframe (one per franchise) with before/after and delta
#'   columns for h2h_wins, allplay_winpct, points_for, playoff_pct
#'
#' @export
ffs_trade_eval <- function(base_simulation, franchise_a, gives_a, franchise_b, gives_b,
                           fast = TRUE) {
  checkmate::assert_class(base_simulation, "ff_simulation")
  franchise_id <- NULL

  rs <- data.table::as.data.table(base_simulation$roster_scores)
  checkmate::assert_true(all(gives_a %in% rs[rs$franchise_id == franchise_a]$player_id))
  checkmate::assert_true(all(gives_b %in% rs[rs$franchise_id == franchise_b]$player_id))

  params <- base_simulation$simulation_params
  use_fast <- isTRUE(fast) && identical(params$lineup_method, "rank") && !isTRUE(params$best_ball)

  if (use_fast) {
    # exact partial re-optimisation: each side re-optimises only the weeks its
    # incoming/outgoing players touch; everyone else stays at the base lineups.
    base_opt <- data.table::as.data.table(base_simulation$optimal_scores)
    a_rows <- .ffs_counterfactual_rows(base_simulation, franchise_a,
                                       remove_ids = gives_a, add_ids = gives_b)
    b_rows <- .ffs_counterfactual_rows(base_simulation, franchise_b,
                                       remove_ids = gives_b, add_ids = gives_a)
    after_opt <- rbind(
      base_opt[!franchise_id %in% c(franchise_a, franchise_b)], a_rows, b_rows, fill = TRUE)
    after <- .ffs_summarise_optimal(base_simulation, after_opt, c(franchise_a, franchise_b))
    before <- .ffs_summarise_optimal(base_simulation, base_opt, c(franchise_a, franchise_b))
  } else {
    # full whole-franchise re-optimisation (efficiency/best-ball)
    move <- function(rows, to_id) {
      rows <- data.table::copy(rows)
      template <- rs[rs$franchise_id == to_id][1]
      for (col in intersect(c("franchise_id", "franchise_name", "league_id"), names(rows))) {
        data.table::set(rows, j = col, value = template[[col]])
      }
      rows
    }
    a_new <- rbind(
      rs[rs$franchise_id == franchise_a & !rs$player_id %in% gives_a],
      move(rs[rs$player_id %in% gives_b], franchise_a))
    b_new <- rbind(
      rs[rs$franchise_id == franchise_b & !rs$player_id %in% gives_b],
      move(rs[rs$player_id %in% gives_a], franchise_b))
    after <- .ffs_franchise_summary(base_simulation, c(franchise_a, franchise_b),
                                    rbind(a_new, b_new))
    before <- .ffs_franchise_summary(base_simulation, c(franchise_a, franchise_b))
  }

  before <- data.table::as.data.table(before)[order(franchise_id)]
  after <- data.table::as.data.table(after)[order(franchise_id)]

  out <- data.frame(
    franchise_id = before$franchise_id,
    h2h_wins_before = before$h2h_wins,
    h2h_wins_after = after$h2h_wins,
    h2h_wins_delta = after$h2h_wins - before$h2h_wins,
    allplay_delta = after$allplay_winpct - before$allplay_winpct,
    points_delta = after$points_for - before$points_for,
    playoff_pct_before = before$playoff_pct,
    playoff_pct_after = after$playoff_pct,
    playoff_pct_delta = after$playoff_pct - before$playoff_pct
  )

  return(out)
}

#' Scan the league for trade fits
#'
#' (EXPERIMENTAL) Two-stage scan for a franchise: a cheap screen ranks every
#' player rostered elsewhere by how many expected points he would add to this
#' franchise's lineup (no optimization), then the shortlist is valued exactly
#' with `ffs_player_value()` for both this franchise and the player's current
#' owner. The interesting column is `surplus` - players worth more to you
#' than to the team that owns them are where trades get made.
#'
#' @param base_simulation an `ff_simulation` object from `ff_simulate(..., return = "all")`
#' @param franchise_id the acquiring franchise
#' @param top_n how many screened candidates to value exactly (default 20)
#'
#' @return a dataframe of candidates: screen proxy, value_to_you +
#'   playoff_delta_you (impact on YOUR team), value_to_owner +
#'   playoff_delta_owner (impact on the OWNER's team), and
#'   surplus = value_to_you - value_to_owner
#'
#' @export
ffs_trade_targets <- function(base_simulation, franchise_id, top_n = 20) {
  checkmate::assert_class(base_simulation, "ff_simulation")

  fid <- franchise_id
  projected_score <- player_id <- player_name <- pos <- mps <- baseline <- proxy <- NULL

  rs <- data.table::as.data.table(base_simulation$roster_scores)
  # replacement-level fillers (player_id like "WR_3") are not trade targets
  rs <- rs[!grepl("^(QB|RB|WR|TE|K)_\\d+$", rs$player_id)]

  # mean weekly points per player across the simulation
  mps_tbl <- rs[, list(
    pos = pos[[1]],
    player_name = if ("player_name" %in% names(rs)) player_name[[1]] else NA_character_,
    owner_id = franchise_id[[1]],
    mps = mean(projected_score, na.rm = TRUE)
  ), by = player_id]

  # this franchise's marginal starter level per position: the mean weekly
  # points of its median-startable player at each position
  lc <- data.table::as.data.table(base_simulation$lineup_constraints)
  mine <- mps_tbl[owner_id == fid]
  baseline_tbl <- mine[
    , list(baseline = {
      n_start <- lc[lc$pos == .BY$pos]$max
      if (length(n_start) == 0) n_start <- 1
      sorted <- sort(mps, decreasing = TRUE)
      sorted[min(length(sorted), max(1, n_start))]
    }),
    by = pos
  ]

  candidates <- merge(mps_tbl[owner_id != fid], baseline_tbl, by = "pos", all.x = TRUE)
  candidates[is.na(baseline), baseline := 0]
  candidates[, proxy := mps - baseline]
  candidates <- candidates[order(-proxy)][seq_len(min(top_n, .N))]

  # base-sim summary per franchise is player-independent - cache it so a scan of
  # many candidates does not recompute my (and each owner's) base standing.
  base_cache <- new.env(parent = emptyenv())
  base_summ <- function(f) {
    k <- as.character(f)
    v <- base_cache[[k]]
    if (is.null(v)) { v <- .ffs_franchise_summary(base_simulation, f); base_cache[[k]] <- v }
    v
  }
  exact <- data.table::rbindlist(lapply(candidates$player_id, function(p) {
    owner_f <- candidates[player_id == p]$owner_id
    to_you <- ffs_player_value(base_simulation, p, fid, base_summary = base_summ(fid))
    to_owner <- ffs_player_value(base_simulation, p, owner_f, base_summary = base_summ(owner_f))
    data.table::data.table(
      player_id = p,
      value_to_you = to_you$h2h_wins,
      playoff_delta_you = to_you$playoff_pct,
      value_to_owner = to_owner$h2h_wins,
      playoff_delta_owner = to_owner$playoff_pct,
      surplus = to_you$h2h_wins - to_owner$h2h_wins
    )
  }))

  out <- merge(candidates[, list(player_id, player_name, pos, owner_id, proxy)],
               exact, by = "player_id")[order(-surplus)]

  return(as.data.frame(out))
}

#' Construct full trade offers
#'
#' (EXPERIMENTAL) Assembles complete, plausible trades for a franchise. For
#' each helpful player rostered elsewhere it builds packages of *your* players
#' of similar current dynasty value (the fairness band), keeps the ones that
#' add more wins than they cost you on a cheap additive screen, then values the
#' survivors exactly with [ffs_trade_eval()] - re-optimizing both rosters and
#' re-ranking the whole league. The result is ranked by your playoff-odds gain
#' and flags deals that help the other side too (`win_win`).
#'
#' Value retention is priced via `future_capital_delta`: the packages you would
#' rather send are declining assets and the ones you want are appreciating, so
#' a positive value means you also come out ahead in next year's market.
#'
#' @param base_simulation an `ff_simulation` object from `ff_simulate(..., return = "all")`
#' @param franchise_id the acquiring franchise
#' @param targets optional precomputed [ffs_trade_targets()] output; computed if `NULL`
#' @param dynasty optional precomputed [ffs_dynasty_outlook()] output; computed if `NULL`
#' @param value_band for **even** trades (same player count each side), the max
#'   allowed `|send - receive|` as a fraction of the received dynasty value
#'   (default 0.15 = within 15pct of current market value)
#' @param uneven_shade the *width* of the overpay band above the
#'   `consolidation_penalty` floor for **uneven** trades - the package (multi-
#'   player) side must overpay the single side by `consolidation_penalty *
#'   extra_players` to that `+ uneven_shade` of the single side's value
#'   (default = `value_band`)
#' @param consolidation_penalty per-*extra*-player premium the multi-player
#'   (package) side must overpay the single player in an **uneven** trade,
#'   because a premium player is worth more than a package of equal summed
#'   market value (roster spots are scarce and studs win). Applies both ways: to
#'   consolidate (send a package for one better player) your package must be
#'   worth this much more than the target; to fragment (give up one player for a
#'   package) the package must be worth this much more than him. `0` (default) =
#'   the package need only match the single side; e.g. `0.05` = package must be
#'   5-15pct more than the single side (with `uneven_shade = 0.10`)
#' @param max_opp_drop drop any deal that cuts the *other* side's playoff odds by
#'   more than this (default `Inf` = no filter; e.g. `0.20` refuses deals no
#'   rational opponent would accept, like giving up their only startable QB)
#' @param winwin_bonus ranking bump (in `score` std-dev units) added to deals
#'   that help both sides (`win_win`) - a soft lean toward mutually beneficial
#'   deals rather than a hard filter (default `0`)
#' @param uneven_require_winwin optional hard filter: for uneven trades only,
#'   drop deals that are not `win_win` (default `FALSE`). Prefer `winwin_bonus`
#'   for a soft lean; this is the strict version.
#' @param shapes list of `c(n_send, n_receive)` package sizes to enumerate
#'   (default 1-for-1, 2-for-1, 1-for-2)
#' @param future_weight how heavily to weigh keeping/gaining future dynasty value
#'   against win-now gain, in standard-deviation units: the screen and final
#'   ranking use `z(win) + future_weight * z(future_capital_delta)`. `0` (default)
#'   is pure win-now; `1` weighs future value equally; `2`+ favours retention.
#' @param min_future_delta hard floor on `future_capital_delta` - drop any deal
#'   that bleeds more future value than this (default `-Inf` = no floor; e.g.
#'   `-500` refuses deals losing more than 500 units of next-year market value)
#' @param opponents optional franchise_ids: only build deals with these
#'   franchises (default `NULL` = anyone). Useful to target the teams a specific
#'   player is worth the most to.
#' @param must_send optional player_ids that every send package must include -
#'   the "how do I sell THIS player" mode (default `NULL` = no constraint).
#'   Shapes smaller than `length(must_send)` produce no packages.
#' @param screen_n how many screened packages to value exactly (default 40)
#' @param top_n how many ranked deals to return (default 20)
#'
#' @return a dataframe of trades: `opponent`, `send`, `receive`, dynasty
#'   `send_value`/`recv_value`/`value_gap`, re-optimized `my_win_delta`,
#'   `my_playoff_delta`, `opp_win_delta`, `opp_playoff_delta`, a `win_win` flag,
#'   `future_capital_delta`, the `score` used to rank them, and list-columns
#'   `send_ids`/`recv_ids` so a deal can be re-evaluated (e.g. confirmed on a
#'   larger simulation with [ffs_trade_eval()])
#'
#' @seealso [ffs_trade_targets()] for the one-sided target scan, [ffs_trade_eval()]
#'   to score a specific proposed trade
#'
#' @export
ffs_build_trades <- function(base_simulation, franchise_id,
                             targets = NULL, dynasty = NULL,
                             value_band = 0.15, uneven_shade = value_band,
                             consolidation_penalty = 0, max_opp_drop = Inf,
                             winwin_bonus = 0, uneven_require_winwin = FALSE,
                             shapes = list(c(1, 1), c(2, 1), c(1, 2)),
                             future_weight = 0, min_future_delta = -Inf,
                             opponents = NULL, must_send = NULL,
                             screen_n = 40L, top_n = 20L) {
  checkmate::assert_class(base_simulation, "ff_simulation")

  fid <- franchise_id
  player_id <- franchise_id <- player_name <- pos <- value_to_you <- owner_id <-
    cur_value <- next_value_mean <- value_to_me <- add_gain <- my_win_delta <-
    my_playoff_delta <- opp_win_delta <- opp_playoff_delta <- h2h_wins_delta <-
    playoff_pct_delta <- future_capital_delta <- screen <- score <- NULL

  # standard-deviation scaling that is safe when the column is constant/singleton
  z <- function(x) {
    s <- stats::sd(x)
    if (length(x) < 2 || is.na(s) || s == 0) return(rep(0, length(x)))
    (x - mean(x)) / s
  }

  rs <- data.table::as.data.table(base_simulation$roster_scores)
  checkmate::assert_true(fid %in% rs$franchise_id)

  if (is.null(targets)) targets <- ffs_trade_targets(base_simulation, fid, top_n = 50)
  targets <- data.table::as.data.table(targets)
  if (is.null(dynasty)) dynasty <- ffs_dynasty_outlook(base_simulation)
  dyn <- data.table::as.data.table(dynasty)[, list(player_id, cur_value, next_value_mean)]

  empty <- data.frame(
    opponent = character(), send = character(), receive = character(),
    send_value = numeric(), recv_value = numeric(), value_gap = numeric(),
    my_win_delta = numeric(), my_playoff_delta = numeric(),
    opp_win_delta = numeric(), opp_playoff_delta = numeric(),
    win_win = logical(), future_capital_delta = numeric(), score = numeric(),
    send_ids = I(list()), recv_ids = I(list())
  )

  # incoming candidates: help me, are priced, owned elsewhere
  inc <- merge(targets[value_to_you > 0 & owner_id != fid], dyn, by = "player_id")
  inc <- inc[!is.na(cur_value) & cur_value > 0]
  if (!is.null(opponents)) inc <- inc[owner_id %in% opponents]

  # my tradeable roster: name/pos + dynasty value + win value to me (valued once)
  mine <- unique(rs[franchise_id == fid & !grepl("^(QB|RB|WR|TE|K)_\\d+$", player_id),
                    list(player_id, player_name, pos)])
  mine <- merge(mine, dyn, by = "player_id")
  mine <- mine[!is.na(cur_value) & cur_value > 0]
  if (!is.null(must_send)) {
    missing <- setdiff(must_send, mine$player_id)
    if (length(missing)) {
      cli::cli_abort("must_send player(s) not on franchise {fid}'s priced roster: {missing}")
    }
  }
  if (nrow(inc) == 0 || nrow(mine) == 0) return(empty)
  base_fid_summary <- .ffs_franchise_summary(base_simulation, fid)
  mine[, value_to_me := vapply(player_id, function(p)
    ffs_player_value(base_simulation, p, fid, base_summary = base_fid_summary)$h2h_wins, numeric(1))]

  send_sizes <- unique(vapply(shapes, function(s) as.integer(s[[1]]), integer(1)))
  recv_sizes <- unique(vapply(shapes, function(s) as.integer(s[[2]]), integer(1)))
  allowed <- function(ns, nr) any(vapply(shapes,
    function(s) s[[1]] == ns && s[[2]] == nr, logical(1)))

  # my send packages (independent of opponent)
  send_pkgs <- list()
  for (ns in send_sizes) {
    if (ns > nrow(mine)) next
    idx <- utils::combn(nrow(mine), ns)
    for (j in seq_len(ncol(idx))) {
      r <- mine[idx[, j]]
      if (!is.null(must_send) && !all(must_send %in% r$player_id)) next
      send_pkgs[[length(send_pkgs) + 1L]] <- list(
        n = ns, ids = r$player_id, names = paste(r$player_name, collapse = " + "),
        value = sum(r$cur_value), next_value = sum(r$next_value_mean),
        cost_me = sum(r$value_to_me))
    }
  }

  # enumerate opponent receive packages, match to send packages, additive screen
  deals <- list()
  for (o in unique(inc$owner_id)) {
    op <- inc[owner_id == o]
    for (nr in recv_sizes) {
      if (nr > nrow(op)) next
      idxr <- utils::combn(nrow(op), nr)
      for (jr in seq_len(ncol(idxr))) {
        rr <- op[idxr[, jr]]
        recv_val <- sum(rr$cur_value)
        recv_gain <- sum(rr$value_to_you)
        recv_next <- sum(rr$next_value_mean)
        for (sp in send_pkgs) {
          if (!allowed(sp$n, nr)) next
          # value matching. even trades use a symmetric band. UNEVEN trades: the
          # multi-player (package) side must OVERPAY the single player, because a
          # premium player is worth more than a package of equal summed value -
          # you can't buy a stud with a cheaper package, and you shouldn't give
          # up a stud for a package worth less. Required overpay is
          # consolidation_penalty * extra .. + uneven_shade of the single side's
          # value, whether I send the package (consolidate) or receive it
          # (fragment).
          if (sp$n == nr) {
            if (abs(sp$value - recv_val) / recv_val > value_band) next
          } else {
            floor_prem <- consolidation_penalty * abs(sp$n - nr)
            prem <- if (sp$n > nr) {
              (sp$value - recv_val) / recv_val   # I send the package -> it overpays the target
            } else {
              (recv_val - sp$value) / sp$value   # I receive the package -> it overpays my player
            }
            if (prem < floor_prem || prem > floor_prem + uneven_shade) next
          }
          gain <- recv_gain - sp$cost_me
          if (gain <= 0) next
          deals[[length(deals) + 1L]] <- data.table::data.table(
            opponent = o, send_ids = list(sp$ids), send = sp$names,
            recv_ids = list(rr$player_id), receive = paste(rr$player_name, collapse = " + "),
            send_value = sp$value, recv_value = recv_val, value_gap = recv_val - sp$value,
            add_gain = gain, future_capital_delta = recv_next - sp$next_value)
        }
      }
    }
  }
  if (length(deals) == 0) return(empty)
  deals <- data.table::rbindlist(deals)

  # hard floor: refuse deals that sacrifice too much future value
  deals <- deals[future_capital_delta >= min_future_delta]
  if (nrow(deals) == 0) return(empty)

  # screen down to the exact-eval budget, blending win gain with retention so
  # future-friendly deals are not pruned before they are valued
  deals[, screen := z(add_gain) + future_weight * z(future_capital_delta)]
  data.table::setorder(deals, -screen)
  deals <- deals[seq_len(min(screen_n, .N))]

  ev <- data.table::rbindlist(lapply(seq_len(nrow(deals)), function(i) {
    te <- data.table::as.data.table(ffs_trade_eval(
      base_simulation, fid, deals$send_ids[[i]], deals$opponent[[i]], deals$recv_ids[[i]]))
    m <- te[franchise_id == fid]
    op <- te[franchise_id == deals$opponent[[i]]]
    data.table::data.table(
      my_win_delta = m$h2h_wins_delta, my_playoff_delta = m$playoff_pct_delta,
      opp_win_delta = op$h2h_wins_delta, opp_playoff_delta = op$playoff_pct_delta)
  }))
  deals <- cbind(deals, ev)
  deals[, win_win := my_win_delta > 0 & opp_win_delta > 0]

  # acceptability: no rational owner takes a deal that tanks their own playoff
  # odds (e.g. surrendering their only startable QB), however much it helps me
  deals <- deals[opp_playoff_delta >= -max_opp_drop]
  if (nrow(deals) == 0) return(empty)

  # consolidation realism: an uneven trade forces one side to fragment its
  # single best player into a package, which it only does if it also gains -
  # so keep lopsided (non-win_win) uneven deals out
  if (isTRUE(uneven_require_winwin)) {
    deals <- deals[lengths(send_ids) == lengths(recv_ids) | win_win == TRUE]
    if (nrow(deals) == 0) return(empty)
  }

  # final ranking: win-now gain traded off against future value by future_weight,
  # with a soft lean toward deals that help both sides
  deals[, score := z(my_playoff_delta) + future_weight * z(future_capital_delta) +
          winwin_bonus * as.numeric(win_win %in% TRUE)]
  data.table::setorder(deals, -score, -my_win_delta)

  out <- deals[seq_len(min(top_n, .N)), list(
    opponent, send, receive, send_value, recv_value, value_gap,
    my_win_delta, my_playoff_delta, opp_win_delta, opp_playoff_delta,
    win_win, future_capital_delta, score, send_ids, recv_ids)]
  return(as.data.frame(out))
}

#' Re-optimize franchise(s) inside a base simulation and summarise
#'
#' Replaces the given franchises' lineups (optionally with modified roster
#' scores), keeps everyone else at their base-simulation optimal scores, and
#' returns per-franchise season aggregates including playoff rate (top-6 by
#' h2h wins within each simulated season).
#'
#' @param base_simulation an `ff_simulation` from return = "all"
#' @param franchise_id franchise(s) of interest
#' @param franchise_scores optional modified roster-scores rows for those franchises;
#'   omit to summarise the base simulation itself
#'
#' @keywords internal
.ffs_franchise_summary <- function(base_simulation, franchise_id, franchise_scores = NULL) {
  fids <- franchise_id
  pos_rank <- projected_score <- pos <- season <- week <- h2h_wins <- lg_rank <- NULL

  if (is.null(franchise_scores)) {
    optimal <- data.table::as.data.table(base_simulation$optimal_scores)
  } else {
    franchise_scores <- data.table::as.data.table(franchise_scores)
    # pos_rank must reflect the modified roster (used by the optimiser's trim)
    franchise_scores[
      order(-projected_score),
      pos_rank := seq_len(.N),
      by = c("league_id", "franchise_id", "pos", "season", "week")
    ]
    params <- base_simulation$simulation_params
    reopt <- ffs_optimise_lineups(
      roster_scores = franchise_scores,
      lineup_constraints = base_simulation$lineup_constraints,
      best_ball = params$best_ball,
      pos_filter = params$pos_filter[[1]],
      lineup_method = params$lineup_method %||% "efficiency",
      lineup_noise_sd = params$lineup_noise_sd %||% 0
    )
    optimal <- rbind(
      data.table::as.data.table(base_simulation$optimal_scores)[!franchise_id %in% fids],
      reopt, fill = TRUE
    )
  }

  return(.ffs_summarise_optimal(base_simulation, optimal, fids))
}

#' Summarise franchise season aggregates from an optimal_scores table
#'
#' Shared tail of the franchise-summary machinery: weekly -> season aggregates
#' plus a deterministic playoff seeding, filtered to the franchises of interest.
#'
#' @param base_simulation an `ff_simulation` (used for its schedules)
#' @param optimal an optimal_scores data.table (league-wide)
#' @param fids franchise_id(s) to return
#'
#' @keywords internal
.ffs_summarise_optimal <- function(base_simulation, optimal, fids) {
  season <- h2h_wins <- points_for <- lg_rank <- h2h_winpct <- allplay_winpct <- NULL

  sw <- ffs_summarise_week(optimal_scores = optimal, schedules = base_simulation$schedules)
  ss <- data.table::as.data.table(ffs_summarise_season(summary_week = sw))
  # playoff seeding: wins, then points-for (how real leagues break ties).
  # Deterministic on purpose - with random tie-breaks, paired comparisons
  # ("with player" vs "without") re-roll the ties and a player who never even
  # starts can show a phantom playoff delta of several percent.
  ss[, lg_rank := data.table::frank(list(-h2h_wins, -points_for), ties.method = "first"),
     by = season]

  ss[franchise_id %in% fids, list(
    h2h_wins = mean(h2h_wins),
    h2h_winpct = mean(h2h_winpct),
    allplay_winpct = mean(allplay_winpct),
    points_for = mean(points_for),
    playoff_pct = mean(lg_rank <= 6)
  ), by = franchise_id]
}

#' Started-lineup-only optimiser (skips the hindsight-optimal LP)
#'
#' For valuation we only need each franchise-week's realised `actual_score` (the
#' lineup a manager sets from weekly rankings, scored on realised points) - not
#' the hindsight `optimal_score`. Computing only the started lineup halves the LP
#' work versus [ffs_optimise_lineups()] with `lineup_method = "rank"`.
#'
#' @param roster_scores modified roster-scores rows (any set of franchise-weeks)
#' @param lineup_constraints the simulation's lineup constraints
#'
#' @return one row per franchise-week: actual_score and starter_player_id
#'
#' @keywords internal
.ffs_optimise_started <- function(roster_scores, lineup_constraints) {
  pos <- projected_score <- pos_rank <- exp_rank <- avg_week <- posmax <- NULL
  lc <- data.table::as.data.table(lineup_constraints)
  DT <- data.table::as.data.table(roster_scores)[pos %in% lc$pos]
  # recompute ranks on the (modified) roster and trim to plausible starters
  # EXACTLY as .ffs_franchise_summary + ffs_optimise_lineups do, so the candidate
  # set - and any resulting LP tie-breaks - match the full optimiser bit for bit
  pm <- data.table::setnames(lc[, c("pos", "max")], "max", "posmax")
  DT <- merge(DT, pm, by = "pos")
  DT[order(-projected_score), pos_rank := seq_len(.N),
     by = c("league_id", "franchise_id", "pos", "season", "week")]
  DT[, exp_rank := data.table::frank(-data.table::fcoalesce(as.numeric(avg_week), 0),
       ties.method = "first"),
     by = c("league_id", "franchise_id", "pos", "season", "week")]
  DT <- DT[pos_rank <= posmax | exp_rank <= posmax]
  DT[, .ff_rank_one_lineup(.SD, lc, noise_sd = 0),
     by = c("league_id", "franchise_id", "franchise_name", "season", "week"),
     .SDcols = c("player_id", "pos", "projected_score", "avg_week")]
}

#' Exact counterfactual franchise optimal-scores rows for a set of add/remove moves
#'
#' The base simulation's lineups already stand for every franchise-week. Removing
#' players only changes weeks where one of them was actually started; adding
#' players only changes weeks where one would crack the lineup (clears the started
#' cut-line, or the base lineup had an unfilled slot that week, e.g. a bye).
#' Removing a started player frees a slot an added player may fill, but such weeks
#' are already flagged by the removal. This re-optimises only the affected weeks
#' with [.ffs_optimise_started()] (no hindsight LP), reuses the base
#' `actual_score` everywhere else, and returns the franchise's modified
#' optimal_scores rows. Identical to a full re-optimisation; only faster. Assumes
#' `lineup_method = "rank"` (guarded by callers). Shared by [ffs_player_value()]
#' (one add or one remove) and [ffs_trade_eval()] (a package each way).
#'
#' @param base_simulation an `ff_simulation` from `return = "all"`
#' @param fid the franchise whose roster changes
#' @param remove_ids player_ids leaving `fid` (can be empty)
#' @param add_ids player_ids joining `fid` (can be empty)
#'
#' @return `fid`'s optimal_scores rows with `actual_score` updated in the affected
#'   weeks (other columns carry base values)
#'
#' @keywords internal
.ffs_counterfactual_rows <- function(base_simulation, fid,
                                     remove_ids = character(0), add_ids = character(0)) {
  season <- week <- franchise_id <- player_id <- avg_week <- starter_player_id <-
    sp <- cutline <- nreal <- p_avg <- new_actual <- actual_score <- NULL

  base_opt <- data.table::as.data.table(base_simulation$optimal_scores)
  rs <- data.table::as.data.table(base_simulation$roster_scores)
  lc <- data.table::as.data.table(base_simulation$lineup_constraints)
  n_slots <- min(lc$offense_starters[[1]], lc$total_starters[[1]])
  me_opt <- base_opt[franchise_id == fid]
  starters <- me_opt[, list(sp = unlist(starter_player_id)), by = list(season, week)]

  # weeks a removed player was actually started (removal changes only these; the
  # freed slot an add may fill is in this set already)
  aff <- if (length(remove_ids))
    unique(starters[sp %in% remove_ids, list(season, week)]) else me_opt[0, list(season, week)]

  # weeks an added player would crack the base lineup: p_avg >= the started
  # cut-line (weakest started expected-points, unranked = 0), OR an unfilled slot
  # existed that week (nreal < n_slots, bye/short roster). A safe superset.
  if (length(add_ids)) {
    st <- merge(starters,
                rs[franchise_id == fid, list(season, week, sp = player_id, avg_week)],
                by = c("season", "week", "sp"))
    agg <- st[, list(cutline = min(data.table::fcoalesce(as.numeric(avg_week), 0)),
                     nreal = .N), by = list(season, week)]
    padd <- rs[player_id %in% add_ids,
               list(season, week, p_avg = data.table::fcoalesce(as.numeric(avg_week), 0))]
    a <- merge(padd, agg, by = c("season", "week"), all.x = TRUE)
    aff <- unique(rbind(aff,
      a[is.na(cutline) | nreal < n_slots | p_avg >= cutline, list(season, week)]))
  }

  # modified roster: fid minus removes plus adds (adds re-tagged to fid)
  mod_rs <- rs[franchise_id == fid & !player_id %in% remove_ids]
  if (length(add_ids)) {
    inc <- data.table::copy(rs[player_id %in% add_ids])
    tmpl <- me_opt[1]
    for (cc in intersect(c("franchise_id", "franchise_name", "league_id"), names(inc)))
      data.table::set(inc, j = cc, value = tmpl[[cc]])
    mod_rs <- rbind(mod_rs, inc)
  }

  new_me <- data.table::copy(me_opt)
  if (nrow(aff)) {
    aff_rs <- merge(mod_rs, aff, by = c("season", "week"))
    ns <- .ffs_optimise_started(aff_rs, base_simulation$lineup_constraints)
    new_me <- merge(new_me, ns[, list(season, week, new_actual = actual_score)],
                    by = c("season", "week"), all.x = TRUE)
    new_me[!is.na(new_actual), actual_score := new_actual][, new_actual := NULL]
  }
  new_me
}