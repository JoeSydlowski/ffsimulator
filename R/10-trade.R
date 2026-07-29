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
    playoff_pct = s * (base_summary$playoff_pct - cf$playoff_pct),
    # report-only championship delta alongside the berth delta (see
    # .ffs_champion_pct); NA-safe for any caller passing a pre-champion summary
    champion_pct = s * (.nz(base_summary$champion_pct) - .nz(cf$champion_pct))
  )

  return(out)
}

# NULL/absent-safe numeric accessor for optional summary columns
.nz <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) 0 else x

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
  # draft picks (id like "PICK_...") are value-only assets: they score no wins,
  # so they are win-neutral in the simulation. Drop them here - their dynasty
  # value is accounted for by the caller (future_capital_delta) - and keep the
  # assertion for any genuinely unknown player id.
  gives_a <- gives_a[!grepl("^PICK_", gives_a)]
  gives_b <- gives_b[!grepl("^PICK_", gives_b)]
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
    playoff_pct_delta = after$playoff_pct - before$playoff_pct,
    champion_pct_before = before$champion_pct,
    champion_pct_after = after$champion_pct,
    champion_pct_delta = after$champion_pct - before$champion_pct
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
      champ_delta_you = to_you$champion_pct,
      value_to_owner = to_owner$h2h_wins,
      playoff_delta_owner = to_owner$playoff_pct,
      champ_delta_owner = to_owner$champion_pct,
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
#' @param future_weight LEGACY (`score_mode = "zscore"` only) how heavily to weigh
#'   future dynasty value against win-now gain in standard-deviation units:
#'   `z(win) + future_weight * z(future_capital_delta)`. Inert in the default
#'   `"rate"` mode, where the fixed exchange rate governs the trade-off instead.
#' @param score_mode `"rate"` (default) scores deals in a deal-set-INDEPENDENT
#'   common currency of equivalent playoff %:
#'   `100*playoff_delta + adj_future_capital * my_haircut / playoff_value + ...`,
#'   where `adj_future_capital` is the reliability-adjusted future value and
#'   `my_haircut` the smooth win-now discount. `"zscore"` reproduces the legacy
#'   z-scored blend (weights in per-deal-set SD units, so the effective exchange
#'   rate silently drifts with the deal spread).
#' @param playoff_value value points per +1% playoff (`k_P`, default 68 from
#'   `dev/suite/exchange_rate_study.R`: win-now price line over both leagues).
#' @param future_certainty blended future-value reliability used only for the
#'   pre-eval screen and as the fallback for unknown positions (default 0.905).
#'   Final scoring uses per-position `future_reliability` instead.
#' @param win_to_playoff +playoff percentage points per +1 h2h win (default 17.8),
#'   used to convert the pre-eval screen's win proxy into playoff-% currency.
#' @param future_reliability named vector of per-position future-value reliability
#'   (fraction of a projected future move that realizes; `NULL` (default)
#'   auto-selects by league format - superflex QB 0.25 / RB 0.94 / WR 0.76 /
#'   TE 0.42). Applied per-deal to the actual players moving, so acquiring
#'   unreliable-future QBs is discounted vs reliable RB/WR future.
#' @param pick_reliability future-value reliability for draft picks (default 0.55).
#' @param haircut_intercept,haircut_floor,haircut_cap smooth win-now haircut
#'   `clamp(haircut_intercept - baseline_playoff, floor, cap)` (default 1.30 /
#'   0.60 / 1.00): a contender (high playoff odds) discounts future most. The
#'   opponent's own haircut (their baseline) drives the `gettable` flag.
#' @param gettable_cut `gettable = opp_score >= gettable_cut` (default -3): the
#'   opponent's OWN score of the deal (their playoff delta + their mirror future at
#'   their haircut); >= this means the other side plausibly accepts.
#' @param min_future_delta hard floor on `future_capital_delta` - drop any deal
#'   that bleeds more future value than this (default `-Inf` = no floor; e.g.
#'   `-500` refuses deals losing more than 500 units of next-year market value)
#' @param opponents optional franchise_ids: only build deals with these
#'   franchises (default `NULL` = anyone). Useful to target the teams a specific
#'   player is worth the most to.
#' @param must_send optional player_ids that every send package must include -
#'   the "how do I sell THIS player" mode (default `NULL` = no constraint).
#'   Shapes smaller than `length(must_send)` produce no packages.
#' @param even_band gap band for **even** trades expressed on the received value,
#'   `|send - receive| / receive <= even_band` (default = `value_band`, so
#'   existing callers are unchanged; set e.g. `0.03` for a tight 3pct even band)
#' @param uneven_gap optional `c(lo, hi)` gap band for **uneven** trades, applied
#'   to the package's overpay premium over the single side (both directions), e.g.
#'   `c(0.02, 0.08)` = a 2-8pct consolidation premium. When `NULL` (default) the
#'   legacy `consolidation_penalty` / `uneven_shade` band is used instead.
#' @param max_gap uniform belt-and-suspenders ceiling on `|value_gap| / recv_value`
#'   applied to every deal after enumeration (default `Inf` = off)
#' @param min_piece_value minimum `cur_value` a **matched** real player you SEND
#'   must have - filters throw-in filler so the second send piece is a real player
#'   (default `0` = no filter). Picks, `must_send` players, and give-back pieces
#'   are exempt.
#' @param min_recv_value minimum `cur_value` a **matched** real player you RECEIVE
#'   must have (default `= min_piece_value`). Set lower than `min_piece_value` for
#'   fragment shapes (e.g. 1-for-3) so a stud can be split into several real but
#'   smaller pieces without the filler floor blocking the smallest one. Picks are
#'   exempt.
#' @param giveback when `TRUE`, for any deal that lowers the opponent's playoff
#'   odds below `giveback_trigger` a variant is also generated that appends one of
#'   your spare players at the position of the top received player to the send
#'   side (a "send Daniel Jones back" sweetener). The give-back is picked to be
#'   *useful to the depleted opponent*, not merely your cheapest: the exact eval
#'   scores the `giveback_try` cheapest candidates at that position and keeps the
#'   one that lifts the opponent's playoff odds the most. It is exempt from the
#'   value band, so the variant shows a softer `opp_playoff_delta` (default `FALSE`)
#' @param giveback_trigger only add a give-back variant when the base deal's
#'   `opp_playoff_delta` is below this (default `0` = whenever the opponent is hurt)
#' @param giveback_try how many of your most-useful spare players at the drained
#'   position to evaluate as the give-back. Candidates are ranked by usefulness to
#'   the *opponent* (mean weekly points), the top `giveback_try` are valued, and the
#'   one with the best opponent playoff-lift *per unit of value you give up* is kept
#'   (default `5`). If none lifts the opponent, no give-back variant is added.
#' @param giveback_max_value optional ceiling on a give-back piece's `cur_value`, so
#'   a valuable player is never used as a mere sweetener (default `Inf` = no cap)
#' @param traj_weight soft down-rank (in `score` std-dev units) applied to deals
#'   that ship your *appreciating* players or acquire *declining* ones, when the
#'   `dynasty` table carries a `rel_change` column (value move relative to the
#'   position's drift). `0` (default) = trajectory-blind, back-compatible. The
#'   penalty is `Sigma_sent max(0, rel_change - rise_cut) + Sigma_recv max(0,
#'   fade_cut - rel_change)` over real (non-pick) pieces; a sent riser or received
#'   decliner is pushed down but never removed.
#' @param rise_cut,fade_cut the `rel_change` thresholds above/below which a piece
#'   counts as appreciating / declining for `traj_weight` (defaults `0.075` /
#'   `-0.075`, matching the trade-intel role cutoffs)
#' @param posture_rebuild_cut opponents with baseline playoff odds below this are
#'   treated as rebuilders and gated on future value instead of playoff odds
#'   (default `0.35`)
#' @param max_opp_future_drop for **rebuilder** opponents, drop deals that cut
#'   their future dynasty capital (`-future_capital_delta`) by more than this
#'   (default `Inf` = rebuilders accept any playoff hit for future value)
#' @param dedupe when `TRUE`, collapse deals that differ only by throw-in filler
#'   or draft-pick *year* (same pick slot) to the single highest-scoring
#'   representative, so near-identical ideas do not flood the board (default `FALSE`)
#' @param screen_n how many screened packages to value exactly (default 40)
#' @param screen_per_opp guarantee at least this many of each opponent's
#'   best-screened packages are valued, so a single team's high-gain deals cannot
#'   crowd every other team out of the exact-eval budget (default `0` = pure global
#'   top-`screen_n`). The eval set becomes the union of the global top-`screen_n`
#'   and each opponent's top-`screen_per_opp`, giving every team coverage.
#' @param top_n how many ranked deals to return (default 20)
#'
#' @return a dataframe of trades: `opponent`, `send`, `receive`, dynasty
#'   `send_value`/`recv_value`/`value_gap`, re-optimized `my_win_delta`,
#'   `my_playoff_delta`, `opp_win_delta`, `opp_playoff_delta`, a `win_win` flag,
#'   `future_capital_delta`, a `give_back` flag (whether a give-back sweetener was
#'   appended), the `score` used to rank them, and list-columns
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
                             champ_weight = 0, ceiling_weight = 0,
                             score_mode = c("rate", "zscore"),
                             playoff_value = 68, future_certainty = 0.905,
                             win_to_playoff = 17.8,
                             future_reliability = NULL, pick_reliability = 0.55,
                             haircut_intercept = 1.30, haircut_floor = 0.60, haircut_cap = 1.00,
                             gettable_cut = -3,
                             opponents = NULL, must_send = NULL, picks = NULL,
                             even_band = value_band, uneven_gap = NULL,
                             max_gap = Inf, min_piece_value = 0,
                             min_recv_value = min_piece_value,
                             giveback = FALSE, giveback_trigger = 0, giveback_try = 5L,
                             giveback_max_value = Inf,
                             traj_weight = 0, rise_cut = 0.075, fade_cut = -0.075,
                             posture_rebuild_cut = 0.35, max_opp_future_drop = Inf,
                             dedupe = FALSE,
                             screen_n = 40L, screen_per_opp = 0L, top_n = 20L) {
  checkmate::assert_class(base_simulation, "ff_simulation")

  fid <- franchise_id
  player_id <- franchise_id <- player_name <- pos <- value_to_you <- owner_id <-
    cur_value <- next_value_mean <- value_to_me <- add_gain <- my_win_delta <-
    my_playoff_delta <- opp_win_delta <- opp_playoff_delta <- h2h_wins_delta <-
    playoff_pct_delta <- future_capital_delta <- screen <- score <- give_back <- NULL
  send_top <- recv_top <- my_champ_delta <- opp_champ_delta <- champion_pct_delta <- NULL
  opp_base <- dk <- value_gap <- recv_value <- win_win <- send <- receive <- NULL
  rel_change <- mps <- traj_bad <- projected_score <- opp_rank <- has_pick <- pick_rank <- NULL
  adj_future_capital <- opp_score <- gettable <- opp_playoff_before <- NULL

  # standard-deviation scaling that is safe when the column is constant/singleton
  z <- function(x) {
    s <- stats::sd(x)
    if (length(x) < 2 || is.na(s) || s == 0) return(rep(0, length(x)))
    (x - mean(x)) / s
  }

  rs <- data.table::as.data.table(base_simulation$roster_scores)
  checkmate::assert_true(fid %in% rs$franchise_id)

  # --- fixed-rate scoring config (see dev/suite/exchange_rate_study.R) ---------
  # "rate" mode scores in EQUIVALENT PLAYOFF % on raw points:
  #   score = 100*playoff_delta + future_capital_delta/future_rate + ...
  # future_rate (R_eff) = future-value pts per +1% playoff = playoff_value /
  # (future_certainty * risk[posture]) - a deal-set-INDEPENDENT exchange rate,
  # unlike the legacy z-scored blend. posture (contend/bubble/rebuild) sets the
  # win-now time-preference haircut; auto-derived from fid's baseline playoff odds
  # if not supplied. "zscore" mode reproduces the pre-2026-07 blend.
  score_mode <- match.arg(score_mode)
  # smooth win-now haircut from baseline playoff odds (a contender values future
  # least): haircut = clamp(haircut_intercept - playoff, floor, cap) - ~0.82 at a
  # 48% bubble, ~0.65 near a 68% contender, 1.00 for a deep rebuilder.
  base_pl <- tryCatch(.ffs_franchise_summary(base_simulation, fid)[["playoff_pct"]][1],
                      error = function(e) NA_real_)
  if (is.na(base_pl)) base_pl <- 0.5
  hc <- function(p) pmin(pmax(haircut_intercept - p, haircut_floor), haircut_cap)
  my_haircut <- hc(base_pl)
  # per-position future-value reliability (auto by format unless supplied): the
  # value-space calibration slope lm(actual_move ~ predicted_move) from
  # dev/suite/dynasty_calibration.R, capped into [0, 1] the same way
  # exchange_rate_study.R derives `future_certainty`. A projected future gain in
  # QBs (~0.55 realized in superflex) is worth far less than one in WRs (~1.0).
  # Picks -> pick_reliability; unknown pos -> future_certainty. Applied per-deal
  # at final scoring; the coarse pre-eval screen uses the blend below.
  #
  # REFIT 2026-07-28 against the log-space transition model (commit 1bc4128). The
  # previous set was fit to the pre-log-space model and became badly stale when
  # that landed - superflex TE 0.423 -> 0.835, WR 0.761 -> 1.046 (capped 1.0) -
  # which systematically over-credited future value in any deal that swapped a WR
  # for a TE. Raw slopes: 1qb QB .651 RB .679 TE 1.362 WR 1.218;
  # superflex QB .551 RB 1.105 TE .835 WR 1.046. Values above 1 are within ~1 se
  # of 1 (except 1qb TE) and would AMPLIFY projected moves, so they are capped.
  if (is.null(future_reliability)) {
    fmt <- tryCatch(as.character(.ffs_detect_qb_format(base_simulation$lineup_constraints)),
                    error = function(e) "superflex")
    future_reliability <- if (identical(fmt, "1qb"))
      c(QB = 0.651, RB = 0.679, TE = 1.000, WR = 1.000) else
      c(QB = 0.551, RB = 1.000, TE = 0.835, WR = 1.000)
  }
  # blended-reliability rate for the screen only (smoothed haircut folded in)
  future_rate <- playoff_value / (future_certainty * my_haircut)
  if (!is.finite(future_rate) || future_rate <= 0) future_rate <- 130

  if (is.null(targets)) targets <- ffs_trade_targets(base_simulation, fid, top_n = 50)
  targets <- data.table::as.data.table(targets)
  if (is.null(dynasty)) dynasty <- ffs_dynasty_outlook(base_simulation)
  # carry rel_change (value move vs position drift) when present, for the optional
  # trajectory soft down-rank; absent -> traj penalty is a no-op
  dyn_cols <- intersect(c("player_id", "cur_value", "next_value_mean", "rel_change"),
                        names(dynasty))
  dyn <- data.table::as.data.table(dynasty)[, dyn_cols, with = FALSE]
  if (!"rel_change" %in% names(dyn)) dyn[, rel_change := NA_real_]

  empty <- data.frame(
    opponent = character(), send = character(), receive = character(),
    send_value = numeric(), recv_value = numeric(), value_gap = numeric(),
    my_win_delta = numeric(), my_playoff_delta = numeric(),
    opp_win_delta = numeric(), opp_playoff_delta = numeric(),
    win_win = logical(), future_capital_delta = numeric(),
    adj_future_capital = numeric(), give_back = logical(), score = numeric(),
    opp_score = numeric(), gettable = logical(),
    send_ids = I(list()), recv_ids = I(list())
  )

  # draft picks (ffs_pick_values) are value-only assets: they add no wins, so
  # value_to_you / value_to_me are 0, but they carry cur_value + next_value_mean.
  # my picks become sendable, opponents' picks become acquirable store-of-value.
  my_picks <- opp_picks <- NULL
  if (!is.null(picks) && nrow(picks)) {
    pk <- data.table::as.data.table(picks)
    pk[, franchise_id := as.character(franchise_id)]
    my_picks  <- pk[franchise_id == fid &  !is.na(cur_value) & cur_value > 0]
    opp_picks <- pk[franchise_id != fid &  !is.na(cur_value) & cur_value > 0]
    if (!is.null(opponents)) opp_picks <- opp_picks[franchise_id %in% opponents]
  }

  # incoming candidates: help me, are priced, owned elsewhere
  inc <- merge(targets[value_to_you > 0 & owner_id != fid], dyn, by = "player_id")
  inc <- inc[!is.na(cur_value) & cur_value > 0]
  # filler filter: a matched received real player must clear min_recv_value
  # (kept lower than the send floor for fragment shapes)
  inc <- inc[cur_value >= min_recv_value]
  if (!is.null(opponents)) inc <- inc[owner_id %in% opponents]
  if (!is.null(opp_picks) && nrow(opp_picks)) {
    inc <- rbind(inc, opp_picks[, list(
      player_id, player_name, pos = "PICK", owner_id = franchise_id,
      value_to_you = 0, cur_value, next_value_mean)], fill = TRUE)
  }

  # my full rostered assets (real players + my picks), unrestricted - this is the
  # pool the give-back sweetener draws from (a cheap filler is a valid give-back).
  mine_all <- unique(rs[franchise_id == fid & !grepl("^(QB|RB|WR|TE|K)_\\d+$", player_id),
                        list(player_id, player_name, pos)])
  mine_all <- merge(mine_all, dyn, by = "player_id")
  mine_all <- mine_all[!is.na(cur_value) & cur_value > 0]
  if (!is.null(my_picks) && nrow(my_picks)) {
    mine_all <- rbind(mine_all, my_picks[, list(
      player_id, player_name, pos = "PICK", cur_value, next_value_mean)], fill = TRUE)
  }
  if (!is.null(must_send)) {
    missing <- setdiff(must_send, mine_all$player_id)
    if (length(missing)) {
      cli::cli_abort("must_send player(s) not on franchise {fid}'s priced roster: {missing}")
    }
  }
  # matched send pieces: filler filtered out, but picks and must_send always kept
  mine <- mine_all[pos == "PICK" | cur_value >= min_piece_value | player_id %in% must_send]
  if (nrow(inc) == 0 || nrow(mine) == 0) return(empty)
  base_fid_summary <- .ffs_franchise_summary(base_simulation, fid)
  # picks contribute 0 win value; only value real players (skip the pick ids)
  is_pick_id <- grepl("^PICK_", mine$player_id)
  mine[, value_to_me := 0]
  mine[!is_pick_id, value_to_me := vapply(player_id, function(p)
    ffs_player_value(base_simulation, p, fid, base_summary = base_fid_summary)$h2h_wins, numeric(1))]

  # opponent baseline playoff odds -> posture. Rebuilders (below
  # posture_rebuild_cut) will accept a playoff hit for future value; everyone
  # else is gated on their playoff drop. Computed once per opponent.
  opp_ids <- unique(inc$owner_id)
  opp_baseline <- vapply(opp_ids, function(o)
    .ffs_franchise_summary(base_simulation, o)$playoff_pct, numeric(1))
  names(opp_baseline) <- as.character(opp_ids)

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
        cost_me = sum(r$value_to_me), top = max(r$value_to_me))
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
            if (abs(sp$value - recv_val) / recv_val > even_band) next
          } else {
            prem <- if (sp$n > nr) {
              (sp$value - recv_val) / recv_val   # I send the package -> it overpays the target
            } else {
              (recv_val - sp$value) / sp$value   # I receive the package -> it overpays my player
            }
            if (!is.null(uneven_gap)) {
              if (prem < uneven_gap[[1]] || prem > uneven_gap[[2]]) next
            } else {
              floor_prem <- consolidation_penalty * abs(sp$n - nr)
              if (prem < floor_prem || prem > floor_prem + uneven_shade) next
            }
          }
          gain <- recv_gain - sp$cost_me
          # additive win-gain gate for even/split shapes only. A consolidation
          # (send a package for one better player) trades slot-count for top-end
          # ceiling - a non-additive payoff (convex championship value) the exact
          # eval + champion screen judge downstream - so let it through here.
          # Receiving draft picks is a pure store-of-value (future-capital) play
          # that adds no wins by construction, so exempt it too - it is judged on
          # future_capital_delta / the future_weight blend downstream.
          recv_has_pick <- any(grepl("^PICK_", rr$player_id))
          if (sp$n <= nr && gain <= 0 && !recv_has_pick) next
          deals[[length(deals) + 1L]] <- data.table::data.table(
            opponent = o, send_ids = list(sp$ids), send = sp$names,
            recv_ids = list(rr$player_id), receive = paste(rr$player_name, collapse = " + "),
            send_value = sp$value, recv_value = recv_val, value_gap = recv_val - sp$value,
            add_gain = gain, send_top = sp$top, recv_top = max(rr$value_to_you),
            future_capital_delta = recv_next - sp$next_value)
        }
      }
    }
  }
  if (length(deals) == 0) return(empty)
  deals <- data.table::rbindlist(deals)

  # hard floor: refuse deals that sacrifice too much future value
  deals <- deals[future_capital_delta >= min_future_delta]
  # uniform fairness ceiling on top of the per-shape bands
  if (is.finite(max_gap)) deals <- deals[abs(value_gap) / recv_value <= max_gap]
  if (nrow(deals) == 0) return(empty)

  # screen down to the exact-eval budget, blending win gain with retention so
  # future-friendly deals are not pruned before they are valued. ceiling_weight
  # adds a top-end proxy (best player received minus best player sent) so
  # consolidations - which lose additive win-gain but raise your ceiling - are
  # not pruned before the exact eval can price their championship value.
  if (score_mode == "rate") {
    # common currency = equivalent playoff %. add_gain is a WIN delta -> * the
    # win->playoff rate; future value and the ceiling proxy -> / future_rate.
    deals[, screen := add_gain * win_to_playoff +
            future_capital_delta / future_rate +
            ceiling_weight * (recv_top - send_top) / future_rate]
  } else {
    deals[, screen := z(add_gain) + future_weight * z(future_capital_delta) +
            ceiling_weight * z(recv_top - send_top)]
  }
  data.table::setorder(deals, -screen)
  if (screen_per_opp > 0L) {
    # coverage: every opponent's top-screen_per_opp packages are valued, plus the
    # global top-screen_n, so one team's high-gain deals can't starve the rest
    deals[, opp_rank := seq_len(.N), by = opponent]
    # picks add NO win-gain (they are win-neutral), so add_gain systematically
    # under-ranks any package that includes a pick - exactly the future-banking
    # deals (e.g. stud+filler for two players + a pick) worth surfacing. Give
    # pick-carrying packages their own per-opponent quota so the win proxy can't
    # starve them out of the exact-eval budget.
    deals[, has_pick := vapply(recv_ids,
      function(r) any(grepl("^PICK_", r)), logical(1))]
    deals[, pick_rank := NA_integer_]
    deals[has_pick == TRUE, pick_rank := seq_len(.N), by = opponent]
    keep <- deals$opp_rank <= screen_per_opp |
      (!is.na(deals$pick_rank) & deals$pick_rank <= screen_per_opp) |
      seq_len(nrow(deals)) <= screen_n
    deals <- deals[keep]
    deals[, c("opp_rank", "has_pick", "pick_rank") := NULL]
  } else {
    deals <- deals[seq_len(min(screen_n, .N))]
  }

  ev <- data.table::rbindlist(lapply(seq_len(nrow(deals)), function(i) {
    te <- data.table::as.data.table(ffs_trade_eval(
      base_simulation, fid, deals$send_ids[[i]], deals$opponent[[i]], deals$recv_ids[[i]]))
    m <- te[franchise_id == fid]
    op <- te[franchise_id == deals$opponent[[i]]]
    data.table::data.table(
      my_win_delta = m$h2h_wins_delta, my_playoff_delta = m$playoff_pct_delta,
      my_champ_delta = m$champion_pct_delta,
      opp_win_delta = op$h2h_wins_delta, opp_playoff_delta = op$playoff_pct_delta,
      opp_champ_delta = op$champion_pct_delta,
      opp_playoff_before = op$playoff_pct_before)   # opponent baseline, for gettability
  }))
  deals <- cbind(deals, ev)
  deals[, win_win := my_win_delta > 0 & opp_win_delta > 0]

  # give-back sweetener: for deals that lower the opponent's playoff odds, offer a
  # variant that sends one of your spare players at the top received player's
  # position back to the depleted team ("send Daniel Jones back on a QB buy", any
  # position). The give-back is exempt from the value band. It is picked to be
  # USEFUL to the opponent, not merely cheap: candidates are ranked by usefulness
  # to THEM (mean weekly points), the strongest are valued, and the one giving the
  # most opponent playoff-lift PER unit of value I give up is kept.
  deals[, give_back := FALSE]
  if (isTRUE(giveback) && nrow(deals)) {
    pos_by_id <- stats::setNames(inc$pos, inc$player_id)
    val_by_id <- stats::setNames(inc$cur_value, inc$player_id)
    # usefulness proxy: a player's mean weekly points (a startable vet outranks a
    # deep-bench flier), same signal ffs_trade_targets screens on
    mps_tbl <- rs[, list(mps = mean(projected_score, na.rm = TRUE)), by = player_id]
    mps_by_id <- stats::setNames(mps_tbl$mps, mps_tbl$player_id)
    gb_pool <- mine_all[pos != "PICK" & cur_value <= giveback_max_value]
    gb_pool[, mps := mps_by_id[player_id]]
    gb_pool <- gb_pool[order(-mps)]           # most useful to any team first
    gb_rows <- which(deals$opp_playoff_delta < giveback_trigger)
    gb_variants <- list()
    for (i in gb_rows) {
      rids <- deals$recv_ids[[i]]
      real <- rids[!grepl("^PICK_", rids)]
      if (!length(real)) next
      P <- pos_by_id[[real[which.max(val_by_id[real])]]]
      # my most-useful spares at the drained position (exclude what I'm already
      # sending); value the top giveback_try, then choose by lift-per-cost
      cand <- gb_pool[pos == P & !player_id %in% deals$send_ids[[i]]]
      if (!nrow(cand)) next
      cand <- cand[seq_len(min(giveback_try, .N))]
      base_opp <- deals$opp_playoff_delta[i]   # opponent delta without the give-back
      best <- NULL
      for (k in seq_len(nrow(cand))) {
        gk <- cand[k]
        ns <- c(deals$send_ids[[i]], gk$player_id)
        te <- data.table::as.data.table(ffs_trade_eval(
          base_simulation, fid, ns, deals$opponent[[i]], rids))
        opk <- te[franchise_id == deals$opponent[[i]]]$playoff_pct_delta
        lift <- opk - base_opp                 # how much this give-back helps them
        eff  <- lift / max(gk$cur_value, 1)     # lift per unit of value I give up
        if (lift > 0 && (is.null(best) || eff > best$eff))
          best <- list(gk = gk, ns = ns, te = te, eff = eff)
      }
      if (is.null(best)) next                  # nothing actually helps them -> skip
      m <- best$te[franchise_id == fid]
      op <- best$te[franchise_id == deals$opponent[[i]]]
      row <- data.table::copy(deals[i])
      # give-back is a sweetener, exempt from the value band: keep it OUT of
      # send_value/gap (so the deal still reads as fair on the matched pieces) but
      # show it in the send string and charge its future value to the deal
      row[, `:=`(
        send = paste0(send, " + ", best$gk$player_name),
        send_ids = list(best$ns),
        future_capital_delta = future_capital_delta - best$gk$next_value_mean,
        my_win_delta = m$h2h_wins_delta, my_playoff_delta = m$playoff_pct_delta,
        my_champ_delta = m$champion_pct_delta,
        opp_win_delta = op$h2h_wins_delta, opp_playoff_delta = op$playoff_pct_delta,
        opp_champ_delta = op$champion_pct_delta,
        win_win = m$h2h_wins_delta > 0 & op$h2h_wins_delta > 0, give_back = TRUE)]
      gb_variants[[length(gb_variants) + 1L]] <- row
    }
    if (length(gb_variants))
      deals <- data.table::rbindlist(c(list(deals), gb_variants), use.names = TRUE)
  }

  # acceptability, posture-aware: contenders/bubble refuse a big playoff hit
  # (e.g. surrendering their only startable QB); rebuilders (baseline playoff
  # below posture_rebuild_cut) instead accept a playoff hit but refuse bleeding
  # future value (their future delta = -future_capital_delta). The rebuilder
  # branch is OPT-IN via a finite max_opp_future_drop, so the default (Inf) keeps
  # the old uniform max_opp_drop gate for every opponent.
  deals[, opp_base := opp_baseline[as.character(opponent)]]
  is_rebuilder <- is.finite(max_opp_future_drop) &
    !is.na(deals$opp_base) & deals$opp_base < posture_rebuild_cut
  deals <- deals[data.table::fifelse(is_rebuilder,
    (-future_capital_delta) >= -max_opp_future_drop,
    opp_playoff_delta >= -max_opp_drop)]
  if (nrow(deals) == 0) return(empty)

  # consolidation realism: an uneven trade forces one side to fragment its
  # single best player into a package, which it only does if it also gains -
  # so keep lopsided (non-win_win) uneven deals out
  if (isTRUE(uneven_require_winwin)) {
    deals <- deals[lengths(send_ids) == lengths(recv_ids) | win_win == TRUE]
    if (nrow(deals) == 0) return(empty)
  }

  # trajectory conscience (opt-in, traj_weight > 0 and dynasty carries rel_change):
  # softly down-rank deals that ship my APPRECIATING players or acquire DECLINING
  # ones, measured vs the position's drift. A sweetener/decliner sinks but is never
  # removed. Picks (not in dyn) read NA -> 0 (neutral store of value).
  rel_by_id <- stats::setNames(dyn$rel_change, dyn$player_id)
  traj_of <- function(send_ids, recv_ids) {
    sr <- rel_by_id[send_ids]; rr <- rel_by_id[recv_ids]
    sum(pmax(0, sr - rise_cut), na.rm = TRUE) + sum(pmax(0, fade_cut - rr), na.rm = TRUE)
  }
  deals[, traj_bad := vapply(seq_len(.N),
    function(i) traj_of(send_ids[[i]], recv_ids[[i]]), numeric(1))]

  # per-position reliability-adjusted future value, and the opponent's OWN
  # valuation (their playoff delta + their mirror future at THEIR haircut) for
  # gettability. Residual method keeps picks: real players carry their position
  # reliability, the pick remainder carries pick_reliability.
  nv_by_id   <- stats::setNames(dyn$next_value_mean, dyn$player_id)
  cur_by_id  <- stats::setNames(dyn$cur_value, dyn$player_id)
  pos_lu     <- rs[, list(pos = pos[1]), by = player_id]
  pos_by_id2 <- stats::setNames(pos_lu$pos, pos_lu$player_id)
  rel_ids <- function(ids) { r <- future_reliability[pos_by_id2[ids]]; r[is.na(r)] <- future_certainty; r }
  # reliability discounts the projected MOVE (next - cur), NOT the whole level:
  # a player's current market value is known; only next year's CHANGE is a
  # projection (QB move projections attenuate hardest, ~0.25 - a scarce, sticky,
  # long-career position the rank-transition model over-swings). Picks are pure
  # speculation, so their future value is level-discounted at pick_reliability.
  reliable_next <- function(ids) {
    ids <- ids[!grepl("^PICK_", ids)]; if (!length(ids)) return(0)
    cu <- cur_by_id[ids]; nx <- nv_by_id[ids]; rl <- rel_ids(ids)
    cu[is.na(cu)] <- 0; nx[is.na(nx)] <- 0
    sum(cu + rl * (nx - cu))
  }
  players_next <- function(ids) { ids <- ids[!grepl("^PICK_", ids)]; sum(nv_by_id[ids], na.rm = TRUE) }
  adj_one <- function(sids, rids, fcd) {
    praw <- players_next(rids) - players_next(sids)                 # players-only raw next
    (reliable_next(rids) - reliable_next(sids)) + pick_reliability * (fcd - praw)
  }
  deals[, adj_future_capital := vapply(seq_len(.N),
    function(i) adj_one(send_ids[[i]], recv_ids[[i]], future_capital_delta[i]), numeric(1))]
  deals[, opp_score := 100 * opp_playoff_delta +
          (-adj_future_capital) * hc(opp_playoff_before) / playoff_value]
  deals[, gettable := opp_score >= gettable_cut]

  # final ranking. rate mode = equivalent playoff %: 100*playoff_delta +
  # reliability-adjusted future * my haircut / price of a playoff point (+winwin
  # bonus, -traj penalty in equiv-% units). future_weight is inert here.
  if (score_mode == "rate") {
    deals[, score := 100 * my_playoff_delta +
            adj_future_capital * my_haircut / playoff_value +
            champ_weight * 100 * my_champ_delta +
            winwin_bonus * as.numeric(win_win %in% TRUE) -
            traj_weight * traj_bad]
  } else {
    deals[, score := z(my_playoff_delta) + champ_weight * z(my_champ_delta) +
            future_weight * z(future_capital_delta) +
            winwin_bonus * as.numeric(win_win %in% TRUE) -
            traj_weight * z(traj_bad)]
  }
  data.table::setorder(deals, -score, -my_win_delta)

  # dedup: collapse deals that differ only by throw-in filler or draft-pick YEAR
  # (same pick slot) to the best-scoring representative, so near-identical ideas
  # do not flood the board. Pick ids "PICK_<season>_<round>_<of>" -> "PICK_R<round>_<of>".
  if (isTRUE(dedupe) && nrow(deals) > 1) {
    norm_ids <- function(ids) paste(sort(sub("^PICK_[0-9]+_", "PICK_R", ids)), collapse = "|")
    deals[, dk := paste(opponent, vapply(send_ids, norm_ids, character(1)),
                        vapply(recv_ids, norm_ids, character(1)), sep = "#")]
    deals <- deals[, .SD[1], by = dk]
    deals[, dk := NULL]
    data.table::setorder(deals, -score, -my_win_delta)
  }

  out <- deals[seq_len(min(top_n, .N)), list(
    opponent, send, receive, send_value, recv_value, value_gap,
    my_win_delta, my_playoff_delta, my_champ_delta,
    opp_win_delta, opp_playoff_delta, opp_champ_delta,
    win_win, future_capital_delta, adj_future_capital, give_back, score,
    opp_score, gettable, send_ids, recv_ids)]
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
  franchise_id <- champion_pct <- top_seed_pct <- NULL

  sw <- data.table::as.data.table(
    ffs_summarise_week(optimal_scores = optimal, schedules = base_simulation$schedules))
  ss <- data.table::as.data.table(ffs_summarise_season(summary_week = sw))
  # playoff seeding: wins, then points-for (how real leagues break ties).
  # Deterministic on purpose - with random tie-breaks, paired comparisons
  # ("with player" vs "without") re-roll the ties and a player who never even
  # starts can show a phantom playoff delta of several percent.
  ss[, lg_rank := data.table::frank(list(-h2h_wins, -points_for), ties.method = "first"),
     by = season]

  # regular-season aggregates + playoff-berth odds (top-6)
  agg <- ss[, list(
    h2h_wins = mean(h2h_wins),
    h2h_winpct = mean(h2h_winpct),
    allplay_winpct = mean(allplay_winpct),
    points_for = mean(points_for),
    playoff_pct = mean(lg_rank <= 6)
  ), by = franchise_id]
  # championship + top-seed odds from a deterministic bracket over the seedings
  # (report-only for now; the trade score still rides on playoff_pct)
  champ <- .ffs_champion_pct(sw, ss)
  agg <- merge(agg, champ, by = "franchise_id", all.x = TRUE)
  agg[is.na(champion_pct), champion_pct := 0][is.na(top_seed_pct), top_seed_pct := 0]
  agg[franchise_id %in% fids]
}

#' Championship + top-seed probability from a deterministic playoff bracket
#'
#' Reuses the already-simulated weekly team scores instead of re-simulating
#' playoff weeks. For each of the simulated seasons the top-6 seeds are taken
#' from `ss` (the same wins-then-points-for seeding used for berth odds), then a
#' standard fixed 6-team bracket (seeds 1-2 bye; R1 3v6 & 4v5; semis 1-vs-(4/5)
#' and 2-vs-(3/6); final) is resolved by **win-probability propagation**, not
#' random draws: each matchup uses `P(A>B) = pnorm((muA-muB)/sqrt(sdA^2+sdB^2))`
#' from each franchise's weekly-score mean/sd. Deterministic on purpose so the
#' paired leave-one-out delta carries no extra Monte-Carlo bracket noise (same
#' rationale as the deterministic seeding tie-break above). Single-elimination
#' rewards week-to-week ceiling (`sd`) that 14-week berth odds wash out - which
#' is exactly the signal we want to surface.
#'
#' @param sw weekly summary (`ffs_summarise_week` output) with `team_score`
#' @param ss season summary with a `lg_rank` seeding column
#' @return a data.table: franchise_id, champion_pct, top_seed_pct (sum to 1 each)
#' @keywords internal
.ffs_champion_pct <- function(sw, ss) {
  team_score <- franchise_id <- lg_rank <- season <- champ <- mu <- sd <- NULL
  champion_pct <- top_seed_pct <- NULL
  sw <- data.table::as.data.table(sw)
  # global per-franchise weekly-score strength (mean + ceiling/variance)
  strength <- sw[, list(mu = mean(team_score), sd = stats::sd(team_score)), by = franchise_id]
  strength[is.na(sd) | sd <= 0, sd := 1e-6]

  seeds <- merge(ss[lg_rank <= 6L, list(season, franchise_id, lg_rank)],
                 strength, by = "franchise_id")
  w <- data.table::dcast(seeds, season ~ lg_rank,
                         value.var = c("franchise_id", "mu", "sd"))
  # any season missing a full 6-team field can't seed a bracket - drop it
  need <- c(paste0("mu_", 1:6), paste0("sd_", 1:6))
  if (!all(need %in% names(w))) return(strength[, list(franchise_id, champion_pct = NA_real_, top_seed_pct = NA_real_)])
  w <- w[stats::complete.cases(w[, need, with = FALSE])]

  pb <- function(a, sa, b, sb) stats::pnorm((a - b) / sqrt(sa^2 + sb^2))
  # round 1: 3v6, 4v5
  p36 <- pb(w$mu_3, w$sd_3, w$mu_6, w$sd_6); p63 <- 1 - p36
  p45 <- pb(w$mu_4, w$sd_4, w$mu_5, w$sd_5); p54 <- 1 - p45
  # semis (fixed bracket): A = 1 vs (4/5 winner); B = 2 vs (3/6 winner)
  a1 <- p45 * pb(w$mu_1, w$sd_1, w$mu_4, w$sd_4) + p54 * pb(w$mu_1, w$sd_1, w$mu_5, w$sd_5)
  a4 <- p45 * pb(w$mu_4, w$sd_4, w$mu_1, w$sd_1)
  a5 <- p54 * pb(w$mu_5, w$sd_5, w$mu_1, w$sd_1)
  b2 <- p36 * pb(w$mu_2, w$sd_2, w$mu_3, w$sd_3) + p63 * pb(w$mu_2, w$sd_2, w$mu_6, w$sd_6)
  b3 <- p36 * pb(w$mu_3, w$sd_3, w$mu_2, w$sd_2)
  b6 <- p63 * pb(w$mu_6, w$sd_6, w$mu_2, w$sd_2)
  # champion = win your semi, then beat whoever comes out of the other semi
  cA <- function(mt, st, pa) pa * (b2 * pb(mt, st, w$mu_2, w$sd_2) +
                                   b3 * pb(mt, st, w$mu_3, w$sd_3) +
                                   b6 * pb(mt, st, w$mu_6, w$sd_6))
  cB <- function(mt, st, pbb) pbb * (a1 * pb(mt, st, w$mu_1, w$sd_1) +
                                     a4 * pb(mt, st, w$mu_4, w$sd_4) +
                                     a5 * pb(mt, st, w$mu_5, w$sd_5))
  champ_long <- data.table::rbindlist(list(
    data.table::data.table(franchise_id = w$franchise_id_1, champ = cA(w$mu_1, w$sd_1, a1)),
    data.table::data.table(franchise_id = w$franchise_id_2, champ = cB(w$mu_2, w$sd_2, b2)),
    data.table::data.table(franchise_id = w$franchise_id_3, champ = cB(w$mu_3, w$sd_3, b3)),
    data.table::data.table(franchise_id = w$franchise_id_4, champ = cA(w$mu_4, w$sd_4, a4)),
    data.table::data.table(franchise_id = w$franchise_id_5, champ = cA(w$mu_5, w$sd_5, a5)),
    data.table::data.table(franchise_id = w$franchise_id_6, champ = cB(w$mu_6, w$sd_6, b6))))
  nseason <- length(unique(ss$season))
  champ_tbl <- champ_long[, list(champion_pct = sum(champ) / nseason), by = franchise_id]
  top_tbl <- ss[lg_rank == 1L, list(top_seed_pct = .N / nseason), by = franchise_id]
  out <- merge(champ_tbl, top_tbl, by = "franchise_id", all = TRUE)
  out[is.na(champion_pct), champion_pct := 0][is.na(top_seed_pct), top_seed_pct := 0]
  out[]
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