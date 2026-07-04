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
#'
#' @return a one-row dataframe: franchise_id, player_id, player_name, owner_id,
#'   and the deltas (h2h_wins, allplay_winpct, points_for, playoff_pct) of
#'   having the player vs not having him
#'
#' @seealso `ff_wins_added()` for league-wide leave-one-out values
#'
#' @export
ffs_player_value <- function(base_simulation, player_id, franchise_id) {
  checkmate::assert_class(base_simulation, "ff_simulation")

  pid <- player_id
  fid <- franchise_id
  projected_score <- avg_week <- pos_rank <- pos <- season <- week <- NULL

  rs <- data.table::as.data.table(base_simulation$roster_scores)
  checkmate::assert_true(pid %in% rs$player_id)

  player_rows <- rs[rs$player_id == pid]
  owner_id <- player_rows$franchise_id[[1]]
  player_name <- if ("player_name" %in% names(player_rows)) player_rows$player_name[[1]] else NA_character_

  if (owner_id == fid) {
    # rostered here: value = base - without
    without_scores <- rs[rs$franchise_id == fid]
    without_scores <- data.table::copy(without_scores)[
      player_id == pid, `:=`(projected_score = NA, avg_week = NA)
    ]
    with_summary <- .ffs_franchise_summary(base_simulation, fid) # base
    without_summary <- .ffs_franchise_summary(base_simulation, fid, without_scores)
  } else {
    # elsewhere (or replacement level): value = with - base
    incoming <- data.table::copy(player_rows)
    franchise_cols <- intersect(c("franchise_id", "franchise_name", "league_id"), names(incoming))
    template <- rs[rs$franchise_id == fid][1]
    for (col in franchise_cols) data.table::set(incoming, j = col, value = template[[col]])
    with_scores <- rbind(rs[rs$franchise_id == fid], incoming)
    with_summary <- .ffs_franchise_summary(base_simulation, fid, with_scores)
    without_summary <- .ffs_franchise_summary(base_simulation, fid) # base
  }

  out <- data.frame(
    franchise_id = fid,
    player_id = pid,
    player_name = player_name,
    owner_id = owner_id,
    h2h_wins = with_summary$h2h_wins - without_summary$h2h_wins,
    allplay_winpct = with_summary$allplay_winpct - without_summary$allplay_winpct,
    points_for = with_summary$points_for - without_summary$points_for,
    playoff_pct = with_summary$playoff_pct - without_summary$playoff_pct
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
#'
#' @return a two-row dataframe (one per franchise) with before/after and delta
#'   columns for h2h_wins, allplay_winpct, points_for, playoff_pct
#'
#' @export
ffs_trade_eval <- function(base_simulation, franchise_a, gives_a, franchise_b, gives_b) {
  checkmate::assert_class(base_simulation, "ff_simulation")

  rs <- data.table::as.data.table(base_simulation$roster_scores)
  checkmate::assert_true(all(gives_a %in% rs[rs$franchise_id == franchise_a]$player_id))
  checkmate::assert_true(all(gives_b %in% rs[rs$franchise_id == franchise_b]$player_id))

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
    move(rs[rs$player_id %in% gives_b], franchise_a)
  )
  b_new <- rbind(
    rs[rs$franchise_id == franchise_b & !rs$player_id %in% gives_b],
    move(rs[rs$player_id %in% gives_a], franchise_b)
  )

  # re-optimize both sides at once so their head-to-heads stay consistent
  after <- .ffs_franchise_summary(
    base_simulation,
    franchise_id = c(franchise_a, franchise_b),
    franchise_scores = rbind(a_new, b_new)
  )
  before <- .ffs_franchise_summary(base_simulation, franchise_id = c(franchise_a, franchise_b))

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
#' @return a dataframe of candidates: screen proxy, value_to_you,
#'   value_to_owner (h2h wins), and surplus = value_to_you - value_to_owner
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

  exact <- data.table::rbindlist(lapply(candidates$player_id, function(p) {
    to_you <- ffs_player_value(base_simulation, p, fid)
    to_owner <- ffs_player_value(base_simulation, p, candidates[player_id == p]$owner_id)
    data.table::data.table(
      player_id = p,
      value_to_you = to_you$h2h_wins,
      playoff_delta_you = to_you$playoff_pct,
      value_to_owner = to_owner$h2h_wins,
      surplus = to_you$h2h_wins - to_owner$h2h_wins
    )
  }))

  out <- merge(candidates[, list(player_id, player_name, pos, owner_id, proxy)],
               exact, by = "player_id")[order(-surplus)]

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

  sw <- ffs_summarise_week(optimal_scores = optimal, schedules = base_simulation$schedules)
  ss <- data.table::as.data.table(ffs_summarise_season(summary_week = sw))
  ss[, lg_rank := data.table::frank(-h2h_wins, ties.method = "random"), by = season]

  out <- ss[franchise_id %in% fids, list(
    h2h_wins = mean(h2h_wins),
    h2h_winpct = mean(h2h_winpct),
    allplay_winpct = mean(allplay_winpct),
    points_for = mean(points_for),
    playoff_pct = mean(lg_rank <= 6)
  ), by = franchise_id]

  return(out)
}