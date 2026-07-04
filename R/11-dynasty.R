#' Simulate post-season dynasty value distributions
#'
#' (EXPERIMENTAL) For every rostered player in a simulation, projects the
#' distribution of *next-preseason* dynasty value conditional on the season
#' he just had in each simulated world. The chain per simulated season:
#' the player's simulated season points imply a season-quality percentile Q
#' (relative to what his redraft rank promised); a year-over-year dynasty
#' transition is resampled from historical players with similar position,
#' age, dynasty rank, and Q; the resulting next-year dynasty rank is mapped
#' through an exponential value curve.
#'
#' Transitions are trained on `fp_dynasty_history()` (2018+) joined to
#' redraft ranks and realized scoring; falling out of the dynasty rankings
#' entirely ("exit") is an explicit outcome, so bust/retirement risk is
#' priced. This is the same empirical-resampling philosophy as the v3
#' projection method - no parametric model.
#'
#' @param base_simulation an `ff_simulation` from `ff_simulate(..., return = "all")`
#' @param max_transition_season trains only on transitions *into* seasons <=
#'   this value (default: all available); used by the backtest to hold out a year
#' @param value_curve function mapping overall dynasty rank to trade value;
#'   default `10000 * exp(-0.023 * rank)` (DynastyProcess-like decay)
#'
#' @return a dataframe: one row per rostered player with a current dynasty
#'   rank - current rank/value/age plus the post-season value distribution
#'   (mean, p10, p90, P(value rises), P(exits rankings)) and franchise totals
#'   are trivially `aggregate()`-able from it
#'
#' @export
ffs_dynasty_outlook <- function(base_simulation,
                                max_transition_season = NULL,
                                value_curve = function(rank) 10000 * exp(-0.023 * rank)) {
  checkmate::assert_class(base_simulation, "ff_simulation")

  season <- fantasypros_id <- pos <- ecr <- rank <- age <- player_name <- NULL
  projected_score <- redraft_rank <- total <- q <- player_id <- NULL

  dynasty <- data.table::as.data.table(fp_dynasty_history())
  current_season <- max(dynasty$season)
  current <- dynasty[season == current_season]

  pools <- .ffs_dynasty_transition_pools(
    scoring_history = base_simulation$scoring_history,
    max_transition_season = max_transition_season
  )

  # rostered players with a current dynasty rank
  rosters <- data.table::as.data.table(base_simulation$rosters)
  players <- merge(
    rosters[, list(fantasypros_id, player_id, player_name, pos, franchise_id, franchise_name)],
    current[, list(fantasypros_id, dyn_rank = rank, dyn_pos_rank = pos_rank, age)],
    by = "fantasypros_id"
  )

  # current redraft positional rank from the sim's own rankings
  lr <- data.table::as.data.table(base_simulation$latest_rankings)
  lr[, redraft_rank := data.table::frank(ecr, ties.method = "first"), by = pos]
  players <- merge(players, lr[, list(fantasypros_id, redraft_rank)],
                   by = "fantasypros_id", all.x = TRUE)

  # simulated season totals -> season-quality percentile per sim season
  ps <- data.table::as.data.table(base_simulation$projected_scores)
  ps <- ps[ps$fantasypros_id %in% players$fantasypros_id]
  sim_totals <- ps[, list(total = sum(projected_score, na.rm = TRUE)),
                   by = list(fantasypros_id, sim_season = season)]
  sim_totals <- merge(sim_totals,
                      players[, list(fantasypros_id, pos, redraft_rank)],
                      by = "fantasypros_id", allow.cartesian = TRUE)
  sim_totals[, q := .ffs_season_quality(pos, redraft_rank, total, pools$quality_pools)]

  # draw a transition per player x sim season (positional rank space), then
  # translate back to overall ranks for the value curve
  draws <- merge(
    sim_totals,
    players[, list(fantasypros_id, dyn_rank, dyn_pos_rank, age)],
    by = "fantasypros_id"
  )
  draws[, c("next_pos_rank", "exited") := .ffs_draw_transition(
    pos = pos, age = age, dyn_pos_rank = dyn_pos_rank, q = q,
    transitions = pools$transitions
  )]
  draws[!exited, next_rank := .ffs_pos_to_overall(pos, next_pos_rank, reference = current)]
  draws[, `:=`(
    cur_value = value_curve(dyn_rank),
    next_value = data.table::fifelse(exited, 0, value_curve(next_rank))
  )]

  out <- draws[, list(
    n_sims = .N,
    cur_value = cur_value[[1]],
    next_value_mean = mean(next_value),
    next_value_p10 = stats::quantile(next_value, .10),
    next_value_p90 = stats::quantile(next_value, .90),
    p_rise = mean(next_value > cur_value),
    p_exit = mean(exited)
  ), by = list(fantasypros_id, dyn_rank, age)]

  out <- merge(
    players[, list(fantasypros_id, player_id, player_name, pos,
                   franchise_id, franchise_name, redraft_rank)],
    out, by = "fantasypros_id"
  )[order(-cur_value)]

  return(as.data.frame(out))
}

#' Build dynasty transition pools
#'
#' Historical year-over-year dynasty transitions with features for
#' kernel-weighted resampling: position, age, overall dynasty rank, and the
#' season-quality percentile Q (percentile of realized season points within
#' the historical pool for the player's redraft rank). Also returns the
#' quality pools themselves so simulated seasons can be scored on the same
#' scale.
#'
#' @param scoring_history scoring history covering the training seasons
#' @param max_transition_season train only transitions into seasons <= this
#' @param weeks weeks that count toward a "season" (default 1:14, matching the sims)
#'
#' @keywords internal
.ffs_dynasty_transition_pools <- function(scoring_history,
                                          max_transition_season = NULL,
                                          weeks = 1:14) {
  season <- fantasypros_id <- pos <- rank <- age <- gsis_id <- week <- points <- NULL
  next_rank <- redraft_rank <- total <- q <- NULL

  dynasty <- data.table::as.data.table(fp_dynasty_history())
  if (!is.null(max_transition_season)) dynasty <- dynasty[season <= max_transition_season]

  # transitions are matched and drawn in POSITIONAL rank space: overall
  # dynasty ranks are sparse for QB/TE, which starves their pools and
  # under-disperses predictions (found in the 2025->2026 backtest)
  nxt <- dynasty[, list(season = season - 1L, fantasypros_id,
                        next_rank = rank, next_pos_rank = pos_rank)]
  transitions <- merge(
    dynasty[season < max(dynasty$season)],
    nxt, by = c("season", "fantasypros_id"), all.x = TRUE
  )
  transitions[, exited := is.na(next_rank)]

  # redraft positional rank that season
  redraft <- data.table::as.data.table(fp_rankings_history())[
    , list(season, fantasypros_id, redraft_rank = rank)
  ]
  transitions <- merge(transitions, redraft, by = c("season", "fantasypros_id"), all.x = TRUE)

  # realized season points (same weeks as the simulator)
  sh <- data.table::as.data.table(scoring_history)[
    !is.na(gsis_id) & week %in% weeks, list(season, gsis_id, points)
  ]
  dp_id <- data.table::as.data.table(ffscrapr::dp_playerids())[
    !is.na(gsis_id) & !is.na(fantasypros_id), c("fantasypros_id", "gsis_id")
  ]
  totals <- merge(sh, dp_id, by = "gsis_id")[
    , list(total = sum(points)), by = list(season, fantasypros_id)
  ]
  transitions <- merge(transitions, totals, by = c("season", "fantasypros_id"), all.x = TRUE)
  transitions[is.na(total), total := 0]

  # quality pools: distribution of realized season totals by pos x redraft
  # rank neighborhood (+/-3), built from every ranked player-season
  ranked_totals <- merge(
    redraft, unique(dynasty[, list(season, fantasypros_id, pos)]),
    by = c("season", "fantasypros_id")
  )
  ranked_totals <- merge(ranked_totals, totals, by = c("season", "fantasypros_id"), all.x = TRUE)
  ranked_totals[is.na(total), total := 0]
  quality_pools <- ranked_totals[
    , list(redraft_rank, total)
    , by = pos
  ]

  transitions[, q := .ffs_season_quality(pos, redraft_rank, total, quality_pools)]

  list(transitions = transitions[], quality_pools = quality_pools[])
}

#' Season-quality percentile
#'
#' Percentile of a season total within the empirical pool of season totals
#' for players of the same position with redraft rank within +/-3. NA when
#' the player has no redraft rank (deep stashes) - transitions then condition
#' on position/age/rank only.
#'
#' @keywords internal
.ffs_season_quality <- function(pos, redraft_rank, total, quality_pools) {
  vapply(seq_along(pos), function(i) {
    if (is.na(redraft_rank[i])) return(NA_real_)
    pool <- quality_pools[quality_pools$pos == pos[i] &
                            abs(quality_pools$redraft_rank - redraft_rank[i]) <= 3]$total
    if (length(pool) < 10) return(NA_real_)
    (sum(pool < total[i]) + 0.5 * sum(pool == total[i])) / length(pool)
  }, numeric(1))
}

#' Draw dynasty transitions by kernel-weighted resampling
#'
#' For each target (pos, age, dynasty *positional* rank, Q), samples one
#' historical transition weighted by positional-rank distance (triangular,
#' bandwidth 8), age distance (triangular, bandwidth 3), and Q distance
#' (triangular, bandwidth 0.25; targets or candidates without Q use a
#' neutral mid weight). The sampled *positional-rank delta* is applied to
#' the target's own positional rank; sampled exits stay exits.
#'
#' @return a list of two parallel vectors: next_pos_rank, exited
#' @keywords internal
.ffs_draw_transition <- function(pos, age, dyn_pos_rank, q, transitions,
                                 h_rank = 8, h_age = 3, h_q = 0.25) {
  tr <- transitions
  n <- length(pos)
  next_pos_rank <- numeric(n)
  exited <- logical(n)

  # pre-split by position for speed
  by_pos <- split(seq_len(nrow(tr)), tr$pos)

  for (i in seq_len(n)) {
    idx <- by_pos[[pos[i]]]
    if (is.null(idx)) { next_pos_rank[i] <- dyn_pos_rank[i]; exited[i] <- FALSE; next }
    cand <- tr[idx]
    w <- pmax(0, 1 - abs(cand$pos_rank - dyn_pos_rank[i]) / h_rank)
    if (!is.na(age[i])) {
      wa <- pmax(0, 1 - abs(cand$age - age[i]) / h_age)
      wa[is.na(wa)] <- 0.3
      w <- w * wa
    }
    if (!is.na(q[i])) {
      wq <- pmax(0, 1 - abs(cand$q - q[i]) / h_q)
      wq[is.na(wq)] <- 0.3
      w <- w * wq
    }
    if (sum(w) == 0) w <- pmax(0.001, 1 - abs(cand$pos_rank - dyn_pos_rank[i]) / (h_rank * 4))
    pick <- cand[sample.int(nrow(cand), 1, prob = w)]
    if (pick$exited) {
      exited[i] <- TRUE
      next_pos_rank[i] <- NA_real_
    } else {
      exited[i] <- FALSE
      next_pos_rank[i] <- max(1, dyn_pos_rank[i] + (pick$next_pos_rank - pick$pos_rank))
    }
  }

  list(next_pos_rank, exited)
}

#' Translate positional dynasty ranks to overall dynasty ranks
#'
#' Uses the pos_rank -> overall-rank relationship from a reference dynasty
#' season (monotone within position), extrapolating linearly beyond the
#' deepest ranked player.
#'
#' @keywords internal
.ffs_pos_to_overall <- function(pos, pos_rank, reference) {
  ref <- data.table::as.data.table(reference)
  vapply(seq_along(pos), function(i) {
    r <- ref[ref$pos == pos[i]]
    if (nrow(r) == 0) return(pos_rank[i] * 4)
    r <- r[order(r$pos_rank)]
    if (pos_rank[i] <= max(r$pos_rank)) {
      stats::approx(r$pos_rank, r$rank, xout = pos_rank[i], rule = 2)$y
    } else {
      # beyond the deepest ranked player: extend at the tail slope
      max(r$rank) + (pos_rank[i] - max(r$pos_rank)) * 4
    }
  }, numeric(1))
}