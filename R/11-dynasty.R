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
#' @param format QB scoring format for dynasty values: "auto" (default) reads
#'   it from the league's lineup constraints (superflex if QB max > 1), or set
#'   "1qb"/"superflex" explicitly. QBs are far more valuable in superflex, so
#'   this materially changes QB dynasty values.
#' @param max_transition_season trains only on transitions *into* seasons <=
#'   this value (default: all available); used by the backtest to hold out a year
#' @param value_curve function mapping overall dynasty rank to trade value;
#'   the synthetic fallback `10000 * exp(-0.023 * rank)` (DynastyProcess-like
#'   decay), used only when no market values are available. Ignored for any
#'   player covered by `dynasty_values`.
#' @param dynasty_values source of *current* dynasty values, both for the
#'   `cur_value` anchor and (via the re-estimated curve) the future projection.
#'   Defaults to `"fantasycalc"`: live market values are scraped with
#'   [fc_dynasty_values()] for this league's detected format. Pass
#'   `"fantasypros"` (or `NULL`) to instead use the synthetic `value_curve` on
#'   FantasyPros dynasty ranks, or pass a pre-scraped dataframe (needs
#'   `fantasypros_id` + `value`, and `format` to match) to avoid re-scraping.
#'   When real market values are used they become the ranking BASELINE: players
#'   are re-ranked by market value so `dyn_rank`, the `value_curve`, and the
#'   transition's starting positional rank are all consistent with the anchored
#'   value (the FantasyPros dynasty rank, which disagrees with the market for
#'   aging vets and rookies, is kept only as `fp_dyn_rank`). Ranked players the
#'   market doesn't price get a market-scale value imputed from the
#'   FP-rank -> market-value relationship so they rank on the same ladder. Rows
#'   whose `fantasypros_id` crosswalk failed (rookies lag in `dp_playerids`) are
#'   recovered by matching their `sleeper_id`/`mfl_id` against this simulation's
#'   roster `player_id`. If the live scrape fails (e.g. offline), it warns and
#'   falls back to the synthetic FantasyPros-rank curve.
#' @param superflex_from_1qb for superflex leagues, train the year-over-year
#'   *transition* dynamics on the deeper 1qb dynasty history (2015+) rather than
#'   the thin native superflex pool (2020+). Positional-rank movement is
#'   format-invariant (a player's `pos_rank` is the same in both formats), so
#'   this borrows a better-sampled, wider-dispersed movement model while all
#'   value/slotting (cross-section, curve, `pos_rank`->overall) stays superflex -
#'   which is what re-slots QBs into their superflex value. Fixes superflex
#'   interval under-dispersion; leaves 1qb unchanged. Default `TRUE` (option
#'   `ffsimulator.dyn_sflx_from_1qb`); set `FALSE` to train on native superflex.
#'
#' @return a dataframe: one row per rostered player with `dyn_rank` (market-value
#'   ordering when real values are supplied, else FantasyPros dynasty rank),
#'   `fp_dyn_rank` (FantasyPros dynasty rank for reference), current value/age
#'   plus the post-season value distribution (mean, median, p10, p90,
#'   P(value rises), P(exits rankings)); franchise totals are trivially
#'   `aggregate()`-able from it. Prefer `next_value_med` for per-player
#'   trajectory reads (exp_change): the rank->value curve is convex, so the
#'   MEAN is inflated by the draw spread (Jensen) - the 11-holdout backtest
#'   finds the mean overstates realized value change (QB worst, ~+50pts) while
#'   the value-draw distribution itself is calibrated, making the median
#'   ~unbiased. The mean remains the right statistic for additive capital
#'   totals (portfolio sums, package comparisons).
#'
#' @export
ffs_dynasty_outlook <- function(base_simulation,
                                format = c("auto", "1qb", "superflex"),
                                max_transition_season = NULL,
                                value_curve = function(rank) 10000 * exp(-0.023 * rank),
                                dynasty_values = "fantasycalc",
                                superflex_from_1qb = getOption("ffsimulator.dyn_sflx_from_1qb", TRUE)) {
  checkmate::assert_class(base_simulation, "ff_simulation")
  format <- rlang::arg_match(format)

  season <- fantasypros_id <- pos <- ecr <- rank <- age <- player_name <- NULL
  projected_score <- redraft_rank <- total <- q <- player_id <- fc_val <- NULL
  cur_curve <- cur_value <- NULL
  fp_rank <- pos_fp <- age_fp <- value <- pos_rank <- fp_dyn_rank <- NULL

  if (format == "auto") format <- .ffs_detect_qb_format(base_simulation$lineup_constraints)
  # local name must differ from the "format" column, else data.table NSE
  # resolves the bare symbol to the column and the filter is a no-op
  qb_format <- format

  # positional-movement dynamics are format-invariant (a player's pos_rank is
  # the same either way), so superflex borrows the deep 1qb transition history
  # (2015+, 11 season-pairs) instead of its own thin 2020+ pool - this fixes
  # superflex interval under-dispersion. Only value/slotting stays superflex
  # (the cross-section, curve, and pos_rank -> overall reference below), which
  # is what re-slots QBs into their superflex value. Set the arg/option FALSE
  # to train on native superflex history.
  movement_format <- if (isTRUE(superflex_from_1qb) && qb_format == "superflex") "1qb" else qb_format

  # resolve where "current" dynasty values come from. Default "fantasycalc"
  # scrapes live market values for this league's format; "fantasypros" (or
  # NULL) falls back to the synthetic rank-decay `value_curve` on FP dynasty
  # ranks; a data.frame is taken as pre-scraped values (see fc_dynasty_values()).
  if (is.character(dynasty_values)) {
    src <- rlang::arg_match0(dynasty_values, c("fantasycalc", "fantasypros"))
    dynasty_values <- if (src == "fantasycalc") {
      tryCatch(
        fc_dynasty_values(num_qbs = if (qb_format == "superflex") 2L else 1L),
        error = function(e) {
          cli::cli_warn(c(
            "!" = "Couldn't fetch live FantasyCalc values ({conditionMessage(e)}).",
            "i" = "Falling back to the synthetic FantasyPros rank-decay curve."
          ))
          NULL
        }
      )
    } else {
      NULL
    }
  }

  dynasty <- data.table::as.data.table(fp_dynasty_history())
  dynasty <- dynasty[dynasty$format == qb_format]
  current_season <- max(dynasty$season)
  fp_current <- dynasty[season == current_season,
                        list(fantasypros_id, fp_rank = rank, pos, age)]

  # optional real market values (e.g. FantasyCalc). When present these become
  # the BASELINE: players are re-ranked by market VALUE so the dynasty rank, the
  # value curve the projection walks, and the transition's starting positional
  # rank are all consistent with the anchored value. FantasyPros dynasty ranks
  # disagree with the market for aging vets and rookies (DK Metcalf sits ~WR49
  # on FantasyCalc but ~ovr-79 on FP), so mixing FP rank with FC value mis-scales
  # the projection. FP ranks are retained only as `fp_dyn_rank` for reference.
  fc <- NULL
  if (!is.null(dynasty_values)) {
    dv <- data.table::as.data.table(dynasty_values)
    if ("format" %in% names(dv)) dv <- dv[dv$format == qb_format]
    # recover fantasypros_id for rows the dp_playerids crosswalk missed -
    # rookies/devy lag there, and they are exactly where market value diverges
    # most from the rank curve (found via Jeremiyah Love: market 7356, curve
    # 6056). The source platform ids (sleeper/mfl) match this simulation's
    # roster player_id, whose fantasypros_id includes ffs_backfill_fp_ids fills.
    id_cols <- intersect(c("sleeper_id", "mfl_id"), names(dv))
    if (any(is.na(dv$fantasypros_id)) && length(id_cols)) {
      ros <- data.table::as.data.table(base_simulation$rosters)
      ros <- unique(ros[!is.na(ros$fantasypros_id),
                        list(pid = as.character(player_id), fp_ros = fantasypros_id)])
      for (col in id_cols) {
        hit <- match(as.character(dv[[col]]), ros$pid)
        fill <- is.na(dv$fantasypros_id) & !is.na(hit)
        dv$fantasypros_id[fill] <- ros$fp_ros[hit[fill]]
      }
    }
    dv <- dv[!is.na(dv$fantasypros_id) & !is.na(dv$value) & dv$value > 0]
    dv <- unique(dv, by = "fantasypros_id")
    if (nrow(dv) >= 20) {
      fc <- dv
    } else {
      # loud fallback: a common cause is passing values for the wrong QB format
      want_qbs <- if (qb_format == "superflex") "num_qbs = 2" else "num_qbs = 1"
      n_dv <- nrow(dv)
      cli::cli_warn(c(
        "!" = "`dynasty_values` matched only {n_dv} {qb_format} players (need >= 20).",
        "i" = "Falling back to the synthetic value curve - did you scrape the right format ({want_qbs})?"
      ))
    }
  }

  if (!is.null(fc)) {
    # normalise: a pre-scraped df may carry only fantasypros_id + value, so pull
    # pos/age from FP where the market source doesn't supply them
    fc <- merge(fc, fp_current[, list(fantasypros_id, pos_fp = pos, age_fp = age)],
                by = "fantasypros_id", all.x = TRUE)
    if (!"pos" %in% names(fc)) fc[, pos := NA_character_]
    if (!"age" %in% names(fc)) fc[, age := NA_real_]
    fc[is.na(pos), pos := pos_fp]
    fc[is.na(age), age := age_fp]

    # one ladder for everyone: market-priced players keep their value; ranked
    # players the market misses get a market-scale value imputed from the
    # FP-rank -> market-value relationship, so all rank together on one scale
    fp2fc <- merge(fp_current[, list(fantasypros_id, fp_rank)],
                   fc[, list(fantasypros_id, value)], by = "fantasypros_id")
    fp2fc <- fp2fc[, list(value = mean(value)), by = fp_rank][order(fp_rank)]
    impute <- stats::approxfun(fp2fc$fp_rank, fp2fc$value, rule = 2)
    fp_only <- fp_current[!fantasypros_id %in% fc$fantasypros_id]
    fp_only[, value := impute(fp_rank)]

    uni <- rbind(
      fc[, list(fantasypros_id, pos, age, value)],
      fp_only[, list(fantasypros_id, pos, age, value)]
    )
    uni <- uni[!is.na(value) & !is.na(pos)]
    uni[, rank := data.table::frank(-value, ties.method = "first")]
    uni[, pos_rank := data.table::frank(-value, ties.method = "first"), by = pos]

    value_curve <- stats::approxfun(uni$rank, uni$value, rule = 2)
    current <- uni                                   # reference for pos -> overall
    fc_val <- uni[, list(fantasypros_id, fc_val = value)]
  } else {
    # synthetic path (unchanged): FP dynasty ranks + the caller's value_curve
    current <- dynasty[season == current_season]
    fc_val <- NULL
  }

  pools <- .ffs_dynasty_transition_pools(
    scoring_history = base_simulation$scoring_history,
    format = movement_format,
    max_transition_season = max_transition_season
  )
  # superflex borrows 1qb movement dispersion but keeps its NATIVE exit rate:
  # ranking depth is format-specific, and 1qb's higher skill-position exit rates
  # otherwise over-predict superflex exits. Built only when the pools differ.
  exit_pools <- if (movement_format != qb_format) {
    .ffs_dynasty_transition_pools(
      scoring_history = base_simulation$scoring_history,
      format = qb_format,
      max_transition_season = max_transition_season
    )
  } else NULL

  # rostered players with a current dynasty rank
  rosters <- data.table::as.data.table(base_simulation$rosters)
  players <- merge(
    rosters[, list(fantasypros_id, player_id, player_name, pos, franchise_id, franchise_name)],
    current[, list(fantasypros_id, dyn_rank = rank, dyn_pos_rank = pos_rank, age)],
    by = "fantasypros_id"
  )
  # keep the FantasyPros dynasty rank alongside for reference (baseline dyn_rank
  # is now the market-value ordering when real values are supplied)
  players <- merge(players, fp_current[, list(fantasypros_id, fp_dyn_rank = fp_rank)],
                   by = "fantasypros_id", all.x = TRUE)

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
    transitions = pools$transitions,
    exit_transitions = if (!is.null(exit_pools)) exit_pools$transitions else NULL
  )]
  draws[exited == FALSE, next_rank := .ffs_pos_to_overall(pos, next_pos_rank, reference = current)]
  # current value from the (possibly empirical) curve, overridden by the actual
  # market value where we have one; next value scales the current value by the
  # curve's predicted rank change, so anchoring to real values stays consistent
  draws[, cur_curve := value_curve(dyn_rank)]
  draws[, cur_value := cur_curve]
  if (!is.null(fc_val)) {
    draws <- merge(draws, fc_val, by = "fantasypros_id", all.x = TRUE)
    draws[!is.na(fc_val), cur_value := fc_val]
  }
  draws[, next_value := data.table::fifelse(
    exited, 0, cur_value * value_curve(next_rank) / cur_curve)]

  out <- draws[, list(
    n_sims = .N,
    cur_value = cur_value[[1]],
    next_value_mean = mean(next_value),
    next_value_med = stats::median(next_value),
    next_value_p10 = stats::quantile(next_value, .10),
    next_value_p90 = stats::quantile(next_value, .90),
    p_rise = mean(next_value > cur_value),
    p_exit = mean(exited)
  ), by = list(fantasypros_id, dyn_rank, age)]

  out <- merge(
    players[, list(fantasypros_id, player_id, player_name, pos,
                   franchise_id, franchise_name, redraft_rank, fp_dyn_rank)],
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
#' @param format QB format ("1qb" or "superflex") to train the transitions on
#' @param max_transition_season train only transitions into seasons <= this
#' @param weeks weeks that count toward a "season" (default 1:14, matching the sims)
#'
#' @keywords internal
.ffs_dynasty_transition_pools <- function(scoring_history,
                                          format = "1qb",
                                          max_transition_season = NULL,
                                          weeks = 1:14) {
  season <- fantasypros_id <- pos <- rank <- age <- gsis_id <- week <- points <- NULL
  next_rank <- redraft_rank <- total <- q <- NULL
  draft_year <- draft_round <- first_season <- years_exp <- prev_pos_rank <- prev_delta <- NULL

  qb_format <- format  # avoid data.table NSE collision with the format column
  dynasty <- data.table::as.data.table(fp_dynasty_history())
  dynasty <- dynasty[dynasty$format == qb_format]
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

  # experience / draft-capital / momentum features for the optional kernel
  # terms (see .ffs_draw_transition): years_exp prefers dp_playerids
  # draft_year, falling back to seasons since first appearing in these
  # rankings (rookie id-lag); drafted players with no recorded round are
  # treated as UDFA (round 8); prev_delta is last year's positional-rank move
  # (NA for new entrants)
  ids <- data.table::as.data.table(ffscrapr::dp_playerids())
  ids <- unique(ids[!is.na(ids$fantasypros_id),
                    list(fantasypros_id = as.character(fantasypros_id),
                         draft_year = suppressWarnings(as.integer(draft_year)),
                         draft_round = suppressWarnings(as.integer(draft_round)))],
                by = "fantasypros_id")
  hit <- match(as.character(transitions$fantasypros_id), ids$fantasypros_id)
  transitions[, `:=`(draft_year = ids$draft_year[hit], draft_round = ids$draft_round[hit])]
  first_seen <- dynasty[, list(first_season = min(season)), by = fantasypros_id]
  transitions <- merge(transitions, first_seen, by = "fantasypros_id", all.x = TRUE)
  transitions[, years_exp := data.table::fifelse(
    !is.na(draft_year), season - draft_year, season - first_season)]
  transitions[years_exp < 0, years_exp := 0L]
  transitions[!is.na(draft_year) & is.na(draft_round), draft_round := 8L]
  prev <- dynasty[, list(season = season + 1L, fantasypros_id, prev_pos_rank = pos_rank)]
  transitions <- merge(transitions, prev, by = c("season", "fantasypros_id"), all.x = TRUE)
  transitions[, prev_delta := pos_rank - prev_pos_rank]

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

#' Detect QB scoring format from lineup constraints
#'
#' Superflex leagues allow more than one starting QB (QB max > 1) or carry an
#' explicit superflex/OP slot; everything else is treated as 1qb.
#'
#' @keywords internal
.ffs_detect_qb_format <- function(lineup_constraints) {
  lc <- data.table::as.data.table(lineup_constraints)
  qb_max <- lc[lc$pos == "QB"]$max
  if (length(qb_max) && max(qb_max) > 1) "superflex" else "1qb"
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
  # split to plain vectors up front: subsetting a data.table with `pos[i]`
  # inside `[` would resolve pos/redraft_rank to the pool's own columns
  # (data.table NSE), not these arguments
  qp <- as.data.frame(quality_pools)
  by_pos <- split(qp[, c("redraft_rank", "total")], qp$pos)
  vapply(seq_along(pos), function(i) {
    if (is.na(redraft_rank[i])) return(NA_real_)
    d <- by_pos[[pos[i]]]
    if (is.null(d)) return(NA_real_)
    pool <- d$total[abs(d$redraft_rank - redraft_rank[i]) <= 3]
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
#' Optional kernel terms (all off by default; enabled via `ffsimulator.dyn_*`
#' options or the bandwidth arguments, and gated on the multi-year dynasty
#' backtest before becoming defaults):
#' - `years_exp` with `h_exp` - seasons since draft (rookie repricing differs
#'   from same-age veterans); `dyn_rookie_strict` additionally restricts
#'   rookie targets (years_exp 0) to rookie candidates when the pool allows
#' - `draft_round` with `h_draft` - draft capital (UDFA = round 8)
#' - `prev_delta` with `h_momentum` - last year's positional-rank move
#' - `ecr_sd` with `h_sd` - expert disagreement (matches uncertain players to
#'   historically uncertain candidates, whose transitions were wider)
#' Candidates missing an enabled feature get the neutral weight 0.3, matching
#' the age/Q convention; targets missing it skip the term.
#'
#' Exit shrinkage (`exit_shrink` / `ffsimulator.dyn_exit_shrink`, default
#' kappa = 10): the raw exit probability is the kernel-weighted mean of the exit
#' indicator over the candidate pool - a Nadaraya-Watson estimate that is
#' high-variance for thin cells (e.g. mid-20s QBs at a fringe rank, whose
#' effective pool is ~5 historical players once age x rank x Q are all matched).
#' The local estimate is shrunk toward a broad (wide rank+age bandwidth)
#' empirical rate by effective sample size,
#' `p = (n_eff * local + kappa * broad) / (n_eff + kappa)`, and exit is then a
#' Bernoulli draw with the rank move resampled among survivors. This tempers
#' noisy per-cell exit spikes without flattening the genuine age/rank trend.
#' Set `kappa = 0` to recover the un-shrunk single-draw behavior. Enabled by
#' default after the multi-year dynasty backtest: at kappa = 10 the pooled exit
#' calibration improves (e.g. the 1qb >50% bin, badly over-dispersed at
#' 0.73 predicted / 0.52 actual, moves to 0.64 / 0.60) with survivor rank
#' calibration held flat; gains plateau by kappa 10-15.
#'
#' `exit_transitions` (optional): a separate pool from which the EXIT
#' probability is estimated, while the rank move is still resampled from
#' `transitions`. Used by superflex, which borrows the deep 1qb pool for
#' movement dispersion but keeps its native (format-specific, ranking-depth)
#' exit rate - importing 1qb's higher skill-position exit rates otherwise
#' over-predicts superflex exits. `NULL` (default) uses `transitions` for both,
#' so 1qb leagues and every other caller are unchanged.
#'
#' `move_slope` / `move_bias` (optional named per-position vectors, e.g.
#' `c(QB = 0.35, RB = 0.5)` / `c(RB = -1)`): point-prediction recalibration.
#' The multi-holdout backtest finds the regression slope of actual on
#' predicted rank move < 1 at every position (predicted move magnitudes too
#' extreme - regression to the mean not fully captured by the kernel
#' resample). The correction shifts each draw's CENTER: the kernel-weighted
#' mean move `dbar` becomes `move_slope * dbar + move_bias` (delta space,
#' positive = rank number grows = decline) while the dispersion around it is
#' left to `disp_factor`. Enabled by default with constants fit on material
#' players (value >= 150) across all 11 holdouts, leave-one-holdout-out
#' stable: `move_slope = c(QB = .48, RB = .76, WR = .64, TE = .51)`,
#' `move_bias = c(QB = 1.7, RB = 2.4, WR = 4.1, TE = 2.5)`. On the
#' confirmation backtest this lifts the material actual~predicted move slope
#' from 0.48-0.76 to 0.56-0.89, cuts the user-facing exp_change optimism
#' (WR +22% -> +6% predicted vs -5% realized; TE +32% -> +8%), improves or
#' holds MAE, and moves material cover80 from over-coverage (0.84-0.91)
#' toward nominal (0.81-0.91) - see
#' dev/validate_outputs/dynasty_point_calibration.txt. Stronger constants
#' saturate (the summarized median stops responding under the rank-1
#' truncation) and degrade value-space slope, so this is the calibrated
#' dose, not a partial one. Pass `NULL` to disable; positions absent from
#' the vector get slope 1 / bias 0.
#'
#' `disp_factor` (optional named per-position vector, e.g.
#' `c(QB = 1.7, TE = 1.4, WR = 1.2, RB = 1.1)`): widens each survivor's rank
#' move around its weighted-mean move by the position's factor, correcting the
#' interval under-dispersion of positions with compressed positional scales
#' (QB/TE rankings are shallow but map through a steep value curve, so the
#' conditional resample under-states their value volatility; effective sample
#' size is ~flat across positions, so this is a per-position, not n_eff-driven,
#' correction). The mean is preserved, so medians/`p_rise` are unchanged - only
#' the interval widens. Enabled by default with backtest-calibrated factors
#' `c(QB = 1.68, TE = 1.43, WR = 1.20, RB = 1.11)`, which lift survivor
#' `cover80` from ~0.60-0.73 to ~0.75-0.82 across both formats (shallower
#' rankings need more widening). Pass `NULL`, or a vector with no matching
#' positions, to disable.
#'
#' @return a list of two parallel vectors: next_pos_rank, exited
#' @keywords internal
.ffs_draw_transition <- function(pos, age, dyn_pos_rank, q, transitions,
                                 h_rank = 8, h_age = 3, h_q = 0.25,
                                 years_exp = NULL, draft_round = NULL,
                                 prev_delta = NULL, ecr_sd = NULL,
                                 h_exp = getOption("ffsimulator.dyn_h_exp", NA),
                                 h_draft = getOption("ffsimulator.dyn_h_draft", NA),
                                 h_momentum = getOption("ffsimulator.dyn_h_momentum", NA),
                                 h_sd = getOption("ffsimulator.dyn_h_sd", NA),
                                 rookie_strict = getOption("ffsimulator.dyn_rookie_strict", FALSE),
                                 exit_shrink = getOption("ffsimulator.dyn_exit_shrink", 10),
                                 exit_broad_rank = getOption("ffsimulator.dyn_exit_broad_rank", 3),
                                 exit_broad_age = getOption("ffsimulator.dyn_exit_broad_age", 3),
                                 exit_transitions = NULL,
                                 disp_factor = getOption("ffsimulator.dyn_disp_factor",
                                                         c(QB = 1.68, TE = 1.43, WR = 1.20, RB = 1.11)),
                                 move_slope = getOption("ffsimulator.dyn_move_slope",
                                                        c(QB = 0.48, RB = 0.76, WR = 0.64, TE = 0.51)),
                                 move_bias = getOption("ffsimulator.dyn_move_bias",
                                                       c(QB = 1.7, RB = 2.4, WR = 4.1, TE = 2.5))) {
  tr <- transitions
  n <- length(pos)
  next_pos_rank <- numeric(n)
  exited <- logical(n)

  # pre-split by position for speed
  by_pos <- split(seq_len(nrow(tr)), tr$pos)
  # optional separate exit pool (superflex: native exit rate, 1qb movement)
  by_pos_exit <- if (!is.null(exit_transitions)) {
    split(seq_len(nrow(exit_transitions)), exit_transitions$pos)
  } else NULL

  kern <- function(w, cand_vals, target, h) {
    if (is.na(h) || is.null(cand_vals) || is.na(target)) return(w)
    wf <- pmax(0, 1 - abs(cand_vals - target) / h)
    wf[is.na(wf)] <- 0.3
    w * wf
  }

  for (i in seq_len(n)) {
    idx <- by_pos[[pos[i]]]
    if (is.null(idx)) { next_pos_rank[i] <- dyn_pos_rank[i]; exited[i] <- FALSE; next }
    # hard rookie stratification: rookie targets draw only from rookie
    # transitions when the pool is deep enough (soft kernel otherwise)
    if (isTRUE(rookie_strict) && !is.null(years_exp) && !is.na(years_exp[i]) &&
        years_exp[i] == 0 && !is.null(tr$years_exp)) {
      ridx <- idx[!is.na(tr$years_exp[idx]) & tr$years_exp[idx] == 0]
      if (length(ridx) >= 30) idx <- ridx
    }
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
    if (!is.null(years_exp))   w <- kern(w, cand$years_exp,   years_exp[i],   h_exp)
    if (!is.null(draft_round)) w <- kern(w, cand$draft_round, draft_round[i], h_draft)
    if (!is.null(prev_delta))  w <- kern(w, cand$prev_delta,  prev_delta[i],  h_momentum)
    if (!is.null(ecr_sd))      w <- kern(w, cand$sd,          ecr_sd[i],      h_sd)
    if (sum(w) == 0) w <- pmax(0.001, 1 - abs(cand$pos_rank - dyn_pos_rank[i]) / (h_rank * 4))
    if (exit_shrink <= 0 && is.null(exit_transitions) &&
        is.null(move_slope) && is.null(move_bias)) {
      # default path (unchanged): one draw, inherit its exit flag - keep the
      # exact RNG stream so backtest defaults are untouched
      pick <- cand[sample.int(nrow(cand), 1, prob = w)]
      if (pick$exited) {
        exited[i] <- TRUE
        next_pos_rank[i] <- NA_real_
      } else {
        exited[i] <- FALSE
        next_pos_rank[i] <- max(1, dyn_pos_rank[i] + (pick$next_pos_rank - pick$pos_rank))
      }
    } else {
      # exit-probability candidate pool: the native exit pool when supplied
      # (superflex), else the movement pool itself (default - identical to the
      # prior behavior). Weights mirror the movement kernel (rank+age+q).
      if (is.null(by_pos_exit)) {
        ecand <- cand; ew <- w
      } else {
        eidx <- by_pos_exit[[pos[i]]]
        if (is.null(eidx)) {
          ecand <- cand; ew <- w
        } else {
          ecand <- exit_transitions[eidx]
          ew <- pmax(0, 1 - abs(ecand$pos_rank - dyn_pos_rank[i]) / h_rank)
          if (!is.na(age[i])) {
            ea <- pmax(0, 1 - abs(ecand$age - age[i]) / h_age); ea[is.na(ea)] <- 0.3; ew <- ew * ea
          }
          if (!is.na(q[i])) {
            eq <- pmax(0, 1 - abs(ecand$q - q[i]) / h_q); eq[is.na(eq)] <- 0.3; ew <- ew * eq
          }
          if (sum(ew) == 0) ew <- pmax(0.001, 1 - abs(ecand$pos_rank - dyn_pos_rank[i]) / (h_rank * 4))
        }
      }
      # empirical-Bayes exit shrinkage: pull the local kernel exit rate toward a
      # broad (wide rank+age, no-q) empirical rate by effective sample size
      sw <- sum(ew)
      p_local <- sum(ew * ecand$exited) / sw
      n_eff <- sw^2 / sum(ew^2)
      wb <- pmax(0, 1 - abs(ecand$pos_rank - dyn_pos_rank[i]) / (h_rank * exit_broad_rank))
      if (!is.na(age[i])) {
        ab <- pmax(0, 1 - abs(ecand$age - age[i]) / (h_age * exit_broad_age))
        ab[is.na(ab)] <- 0.3
        wb <- wb * ab
      }
      if (sum(wb) == 0) wb <- rep(1, length(wb))
      p_broad <- sum(wb * ecand$exited) / sum(wb)
      p_exit_i <- (n_eff * p_local + exit_shrink * p_broad) / (n_eff + exit_shrink)
      if (stats::runif(1) < p_exit_i) {
        exited[i] <- TRUE
        next_pos_rank[i] <- NA_real_
      } else {
        # rank move conditional on surviving: resample among non-exited candidates
        exited[i] <- FALSE
        surv <- !cand$exited
        ws <- w * surv
        if (sum(ws) == 0) {
          ws <- pmax(0.001, 1 - abs(cand$pos_rank - dyn_pos_rank[i]) / (h_rank * 4))
          ws[!surv] <- 0
        }
        pick <- cand[sample.int(nrow(cand), 1, prob = ws)]
        delta <- pick$next_pos_rank - pick$pos_rank
        # per-position interval widening: QB/TE positional scales are compressed
        # (few ranked slots) but map through a steep value curve, so the
        # conditional resample under-disperses their value outcomes. Scale the
        # sampled move around the weighted-mean move by a position factor
        # (backtest-calibrated to cover80 ~ 0.80); mean is preserved so medians
        # don't shift. NULL/1 = no widening (default off until calibrated).
        fp <- if (!is.null(disp_factor) && pos[i] %in% names(disp_factor)) disp_factor[[pos[i]]] else 1
        ms <- if (!is.null(move_slope) && pos[i] %in% names(move_slope)) move_slope[[pos[i]]] else 1
        mb <- if (!is.null(move_bias) && pos[i] %in% names(move_bias)) move_bias[[pos[i]]] else 0
        if (fp != 1 || ms != 1 || mb != 0) {
          sdelta <- cand$next_pos_rank - cand$pos_rank
          dbar <- sum(ws * sdelta, na.rm = TRUE) / sum(ws)
          # recalibrated center + widened dispersion around it
          delta <- (ms * dbar + mb) + fp * (delta - dbar)
        }
        next_pos_rank[i] <- max(1, dyn_pos_rank[i] + delta)
      }
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
  ref <- as.data.frame(reference)
  # build one monotone pos_rank -> overall interpolator per position. doing
  # this in base R avoids the data.table NSE trap where `ref[ref$pos ==
  # pos[i]]` resolves `pos[i]` to the reference's own pos column (which made
  # every position map through the WR curve)
  fns <- lapply(split(ref[, c("pos_rank", "rank")], ref$pos), function(d) {
    d <- d[order(d$pos_rank), ]
    list(
      f = stats::approxfun(d$pos_rank, d$rank, rule = 2),
      max_pos_rank = max(d$pos_rank),
      max_rank = max(d$rank)
    )
  })
  out <- numeric(length(pos))
  for (i in seq_along(pos)) {
    fn <- fns[[pos[i]]]
    if (is.null(fn)) { out[i] <- pos_rank[i] * 4; next }
    out[i] <- if (pos_rank[i] <= fn$max_pos_rank) {
      fn$f(pos_rank[i])
    } else {
      # beyond the deepest ranked player: extend at the tail slope
      fn$max_rank + (pos_rank[i] - fn$max_pos_rank) * 4
    }
  }
  out
}