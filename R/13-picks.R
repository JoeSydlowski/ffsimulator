#### Rookie draft picks as valued dynasty assets ####

#' Get future draft picks for a league
#'
#' (EXPERIMENTAL) Light wrapper around [ffscrapr::ff_draftpicks()] that adds
#' `league_id` and coerces the id columns to character, matching the convention
#' the rest of ffsimulator uses (see [ffs_rosters()]). Returns every current and
#' future pick each franchise holds, including picks acquired via trade
#' (`original_franchise_id` is the team whose finish will set the pick's slot).
#'
#' @param conn a connection object from `ffscrapr::ff_connect()` and friends
#'
#' @return a dataframe: `league_id`, `season`, `round`, `franchise_id` (current
#'   owner), `franchise_name`, `original_franchise_id`
#'
#' @export
ffs_draftpicks <- function(conn) {
  dp <- data.table::as.data.table(ffscrapr::ff_draftpicks(conn))
  dp[, season := as.integer(season)]
  dp[, round := as.integer(round)]
  dp[, franchise_id := as.character(franchise_id)]
  if ("original_franchise_id" %in% names(dp)) {
    dp[, original_franchise_id := as.character(original_franchise_id)]
  } else {
    dp[, original_franchise_id := franchise_id]
  }
  dp[, league_id := as.character(conn$league_id)]
  as.data.frame(dp[])
}

#' Per-franchise finish distribution from a base simulation
#'
#' Returns, for every franchise, the share of simulated seasons it lands at each
#' final-standings rank (`lg_rank`, 1 = best). Uses the same deterministic
#' wins-then-points-for seeding as the trade machinery
#' ([`.ffs_summarise_optimal`]) so the standings a pick's slot is read from match
#' the standings behind playoff odds.
#'
#' @param base_simulation an `ff_simulation` (`return = "all"`)
#'
#' @return a data.table: `franchise_id`, `lg_rank`, `prob` (sums to 1 per
#'   franchise), plus `n_teams`
#' @keywords internal
.ffs_finish_distribution <- function(base_simulation) {
  season <- h2h_wins <- points_for <- lg_rank <- franchise_id <- prob <- NULL

  sw <- data.table::as.data.table(ffs_summarise_week(
    optimal_scores = base_simulation$optimal_scores,
    schedules = base_simulation$schedules))
  ss <- data.table::as.data.table(ffs_summarise_season(summary_week = sw))
  ss[, lg_rank := data.table::frank(list(-h2h_wins, -points_for), ties.method = "first"),
     by = season]

  n_teams <- length(unique(ss$franchise_id))
  n_seas  <- length(unique(ss$season))
  dist <- ss[, list(prob = .N / n_seas), by = list(franchise_id, lg_rank)]
  dist[, n_teams := n_teams]
  dist[order(franchise_id, lg_rank)]
}

#' Parse the FantasyCalc pick rows into a slot-value curve + year discounts
#'
#' FantasyCalc prices a 12-team draft. The explicit `<round>.<slot>` rows give
#' the value-vs-overall-pick shape; the generic `"<year> 1st"` rows give the
#' market's year-over-year time discount on future picks.
#'
#' @param fc a `fc_dynasty_values()` dataframe (rows with `pos == "PICK"`)
#' @param label_teams picks per round FantasyCalc assumes (12)
#' @return list(`B` = approxfun(overall_pick -> value), `fut` = named numeric of
#'   generic-first values by year)
#' @keywords internal
.ffs_fc_pick_curve <- function(fc, label_teams = 12L) {
  fc <- data.table::as.data.table(fc)
  fc <- fc[pos == "PICK"]
  nm <- fc$player_name

  # explicit round.slot -> overall pick number
  rx <- regexpr("[1-9]\\.[0-9]{2}", nm)
  has_slot <- rx != -1L
  slot_lab <- rep(NA_character_, length(nm))
  slot_lab[has_slot] <- regmatches(nm, rx)[seq_len(sum(has_slot))]
  rnd <- as.integer(sub("\\..*$", "", slot_lab))
  inr <- as.integer(sub("^.*\\.", "", slot_lab))
  overall <- (rnd - 1L) * label_teams + inr
  sl <- data.table::data.table(overall = overall[has_slot], value = fc$value[has_slot])
  sl <- sl[order(overall)][, list(value = mean(value)), by = overall]
  B <- if (nrow(sl) >= 2) stats::approxfun(sl$overall, sl$value, rule = 2) else function(x) NA_real_

  # generic "<year> 1st" -> year time-discount anchor
  yrx <- regexpr("^(19|20)[0-9]{2} 1st$", nm)
  fut <- fc$value[yrx != -1L]
  fyr <- as.integer(substr(nm[yrx != -1L], 1, 4))
  fut <- stats::setNames(fut, fyr)
  fut <- fut[order(as.integer(names(fut)))]

  list(B = B, fut = fut)
}

#' Value future draft picks as dynasty assets
#'
#' (EXPERIMENTAL) Prices each future rookie pick a league's franchises hold, so
#' picks can sit in the same dynasty/trade machinery as players. A pick is a
#' zero-wins-now, store-of-value asset: its worth is set by the **slot** it will
#' convert at (inverse of the origin team's projected finish) and the historical
#' hit rate at that slot.
#'
#' The value model (Joe's "option 1"): `cur_value` is the **market** price of the
#' slot (FantasyCalc's 12-team pick curve, evaluated at the projected overall
#' pick number and discounted by the market's year-over-year decay). `next_value`
#' is the same slot one year closer to converting, so picks **appreciate** toward
#' their draft (a pure store of value, `win_now_value <= 0`). The empirical
#' hit-rate curve (`dev/data/pick_value_curve.csv`, from `pick_value_study.R`)
#' supplies the median/tail skew and an `emp_ev` column for the market-vs-hit-rate
#' edge.
#'
#' Column contract matches [ffs_dynasty_outlook()] so downstream code
#' (`ffs_build_trades`, the trade suite) treats picks and players uniformly.
#'
#' @param base_simulation an `ff_simulation` (`return = "all"`)
#' @param picks a `ffs_draftpicks()` dataframe; if `NULL`, fetched via `conn`
#' @param conn a league connection (used only when `picks` is `NULL`)
#' @param pick_curve the empirical curve: a data.frame or path to
#'   `pick_value_curve.csv`
#' @param fc_pick_values a pre-scraped `fc_dynasty_values()` df (pick rows); if
#'   `NULL`, scraped for the simulation's format
#' @param format `"auto"`, `"1qb"`, or `"superflex"` (auto-detected from the
#'   lineup constraints, as in `ffs_dynasty_outlook`)
#' @param sim_season the season being simulated (default: one before the earliest
#'   pick season)
#'
#' @return a data.frame, one row per pick, with `ffs_dynasty_outlook`-compatible
#'   columns plus `season`, `round`, `exp_pick` (expected overall pick number),
#'   `emp_ev`, and `original_franchise_id`
#'
#' @export
ffs_pick_values <- function(base_simulation, picks = NULL, conn = NULL,
                            pick_curve = "dev/data/pick_value_curve.csv",
                            fc_pick_values = NULL, format = "auto",
                            sim_season = NULL) {
  checkmate::assert_class(base_simulation, "ff_simulation")
  lg_rank <- prob <- n_teams <- franchise_id <- original_franchise_id <- NULL
  season <- round <- overall <- pick <- horizon <- mean_val <- med_val <- NULL
  p10_val <- p90_val <- emp_ev <- NULL

  qb_format <- if (format == "auto") {
    .ffs_detect_qb_format(base_simulation$lineup_constraints)
  } else format

  if (is.null(picks)) {
    if (is.null(conn)) cli::cli_abort("Supply either {.arg picks} or {.arg conn}.")
    picks <- ffs_draftpicks(conn)
  }
  picks <- data.table::as.data.table(picks)
  if (!nrow(picks)) return(.ffs_empty_pick_values())
  if (is.null(sim_season)) sim_season <- min(picks$season) - 1L

  # empirical curve (for skew + the edge column)
  pc <- if (is.character(pick_curve)) {
    if (!file.exists(pick_curve)) cli::cli_abort("pick_curve file not found: {.path {pick_curve}}")
    data.table::fread(pick_curve)
  } else data.table::as.data.table(pick_curve)
  pc <- pc[format == qb_format & horizon == 1L][order(pick)]
  emp_f  <- stats::approxfun(pc$pick, pc$mean_val, rule = 2)
  med_r  <- stats::approxfun(pc$pick, pc$med_val / pmax(pc$mean_val, 1), rule = 2)
  p10_r  <- stats::approxfun(pc$pick, pc$p10_val / pmax(pc$mean_val, 1), rule = 2)
  p90_r  <- stats::approxfun(pc$pick, pc$p90_val / pmax(pc$mean_val, 1), rule = 2)

  # market pick curve + year discounts
  if (is.null(fc_pick_values)) {
    fc_pick_values <- tryCatch(
      fc_dynasty_values(num_qbs = if (qb_format == "superflex") 2L else 1L),
      error = function(e) { cli::cli_warn("Couldn't scrape FantasyCalc pick values ({conditionMessage(e)})."); NULL })
  }
  fcpc <- if (!is.null(fc_pick_values)) .ffs_fc_pick_curve(fc_pick_values) else NULL
  # base at-conversion value B(overall): market where available, else empirical
  Bfun <- if (!is.null(fcpc) && !is.na(fcpc$B(1))) fcpc$B else emp_f
  # scale the empirical curve onto the market scale so emp_ev is comparable
  emp_scale <- if (!is.null(fcpc) && !is.na(fcpc$B(1)) && emp_f(1) > 0) fcpc$B(1) / emp_f(1) else 1

  # year discount: disc(k) = value of a mid-first k years out / one year out.
  # disc(1) = 1 (soonest future draft). Falls back to a mild 0.9/yr decay.
  disc <- function(k) {
    k <- pmax(k, 1L)
    if (!is.null(fcpc) && length(fcpc$fut) >= 2) {
      yrs <- as.integer(names(fcpc$fut))
      base_yr <- min(yrs)                       # soonest future first (~sim+1)
      v_base  <- fcpc$fut[[as.character(base_yr)]]
      vapply(k, function(kk) {
        y <- base_yr + (kk - 1L)
        v <- fcpc$fut[[as.character(y)]]
        if (is.null(v) || is.na(v)) v <- v_base * 0.9^(kk - 1L)   # extrapolate
        v / v_base
      }, numeric(1))
    } else 0.9^(k - 1L)
  }

  # finish distribution -> per-origin-team distribution over slot value
  dist <- .ffs_finish_distribution(base_simulation)
  n_teams <- dist$n_teams[1]

  fr <- data.table::as.data.table(base_simulation$rosters)
  fr <- unique(fr[, list(franchise_id = as.character(franchise_id), franchise_name)])

  # expand each pick over its origin team's finish distribution
  px <- merge(picks[, list(season, round = as.integer(round),
                           franchise_id, original_franchise_id)],
              dist[, list(original_franchise_id = franchise_id, lg_rank, prob)],
              by = "original_franchise_id", allow.cartesian = TRUE)
  # inverse final standings: worst finish (lg_rank = n_teams) picks first (slot 1)
  px[, slot_in_round := n_teams - lg_rank + 1L]
  px[, overall := (round - 1L) * n_teams + slot_in_round]
  px[, years_until := pmax(season - sim_season, 1L)]

  px[, `:=`(
    B_at   = Bfun(overall),
    emp_at = emp_scale * emp_f(overall)
  )]
  px[, `:=`(
    cur_contrib  = prob * B_at * disc(years_until),
    next_contrib = prob * B_at * disc(years_until - 1L),
    exp_contrib  = prob * overall,
    emp_contrib  = prob * emp_at
  )]

  pick_id <- function(s, r, of) sprintf("PICK_%d_%d_%s", s, r, of)
  out <- px[, list(
    cur_value       = sum(cur_contrib),
    next_value_mean = sum(next_contrib),
    exp_pick        = sum(exp_contrib),
    emp_ev          = sum(emp_contrib)
  ), by = list(season, round, franchise_id, original_franchise_id)]

  # median / tail skew from the empirical hit-rate curve at the expected slot
  out[, `:=`(
    next_value_med = next_value_mean * med_r(exp_pick),
    next_value_p10 = next_value_mean * p10_r(exp_pick),
    next_value_p90 = next_value_mean * p90_r(exp_pick)
  )]
  out <- merge(out, fr, by = "franchise_id", all.x = TRUE)

  out[, `:=`(
    fantasypros_id = pick_id(season, round, original_franchise_id),
    player_id      = pick_id(season, round, original_franchise_id),
    player_name    = sprintf("%d R%d (%s)", season, round,
                             ifelse(round == 1L,
                                    sprintf("~1.%02.0f", pmin(pmax(round(exp_pick), 1), 99)),
                                    sprintf("~pick %.0f", round(exp_pick)))),
    pos            = "PICK",
    age            = NA_real_,
    redraft_rank   = NA_integer_,
    fp_dyn_rank    = NA_integer_,
    dyn_rank       = NA_integer_,
    n_sims         = NA_integer_,
    p_rise         = as.numeric(next_value_mean > cur_value),
    p_exit         = 0
  )]

  data.table::setcolorder(out, c(
    "fantasypros_id", "player_id", "player_name", "pos",
    "franchise_id", "franchise_name", "redraft_rank", "fp_dyn_rank",
    "dyn_rank", "age", "n_sims", "cur_value",
    "next_value_mean", "next_value_med", "next_value_p10", "next_value_p90",
    "p_rise", "p_exit", "season", "round", "exp_pick", "emp_ev",
    "original_franchise_id"))
  as.data.frame(out[order(-cur_value)])
}

#' @keywords internal
.ffs_empty_pick_values <- function() {
  data.frame(
    fantasypros_id = character(), player_id = character(), player_name = character(),
    pos = character(), franchise_id = character(), franchise_name = character(),
    redraft_rank = integer(), fp_dyn_rank = integer(), dyn_rank = integer(),
    age = numeric(), n_sims = integer(), cur_value = numeric(),
    next_value_mean = numeric(), next_value_med = numeric(),
    next_value_p10 = numeric(), next_value_p90 = numeric(),
    p_rise = numeric(), p_exit = numeric(), season = integer(), round = integer(),
    exp_pick = numeric(), emp_ev = numeric(), original_franchise_id = character())
}
