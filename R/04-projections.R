#' Generate Projections
#'
#' Runs the bootstrapped resampling of player week outcomes on the latest rankings and rosters for a given number of seasons and weeks per season.
#'
#' @param adp_outcomes a dataframe of adp-based weekly outcomes, as created by `ffs_adp_outcomes()`
#' @param latest_rankings a dataframe of rankings, as created by `ffs_latest_rankings()`
#' @param rosters a dataframe of rosters, as created by `ffs_rosters()` - optional, reduces computation to just rostered players
#' @param n_seasons number of seasons, default is 100
#' @param weeks a numeric vector of weeks to simulate, defaults to 1:14
#' @param version projection method: "v2" (default) samples weekly ranks iid from the draft-rank crosswalk, "v1" samples scores directly by preseason rank, "v3" (experimental) resamples whole historical weekly-rank trajectories to preserve within-season correlation
#'
#' @examples \donttest{
#' # cached examples
#' adp_outcomes <- .ffs_cache_example("adp_outcomes.rds")
#' latest_rankings <- .ffs_cache_example("latest_rankings.rds")
#'
#' ffs_generate_projections(adp_outcomes, latest_rankings)
#' }
#'
#' @seealso vignette("custom") for example usage
#'
#' @return a dataframe of weekly scores for each player in the simulation, approximately of length n_seasons x n_weeks x latest_rankings
#' @export
ffs_generate_projections <- function(adp_outcomes,
                                     latest_rankings,
                                     n_seasons = 100,
                                     weeks = 1:14,
                                     version = c("v2","v1","v3"),
                                     rosters = NULL
) {
  version <- rlang::arg_match0(version,values = c("v2","v1","v3"))
  checkmate::assert_number(n_seasons, lower = 1)
  checkmate::assert_numeric(weeks, lower = 1, min.len = 1)
  assert_df(adp_outcomes, c("pos", "rank", "prob_gp", "week_outcomes"))
  assert_df(latest_rankings, c("player", "pos", "team", "ecr", "sd", "bye", "fantasypros_id", "scrape_date"))

  weeks <- unique(weeks)
  n_weeks <- length(weeks)

  adp_outcomes <- data.table::as.data.table(adp_outcomes)[
    , c("pos", "rank", "avg_week", "prob_gp", "week_outcomes")
  ]
  latest_rankings <- data.table::as.data.table(latest_rankings)[
    , c("player", "pos", "team", "ecr", "sd", "bye", "fantasypros_id", "scrape_date")
  ]

  if (is.null(rosters)) rosters <- latest_rankings[, "fantasypros_id"]
  assert_df(rosters, "fantasypros_id")
  rosters <- data.table::as.data.table(rosters)

  .ffs_projections <- switch(
    version,
    "v2" = .ffs_projections_v2,
    "v1" = .ffs_projections_v1,
    "v3" = .ffs_projections_v3
  )

  ps <- .ffs_projections(
    adp_outcomes = adp_outcomes,
    latest_rankings = latest_rankings,
    n_seasons = n_seasons,
    weeks = weeks,
    n_weeks = n_weeks,
    rosters = rosters
  )

  return(ps)
}

.ffs_projections_v1 <- function(adp_outcomes, latest_rankings, n_seasons, weeks, n_weeks, rosters){

  rankings <- latest_rankings[
    latest_rankings$fantasypros_id %in% rosters$fantasypros_id
  ][
    ,
    list(
      scrape_date = rep(.SD$scrape_date),
      player = rep(.SD$player),
      pos = rep(.SD$pos),
      team = rep(.SD$team),
      bye = rep(.SD$bye),
      ecr = rep(.SD$ecr),
      sd = rep(.SD$sd),
      season = seq_len(n_seasons),
      rank = stats::rnorm(n = n_seasons, mean = .SD$ecr, sd = .SD$sd / 2) %>%
        round() %>%
        .replace_zero()
    ),
    by = "fantasypros_id"
  ]

  ps <- merge(rankings, adp_outcomes, by = c("pos", "rank"))
  ps <- ps[
    !is.na(ps$ecr) & !is.na(ps$prob_gp)
  ][
    # ranks with no week-outcome population (e.g. injury-table ranks beyond
    # the training data) cannot be sampled - drop instead of crashing sample()
    !sapply(week_outcomes, is.null)
  ][
    , list(
      week = weeks,
      projection = as.numeric(sample(x = .SD$week_outcomes[[1]], size = n_weeks, replace = TRUE)),
      gp_model = stats::rbinom(n = n_weeks, size = 1, prob = .SD$prob_gp)
    ),
    by = c("season", "fantasypros_id", "player", "pos", "team", "bye", "ecr", "sd", "rank", "scrape_date"),
    .SDcols = c("week_outcomes", "prob_gp")
  ]

  ps <- ps[
    ,
    `:=`(
      projected_score = ps$projection * ps$gp_model * (ps$week != ps$bye)
    )
  ]
  return(ps[])
}

.ffs_projections_v2 <- function(adp_outcomes, latest_rankings, n_seasons, weeks, n_weeks, rosters){

  draft_rankings <- latest_rankings[
    latest_rankings$fantasypros_id %in% rosters$fantasypros_id
  ][
    ,
    list(
      scrape_date = rep(.SD$scrape_date),
      player = rep(.SD$player),
      pos = rep(.SD$pos),
      team = rep(.SD$team),
      bye = rep(.SD$bye),
      ecr = rep(.SD$ecr),
      sd = rep(.SD$sd),
      season = seq_len(n_seasons),
      draft_rank = .replace_zero(round(stats::rnorm(n = n_seasons, mean = .SD$ecr, sd = .SD$sd / 2)))
    ),
    by = "fantasypros_id"
  ]

  week_ranks <- merge(
    draft_rankings,
    .ffs_draft_to_week()[,c("pos", "draft_rank", "week_rank")],
    by = c("pos", "draft_rank"),
    all.x = TRUE
  )[
    !is.na(ecr)
  ][
    # draft ranks with no historical weekly-rank population (no crosswalk
    # match) cannot be sampled - drop them here instead of crashing sample()
    !sapply(week_rank, is.null)
  ][
    , list(
      week = weeks,
      week_rank = as.numeric(sample(x = .SD$week_rank[[1]], size = n_weeks, replace = TRUE))
    ),
    by = c("season", "fantasypros_id", "player", "pos", "team", "bye", "ecr", "sd", "draft_rank", "scrape_date")
  ]

  projected_scores <- merge(
    week_ranks,
    adp_outcomes[, list(pos, week_rank = rank, avg_week, prob_gp, week_outcomes)],
    by = c("pos", "week_rank"),
    all.x = TRUE
  )[
    !is.na(prob_gp)
  ][
    , list(
      season, week,
      pos, scrape_date, fantasypros_id, player, team, bye, ecr, sd,
      draft_rank, week_rank, avg_week, prob_gp,
      gp_model = sapply(prob_gp, function(p) stats::rbinom(n = 1, size = 1, p = p)),
      projection = sapply(week_outcomes, function(x) {if(length(x) == 0) return(0) else sample(x = x, size = 1)})
    )
  ][
    ,
    `:=`(
      projected_score = projection * gp_model * (week != bye)
    )
  ][
    order(season, week, pos, ecr)
  ]

  return(projected_scores[])
}

.ffs_projections_v3 <- function(adp_outcomes, latest_rankings, n_seasons, weeks, n_weeks, rosters){

  season <- fantasypros_id <- player <- pos <- team <- bye <- ecr <- draft_rank <- NULL
  week <- week_rank <- week_outcomes <- projection <- gp_model <- projected_score <- scrape_date <- NULL
  trajectories <- NULL

  draft_rankings <- latest_rankings[
    latest_rankings$fantasypros_id %in% rosters$fantasypros_id
  ][
    ,
    list(
      scrape_date = rep(.SD$scrape_date),
      player = rep(.SD$player),
      pos = rep(.SD$pos),
      team = rep(.SD$team),
      bye = rep(.SD$bye),
      ecr = rep(.SD$ecr),
      sd = rep(.SD$sd),
      season = seq_len(n_seasons),
      draft_rank = .replace_zero(round(stats::rnorm(n = n_seasons, mean = .SD$ecr, sd = .SD$sd / 2)))
    ),
    by = "fantasypros_id"
  ]

  week_ranks <- merge(
    draft_rankings,
    .ffs_draft_to_week_trajectories(),
    by = c("pos", "draft_rank"),
    all.x = TRUE
  )[
    !is.na(ecr)
  ][
    # draft ranks beyond the historical draft-rank range have no trajectory
    # population to resample from
    !sapply(trajectories, is.null)
  ][
    , list(
      week = weeks,
      # one whole historical player-season of weekly ranks: within-season
      # autocorrelation (busts/breakouts/injuries persist across weeks) and
      # never-ranked weeks stay in the data as NA (= zero points)
      week_rank = {
        pool <- .SD$trajectories[[1]]
        as.numeric(pool[[sample.int(length(pool), 1)]][weeks])
      }
    ),
    by = c("season", "fantasypros_id", "player", "pos", "team", "bye", "ecr", "sd", "draft_rank", "scrape_date")
  ]

  projected_scores <- merge(
    week_ranks,
    adp_outcomes[, list(pos, week_rank = rank, avg_week, week_outcomes)],
    by = c("pos", "week_rank"),
    all.x = TRUE
  )[
    ,
    `:=`(
      # availability is encoded by the trajectory itself (unranked week = NA
      # = zero), so no separate games-played model is applied
      gp_model = as.integer(!is.na(week_rank)),
      projection = sapply(week_outcomes, function(x) {
        x <- x[!is.na(x)]
        if (length(x) == 0) return(0)
        sample(x = x, size = 1)
      })
    )
  ][
    ,
    `:=`(
      projected_score = projection * gp_model * (week != bye),
      week_outcomes = NULL
    )
  ][
    order(season, week, pos, ecr)
  ]

  return(projected_scores[])
}

.replace_zero <- function(x) {
  replace(x, x == 0, 1)
}

#' Build draft-rank keyed pools of historical weekly-rank trajectories
#'
#' For every preseason-ranked player-season, records the full sequence of
#' weekly FantasyPros ranks over that season's non-bye weeks (NA where the
#' player went unranked). Pools these trajectories by position and draft rank,
#' so a simulated season can resample one coherent historical season rather
#' than independent weeks. Player-seasons with zero weekly rankings are
#' retained as all-NA trajectories, pricing in flameout/irrelevance risk.
#'
#' Pool width around each draft rank is controlled by
#' `options(ffsimulator.v3_bandwidth = c(QB = 4, RB = 6, ...))`: a triangular
#' kernel replicates each trajectory into nearby draft ranks with weight
#' declining in rank distance (bandwidth = the distance at which weight hits
#' zero). Positions not named in the option fall back to bandwidth 2. The
#' defaults were tuned by held-out backtest (dev/validate_projections.md) -
#' QB needs wide pools (elite QB seasons are scarce), TE narrow ones (steep
#' quality cliff). Set the option to `NA` to use the legacy hard +/-2 uniform
#' window (`.ff_rank_expand()`).
#'
#' @keywords internal
.ffs_draft_to_week_trajectories <- function(max_week = 16,
                                            bandwidth = getOption(
                                              "ffsimulator.v3_bandwidth",
                                              c(QB = 11, RB = 7, WR = 7, TE = 4)
                                            )){
  if (length(bandwidth) == 1 && is.na(bandwidth)) bandwidth <- NULL

  season <- fantasypros_id <- player_name <- pos <- team <- rank <- week <- NULL
  draft_rank <- week_rank <- trajectory <- NULL

  draft <- data.table::as.data.table(fp_rankings_history())[
    , list(season, fantasypros_id, pos, team, draft_rank = rank)
  ]

  # bye-adjusted so trajectory ranks share units with the bye-adjusted
  # outcome pools built by ffs_adp_outcomes(version = "v3")
  wk <- .ff_bye_adjust_rank(fp_rankings_history_week())[
    week <= max_week,
    list(season, week, fantasypros_id, team, week_rank = rank)
  ]

  # infer team bye weeks from the weekly rankings themselves: a team's bye is
  # a week in which none of its players are ranked
  team_weeks <- unique(wk[!is.na(team) & team != "FA", list(season, team, week)])
  teams <- unique(team_weeks[, list(season, team)])
  byes <- teams[
    , list(week = seq_len(max_week)), by = list(season, team)
  ][
    !team_weeks, on = c("season", "team", "week")
  ]

  player_weeks <- draft[
    , list(week = seq_len(max_week)),
    by = list(season, fantasypros_id, pos, team, draft_rank)
  ][
    !byes, on = c("season", "team", "week")
  ]

  trajectories <- merge(
    player_weeks,
    wk[, list(season, week, fantasypros_id, week_rank)],
    by = c("season", "fantasypros_id", "week"),
    all.x = TRUE
  )[
    order(week),
    list(trajectory = list(week_rank)),
    by = list(season, fantasypros_id, pos, draft_rank)
  ]

  if (is.null(bandwidth)) {
    # historical behavior: uniform hard +/-2 window
    expanded <- trajectories[
      , list(
        pos = rep(pos, each = 5),
        trajectory = rep(trajectory, each = 5),
        draft_rank = unlist(lapply(draft_rank, .ff_rank_expand))
      )
    ]
  } else {
    # triangular kernel: copies = round(4 * (1 - |offset| / h)), per position
    offset <- copies <- h <- NULL
    bw <- unlist(bandwidth)
    kernel <- data.table::rbindlist(lapply(
      unique(trajectories$pos),
      function(p) {
        h <- if (is.null(names(bw))) bw[[1]] else if (p %in% names(bw)) bw[[p]] else 2
        off <- seq.int(-ceiling(h) + 1L, ceiling(h) - 1L)
        data.table::data.table(pos = p, offset = off,
                               copies = pmax(1L, as.integer(round(4 * (1 - abs(off) / h)))))
      }
    ))
    expanded <- merge(trajectories, kernel, by = "pos", allow.cartesian = TRUE)
    expanded <- expanded[rep(seq_len(.N), copies)]
    expanded[, `:=`(draft_rank = pmax(1L, draft_rank + offset), offset = NULL, copies = NULL)]
  }

  pools <- expanded[
    , list(trajectories = list(trajectory)),
    by = list(pos, draft_rank)
  ][
    order(pos, draft_rank)
  ]

  return(pools)
}

.ffs_draft_to_week <- function(){

  season <- fantasypros_id <- player_name <- pos <- rank <- ecr <- sd <- NULL
  week <- draft_rank <- draft_ecr <- draft_sd <- week_rank <- week_ecr <- week_sd <- NULL

  draft_rank <- data.table::as.data.table(fp_rankings_history())[
    , list(
      season,
      fantasypros_id,
      player_name,
      pos,
      draft_rank = rank,
      draft_ecr = ecr,
      draft_sd = sd
    )
  ]

  week_rank <- data.table::as.data.table(fp_rankings_history_week())[
    , list(
      season,
      week,
      fantasypros_id,
      player_name,
      pos,
      week_rank = rank,
      week_ecr = ecr,
      week_sd = sd
    )
  ]

  draft_week_rank <- merge(
    draft_rank,
    week_rank,
    by = c("season", "fantasypros_id", "player_name", "pos"),
    all.x = TRUE
  )[
    order(season, pos, draft_rank)
  ][
    , list(week_rank = list(week_rank),
           player = list(player_name))
    , by = list(pos, draft_rank)
  ]

  return(draft_week_rank)
}
