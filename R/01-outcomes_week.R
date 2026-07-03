#' Connects ff_scoringhistory to past ADP rankings
#'
#' The backbone of the ffsimulator resampling process is coming up with a population of weekly outcomes for every inseason weekly rank. This function creates that dataframe by connecting historical FantasyPros.com rankings to nflfastR-based scoring data, as created by `ffscrapr::ff_scoringhistory()`.
#'
#' @param scoring_history a scoring history table as created by `ffscrapr::ff_scoringhistory()`
# @param gp_model either "simple" or "none" - simple uses the average games played per season for each position/adp combination, none assumes every game is played.
#' @param pos_filter a character vector: filter the positions returned to these specific positions, default: c("QB","RB","WR","TE)
#' @param bye_adjust a logical (default FALSE): rescale weekly ranks to full-slate equivalents so rank N means the same thing in heavy-bye and no-bye weeks; see `.ff_bye_adjust_rank()`
#'
#' @return a dataframe with position, rank, probability of games played, and a corresponding nested list per row of all week score outcomes.
#'
#' @examples
#' \donttest{
#' # cached data
#' scoring_history <- .ffs_cache_example("mfl_scoring_history.rds")
#' ffs_adp_outcomes_week(scoring_history, pos_filter = c("QB", "RB", "WR", "TE"))
#' }
#'
#' @seealso `fp_rankings_history_week` for the included historical rankings
#'
#' @export
ffs_adp_outcomes_week <- function(scoring_history,
                                  pos_filter = c("QB", "RB", "WR", "TE"),
                                  bye_adjust = FALSE) {
  # ASSERTIONS #
  assert_character(pos_filter)
  assert_df(scoring_history, c("gsis_id", "week", "season", "points"))
  checkmate::assert_flag(bye_adjust)

  gsis_id <- NULL
  fantasypros_id <- NULL
  pos <- NULL
  rank <- NULL
  points <- NULL
  week <- NULL
  week_outcomes <- NULL
  player_name <- NULL
  fantasypros_id <- NULL
  len <- NULL
  season <- NULL
  games_played <- NULL

  sh <- data.table::as.data.table(scoring_history)[!is.na(gsis_id) & week <= 16, c("gsis_id", "week", "season", "points")]
  fp_rh <- data.table::as.data.table(fp_rankings_history_week())[, -"page_pos"]
  if (bye_adjust) fp_rh <- .ff_bye_adjust_rank(fp_rh)
  dp_id <- data.table::as.data.table(ffscrapr::dp_playerids())[!is.na(gsis_id) & !is.na(fantasypros_id), c("fantasypros_id", "gsis_id")]

  ao <- fp_rh[
    dp_id
    , on = "fantasypros_id"
    , nomatch = 0
  ][
    !is.na(gsis_id) & pos %in% pos_filter
  ][
    sh
    , on = c("season", "week", "gsis_id")
    , nomatch = 0
  ][
    , list(week_outcomes = list(points), games_played = .N)
    , by = c("season", "pos", "rank", "fantasypros_id", "player_name")
  ][
    , list(
      season = rep(season, each = 5),
      pos = rep(pos, each = 5),
      fantasypros_id = rep(fantasypros_id, each = 5),
      player_name = rep(player_name, each = 5),
      games_played = rep(games_played, each = 5),
      week_outcomes = rep(week_outcomes, each = 5),
      rank = unlist(lapply(rank, .ff_rank_expand))
    )
  ][
    , list(week_outcomes = list(c(unlist(week_outcomes))),
           player_name = list(player_name),
           fantasypros_id = list(fantasypros_id)
    ),
    by = c("pos", "rank")
  ][
    , len := sapply(week_outcomes, length)
  ][
    , len := max(len) - len
    , by = "pos"
  ][
    , `:=`(
      week_outcomes = mapply(.ff_rep_na, week_outcomes, len, SIMPLIFY = FALSE),
      avg_week = sapply(week_outcomes, mean, na.rm = TRUE),
      len = NULL
    )
  ][
    order(pos, rank)
  ]

  return(ao)
}

.ff_rep_na <- function(week_outcomes, len) {
  c(unlist(week_outcomes), rep(NA, times = len))
}

#' Rescale weekly ranks to full-slate equivalents
#'
#' Weekly FantasyPros rankings only include players with a game, so rank N in
#' a heavy-bye week reflects a shallower pool than rank N in a full week
#' (empirically this matters from roughly rank 25 down). Rescales each week's
#' ranks by n_teams / n_teams_playing, inferring byes from the rankings
#' themselves: a team is on bye in a week where none of its players are ranked.
#'
#' @param fp_rh a weekly rankings history dataframe with season, week, team, rank
#' @keywords internal
.ff_bye_adjust_rank <- function(fp_rh) {
  season <- week <- team <- rank <- n_teams <- n_playing <- bye_scale <- NULL

  fp_rh <- data.table::as.data.table(fp_rh)
  team_weeks <- unique(fp_rh[!is.na(team) & team != "FA", c("season", "week", "team")])
  n_szn <- team_weeks[, list(n_teams = data.table::uniqueN(team)), by = "season"]
  n_wk <- team_weeks[, list(n_playing = data.table::uniqueN(team)), by = c("season", "week")]

  scales <- merge(n_wk, n_szn, by = "season")
  # cap at 8 teams out so sparse scrape weeks can't blow the scale up
  scales[, bye_scale := n_teams / pmax(n_playing, n_teams - 8)]

  fp_rh <- merge(fp_rh, scales[, c("season", "week", "bye_scale")],
                 by = c("season", "week"), all.x = TRUE)
  fp_rh[is.na(bye_scale), bye_scale := 1]
  fp_rh[, `:=`(rank = as.integer(round(rank * bye_scale)), bye_scale = NULL)]

  return(fp_rh[])
}
