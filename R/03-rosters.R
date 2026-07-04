#### Get Rosters ####

#' Get Rosters
#'
#' This function lightly wraps `ffscrapr::ff_rosters()` and adds fantasypros_id, which is a required column for ffsimulator.
#'
#' @param conn a connection object as created by `ffscrapr::ff_connect()` and friends.
#'
#' @return a dataframe of rosters that includes a fantasypros_id column
#'
#' @examples
#' \donttest{
#' # cached examples
#' conn <- .ffs_cache_example("mfl_conn.rds")
#'
#' try({ # prevents CRAN connectivity issues, not actually required in normal usage
#'   ffs_rosters(conn)
#' })
#' }
#'
#' @seealso vignette("custom") for more detailed example usage
#'
#' @export
ffs_rosters <- function(conn) {
  UseMethod("ffs_rosters")
}

#' Backfill missing fantasypros_ids by name + position
#'
#' `dp_playerids()` lags behind for new rookie classes, which silently drops
#' every rostered rookie from the simulation (they exist in the FantasyPros
#' rankings but their platform id cannot be crosswalked). Backfills
#' `fantasypros_id` on rosters by unambiguous cleaned-name + position match
#' against the scraped rankings.
#'
#' @param rosters a rosters dataframe (`ffs_rosters()`)
#' @param latest_rankings a rankings dataframe (`ffs_latest_rankings()`)
#'
#' @return the rosters dataframe with fantasypros_id filled where matchable
#'
#' @export
ffs_backfill_fp_ids <- function(rosters, latest_rankings) {
  fantasypros_id <- player_name <- player <- pos <- clean_name <- NULL

  rosters <- data.table::as.data.table(rosters)
  missing <- is.na(rosters$fantasypros_id)
  if (!any(missing)) return(rosters[])

  lookup <- data.table::as.data.table(latest_rankings)[
    , list(clean_name = nflreadr::clean_player_names(player), pos, fantasypros_id)
  ]
  # only unambiguous name+pos matches
  lookup <- lookup[, if (.N == 1) .SD, by = c("clean_name", "pos")]

  rosters[, clean_name := nflreadr::clean_player_names(player_name)]
  rosters[
    lookup,
    on = c("clean_name", "pos"),
    fantasypros_id := data.table::fifelse(is.na(fantasypros_id), i.fantasypros_id, fantasypros_id)
  ]
  n_fixed <- sum(missing) - sum(is.na(rosters$fantasypros_id))
  if (n_fixed > 0) {
    cli::cli_alert_info("Backfilled fantasypros_id for {n_fixed} rostered player{?s} not in dp_playerids (likely rookies)")
  }
  rosters[, clean_name := NULL]

  return(rosters[])
}

#' @rdname ffs_rosters
#' @export
ffs_rosters.mfl_conn <- function(conn) {
  r <- ffscrapr::ff_rosters(conn)

  r$player_id <- as.character(r$player_id)

  r <- merge(r,
             ffscrapr::dp_playerids()[, c("mfl_id", "fantasypros_id")],
             by.x = "player_id",
             by.y = "mfl_id",
             all.x = TRUE)
  r$league_id <- as.character(conn$league_id)
  r$franchise_id <- as.character(r$franchise_id)

  return(r)
}

#' @rdname ffs_rosters
#' @export
ffs_rosters.sleeper_conn <- function(conn) {
  r <- ffscrapr::ff_rosters(conn)

  r$player_id <- as.character(r$player_id)

  r <- merge(r,
             ffscrapr::dp_playerids()[, c("sleeper_id", "fantasypros_id")],
             by.x = "player_id",
             by.y = "sleeper_id",
             all.x = TRUE)
  r$league_id <- as.character(conn$league_id)
  r$franchise_id <- as.character(r$franchise_id)

  return(r)
}

#' @rdname ffs_rosters
#' @export
ffs_rosters.flea_conn <- function(conn) {
  r <- ffscrapr::ff_rosters(conn)

  r$player_id <- as.character(r$player_id)

  r <- merge(r,
             ffscrapr::dp_playerids()[, c("sportradar_id", "fantasypros_id")],
             by.x = "sportradar_id",
             by.y = "sportradar_id",
             all.x = TRUE)
  r$league_id <- as.character(conn$league_id)
  r$franchise_id <- as.character(r$franchise_id)

  return(r)
}

#' @rdname ffs_rosters
#' @export
ffs_rosters.espn_conn <- function(conn) {
  r <- ffscrapr::ff_rosters(conn)

  r$player_id <- as.character(r$player_id)

  r <- merge(r,
             ffscrapr::dp_playerids()[, c("espn_id", "fantasypros_id")],
             by.x = "player_id",
             by.y = "espn_id",
             all.x = TRUE)
  r$league_id <- as.character(conn$league_id)
  r$franchise_id <- as.character(r$franchise_id)

  return(r)
}


#' @noRd
#' @export
ffs_rosters.default <- function(conn) {
  # nocov start
  stop(glue::glue("Could not find a method of <ff_rosters> for class {class(conn)} - was this created by ff_connect()?"),
       call. = FALSE
  )
  # nocov end
}

#' Get Franchises
#'
#' This function lightly wraps `ffscrapr::ff_franchises()` and adds league_id, which is a required column for ffsimulator.
#'
#' @param conn a connection object as created by `ffscrapr::ff_connect()` and friends.
#'
#' @return a dataframe of franchises that includes the league_id column
#'
#' @examples
#' \donttest{
#' # cached examples
#' conn <- .ffs_cache_example("mfl_conn.rds")
#'
#' try({ # prevents CRAN connectivity issues, not actually required in normal usage
#' ffs_franchises(conn)
#' })
#' }
#'
#' @seealso vignette("Custom Simulations") for more detailed example usage
#'
#' @export
ffs_franchises <- function(conn) {
  f <- ffscrapr::ff_franchises(conn)
  f$league_id <- as.character(conn$league_id)
  f$franchise_id <- as.character(f$franchise_id)

  return(f)
}
