#' Live rest-of-season rankings, scraped from FantasyPros
#'
#' (EXPERIMENTAL) [ffs_latest_rankings()] reads DynastyProcess's mirror, which
#' is a scrape of a scrape and can sit several days behind - stale enough to
#' miss the waiver-relevant news of the week. This hits FantasyPros directly
#' through `ffpros` for the rest-of-season pages, and carries back two fields
#' the mirror drops: `roster_pct` (how much of the market already owns him) and
#' `ecr_delta` (how far he has moved in the rankings).
#'
#' Falls back to [ffs_latest_rankings()] when `ffpros` is missing or the scrape
#' fails, so a caller always gets rankings - check `scrape_date` to see which.
#'
#' @param season the season to scrape
#' @param ppr points per reception: 1, 0.5 or 0 - picks the scoring variant of
#'   each positional page
#' @param pos_filter positions to scrape
#'
#' @return a dataframe: `fantasypros_id`, `player`, `pos`, `team`, `bye`, `ecr`,
#'   `sd`, `pos_rank`, `roster_pct`, `ecr_delta`, `scrape_date`, `source`
#' @export
ffs_ros_rankings <- function(season = nflreadr::most_recent_season(),
                             ppr = 1,
                             pos_filter = c("QB", "RB", "WR", "TE")) {
  pos <- ecr <- pos_rank <- NULL

  scoring <- if (ppr >= 1) "ppr-" else if (ppr > 0) "half-point-ppr-" else ""
  # QB, K and DST have no scoring variants; the skill positions do
  page_of <- function(p) {
    lp <- tolower(p)
    if (lp %in% c("qb", "k", "dst")) paste0("ros-", lp) else paste0("ros-", scoring, lp)
  }

  fallback <- function(why) {
    cli::cli_alert_warning("live ROS scrape unavailable ({why}) - using {.fn ffs_latest_rankings}")
    lr <- data.table::as.data.table(ffs_latest_rankings(type = "draft"))
    lr <- lr[pos %in% pos_filter]
    lr[, `:=`(roster_pct = NA_real_, ecr_delta = NA_real_, source = "dynastyprocess")]
    data.table::setorder(lr, pos, ecr)
    lr[, pos_rank := seq_len(.N), by = "pos"]
    lr[, list(fantasypros_id = as.character(fantasypros_id), player, pos, team,
              bye = get("bye"), ecr, sd = get("sd"), pos_rank,
              roster_pct, ecr_delta, scrape_date = get("scrape_date"), source)]
  }

  if (!rlang::is_installed("ffpros")) return(fallback("ffpros not installed"))

  rows <- lapply(pos_filter, function(p) {
    x <- tryCatch(ffpros::fp_rankings(page = page_of(p), year = season),
                  error = function(e) {
                    cli::cli_alert_warning("{page_of(p)}: {conditionMessage(e)}")
                    NULL
                  })
    if (is.null(x) || !NROW(x)) return(NULL)
    x <- data.table::as.data.table(x)
    data.table::data.table(
      fantasypros_id = as.character(x$fantasypros_id),
      player = x$player_name,
      pos = toupper(p),
      team = x$team,
      bye = suppressWarnings(as.integer(x$player_bye_week)),
      ecr = suppressWarnings(as.numeric(x$ecr)),
      sd = suppressWarnings(as.numeric(x$sd)),
      # FantasyPros' own positional rank ("RB20"), kept as an integer
      pos_rank = suppressWarnings(as.integer(sub("^[A-Za-z]+", "", x$pos_rank))),
      roster_pct = suppressWarnings(as.numeric(x$player_owned_avg)),
      ecr_delta = suppressWarnings(as.numeric(x$player_ecr_delta))
    )
  })
  rows <- data.table::rbindlist(Filter(Negate(is.null), rows))
  if (!nrow(rows)) return(fallback("every page failed"))

  rows[is.na(pos_rank), pos_rank := NA_integer_]
  rows[, pos_rank := data.table::fifelse(is.na(pos_rank),
    as.integer(data.table::frank(ecr, ties.method = "first")), pos_rank), by = "pos"]
  rows[, `:=`(scrape_date = Sys.Date(), source = "fantasypros")]
  data.table::setorder(rows, pos, ecr)
  as.data.frame(rows)
}

#' Players the Sleeper universe is adding or dropping
#'
#' (EXPERIMENTAL) Sleeper publishes the most-added and most-dropped players
#' across every league on the platform. It is the fastest-moving signal
#' available - it moves hours after news breaks, long before rankings do - and
#' it is pure market behaviour, so read it as "what everyone else is doing",
#' not as a valuation.
#'
#' @param type `"add"` or `"drop"`
#' @param lookback_hours the window Sleeper aggregates over (default 24)
#' @param limit how many players to return (default 200)
#'
#' @return a dataframe: `fantasypros_id`, `sleeper_id`, `count`
#' @export
ffs_sleeper_trending <- function(type = c("add", "drop"),
                                 lookback_hours = 24L,
                                 limit = 200L) {
  rlang::check_installed(c("httr", "jsonlite"), "to read Sleeper trending players")
  type <- rlang::arg_match(type)
  sleeper_id <- fantasypros_id <- NULL

  url <- sprintf(
    "https://api.sleeper.app/v1/players/nfl/trending/%s?lookback_hours=%d&limit=%d",
    type, as.integer(lookback_hours), as.integer(limit))
  resp <- tryCatch(httr::GET(url), error = function(e) e)
  if (inherits(resp, "error") || httr::status_code(resp) != 200) {
    cli::cli_alert_warning("Sleeper trending/{type} unavailable")
    return(data.frame(fantasypros_id = character(0), sleeper_id = character(0),
                      count = integer(0)))
  }
  raw <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
  if (!NROW(raw)) {
    return(data.frame(fantasypros_id = character(0), sleeper_id = character(0),
                      count = integer(0)))
  }
  dt <- data.table::data.table(sleeper_id = as.character(raw$player_id),
                               count = as.integer(raw$count))

  ids <- data.table::as.data.table(ffscrapr::dp_playerids())
  ids <- unique(ids[!is.na(sleeper_id) & !is.na(fantasypros_id),
                    list(sleeper_id = as.character(sleeper_id),
                         fantasypros_id = as.character(fantasypros_id))],
                by = "sleeper_id")
  out <- merge(dt, ids, by = "sleeper_id", all.x = TRUE)
  data.table::setorderv(out, "count", -1L)
  as.data.frame(out[, list(fantasypros_id, sleeper_id, count)])
}

#' Weekly waiver watchlist for a league
#'
#' (EXPERIMENTAL) Every ranked player in a league, free agent or rostered, with
#' each signal that says something different about him:
#'
#' - **rankings** (`ecr`, `pos_rank`, `ecr_delta`): where the analyst consensus
#'   has him for the rest of the season, and which way it is moving
#' - **market** (`roster_pct`, `trend_add`, `trend_drop`): what the rest of
#'   fantasy is doing about him, right now
#' - **usage** (`snap_pct`, `xfp_g`, `fp_g`, `fpoe`): what his team is actually
#'   giving him, which moves first when a role changes
#' - **value** (`redraft_value`, `dynasty_value`): what the trade market pays
#'
#' `signal` averages the available components as percentile ranks **within
#' position**, over the free agents only: consensus rank, expected points, snap
#' share, roster percentage and Sleeper adds. It is a rough ordering to open the
#' table on, not a model - every input is there to be sorted on itself.
#'
#' No simulation is involved, so this is quick enough to run for every league
#' every week. To price a specific add against a specific drop, take the
#' shortlist to [ffs_waiver_report()].
#'
#' @param conn a league connection from `ff_connect()` and friends
#' @param ros rankings from [ffs_ros_rankings()], or `NULL` to fetch them
#' @param usage signals from [ffs_usage_signals()], or `NULL` to skip
#' @param values a FantasyCalc table from [fc_dynasty_values()], or `NULL`
#' @param trending a list with `add` and/or `drop` elements from
#'   [ffs_sleeper_trending()], or `NULL`
#' @param pos_filter positions to include
#' @param my_franchise optional franchise_id: marks your own players `mine`
#'
#' @return a dataframe, one row per ranked player: identity, `status`
#'   (`"free agent"` / `"rostered"`), `rostered_by`, `mine`, and the signal
#'   columns above
#' @export
ffs_waiver_watchlist <- function(conn,
                                 ros = NULL,
                                 usage = NULL,
                                 values = NULL,
                                 trending = NULL,
                                 pos_filter = c("QB", "RB", "WR", "TE"),
                                 my_franchise = NULL) {
  fantasypros_id <- pos <- ecr <- status <- mine <- signal <- NULL
  rostered_by <- franchise_id <- player <- NULL

  if (is.null(ros)) ros <- ffs_ros_rankings(pos_filter = pos_filter)
  r <- data.table::as.data.table(ros)[pos %in% pos_filter & !is.na(ecr)]
  r <- unique(r[!is.na(fantasypros_id)], by = "fantasypros_id")

  rosters <- data.table::as.data.table(ffs_rosters(conn))
  # rookies lag in dp_playerids; match them to the rankings by name
  rosters <- data.table::as.data.table(
    ffs_backfill_fp_ids(rosters, r[, list(player, pos, fantasypros_id)]))
  owned <- unique(rosters[!is.na(fantasypros_id),
                          list(fantasypros_id = as.character(fantasypros_id),
                               rostered_by = get("franchise_name"),
                               owner_id = as.character(franchise_id))],
                  by = "fantasypros_id")

  out <- merge(r, owned, by = "fantasypros_id", all.x = TRUE)
  out[, status := data.table::fifelse(is.na(rostered_by), "free agent", "rostered")]
  mine_ids <- if (is.null(my_franchise)) character(0) else as.character(my_franchise)
  out[, mine := !is.na(get("owner_id")) & get("owner_id") %in% mine_ids]

  if (!is.null(usage)) {
    u <- data.table::as.data.table(usage)
    keep <- intersect(c("fantasypros_id", "games_recent", "snap_pct_recent",
                        "exp_pts_recent", "fpts_recent", "fpoe_recent",
                        "snap_trend", "exp_pts_trend"), names(u))
    u <- unique(u[, keep, with = FALSE], by = "fantasypros_id")
    u[, fantasypros_id := as.character(fantasypros_id)]
    data.table::setnames(u,
      c("games_recent", "snap_pct_recent", "exp_pts_recent", "fpts_recent", "fpoe_recent"),
      c("games", "snap_pct", "xfp_g", "fp_g", "fpoe"), skip_absent = TRUE)
    out <- merge(out, u, by = "fantasypros_id", all.x = TRUE)
  }

  if (!is.null(values)) {
    v <- data.table::as.data.table(values)[!is.na(fantasypros_id)]
    v <- unique(v[, list(fantasypros_id = as.character(fantasypros_id),
                         dynasty_value = get("value"),
                         redraft_value = get("redraft_value"),
                         value_trend_30d = get("trend_30day"))], by = "fantasypros_id")
    out <- merge(out, v, by = "fantasypros_id", all.x = TRUE)
  }

  for (tt in c("add", "drop")) {
    tr <- trending[[tt]]
    if (is.null(tr) || !NROW(tr)) next
    tr <- data.table::as.data.table(tr)[!is.na(fantasypros_id)]
    tr <- unique(tr[, list(fantasypros_id = as.character(fantasypros_id),
                           count = as.integer(get("count")))], by = "fantasypros_id")
    data.table::setnames(tr, "count", paste0("trend_", tt))
    out <- merge(out, tr, by = "fantasypros_id", all.x = TRUE)
  }

  # percentile within position, over free agents only - a signal is about how a
  # player stacks up against what you could actually claim instead of him
  pctl <- function(x, higher_is_better = TRUE) {
    v <- if (higher_is_better) x else -x
    r <- data.table::frank(v, na.last = "keep", ties.method = "average")
    (r - 1) / pmax(1, sum(!is.na(v)) - 1)
  }
  comps <- intersect(c("ecr", "xfp_g", "snap_pct", "roster_pct", "trend_add"), names(out))
  fa <- out$status == "free agent"
  parts <- lapply(comps, function(cc) {
    v <- rep(NA_real_, nrow(out))
    x <- out[[cc]]
    # trending counts are absent for the vast majority - a missing count is a
    # zero, not an unknown; every other input stays NA and drops out of the mean
    if (cc == "trend_add") x <- data.table::fifelse(is.na(x), 0, as.numeric(x))
    for (p in unique(out$pos)) {
      idx <- which(fa & out$pos == p)
      if (length(idx)) v[idx] <- pctl(x[idx], higher_is_better = cc != "ecr")
    }
    v
  })
  m <- do.call(cbind, parts)
  out[, signal := round(rowMeans(m, na.rm = TRUE), 3)]
  out[is.nan(signal), signal := NA_real_]

  data.table::setorderv(out, c("status", "signal"), c(1L, -1L))
  front <- intersect(c("player", "pos", "team", "status", "rostered_by", "mine",
                       "signal", "pos_rank", "ecr", "ecr_delta", "roster_pct",
                       "trend_add", "trend_drop", "snap_pct", "xfp_g", "fp_g",
                       "fpoe", "games", "redraft_value", "dynasty_value",
                       "value_trend_30d", "bye", "sd", "fantasypros_id"), names(out))
  data.table::setcolorder(out, front)
  as.data.frame(out[, c(front, setdiff(names(out), front)), with = FALSE])
}
