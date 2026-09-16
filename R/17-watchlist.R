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
#' @return a dataframe: `fantasypros_id`, `sleeper_id`, `player`, `pos`, `team`,
#'   `count`
#' @export
ffs_sleeper_trending <- function(type = c("add", "drop"),
                                 lookback_hours = 24L,
                                 limit = 200L) {
  rlang::check_installed(c("httr", "jsonlite"), "to read Sleeper trending players")
  type <- rlang::arg_match(type)
  sleeper_id <- fantasypros_id <- player <- pos <- team <- NULL

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
                      player = character(0), pos = character(0),
                      team = character(0), count = integer(0)))
  }
  dt <- data.table::data.table(sleeper_id = as.character(raw$player_id),
                               count = as.integer(raw$count))

  ids <- data.table::as.data.table(ffscrapr::dp_playerids())
  ids <- unique(ids[!is.na(sleeper_id) & !is.na(fantasypros_id),
                    list(sleeper_id = as.character(sleeper_id),
                         fantasypros_id = as.character(fantasypros_id),
                         player = get("name"), pos = get("position"),
                         team = get("team"))],
                by = "sleeper_id")
  out <- merge(dt, ids, by = "sleeper_id", all.x = TRUE)
  data.table::setorderv(out, "count", -1L)
  as.data.frame(out[, list(fantasypros_id, sleeper_id, player, pos, team, count)])
}

#' Sleeper's own projection for the upcoming week
#'
#' (EXPERIMENTAL) The number every manager in the league is looking at when they
#' set a lineup, pulled straight from Sleeper for the week that has not been
#' played yet. It is a projection, not a ranking: it already prices in the
#' matchup, and it is the only signal here that says what a player is expected
#' to do *next Sunday* rather than over the rest of the season.
#'
#' @param season the season
#' @param week the week to project; defaults to the current (upcoming) one
#' @param pos_filter positions to fetch - one request each
#'
#' @return a dataframe: `fantasypros_id`, `sleeper_id`, `player`, `pos`, `team`,
#'   `proj_ppr`, `proj_half_ppr`, `proj_std`, `week`
#' @export
ffs_sleeper_projections <- function(season = nflreadr::most_recent_season(),
                                    week = nflreadr::get_current_week(),
                                    pos_filter = c("QB", "RB", "WR", "TE")) {
  rlang::check_installed(c("httr", "jsonlite"), "to read Sleeper projections")
  sleeper_id <- fantasypros_id <- player <- pos <- team <- proj_ppr <- NULL

  empty <- data.frame(fantasypros_id = character(0), sleeper_id = character(0),
                      player = character(0), pos = character(0), team = character(0),
                      proj_ppr = numeric(0), proj_half_ppr = numeric(0),
                      proj_std = numeric(0), week = integer(0))

  rows <- lapply(pos_filter, function(p) {
    url <- sprintf(
      "https://api.sleeper.app/projections/nfl/%d/%d?season_type=regular&position[]=%s&order_by=ppr",
      as.integer(season), as.integer(week), toupper(p))
    resp <- tryCatch(httr::GET(url), error = function(e) e)
    if (inherits(resp, "error") || httr::status_code(resp) != 200) {
      cli::cli_alert_warning("Sleeper projections unavailable for {p} week {week}")
      return(NULL)
    }
    x <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                            flatten = TRUE)
    if (!NROW(x)) return(NULL)
    x <- data.table::as.data.table(x)
    gv <- function(nm) if (nm %in% names(x)) suppressWarnings(as.numeric(x[[nm]])) else NA_real_
    out <- data.table::data.table(
      sleeper_id = as.character(x$player_id),
      pos = toupper(p),
      team = if ("team" %in% names(x)) as.character(x$team) else NA_character_,
      proj_ppr = gv("stats.pts_ppr"),
      proj_half_ppr = gv("stats.pts_half_ppr"),
      proj_std = gv("stats.pts_std"),
      week = as.integer(week)
    )
    # the endpoint returns every player it has ever heard of at the position;
    # only the ones with a projection are playing
    out[!is.na(proj_ppr)]
  })
  rows <- data.table::rbindlist(Filter(Negate(is.null), rows), use.names = TRUE)
  if (!NROW(rows)) return(empty)

  ids <- data.table::as.data.table(ffscrapr::dp_playerids())
  ids <- unique(ids[!is.na(sleeper_id),
                    list(sleeper_id = as.character(sleeper_id),
                         fantasypros_id = as.character(fantasypros_id),
                         player = get("name"))],
                by = "sleeper_id")
  out <- merge(rows, ids, by = "sleeper_id", all.x = TRUE)
  data.table::setorderv(out, "proj_ppr", -1L)
  as.data.frame(out[, list(fantasypros_id, sleeper_id, player, pos, team,
                           proj_ppr, proj_half_ppr = get("proj_half_ppr"),
                           proj_std = get("proj_std"), week = get("week"))])
}

#' A name-and-position key, for when a FantasyPros id moves
#'
#' FantasyPros re-issues player ids: their live rest-of-season page had Roman
#' Wilson as 28896 while every id map still carried 26160, which silently cost
#' him his snaps, his values and - worse - would have shown a rostered player as
#' a free agent. Every join in the watchlist therefore falls back to this key.
#'
#' @param player,pos player name and position vectors
#' @return a character key
#' @keywords internal
.ffs_pkey <- function(player, pos) {
  paste(nflreadr::clean_player_names(as.character(player), lowercase = TRUE),
        toupper(as.character(pos)))
}

#' Attach columns from a signal table by id, falling back to name + position
#'
#' @param base the board, carrying `fantasypros_id` and `pkey`
#' @param side a signal table carrying `fantasypros_id`, `pkey` and `cols`
#' @param cols the columns to copy across
#' @return `base`, with `cols` attached
#' @keywords internal
.ffs_attach <- function(base, side, cols) {
  side <- data.table::as.data.table(side)
  cols <- intersect(cols, names(side))
  if (!length(cols)) return(base)
  by_id <- unique(side[!is.na(side$fantasypros_id)], by = "fantasypros_id")
  by_key <- unique(side[!is.na(side$pkey)], by = "pkey")
  i_id <- match(base$fantasypros_id, by_id$fantasypros_id)
  i_key <- match(base$pkey, by_key$pkey)
  for (cc in cols) {
    v <- by_id[[cc]][i_id]
    w <- by_key[[cc]][i_key]
    data.table::set(base, j = cc, value = data.table::fifelse(is.na(v), w, v))
  }
  base
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
#' - **next week** (`proj`): Sleeper's own projection for the upcoming week, in
#'   this league's scoring
#' - **value** (`redraft_value`, `dynasty_value`): what the trade market pays
#'
#' Every join falls back from `fantasypros_id` to a cleaned name and position
#' (see `.ffs_pkey`), because FantasyPros re-issues ids and a moved id otherwise
#' blanks a player's usage - or shows a rostered one as free.
#'
#' `signal` averages the available components as percentile ranks **within
#' position**, against the free agents: consensus rank, expected points, snap
#' share, roster percentage and Sleeper adds. Rostered
#' players are scored on that same free-agent scale, so a claim and the man you
#' would cut for him sort in one list. It is a rough ordering to open the table
#' on, not a model - every input is there to be sorted on itself.
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
#' @param projections a table from [ffs_sleeper_projections()], or `NULL`
#' @param ppr the league's points per reception, which picks the scoring
#'   variant of the projection
#' @param pos_filter positions to include
#' @param my_franchise optional franchise_id: marks your own players `mine`
#' @param include_unranked also carry players FantasyPros does not rank at all,
#'   when the usage or market data says they are live (default `TRUE`). Their
#'   ranking columns are blank and `ranked` is `FALSE`
#' @param unranked_min_snap,unranked_min_xfp how much usage an unranked player
#'   needs to earn a row: snap share, or expected points per game
#'
#' @return a dataframe, one row per player: identity, `status`
#'   (`"free agent"` / `"rostered"`), `rostered_by`, `mine`, `ranked`, and the
#'   signal columns above
#' @export
ffs_waiver_watchlist <- function(conn,
                                 ros = NULL,
                                 usage = NULL,
                                 values = NULL,
                                 trending = NULL,
                                 projections = NULL,
                                 ppr = 1,
                                 pos_filter = c("QB", "RB", "WR", "TE"),
                                 my_franchise = NULL,
                                 include_unranked = TRUE,
                                 unranked_min_snap = 0,
                                 unranked_min_xfp = 0) {
  fantasypros_id <- pos <- ecr <- status <- mine <- signal <- NULL
  rostered_by <- franchise_id <- player <- team <- ranked <- pkey <- NULL

  if (is.null(ros)) ros <- ffs_ros_rankings(pos_filter = pos_filter)
  r <- data.table::as.data.table(ros)[pos %in% pos_filter & !is.na(ecr)]
  r <- unique(r[!is.na(fantasypros_id)], by = "fantasypros_id")
  r[, `:=`(ranked = TRUE, pkey = .ffs_pkey(player, pos))]

  rosters <- data.table::as.data.table(ffs_rosters(conn))
  # rookies lag in dp_playerids; match them to the rankings by name
  rosters <- data.table::as.data.table(
    ffs_backfill_fp_ids(rosters, r[, list(player, pos, fantasypros_id)]))
  rosters <- rosters[pos %in% pos_filter]
  rosters[, pkey := .ffs_pkey(get("player_name"), pos)]

  # FantasyPros ranks a fixed number of players per position, so a back-up who
  # has just taken over a starting job is simply absent - Drew Lock was playing
  # 90% of Seattle's snaps and sat outside all 66 ranked QBs. Anyone the usage
  # data or the Sleeper market says is live gets a row, with blank rankings.
  if (isTRUE(include_unranked)) {
    extra <- list()
    if (!is.null(usage)) {
      u <- data.table::as.data.table(usage)
      # anyone who has taken a snap this season, not just the ones over a
      # threshold - a 16%-snap back-up is exactly who a depth chart is for
      # strictly greater than the threshold, so the zero defaults mean "has
      # played", not "exists in the id map"
      u <- u[!is.na(fantasypros_id) & pos %in% pos_filter &
               !as.character(fantasypros_id) %in% r$fantasypros_id &
               (data.table::fcoalesce(as.numeric(get("games_recent")), 0) >= 1 |
                  data.table::fcoalesce(get("snap_pct_recent"), 0) > unranked_min_snap |
                  data.table::fcoalesce(get("exp_pts_recent"), 0) > unranked_min_xfp)]
      if (nrow(u)) extra[["usage"]] <- u[, list(
        fantasypros_id = as.character(fantasypros_id), player, pos, team)]
    }
    if (!is.null(values)) {
      vv <- data.table::as.data.table(values)
      vv <- vv[!is.na(fantasypros_id) & pos %in% pos_filter &
                 !as.character(fantasypros_id) %in% r$fantasypros_id]
      if (nrow(vv)) extra[["values"]] <- vv[, list(
        fantasypros_id = as.character(fantasypros_id),
        player = get("player_name"), pos, team = as.character(team))]
    }
    tr_add <- trending[["add"]]
    if (!is.null(tr_add) && NROW(tr_add) && "player" %in% names(tr_add)) {
      t <- data.table::as.data.table(tr_add)
      t <- t[!is.na(fantasypros_id) & pos %in% pos_filter &
               !as.character(fantasypros_id) %in% r$fantasypros_id]
      if (nrow(t)) extra[["trending"]] <- t[, list(
        fantasypros_id = as.character(fantasypros_id), player, pos, team)]
    }
    if (length(extra)) {
      ex <- unique(data.table::rbindlist(extra, use.names = TRUE, fill = TRUE),
                   by = "fantasypros_id")
      # a player whose FantasyPros id has moved (see .ffs_pkey) is already on the
      # board under his ranked row - do not add him a second time as "unranked"
      ex[, pkey := .ffs_pkey(player, pos)]
      ex <- ex[!pkey %in% r$pkey]
      ex[, `:=`(ecr = NA_real_, sd = NA_real_, pos_rank = NA_integer_,
                bye = NA_integer_, roster_pct = NA_real_, ecr_delta = NA_real_,
                scrape_date = r$scrape_date[[1]], source = r$source[[1]],
                ranked = FALSE)]
      if (nrow(ex)) {
        r <- data.table::rbindlist(list(r, ex), use.names = TRUE, fill = TRUE)
        cli::cli_alert_info("{nrow(ex)} unranked player{?s} carried in on usage or market signal")
      }
    }
  }

  # Anyone on a roster in THIS league belongs on the board whatever the rankings
  # and the usage thresholds say - otherwise a deep bench player is simply
  # missing from your own roster view, and you cannot decide who to cut on a
  # list that does not have him. Dohnte Meyers: 16% of Cincinnati's snaps, no
  # FantasyPros rest-of-season rank, rostered in JML.
  own_missing <- rosters[!pkey %in% r$pkey &
                           !as.character(fantasypros_id) %in% r$fantasypros_id]
  own_missing <- unique(own_missing, by = "pkey")
  if (nrow(own_missing)) {
    add <- own_missing[, list(
      fantasypros_id = as.character(fantasypros_id),
      player = get("player_name"), pos, team = as.character(get("team")), pkey,
      ecr = NA_real_, sd = NA_real_, pos_rank = NA_integer_, bye = NA_integer_,
      roster_pct = NA_real_, ecr_delta = NA_real_,
      scrape_date = r$scrape_date[[1]], source = r$source[[1]], ranked = FALSE)]
    r <- data.table::rbindlist(list(r, add), use.names = TRUE, fill = TRUE)
    cli::cli_alert_info("{nrow(add)} rostered player{?s} carried in with no rest-of-season rank")
  }

  owned <- rosters[, list(
    fantasypros_id = as.character(fantasypros_id),
    pkey = .ffs_pkey(get("player_name"), pos),
    rostered_by = get("franchise_name"),
    owner_id = as.character(franchise_id))]

  out <- data.table::copy(r)
  out <- .ffs_attach(out, owned, c("rostered_by", "owner_id"))
  out[, status := data.table::fifelse(is.na(rostered_by), "free agent", "rostered")]
  mine_ids <- if (is.null(my_franchise)) character(0) else as.character(my_franchise)
  out[, mine := !is.na(get("owner_id")) & get("owner_id") %in% mine_ids]

  if (!is.null(usage)) {
    u <- data.table::as.data.table(usage)
    keep <- intersect(c("fantasypros_id", "player", "pos", "games_recent",
                        "snap_pct_recent", "exp_pts_recent", "fpts_recent",
                        "fpoe_recent", "snap_trend", "exp_pts_trend"), names(u))
    u <- u[, keep, with = FALSE]
    u[, `:=`(fantasypros_id = as.character(fantasypros_id),
             pkey = .ffs_pkey(player, pos))]
    data.table::setnames(u,
      c("games_recent", "snap_pct_recent", "exp_pts_recent", "fpts_recent", "fpoe_recent"),
      c("games", "snap_pct", "xfp_g", "fp_g", "fpoe"), skip_absent = TRUE)
    out <- .ffs_attach(out, u, intersect(
      c("games", "snap_pct", "xfp_g", "fp_g", "fpoe", "snap_trend", "exp_pts_trend"),
      names(u)))
  }

  if (!is.null(values)) {
    v <- data.table::as.data.table(values)
    v <- v[, list(fantasypros_id = as.character(fantasypros_id),
                  pkey = .ffs_pkey(get("player_name"), pos),
                  dynasty_value = get("value"),
                  redraft_value = get("redraft_value"),
                  value_trend_30d = get("trend_30day"))]
    out <- .ffs_attach(out, v,
                       c("dynasty_value", "redraft_value", "value_trend_30d"))
  }

  for (tt in c("add", "drop")) {
    tr <- trending[[tt]]
    if (is.null(tr) || !NROW(tr)) next
    tr <- data.table::as.data.table(tr)
    nm <- paste0("trend_", tt)
    tr <- tr[, list(fantasypros_id = as.character(fantasypros_id),
                    pkey = if ("player" %in% names(tr)) .ffs_pkey(player, pos) else NA_character_,
                    count = as.integer(get("count")))]
    data.table::setnames(tr, "count", nm)
    out <- .ffs_attach(out, tr, nm)
  }

  if (!is.null(projections) && NROW(projections)) {
    pr <- data.table::as.data.table(projections)
    col <- if (ppr >= 1) "proj_ppr" else if (ppr > 0) "proj_half_ppr" else "proj_std"
    if (!col %in% names(pr)) col <- "proj_ppr"
    pr <- pr[, list(fantasypros_id = as.character(fantasypros_id),
                    pkey = .ffs_pkey(player, pos),
                    proj = round(as.numeric(get(col)), 1))]
    out <- .ffs_attach(out, pr, "proj")
  }

  # Percentile within position over EVERY player in the league, rostered and
  # free alike. Scoring against the free agents alone reads better in principle
  # ("where would he sit if he were available") but saturates: every startable
  # rostered player beats the whole wire and lands on 0.98, so a roster cannot
  # be ranked against itself and a depth chart comes out in ties.
  comps <- intersect(c("ecr", "proj", "xfp_g", "snap_pct", "roster_pct", "trend_add"),
                     names(out))
  parts <- lapply(comps, function(cc) {
    v <- rep(NA_real_, nrow(out))
    x <- as.numeric(out[[cc]])
    # trending counts are absent for the vast majority - a missing count is a
    # zero, not an unknown; every other input stays NA and drops out of the mean
    if (cc == "trend_add") x <- data.table::fifelse(is.na(x), 0, x)
    if (cc == "ecr") x <- -x           # a lower consensus rank is a better one
    for (p in unique(out$pos)) {
      idx <- which(out$pos == p & !is.na(x))
      if (!length(idx)) next
      v[idx] <- data.table::frank(x[idx], ties.method = "average") / length(idx)
    }
    v
  })
  m <- do.call(cbind, parts)
  out[, signal := round(rowMeans(m, na.rm = TRUE), 3)]
  out[is.nan(signal), signal := NA_real_]

  data.table::setorderv(out, c("status", "signal"), c(1L, -1L))
  front <- intersect(c("player", "pos", "team", "status", "rostered_by", "mine",
                       "ranked", "signal", "pos_rank", "ecr", "ecr_delta", "roster_pct",
                       "trend_add", "trend_drop",
                       "snap_pct", "xfp_g", "fp_g", "proj",
                       "fpoe", "games", "redraft_value", "dynasty_value",
                       "value_trend_30d", "bye", "sd", "fantasypros_id"), names(out))
  data.table::setcolorder(out, front)
  # pkey is join scaffolding, not a column anyone reads
  keep <- c(front, setdiff(names(out), c(front, "pkey")))
  as.data.frame(out[, keep, with = FALSE])
}
