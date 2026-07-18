#' Scrape current FantasyCalc dynasty (or redraft) values
#'
#' (EXPERIMENTAL) Pulls the live player-value table from the FantasyCalc API for
#' a given league configuration and crosswalks each player to a `fantasypros_id`
#' (via `ffscrapr::dp_playerids()`, matching on Sleeper then MFL id) so the
#' values join straight onto the rest of the ffsimulator machinery.
#'
#' FantasyCalc publishes market values derived from real dynasty trades; these
#' are a far better anchor than a synthetic rank-decay curve. `num_qbs` controls
#' the QB format (1 = 1QB, 2 = superflex), which materially changes QB values.
#'
#' @param num_qbs starting QBs: 1 (1qb) or 2 (superflex); sets the `format` label
#' @param num_teams league size (default 12)
#' @param ppr points per reception (default 1)
#' @param is_dynasty TRUE for dynasty values, FALSE for redraft (default TRUE)
#'
#' @return a dataframe: `scraped_date`, `format`, league settings, `fantasypros_id`,
#'   player fields, and values (`value`, `overall_rank`, `pos_rank`, `trend_30day`,
#'   `redraft_value`, `value_sd`, `tier`, `adp`) plus the source ids
#'
#' @export
fc_dynasty_values <- function(num_qbs = 2, num_teams = 12, ppr = 1, is_dynasty = TRUE) {
  rlang::check_installed(c("httr", "jsonlite"), "to scrape FantasyCalc values")
  fantasypros_id <- sleeper_id <- mfl_id <- fp_s <- fp_m <- overall_rank <- NULL

  url <- sprintf(
    "https://api.fantasycalc.com/values/current?isDynasty=%s&numQbs=%d&numTeams=%d&ppr=%s&includeAdp=true",
    tolower(as.character(is_dynasty)), num_qbs, num_teams, ppr
  )
  resp <- httr::GET(url)
  if (httr::status_code(resp) != 200) {
    cli::cli_abort("FantasyCalc API returned status {httr::status_code(resp)}")
  }
  raw <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
  if (!NROW(raw)) cli::cli_abort("FantasyCalc returned no rows")
  dt <- data.table::as.data.table(raw)

  num <- function(x) suppressWarnings(as.numeric(x))
  int <- function(x) suppressWarnings(as.integer(x))
  out <- data.table::data.table(
    scraped_date = Sys.Date(),
    format = if (num_qbs > 1) "superflex" else "1qb",
    num_qbs = as.integer(num_qbs), num_teams = as.integer(num_teams), ppr = as.numeric(ppr),
    is_dynasty = isTRUE(is_dynasty),
    player_name = dt$player.name,
    pos = dt$player.position,
    team = dt$player.maybeTeam,
    age = num(dt$player.maybeAge),
    value = num(dt$value),
    overall_rank = int(dt$overallRank),
    pos_rank = int(dt$positionRank),
    trend_30day = num(dt$trend30Day),
    redraft_value = num(dt$redraftValue),
    value_sd = num(dt$maybeMovingStandardDeviation),
    tier = int(dt$maybeTier),
    adp = num(dt$maybeAdp),
    fc_id = as.character(dt$player.id),
    sleeper_id = as.character(dt$player.sleeperId),
    mfl_id = as.character(dt$player.mflId)
  )

  # crosswalk to fantasypros_id: sleeper id first, mfl id as fallback
  ids <- data.table::as.data.table(ffscrapr::dp_playerids())
  ids <- ids[!is.na(ids$fantasypros_id)]
  by_sleeper <- unique(ids[!is.na(ids$sleeper_id),
    list(sleeper_id = as.character(sleeper_id), fp_s = fantasypros_id)])
  by_mfl <- unique(ids[!is.na(ids$mfl_id),
    list(mfl_id = as.character(mfl_id), fp_m = fantasypros_id)])
  out <- merge(out, by_sleeper, by = "sleeper_id", all.x = TRUE)
  out <- merge(out, by_mfl, by = "mfl_id", all.x = TRUE)
  out[, fantasypros_id := data.table::fifelse(!is.na(fp_s), fp_s, fp_m)]
  out[, c("fp_s", "fp_m") := NULL]

  data.table::setcolorder(out, c(
    "scraped_date", "format", "num_qbs", "num_teams", "ppr", "is_dynasty",
    "fantasypros_id", "player_name", "pos", "team", "age", "value",
    "overall_rank", "pos_rank", "trend_30day", "redraft_value", "value_sd",
    "tier", "adp", "fc_id", "sleeper_id", "mfl_id"))
  as.data.frame(out[order(overall_rank)])
}

#' Append FantasyCalc value snapshots to a local database
#'
#' (EXPERIMENTAL) Scrapes one or more league configurations and appends the
#' results to a growing parquet database keyed by `scraped_date` + settings, so
#' repeated runs build a value time-series. Re-running on the same day for the
#' same settings replaces those rows (idempotent).
#'
#' @param db_path path to the parquet database file
#' @param configs a list of argument-lists for [fc_dynasty_values()]; default
#'   scrapes 1QB and superflex (12-team, 1PPR dynasty)
#'
#' @return (invisibly) `db_path`
#' @export
fc_snapshot_append <- function(db_path,
                               configs = list(list(num_qbs = 1), list(num_qbs = 2))) {
  rlang::check_installed("arrow", "to store the FantasyCalc value database")
  scraped_date <- format <- num_teams <- ppr <- NULL

  new <- data.table::rbindlist(lapply(configs, function(cfg) {
    do.call(fc_dynasty_values, cfg)
  }), fill = TRUE)
  keyof <- function(d) paste(d$scraped_date, d$format, d$num_teams, d$ppr, d$is_dynasty)

  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(db_path)) {
    old <- data.table::as.data.table(arrow::read_parquet(db_path))
    old <- old[!(keyof(old) %in% unique(keyof(new)))]
    all <- rbind(old, new, fill = TRUE)
  } else {
    all <- new
  }
  arrow::write_parquet(all, db_path)
  cli::cli_alert_success(
    "Wrote {nrow(new)} rows ({nrow(all)} total, {length(unique(all$scraped_date))} snapshots) to {.path {db_path}}")
  invisible(db_path)
}
