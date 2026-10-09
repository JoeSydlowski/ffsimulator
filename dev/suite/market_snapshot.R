# Weekly trade-price log -> dev/data/market_panel.parquet
#
#   Rscript dev/suite/market_snapshot.R              # scrape today, rebuild panel
#   FFS_MARKET_SCRAPE=0 Rscript dev/suite/market_snapshot.R   # rebuild only
#
# dev/suite/ecr_archive.R gets us six years of weekly RANKINGS for free, because
# DynastyProcess archives the FantasyPros pages. Nobody archives the trade-based
# market: FantasyCalc and KeepTradeCut serve only "current", and DynastyProcess's
# own values-players.csv is a strictly monotone transform of its ECR (spearman
# -0.998), so it carries no independent price information. The FantasyCalc price
# series therefore only exists going FORWARD, from whenever we start logging it.
#
# Hence this script. It is deliberately tiny: the scrape is `fc_snapshot_append()`
# which already exists and is idempotent per date+settings, so running it twice
# in a day is harmless and running it weekly costs seconds.
#
# The panel unions two sources:
#   fc         full fc_dynasty_values() scrapes, whole universe, full schema
#   watchlist  dynasty/redraft values recovered from the waiver watchlist CSVs
#              (dev/league_sims/waivers/<date>/watchlist.csv) which happened to
#              log FantasyCalc values per league from 2026-09-15 onward
#
# Watchlist rows are per-league (their scale depends on that league's format and
# size, which the CSV does not record), so they carry NA settings and are only
# comparable WITHIN a league. They are kept because they are the only price
# history that predates this script.
#
# `value_share` (value / total value of that snapshot's pool) is computed here so
# downstream code cannot forget it: the FantasyCalc pool deflates a couple of
# percent a week, so raw value deltas contain a drift that masquerades as signal.
#
# Reuse the readers rather than re-implementing them:
#   MARKET_SNAPSHOT_SOURCE_ONLY <- TRUE
#   source(here::here("dev", "suite", "market_snapshot.R"))
#   mk <- market_panel_read()

library(data.table)

market_fc_db_path <- function() here::here("dev", "data", "fantasycalc_values.parquet")
market_panel_path <- function() here::here("dev", "data", "market_panel.parquet")

MARKET_CONFIGS <- list(
  list(num_qbs = 1, num_teams = 12, ppr = 1),
  list(num_qbs = 2, num_teams = 12, ppr = 1)
)

#' Scrape today's FantasyCalc values into the snapshot database (idempotent).
market_snapshot_append <- function(configs = MARKET_CONFIGS) {
  fc_snapshot_append(market_fc_db_path(), configs = configs)
}

.market_from_fc <- function() {
  path <- market_fc_db_path()
  if (!file.exists(path)) return(NULL)
  d <- data.table::as.data.table(arrow::read_parquet(path, mmap = FALSE))
  d[, list(
    snapshot_date = as.Date(scraped_date), source = "fc", league = NA_character_,
    format, num_qbs, num_teams, ppr,
    fantasypros_id = as.character(fantasypros_id), player_name, pos, team, age,
    value, redraft_value, trend_30day, value_sd, tier, overall_rank, pos_rank
  )]
}

.market_from_watchlist <- function() {
  files <- Sys.glob(here::here("dev", "league_sims", "waivers", "*", "watchlist.csv"))
  if (!length(files)) return(NULL)
  rows <- lapply(files, function(f) {
    d <- data.table::fread(f, colClasses = list(character = "fantasypros_id"),
                           showProgress = FALSE)
    if (!"dynasty_value" %in% names(d)) return(NULL)
    d <- d[!is.na(dynasty_value) & dynasty_value > 0]
    if (!nrow(d)) return(NULL)
    # the folder date is the run date; the csv's own scrape_date is FantasyPros'
    d[, list(
      snapshot_date = as.Date(basename(dirname(f))), source = "watchlist",
      league, format = NA_character_, num_qbs = NA_integer_,
      num_teams = NA_integer_, ppr = NA_real_,
      fantasypros_id = as.character(fantasypros_id), player_name = player, pos, team,
      age = NA_real_, value = dynasty_value, redraft_value,
      trend_30day = if ("value_trend_30d" %in% names(d)) value_trend_30d else NA_real_,
      value_sd = NA_real_, tier = NA_integer_,
      overall_rank = NA_integer_, pos_rank = NA_integer_
    )]
  })
  out <- data.table::rbindlist(rows, fill = TRUE)
  if (!nrow(out)) return(NULL)
  unique(out, by = c("snapshot_date", "league", "fantasypros_id"))
}

#' Union every price source into one panel and write it.
market_panel_build <- function() {
  rlang::check_installed("arrow", "to store the market panel")
  d <- data.table::rbindlist(list(.market_from_fc(), .market_from_watchlist()),
                             fill = TRUE)
  if (!nrow(d)) stop("no price rows found - run market_snapshot_append() first")

  # share-of-pool: the only scale that is comparable across snapshots, formats
  # and leagues, because the raw pool total drifts week to week
  d[, value_share := value / sum(value, na.rm = TRUE),
    by = list(snapshot_date, source, format, league, num_teams, ppr)]
  data.table::setorder(d, snapshot_date, source, league, -value)

  arrow::write_parquet(d, market_panel_path())
  message("wrote ", market_panel_path(), " (", nrow(d), " rows) @ ", Sys.time())
  invisible(d)
}

# `src` rather than `source`: the panel has a column of that name, and shadowing
# it inside the data.table call is a silent-wrong-answer waiting to happen
market_panel_read <- function(src = NULL, since = NULL) {
  path <- market_panel_path()
  if (!file.exists(path)) stop("no market panel yet - run dev/suite/market_snapshot.R")
  d <- data.table::as.data.table(arrow::read_parquet(path, mmap = FALSE))
  if (!is.null(src)) d <- d[source %chin% src]
  if (!is.null(since)) d <- d[snapshot_date >= as.Date(since)]
  d[]
}

if (!exists("MARKET_SNAPSHOT_SOURCE_ONLY")) {
  devtools::load_all(here::here(), quiet = TRUE)
  if (!identical(Sys.getenv("FFS_MARKET_SCRAPE"), "0")) market_snapshot_append()
  d <- market_panel_build()
  print(d[, list(players = uniqueN(fantasypros_id), rows = .N),
          by = list(snapshot_date, source)][order(snapshot_date, source)])
}
