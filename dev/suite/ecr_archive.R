# Weekly FantasyPros ECR archive -> dev/data/ecr_panel.parquet
#
#   Rscript dev/suite/ecr_archive.R
#   FFS_ECR_REFRESH=1 Rscript dev/suite/ecr_archive.R   # force a fresh download
#
# The bundled fp_dynasty_history() holds ONE dynasty snapshot per season, and
# FantasyPros itself only serves one per year (its `week=` argument is silently
# ignored). DynastyProcess, however, has been archiving the FantasyPros ECR
# pages weekly since 2020 and publishes the lot as a single parquet:
#
#   dynasty-overall  (1QB)       323 snapshots  2020-10-16 .. 2026-09-18
#   dynasty-op       (superflex) 323 snapshots  same range, median gap 7 days
#   dynasty-qb/rb/wr/te, dynasty-rk (rookies)
#   redraft-overall + positional 323 snapshots  <- the FAST signal, archived
#   weekly-*                     in-season start/sit ranks
#
# That is the slow market AND the fast signal at weekly resolution for six
# years, keyed on fantasypros_id - which is what makes the lead-lag question
# (do redraft rankings lead dynasty rankings?) answerable historically.
#
# This script caches the upstream file and reshapes it into a tidy panel:
#   scrape_date, page_type, fp_page, fantasypros_id, player, pos, team, ecr, sd,
#   rank_delta, n_ranked, pct
# `pct` is the within-snapshot percentile of ecr, and is only safe for ordering
# WITHIN one snapshot. Do NOT difference it across snapshots: the boards change
# SIZE and COMPOSITION, and rank/N then moves for players who did not.
#
# `fp_page` is kept because one page_type can be served by different pages over
# time, and that is not cosmetic. `redraft-overall` is the preseason draft board
# (/nfl/rankings/ppr-cheatsheets.php) until early September and the rest-of-season
# board (/nfl/rankings/ros-ppr-overall.php) after it - in 2026 the universe fell
# 525 -> 404 players at the 2026-09-11 switch. Differencing percentiles across
# that boundary makes improving players look like decliners, in proportion to how
# deep they are ranked. Switch dates: 2021-09-10, 2022-09-16, 2023-09-15,
# 2024-09-27, 2025-09-12, 2026-09-11. Anything comparing two snapshots must
# re-rank on the players common to both and refuse to straddle a page change.
#
# Other scripts reuse the readers rather than re-implementing them:
#   ECR_ARCHIVE_SOURCE_ONLY <- TRUE
#   source(here::here("dev", "suite", "ecr_archive.R"))
#   panel <- ecr_panel_read(pages = c("dynasty-overall", "redraft-overall"))

library(data.table)

ECR_SOURCE_URL <- paste0(
  "https://raw.githubusercontent.com/dynastyprocess/data/master/files/db_fpecr.parquet"
)

# every page worth keeping; the upstream file also carries IDP/K/DST pages
ECR_PAGES_DEFAULT <- c(
  "dynasty-overall", "dynasty-op", "dynasty-rk",
  "dynasty-qb", "dynasty-rb", "dynasty-wr", "dynasty-te",
  "redraft-overall", "redraft-op",
  "redraft-qb", "redraft-rb", "redraft-wr", "redraft-te",
  "weekly-qb", "weekly-rb", "weekly-wr", "weekly-te", "weekly-op"
)

ecr_source_path <- function() here::here("dev", "data", "db_fpecr.parquet")
ecr_panel_path <- function() here::here("dev", "data", "ecr_panel.parquet")

#' Download the upstream archive, unless the local copy is recent enough.
ecr_fetch_source <- function(max_age_days = 3,
                             force = identical(Sys.getenv("FFS_ECR_REFRESH"), "1")) {
  path <- ecr_source_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  age <- if (file.exists(path)) {
    as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "days"))
  } else Inf
  if (!force && age <= max_age_days) {
    message("ecr source cached, ", round(age, 1), "d old @ ", Sys.time())
    return(invisible(path))
  }
  message("downloading ecr archive (~39MB) @ ", Sys.time())
  # written to a temp file first so an interrupted download can't leave a
  # truncated parquet behind that every later run would then read happily
  tmp <- paste0(path, ".part")
  utils::download.file(ECR_SOURCE_URL, tmp, mode = "wb", quiet = TRUE)
  file.rename(tmp, path)
  invisible(path)
}

#' Reshape the archive into the tidy panel and write it.
ecr_panel_build <- function(pages = ECR_PAGES_DEFAULT, refresh = TRUE) {
  rlang::check_installed("arrow", "to read the ECR archive")
  if (refresh) ecr_fetch_source()

  message("reading archive @ ", Sys.time())
  d <- data.table::as.data.table(arrow::read_parquet(
    ecr_source_path(),
    col_select = c("page_type", "fp_page", "id", "player", "pos", "tm", "ecr", "sd",
                   "rank_delta", "scrape_date")
  ))
  d <- d[page_type %chin% pages & !is.na(ecr) & !is.na(id)]
  data.table::setnames(d, c("id", "tm"), c("fantasypros_id", "team"))
  d[, fantasypros_id := as.character(fantasypros_id)]
  d[, scrape_date := as.Date(scrape_date)]
  d <- d[!is.na(scrape_date)]

  # one row per player per page per snapshot; upstream is already clean, but a
  # silent duplicate would corrupt every percentile below
  dup <- anyDuplicated(d, by = c("page_type", "scrape_date", "fantasypros_id"))
  if (dup) {
    warning("dropping ", sum(duplicated(d, by = c("page_type", "scrape_date", "fantasypros_id"))),
            " duplicate page/date/player rows")
    d <- unique(d, by = c("page_type", "scrape_date", "fantasypros_id"))
  }

  # percentile WITHIN a snapshot: boards grow and shrink, so a raw rank of 100
  # means different things in 2020 and 2026
  d[, n_ranked := .N, by = list(page_type, scrape_date)]
  d[, pct := data.table::frank(ecr, ties.method = "average") / n_ranked,
    by = list(page_type, scrape_date)]
  data.table::setcolorder(d, c("scrape_date", "page_type", "fp_page", "fantasypros_id",
                               "player", "pos", "team", "ecr", "sd",
                               "rank_delta", "n_ranked", "pct"))
  data.table::setorder(d, page_type, scrape_date, ecr)

  arrow::write_parquet(d, ecr_panel_path())
  message("wrote ", ecr_panel_path(), " (", nrow(d), " rows) @ ", Sys.time())
  invisible(d)
}

#' Read the panel, optionally narrowed to some pages / a start date.
ecr_panel_read <- function(pages = NULL, since = NULL) {
  path <- ecr_panel_path()
  if (!file.exists(path)) stop("no ecr panel yet - run dev/suite/ecr_archive.R")
  d <- data.table::as.data.table(arrow::read_parquet(path, mmap = FALSE))
  if (!is.null(pages)) d <- d[page_type %chin% pages]
  if (!is.null(since)) d <- d[scrape_date >= as.Date(since)]
  d[]
}

if (!exists("ECR_ARCHIVE_SOURCE_ONLY")) {
  d <- ecr_panel_build()
  s <- d[, list(snapshots = uniqueN(scrape_date),
                first = min(scrape_date), last = max(scrape_date),
                median_players = as.integer(median(n_ranked))),
         by = page_type][order(-snapshots)]
  print(s)
  gaps <- d[page_type == "dynasty-overall", sort(unique(scrape_date))]
  cat("\ndynasty-overall median gap:", median(as.numeric(diff(gaps))), "days\n")
}
