# Phase 0 - ingest Underdog Best Ball Mania field dumps into one canonical
# Parquet schema. See dev/suite/BESTBALL_PROPOSAL.md for the data landscape
# (6 seasons, 3 schema tiers) and dev/bestball/README.md for status.
#
# The raw dumps drift across years (renamed/absent columns, xlsx in 2020, a
# second host in 2021, split-part files in 2022). This script maps each year's
# source columns onto ONE canonical schema, filling absent columns with NA so
# downstream code is year-agnostic. Add a year = add a `specs` entry.
#
# Memory note: thin years (2021/2022 ~0.5-1.4 GB) ingest fine with fread. The
# rich 5 GB years (2023-25) should switch to a streaming reader (arrow
# open_dataset or duckdb) - flagged where relevant, not needed for this pass.

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

# --- canonical schema (one row per pick) -----------------------------------
CANON <- c(
  "season", "tournament", "round",           # partition keys
  "draft_id", "entry_id",                     # pod (12-team draft) + stable entry id
  "user_id", "username", "source",            # who / how (rich years only)
  "player_name", "player_id", "pos",          # player
  "adp", "overall_pick", "team_pick", "pick_slot", "bye_week",  # draft context
  "draft_time",
  "pick_points", "roster_points", "made_playoffs"               # outcome (round-scoped)
)

INT_COLS <- c("round", "overall_pick", "team_pick", "pick_slot", "bye_week", "made_playoffs")
DBL_COLS <- c("adp", "pick_points", "roster_points")

# --- per-year source spec: canonical <- source column name -----------------
# Only list columns that exist in that year's files; the rest become NA.
specs <- list(
  bbm2_2021 = list(
    season = 2021L, tournament = "BBM II",
    files = "data/raw/bbm2021_regular.csv",  # regular season only, this pass
    rename = c(
      draft_id = "draft_id", entry_id = "tournament_entry_id",
      round = "tournament_round_number", player_name = "player_name",
      pos = "position_name", adp = "projection_adp",
      overall_pick = "overall_pick_number", team_pick = "team_pick_number",
      pick_slot = "pick_order", bye_week = "bye_week", draft_time = "draft_time",
      pick_points = "pick_points", roster_points = "roster_points",
      made_playoffs = "playoff_team"
    )
  )
)

# --- normalize one raw table onto the canonical schema ----------------------
normalize <- function(dt, spec) {
  n <- nrow(dt)
  out <- data.table(season = rep(spec$season, n), tournament = rep(spec$tournament, n))
  for (canon in setdiff(CANON, c("season", "tournament"))) {
    src <- if (canon %in% names(spec$rename)) spec$rename[[canon]] else NA_character_
    out[[canon]] <- if (!is.na(src) && src %in% names(dt)) dt[[src]] else NA
  }
  # types (source may read some as character/logical when all-NA)
  for (cc in intersect(INT_COLS, names(out))) out[[cc]] <- as.integer(out[[cc]])
  for (cc in intersect(DBL_COLS, names(out))) out[[cc]] <- suppressWarnings(as.numeric(out[[cc]]))
  setcolorder(out, CANON)
  out[]
}

ingest_spec <- function(spec, out_dir = "data/parquet") {
  raw <- rbindlist(lapply(spec$files, fread, showProgress = FALSE),
                   use.names = TRUE, fill = TRUE)
  norm <- normalize(raw, spec)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out <- file.path(out_dir, sprintf("%d_%s.parquet", spec$season,
                                    gsub("[^A-Za-z0-9]+", "", spec$tournament)))
  arrow::write_parquet(norm, out)
  cat(sprintf("[ingest] %s: %s rows -> %s\n", spec$tournament,
              format(nrow(norm), big.mark = ","), out))
  invisible(norm)
}

if (sys.nframe() == 0L) {
  # run from dev/bestball
  for (s in specs) ingest_spec(s)
}
