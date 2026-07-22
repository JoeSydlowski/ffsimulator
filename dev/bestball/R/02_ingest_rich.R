# Phase 0 (rich years) - streaming ingest of a ~5 GB rd1 dump via arrow.
# fread would blow RAM; open_dataset + select streams column-pruned batches.
# Emits two pruned Parquet outputs:
#   *_players.parquet - small player dimension (id/name/pos) for name->team join
#   *_fact/           - per-pick fact, pruned to what the field study needs
# rd1 files are regular season only, so no round filter is needed.

suppressPackageStartupMessages({ library(arrow); library(dplyr) })

ingest_rich <- function(csv, season, tournament, out_prefix) {
  ds <- open_dataset(csv, format = "csv")

  # player dimension (distinct is streamed; collapses ~12M rows -> ~hundreds)
  players <- ds %>%
    select(player_id, player_name, pos = position_name) %>%
    distinct() %>% collect()
  write_parquet(players, paste0(out_prefix, "_players.parquet"))
  cat(sprintf("[rich] %s players -> %s_players.parquet\n",
              nrow(players), out_prefix))

  # big fact, column-pruned, streamed to a parquet dataset dir
  fact_dir <- paste0(out_prefix, "_fact")
  if (dir.exists(fact_dir)) unlink(fact_dir, recursive = TRUE)
  ds %>%
    transmute(
      season = season, tournament = tournament,
      draft_id, entry_id = tournament_entry_id, player_id,
      pos = position_name, adp = projection_adp,
      overall_pick = overall_pick_number,
      made_playoffs, roster_points
    ) %>%
    write_dataset(fact_dir, format = "parquet")
  cat(sprintf("[rich] fact -> %s/\n", fact_dir))
}

rich_specs <- list(
  list(csv = "data/raw/bbm2023_rd1.csv", season = 2023L, tournament = "BBM IV",  prefix = "data/parquet/2023_BBMIV"),
  list(csv = "data/raw/bbm2024_rd1.csv", season = 2024L, tournament = "BBM V",   prefix = "data/parquet/2024_BBMV"),
  list(csv = "data/raw/bbm2025_rd1.csv", season = 2025L, tournament = "BBM VI",  prefix = "data/parquet/2025_BBMVI")
)

if (sys.nframe() == 0L) {
  # ingest any year whose raw file is present and whose fact isn't built yet
  only <- commandArgs(trailingOnly = TRUE)  # optional: restrict to season(s)
  for (s in rich_specs) {
    if (length(only) && !(as.character(s$season) %in% only)) next
    if (!file.exists(s$csv)) { cat(sprintf("[skip] %d - raw not present\n", s$season)); next }
    if (dir.exists(paste0(s$prefix, "_fact"))) { cat(sprintf("[skip] %d - already ingested\n", s$season)); next }
    ingest_rich(s$csv, s$season, s$tournament, s$prefix)
  }
}
