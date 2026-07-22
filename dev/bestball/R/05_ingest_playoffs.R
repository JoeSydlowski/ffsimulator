# Phase 1 - ingest rich-year quarterfinal (rd2 = week 15) dumps for the
# single-week ceiling test. Reuses ingest_rich() from 02 (sourcing skips 02's
# own run block). rd2 files are the biggest single-week sample (all advancers).
# 2021 (thin) is fread directly by the analysis; nothing to ingest here.

source("R/02_ingest_rich.R")  # defines ingest_rich(); main block is sys.nframe-gated

pk_specs <- list(
  list(csv = "data/raw/bbm2023_rd2.csv", season = 2023L, tournament = "BBM IV", prefix = "data/parquet/2023_BBMIV_rd2"),
  list(csv = "data/raw/bbm2024_rd2.csv", season = 2024L, tournament = "BBM V",  prefix = "data/parquet/2024_BBMV_rd2"),
  list(csv = "data/raw/bbm2025_rd2.csv", season = 2025L, tournament = "BBM VI", prefix = "data/parquet/2025_BBMVI_rd2")
)

for (s in pk_specs) {
  if (!file.exists(s$csv)) { cat(sprintf("[skip] %d rd2 - raw not present\n", s$season)); next }
  if (dir.exists(paste0(s$prefix, "_fact"))) { cat(sprintf("[skip] %d rd2 - already ingested\n", s$season)); next }
  ingest_rich(s$csv, s$season, s$tournament, s$prefix)
}
