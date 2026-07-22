# Phase 1 - cross-year replication of the fix-the-anchor + ceiling stack tests.
# A STRUCTURAL edge recurs across years; a single-year receiver-realization
# artifact does not. Runs every year whose Parquet is present (thin: canonical
# file; rich: *_fact dir), one at a time with gc() between (each rich year's
# fact is ~12M rows).

source("R/stack_lib.R")

jobs <- list(
  list(y = 2021L, kind = "thin", path = "data/parquet/2021_BBMII.parquet"),
  list(y = 2023L, kind = "rich", path = "data/parquet/2023_BBMIV"),
  list(y = 2024L, kind = "rich", path = "data/parquet/2024_BBMV"),
  list(y = 2025L, kind = "rich", path = "data/parquet/2025_BBMVI")
)

summ <- list(); detail <- list()
for (j in jobs) {
  present <- if (j$kind == "rich") dir.exists(paste0(j$path, "_fact")) else file.exists(j$path)
  if (!present) { cat(sprintf("[skip] %d - not ingested yet\n", j$y)); next }
  cat(sprintf("[run ] %d (%s)...\n", j$y, j$kind))
  prep <- if (j$kind == "rich") prep_rich(j$path, j$y) else prep_thin(j$path, j$y)
  r <- stack_tests(prep, j$y)
  summ[[as.character(j$y)]] <- r$summary
  detail[[as.character(j$y)]] <- r$per_qb
  rm(prep, r); gc(verbose = FALSE)
}

cat("\n================= CROSS-YEAR STACK SUMMARY =================\n")
S <- rbindlist(summ)
print(S)
cat("\nadv_lift_w  = de-confounded within-QB advance-rate lift from stacking\n")
cat("med/p95_lift_w = season-points lift at median vs 95th pct (within QB)\n")
cat("tail_extra_w   = p95_lift - med_lift  (>0 = ceiling fattens beyond the level shift)\n")

# persist for later
if (length(summ)) {
  arrow::write_parquet(S, "data/parquet/stack_summary.parquet")
  cat(sprintf("\n[saved] cross-year summary (%d years) -> data/parquet/stack_summary.parquet\n", nrow(S)))
}
