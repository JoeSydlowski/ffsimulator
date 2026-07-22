# Phase 1 - the PLAYOFF-WEEK ceiling test. The regular-season test showed
# stacking doesn't fatten the 14-week-TOTAL tail (tail_extra < 0 every year) -
# but a 14-week sum averages weekly correlation away. Here we test the SINGLE
# WEEK (rd2 = week 15, the largest playoff sample), where the correlation-ceiling
# mechanism should live if it exists at all. We compare within-QB single-week
# tail_extra / sd_ratio against the season-total result.

source("R/stack_lib.R")

jobs <- list(
  list(y = 2021L, kind = "thin_raw", path = "data/raw/bbm2021_rd2.csv"),
  list(y = 2023L, kind = "rich",     path = "data/parquet/2023_BBMIV_rd2"),
  list(y = 2024L, kind = "rich",     path = "data/parquet/2024_BBMV_rd2"),
  list(y = 2025L, kind = "rich",     path = "data/parquet/2025_BBMVI_rd2")
)

out <- list()
for (j in jobs) {
  present <- if (j$kind == "rich") dir.exists(paste0(j$path, "_fact")) else file.exists(j$path)
  if (!present) { cat(sprintf("[skip] %d - not present\n", j$y)); next }
  cat(sprintf("[run ] %d single-week (wk15) ceiling...\n", j$y))
  prep <- switch(j$kind,
                 rich     = prep_rich(j$path, j$y),
                 thin_raw = prep_thin_raw(j$path, j$y))
  out[[as.character(j$y)]] <- single_week_ceiling(prep, j$y)
  rm(prep); gc(verbose = FALSE)
}

SW <- rbindlist(out)
cat("\n============ SINGLE-WEEK (wk15) ceiling, within-QB ============\n")
print(SW)
cat("\ntail_extra_w > 0 & sd_ratio_w > 1  => stacking fattens the WEEKLY ceiling\n")

# contrast with the 14-week season-total ceiling from 04
if (file.exists("data/parquet/stack_summary.parquet")) {
  ST <- as.data.table(arrow::read_parquet("data/parquet/stack_summary.parquet"))
  cmp <- merge(ST[, .(season, season_total_tail = tail_extra_w)],
               SW[, .(season, single_week_tail = tail_extra_w, sd_ratio_w)],
               by = "season")
  cat("\n===== season-total vs single-week tail_extra (the key contrast) =====\n")
  print(cmp)
}
arrow::write_parquet(SW, "data/parquet/playoff_ceiling_summary.parquet")
