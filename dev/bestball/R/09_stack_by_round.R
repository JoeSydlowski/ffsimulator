# Phase 1 - STACK PREVALENCE BY ROUND. Do stacked teams get over-represented as
# you go deeper (field -> quarterfinals -> semis -> finals -> winner)? If stacking
# helped, stack rate would RISE with round depth. If the causal null (08) is right,
# it should stay ~flat. This is the exact "the winners were all stacked!" analysis,
# done against the field base rate so survivorship can't fool us.

source("R/stack_lib.R")

cfg <- list(
  list(y = 2021L, kind = "thin", field = "data/parquet/2021_BBMII.parquet",
       qf = "data/raw/bbm2021_rd2.csv", sf = "data/raw/bbm2021_rd3.csv", fin = "data/raw/bbm2021_rd4.csv"),
  list(y = 2022L, kind = "thin", field = "data/parquet/2022_BBMIII.parquet",
       qf = sprintf("data/raw/bbm2022_rd2_%02d.csv", 0:2), sf = "data/raw/bbm2022_rd3.csv", fin = "data/raw/bbm2022_rd4.csv"),
  list(y = 2023L, kind = "rich", field = "data/parquet/2023_BBMIV",
       qf = "data/raw/bbm2023_rd2.csv", sf = "data/raw/bbm2023_rd3.csv", fin = "data/raw/bbm2023_rd4.csv"),
  list(y = 2024L, kind = "rich", field = "data/parquet/2024_BBMV",
       qf = "data/raw/bbm2024_rd2.csv", sf = "data/raw/bbm2024_rd3.csv", fin = "data/raw/bbm2024_rd4.csv"),
  list(y = 2025L, kind = "rich", field = "data/parquet/2025_BBMVI",
       qf = "data/raw/bbm2025_rd2.csv", sf = "data/raw/bbm2025_rd3.csv", fin = "data/raw/bbm2025_rd4.csv")
)
field_fact <- function(c) if (c$kind == "rich") prep_rich(c$field, c$y)$fact else prep_thin(c$field, c$y)$fact

prev_row <- function(ent, season, round) {
  ent[, .(season = season, round = round, n = .N,
          pct_stacked = round(mean(max_ss >= 1), 3),
          pct_2plus   = round(mean(max_ss >= 2), 3),
          mean_ss     = round(mean(max_ss), 3),
          pct_WRstk   = round(mean(wr), 3),
          pct_TEstk   = round(mean(te), 3))]
}

out <- list(); winners <- list()
for (c in cfg) {
  cat(sprintf("[year] %d\n", c$y))
  ff <- field_fact(c)
  out[[paste0(c$y, "1")]] <- prev_row(stack_prevalence(ff), c$y, "1_field"); rm(ff); gc(FALSE)
  for (rd in list(list("2_QF", c$qf), list("3_SF", c$sf), list("4_Finals", c$fin))) {
    f <- prep_csv(rd[[2]], c$y)
    ent <- stack_prevalence(f)
    out[[paste0(c$y, substr(rd[[1]],1,1))]] <- prev_row(ent, c$y, rd[[1]])
    if (rd[[1]] == "4_Finals") {                       # winner + top-10 finalists
      m <- merge(unique(f[, .(entry_id, roster_points)]), ent, by = "entry_id")
      setorder(m, -roster_points)
      winners[[as.character(c$y)]] <- data.table(
        season = c$y, n_finalists = nrow(m),
        winner_ss = m$max_ss[1], winner_WR = m$wr[1], winner_TE = m$te[1],
        top10_pct_stacked = round(mean(m$max_ss[1:10] >= 1), 2),
        top10_mean_ss = round(mean(m$max_ss[1:10]), 2),
        finals_mean_ss = round(mean(m$max_ss), 2))
    }
    rm(f, ent); gc(FALSE)
  }
}

cat("\n================ STACK PREVALENCE BY ROUND ================\n")
cat("(field = full draft pool; QF = made playoffs; SF/Finals = deeper single-week rounds)\n")
print(rbindlist(out)[order(season, round)])
cat("\n================ WINNER & TOP-10 FINALISTS ================\n")
print(rbindlist(winners))
cat("\nRead: if pct_stacked / mean_ss rise from field -> Finals, stacks are getting there.\n")
