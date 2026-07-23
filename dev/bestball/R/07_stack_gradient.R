# Phase 1 - STACK-SIZE GRADIENT across all available seasons. Does a bigger
# same-team stack (QB + 1 vs +2 vs +3 pass-catchers) help more than a small one?
# Underdog's own correlation research says no (modest teammate correlation; they
# recommend spreading mini-stacks). We test advance lift (regular season) and the
# ceiling (single playoff week) by stack size, vs ssize 0, within QB.

source("R/stack_lib.R")

cfg <- list(
  list(y = 2021L, kind = "thin", reg = "data/parquet/2021_BBMII.parquet",
       wk = "data/raw/bbm2021_rd2.csv"),
  list(y = 2022L, kind = "thin", reg = "data/parquet/2022_BBMIII.parquet",
       wk = sprintf("data/raw/bbm2022_rd2_%02d.csv", 0:2)),
  list(y = 2023L, kind = "rich", reg = "data/parquet/2023_BBMIV",
       wk = "data/parquet/2023_BBMIV_rd2"),
  list(y = 2024L, kind = "rich", reg = "data/parquet/2024_BBMV",
       wk = "data/parquet/2024_BBMV_rd2"),
  list(y = 2025L, kind = "rich", reg = "data/parquet/2025_BBMVI",
       wk = "data/parquet/2025_BBMVI_rd2")
)

reg_prep <- function(c) if (c$kind == "rich") prep_rich(c$reg, c$y) else prep_thin(c$reg, c$y)
wk_prep  <- function(c) if (c$kind == "rich") prep_rich(c$wk, c$y)  else prep_thin_raw(c$wk, c$y)
reg_ok <- function(c) if (c$kind == "rich") dir.exists(paste0(c$reg, "_fact")) else file.exists(c$reg)
wk_ok  <- function(c) if (c$kind == "rich") dir.exists(paste0(c$wk, "_fact"))  else all(file.exists(c$wk))

adv <- list(); wk <- list()
for (c in cfg) {
  if (reg_ok(c)) { cat(sprintf("[adv ] %d\n", c$y)); p <- reg_prep(c)
                   adv[[as.character(c$y)]] <- stack_gradient_adv(p, c$y); rm(p); gc(FALSE) }
  if (wk_ok(c))  { cat(sprintf("[week] %d\n", c$y)); p <- wk_prep(c)
                   wk[[as.character(c$y)]]  <- stack_gradient_week(p, c$y); rm(p); gc(FALSE) }
}

cat("\n===== REGULAR-SEASON advance lift by stack size (vs ssize 0, within QB) =====\n")
cat("ssize = # same-team pass-catchers rostered with the QB (3 = 3+)\n")
print(rbindlist(adv))
cat("\n===== SINGLE-WEEK (wk15) ceiling by stack size (vs ssize 0, within QB) =====\n")
print(rbindlist(wk))
cat("\nRead: if adv_lift and sd_ratio rise with ssize, concentration helps.\n")
