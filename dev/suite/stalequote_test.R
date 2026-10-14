# Is the redraft -> dynasty lead-lag just stale expert quotes?
#
#   Rscript dev/suite/stalequote_test.R
#
# Dynasty ECR is an average over experts who refresh on different schedules. After
# a shock the experts who have refreshed move at once and the average then drifts
# toward them as the rest catch up - a predictable lag even if every expert is
# individually efficient (the stale-quote / nonsynchronous-trading effect). The
# archive carries, per player per snapshot, the expert spread (sd) and the most
# and least bullish expert ranks (best, worst), which lets us separate the stories:
#
#   T1  decisive: does the redraft move still predict the forward dynasty move once
#       the dynasty board's OWN internal movement over the same window (best, worst,
#       sd) is controlled? Stale quotes -> redraft adds little beyond the fresh
#       dynasty experts. Genuine slowness -> redraft still predicts.
#   T2  spread dynamics: after a redraft drop, does the dynasty spread widen over
#       the window (fresh experts moved, stale did not) and narrow afterwards?
#   T3  horizon profile: does the forward drift stop within a refresh cycle, or
#       keep going for months?
#
# Same anchors, windows, common-universe re-ranking, tradeable tier and block
# bootstrap as dev/suite/leadlag_study.R, so the baseline reproduces that study.
# Output: dev/validate_outputs/stalequote_<tag>.txt + .rds
suppressPackageStartupMessages(library(data.table))
cfg <- list(
  h = as.integer(Sys.getenv("FFS_SQ_H", "4")),
  fwd = as.integer(strsplit(Sys.getenv("FFS_SQ_FWD", "1,2,4,8,12"), ",")[[1]]),
  tier = 0.33, tol = 5L, min_n = 50L, boot = 2000L,
  tag = Sys.getenv("FFS_SQ_TAG", format(Sys.Date()))
)
out_dir <- here::here("dev", "validate_outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

raw <- as.data.table(arrow::read_parquet(here::here("dev", "data", "db_fpecr.parquet"),
  col_select = c("page_type", "fp_page", "id", "pos", "ecr", "sd", "best", "worst", "scrape_date"),
  mmap = FALSE))
raw <- raw[page_type %chin% c("dynasty-overall", "dynasty-op", "redraft-overall") & !is.na(ecr) & !is.na(id)]
raw[, `:=`(fantasypros_id = as.character(id), dt = as.Date(scrape_date))]
raw <- unique(raw[!is.na(dt)], by = c("page_type", "dt", "fantasypros_id"))
raw[, N := .N, by = .(page_type, dt)]
# expert extremes as a share of the board (so a growing board can't move them)
raw[, `:=`(best_p = best / N, worst_p = worst / N, lsd = log1p(sd))]

.near <- function(dates, target, tol) {
  i <- which.min(abs(as.numeric(dates - target)))
  if (length(i) && abs(as.numeric(dates[i] - target)) <= tol) dates[i] else as.Date(NA)
}
.safe_cor <- function(x, y) {
  ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]
  if (length(x) < 10 || sd(x) == 0 || sd(y) == 0) return(NA_real_)
  suppressWarnings(cor(x, y, method = "spearman"))
}
# partial spearman of x and y given a set of controls (rank-residualised)
.pcor <- function(d, x, y, ctrl) {
  d <- d[complete.cases(d[, c(x, y, ctrl), with = FALSE])]
  if (nrow(d) < 30) return(NA_real_)
  rk <- function(v) rank(v)
  X <- if (length(ctrl)) as.data.frame(lapply(d[, ctrl, with = FALSE], rk)) else NULL
  rx <- if (is.null(X)) rk(d[[x]]) else resid(lm(rk(d[[x]]) ~ ., data = X))
  ry <- if (is.null(X)) rk(d[[y]]) else resid(lm(rk(d[[y]]) ~ ., data = X))
  suppressWarnings(cor(rx, ry))
}
.block_boot_t <- function(x, block, n_boot) {
  x <- x[is.finite(x)]; n <- length(x)
  if (n < 5) return(c(mean = mean(x), se = NA, t = NA))
  block <- max(1L, min(as.integer(block), n)); nb <- ceiling(n / block); starts <- seq_len(n - block + 1L)
  mus <- vapply(seq_len(n_boot), function(i) {
    idx <- unlist(lapply(sample(starts, nb, replace = TRUE), function(s) s:(s + block - 1L)))
    mean(x[idx[seq_len(n)]])
  }, numeric(1))
  c(mean = mean(x), se = sd(mus), t = mean(x) / sd(mus))
}

measure <- function(dyn_page) {
  dyn <- raw[page_type == dyn_page]; rdr <- raw[page_type == "redraft-overall"]
  dates <- sort(unique(dyn$dt)); rdr_page <- unique(rdr[, .(dt, fp_page)])
  lag <- 7L * cfg$h
  rows <- list(); spread <- list(); prof <- list()
  for (t0 in dates) {
    t0 <- as.Date(t0, origin = "1970-01-01")
    if (!as.integer(format(t0, "%m")) %in% 9:12) next          # in-season anchors only
    tm <- .near(dates, t0 - lag, cfg$tol)
    if (is.na(tm) || tm >= t0) next
    if (length(rdr_page[dt %in% c(tm, t0), unique(fp_page)]) != 1L) next
    ids <- Reduce(intersect, list(dyn[dt == t0, fantasypros_id], dyn[dt == tm, fantasypros_id],
                                  rdr[dt == t0, fantasypros_id], rdr[dt == tm, fantasypros_id]))
    if (length(ids) < cfg$min_n) next
    pz <- function(src, when) {
      x <- src[dt == when & fantasypros_id %chin% ids]
      x[, p := frank(ecr, ties.method = "average") / .N][, .(fantasypros_id, p, best_p, worst_p, lsd)]
    }
    d0 <- pz(dyn, t0); dm <- pz(dyn, tm); r0 <- pz(rdr, t0); rm_ <- pz(rdr, tm)
    a <- merge(d0, dm, by = "fantasypros_id", suffixes = c("0", "m"))
    a <- merge(a, r0[, .(fantasypros_id, r0 = p)], by = "fantasypros_id")
    a <- merge(a, rm_[, .(fantasypros_id, rm = p)], by = "fantasypros_id")
    a <- a[p0 <= cfg$tier]
    if (nrow(a) < cfg$min_n) next
    a[, `:=`(fast = r0 - rm, dyn_past = p0 - pm,
             d_best = best_p0 - best_pm, d_worst = worst_p0 - worst_pm, d_lsd = lsd0 - lsdm)]
    # forward outcomes at several horizons, same universe, exits = worst percentile
    for (k in cfg$fwd) {
      tp <- .near(dates, t0 + 7L * k, cfg$tol)
      if (is.na(tp) || tp <= t0) next
      f <- dyn[dt == tp & fantasypros_id %chin% ids][, p := frank(ecr, ties.method = "average") / .N]
      b <- merge(a, f[, .(fantasypros_id, pf = p, lsdf = lsd)], by = "fantasypros_id", all.x = TRUE)
      b[is.na(pf), pf := 1]
      b[, `:=`(dyn_fwd = pf - p0, d_lsd_fwd = lsdf - lsd0)]
      b[, grp := cut(frank(fast) / .N, c(0, .2, .8, 1), labels = c("risers", "mid", "fallers"))]
      prof[[length(prof) + 1L]] <- b[, .(t0 = t0, k = k, fwd = mean(dyn_fwd), n = .N), by = grp]
      if (k == cfg$h) {
        rows[[length(rows) + 1L]] <- data.table(
          t0 = t0, n = nrow(b),
          ic_base = .pcor(b, "fast", "dyn_fwd", "dyn_past"),                        # the published study
          ic_ctrl = .pcor(b, "fast", "dyn_fwd", c("dyn_past", "d_best", "d_worst", "d_lsd")),
          ic_best = .pcor(b, "d_best", "dyn_fwd", "dyn_past"),
          ic_worst = .pcor(b, "d_worst", "dyn_fwd", "dyn_past"),
          # do the dynasty board's own extremes LEAD its average? (staleness signature)
          ic_ext_ctrl_fast = .pcor(b, "d_worst", "dyn_fwd", c("dyn_past", "fast")))
        spread[[length(spread) + 1L]] <- b[, .(t0 = t0, d_lsd_in = mean(d_lsd, na.rm = TRUE),
                                               d_lsd_out = mean(d_lsd_fwd, na.rm = TRUE)), by = grp]
      }
    }
  }
  list(rows = rbindlist(rows), spread = rbindlist(spread), prof = rbindlist(prof))
}

con <- file(file.path(out_dir, paste0("stalequote_", cfg$tag, ".txt")), "wt")
say <- function(...) { cat(..., "\n", sep = ""); cat(..., "\n", sep = "", file = con) }
say("stale-quote test  ", format(Sys.Date()), "   prior window h=", cfg$h, " weeks, in-season anchors")
res <- list()
for (fmt in c(`1qb` = "dynasty-overall", superflex = "dynasty-op")) {
  nm <- if (fmt == "dynasty-overall") "1qb" else "superflex"
  m <- measure(fmt); res[[nm]] <- m
  R <- m$rows
  bt <- function(v) { b <- .block_boot_t(v, cfg$h, cfg$boot); sprintf("%+.3f (t=%.1f)", b[["mean"]], b[["t"]]) }
  say("")
  say("== ", nm, "  (", nrow(R), " in-season anchors, median n=", median(R$n), ")")
  say("T1 redraft -> dynasty, partial IC")
  say("   controls: dynasty own move (published study)      ", bt(R$ic_base))
  say("   + dynasty best/worst expert moves + spread change ", bt(R$ic_ctrl))
  say("   share of the redraft signal that survives         ", sprintf("%.0f%%", 100 * mean(R$ic_ctrl, na.rm = TRUE) / mean(R$ic_base, na.rm = TRUE)))
  say("   dynasty best-expert move -> fwd (ctrl own move)   ", bt(R$ic_best))
  say("   dynasty worst-expert move -> fwd (ctrl own move)  ", bt(R$ic_worst))
  say("   worst-expert move -> fwd, ctrl own move + redraft ", bt(R$ic_ext_ctrl_fast))
  S <- m$spread
  say("T2 dynasty spread (log1p sd) change: during the prior window / over the next window")
  for (g in c("fallers", "mid", "risers")) {
    s <- S[grp == g]
    say(sprintf("   %-8s in %+.4f   out %+.4f", g, mean(s$d_lsd_in, na.rm = TRUE), mean(s$d_lsd_out, na.rm = TRUE)))
  }
  P <- m$prof[, .(fwd = mean(fwd)), by = .(grp, k)][order(grp, k)]
  W <- dcast(P, k ~ grp, value.var = "fwd")[, spread := fallers - risers]
  say("T3 forward dynasty move by horizon (weeks after anchor), fallers - risers spread:")
  for (i in seq_len(nrow(W))) say(sprintf("   +%2d wk  fallers %+.4f  risers %+.4f  spread %+.4f",
                                          W$k[i], W$fallers[i], W$risers[i], W$spread[i]))
}
close(con)
saveRDS(res, file.path(out_dir, paste0("stalequote_", cfg$tag, ".rds")))
cat("done\n")
