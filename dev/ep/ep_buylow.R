# The "buy low" screen, tested the way it is used: rolling, time-ordered windows.
#   Rscript dev/ep/ep_buylow.R      (needs dev/ep/ep_panel.rds)
# Every week t of 2022-25: trailing window = weeks t-k+1..t (k = 3,4,5), outcome =
# weeks t+1..t+4. A player is scored if he played >= k-1 games in the trailing window
# and >= 3 of the next 4. Uncertainty: bootstrap over players.
suppressPackageStartupMessages(library(data.table))
set.seed(11)
S <- here::here("dev", "ep")
p <- readRDS(file.path(S, "ep_panel.rds"))[season >= 2022 & position %in% c("WR", "RB", "TE", "QB")]
p[, `:=`(oe_yards = 0.1 * (rec_yds_oe + rush_yds_oe), oe_td = 6 * (rec_td_oe + rush_td_oe), oe_catch = catch_oe)]
B <- 400

rows <- list()
for (k in 3:5) for (s in 2022:2025) {
  ps <- p[season == s]
  for (t in k:14) {
    tr <- ps[week > t - k & week <= t, .(g_tr = .N, fp_tr = mean(fp), xfp_tr = mean(xfp), fpoe_tr = mean(fpoe),
                                          oe_yards_tr = mean(oe_yards), oe_td_tr = mean(oe_td), position = position[1]), by = gsis_id]
    fu <- ps[week > t & week <= t + 4, .(g_fu = .N, fp_fu = mean(fp), xfp_fu = mean(xfp)), by = gsis_id]
    x <- merge(tr[g_tr >= k - 1], fu[g_fu >= 3], by = "gsis_id")
    if (nrow(x)) rows[[length(rows) + 1]] <- x[, `:=`(k = k, season = s, week = t)]
  }
}
d <- rbindlist(rows)
d[, pid := gsis_id]
cat("player-week anchors:", nrow(d), "\n")
boot <- function(z, f) {
  u <- unique(z$pid); idx <- split(seq_len(nrow(z)), z$pid)
  quantile(replicate(B, f(z[unlist(idx[sample(u, length(u), TRUE)], use.names = FALSE)])), c(.025, .975), na.rm = TRUE)
}

cat("\n==== 1. same trailing points, more expected points -> more future points?\n")
cat("   next-4-week fp/g ~ trailing fp/g + trailing xfp/g;  share_to_xfp = b_xfp / (b_fp + b_xfp)\n")
cat("   (share_to_xfp = 1: future points follow usage only; 0: follow past points only)\n")
r1 <- d[, {
  f <- function(z) { co <- coef(lm(fp_fu ~ fp_tr + xfp_tr, z)); co[3] / (co[2] + co[3]) }
  co <- coef(lm(fp_fu ~ fp_tr + xfp_tr)); ci <- boot(.SD, f)
  .(n = .N, b_fp = co[2], b_xfp = co[3], share_to_xfp = co[3] / (co[2] + co[3]), lo = ci[1], hi = ci[2])
}, by = .(position, k)][order(position, k)]
print(r1[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])

cat("\n==== 2. buckets by trailing over-performance (quintiles within position x k)\n")
d[, q := cut(frank(fpoe_tr, ties.method = "random") / .N, c(0, .2, .4, .6, .8, 1),
             labels = c("Q1 under (buy low)", "Q2", "Q3", "Q4", "Q5 over (sell high)")), by = .(position, k)]
r2 <- d[k == 4, .(n = .N, fp_trailing = mean(fp_tr), xfp_trailing = mean(xfp_tr), fp_next = mean(fp_fu), xfp_next = mean(xfp_fu),
                  closed = (mean(fp_fu) - mean(fp_tr)) / (mean(xfp_tr) - mean(fp_tr))), by = .(position, q)][order(position, q)]
cat("   k = 4 trailing weeks. closed = share of the trailing (xfp - fp) gap that closed next 4 weeks\n")
print(r2[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 2) else v)])

cat("\n==== 3. which kind of under-performance bounces back? (k = 4, WR/RB/TE pooled)\n")
cat("   next fp/g ~ trailing xfp/g + trailing yardage-over-expected + trailing TD-over-expected (points/g)\n")
cat("   coefficient on a component: 1 = it all persists, 0 = it all washes out (fully 'regresses to expected')\n")
z <- d[k == 4 & position != "QB"]
f3 <- function(zz) coef(lm(fp_fu ~ xfp_tr + oe_yards_tr + oe_td_tr, zz))
co3 <- f3(z)
for (nm in c("xfp_tr", "oe_yards_tr", "oe_td_tr")) {
  ci <- boot(z, function(zz) f3(zz)[[nm]])
  cat(sprintf("   %-12s %+.3f [%+.3f, %+.3f]\n", nm, co3[[nm]], ci[1], ci[2]))
}
cat("   by position:\n")
print(d[k == 4 & position != "QB", as.list(round(coef(lm(fp_fu ~ xfp_tr + oe_yards_tr + oe_td_tr)), 3)), by = position])

cat("\n==== 4. does the opportunity itself hold up? next xfp/g vs trailing xfp/g by bucket (k = 4)\n")
r4 <- d[k == 4, .(n = .N, xfp_trailing = mean(xfp_tr), xfp_next = mean(xfp_fu), xfp_kept = mean(xfp_fu) / mean(xfp_tr)), by = .(position, q)][order(position, q)]
print(r4[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])

cat("\n==== 5. the screen as a decision: buy-low (Q1) vs sell-high (Q5) players matched on trailing points\n")
cat("   among players within 1 fp/g of each other on trailing points, does the Q1 player outscore the Q5 player next month?\n")
r5 <- d[k == 4 & q %in% c("Q1 under (buy low)", "Q5 over (sell high)")][, fpb := round(fp_tr)]
r5 <- dcast(r5[, .(fp_next = mean(fp_fu), n = .N), by = .(position, fpb, q)], position + fpb ~ q, value.var = c("fp_next", "n"))
setnames(r5, c("position", "fpb", "next_buylow", "next_sellhigh", "n_buylow", "n_sellhigh"))
r5 <- r5[!is.na(next_buylow) & !is.na(next_sellhigh)]
print(r5[, .(cells = .N, n_pairs = sum(pmin(n_buylow, n_sellhigh)),
             buylow_minus_sellhigh = round(weighted.mean(next_buylow - next_sellhigh, pmin(n_buylow, n_sellhigh)), 2)), by = position])
saveRDS(list(r1 = r1, r2 = r2, r4 = r4, r5 = r5), file.path(S, "ep_buylow.rds"))
cat("\nsaved dev/ep/ep_buylow.rds\n")
