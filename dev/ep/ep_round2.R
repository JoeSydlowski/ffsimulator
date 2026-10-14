# Round 2 of the expected-points tests (2022-25, out of sample).
#   Rscript dev/ep/ep_round2.R     (needs dev/ep/ep_panel.rds)
#   A  per-snap metrics done properly (min snaps, rank correlation) + usage baselines
#      incl. RB touch / carry share
#   B  which part of over-performance is skill: catch rate, yards, TDs - stability,
#      prediction, and whether it travels when a player changes team
#   C  market test: does last season's xFP add anything beyond THIS season's
#      preseason ECR (which already knows everything about last season)?
#   D  why the xFP edge fades in time order: edge vs the gap between the games used
#      to predict and the games predicted
suppressPackageStartupMessages(library(data.table))
set.seed(7)
S <- here::here("dev", "ep")
p <- readRDS(file.path(S, "ep_panel.rds"))[season >= 2022]
p[is.na(snaps), snaps := 0][is.na(snap_pct), snap_pct := 0]
p[, ps := paste(season, gsis_id)]
setorder(p, ps, week)
p[, gi := seq_len(.N), by = ps]
B <- 400
sp <- function(x, y) suppressWarnings(cor(x, y, method = "spearman", use = "complete.obs"))
pe <- function(x, y) suppressWarnings(cor(x, y, use = "complete.obs"))
rnd <- function(d, digits = 3) d[, lapply(.SD, function(v) if (is.numeric(v)) round(v, digits) else v)]
pboot <- function(d, f, Bn = B) {      # bootstrap over players of a statistic f(d)
  u <- unique(d$ps); idx <- split(seq_len(nrow(d)), d$ps)
  quantile(replicate(Bn, f(d[unlist(idx[sample(u, length(u), TRUE)], use.names = FALSE)])), c(.025, .975), na.rm = TRUE)
}

# fantasy scoring weights behind ffopportunity's points (recovered, not assumed)
w_rec <- coef(lm(rec_fp ~ 0 + receptions + rec_yards + rec_td, p[targets > 0]))
w_rush <- coef(lm(rush_fp ~ 0 + rush_yards + rush_td, p[carries > 0]))
cat("recovered scoring: reception", round(w_rec[1], 2), " rec yd", round(w_rec[2], 3), " rec TD", round(w_rec[3], 2),
    " | rush yd", round(w_rush[1], 3), " rush TD", round(w_rush[2], 2), "\n")
# over-performance by component, in fantasy points per game
p[, `:=`(oe_catch = catch_oe * w_rec[[1]], oe_recyd = rec_yds_oe * w_rec[[2]], oe_rectd = rec_td_oe * w_rec[[3]],
         oe_rushyd = rush_yds_oe * w_rush[[1]], oe_rushtd = rush_td_oe * w_rush[[2]])]
p[, `:=`(oe_yards = oe_recyd + oe_rushyd, oe_td = oe_rectd + oe_rushtd)]

agg <- function(d, by = c("ps", "position")) d[, .(
  games = .N, snaps = sum(snaps), fp_g = mean(fp), xfp_g = mean(xfp), fpoe_g = mean(fpoe),
  fp_snap = sum(fp) / sum(snaps), xfp_snap = sum(xfp) / sum(snaps), fpoe_snap = sum(fpoe) / sum(snaps),
  touch_share = sum(carries + targets) / sum(team_carries + team_targets),
  carry_share = sum(carries) / pmax(sum(team_carries), 1), target_share = mean(target_share, na.rm = TRUE),
  oe_catch = mean(oe_catch), oe_yards = mean(oe_yards), oe_td = mean(oe_td),
  oe_recyd = mean(oe_recyd), oe_rushyd = mean(oe_rushyd)), by = by]
gm <- p[, .N, by = ps]
draw <- function(k, kb, R) rbindlist(lapply(seq_len(R), function(r) {
  d <- p[ps %chin% gm[N >= k + kb]$ps]; d[, o := sample.int(.N), by = ps]
  x <- merge(agg(d[o <= k]), agg(d[o > k & o <= k + kb]), by = c("ps", "position"), suffixes = c("_A", "_B"))
  x[, rep := r]
}))

# ---- A. per-snap done properly + usage baselines ---------------------------------
cat("\n==== A. k random games -> 6 other games; Spearman; per-snap rows need >= 60 snaps in BOTH sets\n")
A8 <- draw(8, 6, 10); A4 <- draw(4, 6, 10)
tabA <- rbindlist(lapply(list(A4, A8), function(x) {
  k <- x$games_A[1]
  x[, .(k = k, n = .N / 10,
        fp_g = sp(fp_g_A, fp_g_B), xfp_g = sp(xfp_g_A, fp_g_B),
        touch_share = sp(touch_share_A, fp_g_B), carry_share = sp(carry_share_A, fp_g_B), target_share = sp(target_share_A, fp_g_B),
        fp_snap = sp(fp_snap_A[snaps_A >= 60 & snaps_B >= 60], (fp_g_B)[snaps_A >= 60 & snaps_B >= 60]),
        xfp_snap = sp(xfp_snap_A[snaps_A >= 60 & snaps_B >= 60], (fp_g_B)[snaps_A >= 60 & snaps_B >= 60]),
        rel_fp_snap = sp(fp_snap_A[snaps_A >= 60 & snaps_B >= 60], fp_snap_B[snaps_A >= 60 & snaps_B >= 60]),
        rel_xfp_snap = sp(xfp_snap_A[snaps_A >= 60 & snaps_B >= 60], xfp_snap_B[snaps_A >= 60 & snaps_B >= 60]),
        rel_fpoe_snap = sp(fpoe_snap_A[snaps_A >= 60 & snaps_B >= 60], fpoe_snap_B[snaps_A >= 60 & snaps_B >= 60])),
    by = position]
}))
print(rnd(tabA[order(position, k)], 2))

# ---- B. components of over-performance -----------------------------------------
cat("\n==== B1. split-half reliability of over-performance components (8 vs 8 random games, Pearson)\n")
B8 <- draw(8, 8, 20)
comps <- c("fpoe_g", "oe_catch", "oe_recyd", "oe_rushyd", "oe_td")
relB <- B8[, c(list(n = .N / 20), lapply(setNames(comps, comps), function(m) pe(get(paste0(m, "_A")), get(paste0(m, "_B"))))), by = position]
print(rnd(relB, 3))
cat("   95% CI (players resampled) for WR and RB, first draw:\n")
for (pz in c("WR", "RB", "TE")) for (m in comps) {
  d <- B8[rep == 1 & position == pz]
  ci <- pboot(d, function(z) pe(z[[paste0(m, "_A")]], z[[paste0(m, "_B")]]), 300)
  cat(sprintf("   %s %-9s r=%+.3f [%+.3f, %+.3f]\n", pz, m, pe(d[[paste0(m, "_A")]], d[[paste0(m, "_B")]]), ci[1], ci[2]))
}

cat("\n==== B2. season t -> t+1 persistence of components (>= 8 games each), stayed vs changed team\n")
sz <- agg(p, by = c("season", "gsis_id", "position"))[games >= 8]
tm <- p[, .(team = names(which.max(table(team)))), by = .(season, gsis_id)]
sz <- merge(sz, tm, by = c("season", "gsis_id"))
nx <- copy(sz)[, season := season - 1L]
st <- merge(sz, nx, by = c("season", "gsis_id", "position"), suffixes = c("_t", "_t1"))
st[, moved := team_t != team_t1]
persist <- st[position != "QB", c(list(n = .N), lapply(setNames(comps, comps), function(m) pe(get(paste0(m, "_t")), get(paste0(m, "_t1"))))),
              by = .(position, moved)][order(position, moved)]
print(rnd(persist, 3))
st[, ps := paste(gsis_id, season)]
cat("   pooled WR/TE/RB, 95% CI by player bootstrap:\n")
for (mv in c(FALSE, TRUE)) for (m in comps) {
  d <- st[position != "QB" & moved == mv]
  ci <- pboot(d, function(z) pe(z[[paste0(m, "_t")]], z[[paste0(m, "_t1")]]), 300)
  cat(sprintf("   moved=%-5s %-9s r=%+.3f [%+.3f, %+.3f]  n=%d\n", mv, m, pe(d[[paste0(m, "_t")]], d[[paste0(m, "_t1")]]), ci[1], ci[2], nrow(d)))
}

cat("\n==== B3. which component predicts future points? fp_B ~ xfp_A + components (8 random games -> 6 others)\n")
cat("   coefficient = fantasy points/game next games per 1 point/game of that component (xFP coef for scale)\n")
B86 <- draw(8, 6, 5)[, season := as.integer(substr(ps, 1, 4))]
fitB <- B86[position != "QB", {
  m <- lm(fp_g_B ~ xfp_g_A + oe_catch_A + oe_yards_A + oe_td_A)
  co <- coef(m)
  tr <- .SD[season <= 2023]; te <- .SD[season >= 2024]
  m1 <- lm(fp_g_B ~ xfp_g_A, tr); m2 <- lm(fp_g_B ~ xfp_g_A + fpoe_g_A, tr); m3 <- lm(fp_g_B ~ xfp_g_A + oe_catch_A + oe_yards_A + oe_td_A, tr)
  rm <- function(mm) sqrt(mean((te$fp_g_B - predict(mm, te))^2))
  .(n = .N / 5, b_xfp = co[["xfp_g_A"]], b_catch = co[["oe_catch_A"]], b_yards = co[["oe_yards_A"]], b_td = co[["oe_td_A"]],
    rmse_xfp = rm(m1), rmse_xfp_fpoe = rm(m2), rmse_xfp_components = rm(m3))
}, by = position]
print(rnd(fitB, 3))

# ---- C. market test --------------------------------------------------------------
cat("\n==== C. season t+1 fp/game ~ preseason ECR at t+1 (+ last season's numbers)\n")
pr <- unique(p[!is.na(pre_rank), .(season, gsis_id, pre_rank)], by = c("season", "gsis_id"))
mk <- merge(st, pr[, .(season = season - 1L, gsis_id, pre_rank_t1 = pre_rank)], by = c("season", "gsis_id"))
mk[, lr := log(pre_rank_t1)]
cv_r2 <- function(d, f) {   # leave-one-season-pair-out R^2
  ss <- sort(unique(d$season))
  pr_ <- unlist(lapply(ss, function(s) { m <- lm(f, d[season != s]); predict(m, d[season == s]) }))
  y <- unlist(lapply(ss, function(s) d[season == s]$fp_g_t1))
  1 - sum((y - pr_)^2) / sum((y - mean(y))^2)
}
mkt <- mk[, {
  f0 <- fp_g_t1 ~ splines::ns(lr, 3)
  .(n = .N, r2_ecr = cv_r2(.SD, f0),
    r2_ecr_fp = cv_r2(.SD, update(f0, . ~ . + fp_g_t)),
    r2_ecr_xfp = cv_r2(.SD, update(f0, . ~ . + xfp_g_t)),
    r2_ecr_xfp_fpoe = cv_r2(.SD, update(f0, . ~ . + xfp_g_t + fpoe_g_t)),
    b_xfp = coef(lm(update(f0, . ~ . + xfp_g_t + fpoe_g_t), .SD))[["xfp_g_t"]],
    b_fpoe = coef(lm(update(f0, . ~ . + xfp_g_t + fpoe_g_t), .SD))[["fpoe_g_t"]])
}, by = position]
print(rnd(mkt, 3))
cat("   coefficient CIs (players resampled), pooled positions with position-specific ECR curves:\n")
mk[, ps := paste(gsis_id, season)]
fC <- function(z) coef(lm(fp_g_t1 ~ position * splines::ns(lr, 3) + xfp_g_t + fpoe_g_t, z))
co <- fC(mk)
ci_x <- pboot(mk, function(z) fC(z)[["xfp_g_t"]]); ci_o <- pboot(mk, function(z) fC(z)[["fpoe_g_t"]])
cat(sprintf("   xfp_t %+.3f [%+.3f, %+.3f]   fpoe_t %+.3f [%+.3f, %+.3f]   n=%d\n",
            co[["xfp_g_t"]], ci_x[1], ci_x[2], co[["fpoe_g_t"]], ci_o[1], ci_o[2], nrow(mk)))

# ---- D. why does the edge fade in time order? ---------------------------------------
cat("\n==== D1. same players (>= 14 games): A = 4 games, B = 4 games; edge = r(xfp_A, fp_B) - r(fp_A, fp_B)\n")
long <- p[ps %chin% gm[N >= 14]$ps]
blocks <- function(a_idx, b_idx, label) {
  A <- agg(long[gi %in% a_idx]); Bb <- agg(long[gi %in% b_idx])
  x <- merge(A, Bb, by = c("ps", "position"), suffixes = c("_A", "_B"))
  x[position != "QB", .(design = label, n = .N, r_fp = pe(fp_g_A, fp_g_B), r_xfp = pe(xfp_g_A, fp_g_B),
                        edge = pe(xfp_g_A, fp_g_B) - pe(fp_g_A, fp_g_B), r_role = pe(touch_share_A, touch_share_B))]
}
D <- rbind(blocks(1:4, 5:8, "games 1-4 -> 5-8 (adjacent)"),
           blocks(1:4, 7:10, "games 1-4 -> 7-10 (gap 2)"),
           blocks(1:4, 9:12, "games 1-4 -> 9-12 (gap 4)"),
           blocks(1:4, 11:14, "games 1-4 -> 11-14 (gap 6)"),
           blocks(c(1, 4, 7, 10), c(2, 5, 8, 11), "interleaved (1,4,7,10 -> 2,5,8,11)"))
print(rnd(D, 3))
cat("   r_role = touch-share correlation between the two blocks (how much the player's role changed)\n")
cat("   edge CI for adjacent vs gap 6 (players resampled):\n")
for (des in list(list(1:4, 5:8, "adjacent"), list(1:4, 11:14, "gap 6"), list(c(1, 4, 7, 10), c(2, 5, 8, 11), "interleaved"))) {
  x <- merge(agg(long[gi %in% des[[1]]]), agg(long[gi %in% des[[2]]]), by = c("ps", "position"), suffixes = c("_A", "_B"))[position != "QB"]
  ci <- pboot(x, function(z) pe(z$xfp_g_A, z$fp_g_B) - pe(z$fp_g_A, z$fp_g_B))
  cat(sprintf("   %-12s edge %+.3f [%+.3f, %+.3f]\n", des[[3]], pe(x$xfp_g_A, x$fp_g_B) - pe(x$fp_g_A, x$fp_g_B), ci[1], ci[2]))
}
saveRDS(list(tabA = tabA, relB = relB, persist = persist, fitB = fitB, mkt = mkt, D = D,
             w_rec = w_rec, w_rush = w_rush), file.path(S, "ep_round2.rds"))
cat("\nsaved dev/ep/ep_round2.rds\n")
