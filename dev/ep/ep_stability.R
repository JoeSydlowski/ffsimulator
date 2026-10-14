# Out-of-sample stability and predictive validity of expected fantasy points.
#   Rscript dev/ep/ep_stability.R    (needs dev/ep/ep_panel.rds from ep_panel.R)
# 2022-2025 only: the ffopportunity models were frozen in Jan 2022.
#
#   D1 reliability: random disjoint k-game halves of a player-season, same metric
#   D2 predictive validity: k random games of metric M -> fp/game in 6 OTHER random games
#   D3 time order: first k weeks -> rest of season
#   D4 season to season: season t -> season t+1 fp/game (team change split)
#   D5 subsets: preseason top-X vs rest; high vs low snap share (measured in A)
#   D6 shrinkage: fp_B ~ xfp_A + fpoe_A, fit 2022-23, test 2024-25
# Uncertainty: bootstrap over players (a player-season's games are not independent).
suppressPackageStartupMessages(library(data.table))
set.seed(2022)
S <- here::here("dev", "ep")
p <- readRDS(file.path(S, "ep_panel.rds"))[season >= 2022]
p[is.na(snaps), snaps := 0][is.na(snap_pct), snap_pct := 0]
p[, ps := paste(season, gsis_id)]
R <- as.integer(Sys.getenv("FFS_EP_REPS", "30")); B <- as.integer(Sys.getenv("FFS_EP_BOOT", "300"))
metrics <- c("fp_g", "xfp_g", "fpoe_g", "snap_pct", "fp_snap", "xfp_snap", "fpoe_snap", "opp_share", "target_share", "wopr")

# aggregate a set of games to per-game / per-snap rates
agg <- function(d, by = c("ps", "position")) d[, .(
  games = .N, fp_g = mean(fp), xfp_g = mean(xfp), fpoe_g = mean(fpoe), snap_pct = mean(snap_pct),
  fp_snap = sum(fp) / pmax(sum(snaps), 1), xfp_snap = sum(xfp) / pmax(sum(snaps), 1),
  fpoe_snap = sum(fpoe) / pmax(sum(snaps), 1),
  opp_share = sum(xfp) / pmax(sum(team_xfp_g, na.rm = TRUE), 1e-9),
  target_share = mean(target_share, na.rm = TRUE), wopr = mean(wopr, na.rm = TRUE)), by = by]
boot_ci <- function(x, y, players, Bn = B) {
  u <- unique(players); idx <- split(seq_along(players), players)
  s <- replicate(Bn, { j <- unlist(idx[sample(u, length(u), replace = TRUE)], use.names = FALSE)
    suppressWarnings(cor(x[j], y[j], use = "complete.obs")) })
  quantile(s, c(.025, .975), na.rm = TRUE)
}

# ---- random disjoint splits (D1, D2, D5, D6 share the draws) -----------------
games <- p[, .N, by = ps]
split_draw <- function(k, kb) {
  d <- p[ps %chin% games[N >= k + kb]$ps]
  d[, o := sample.int(.N), by = ps]
  A <- agg(d[o <= k]); Bset <- agg(d[o > k & o <= k + kb])
  merge(A, Bset, by = c("ps", "position"), suffixes = c("_A", "_B"))
}
d1 <- list(); d2 <- list(); pairs2 <- list()
for (k in 1:8) {
  for (r in seq_len(R)) {
    x <- split_draw(k, k)                         # D1: equal halves
    d1[[length(d1) + 1]] <- x[, c(list(k = k, rep = r, n = .N),
      lapply(setNames(metrics, metrics), function(m) suppressWarnings(cor(get(paste0(m, "_A")), get(paste0(m, "_B")), use = "complete.obs")))),
      by = position]
    y <- split_draw(k, 6)                         # D2: k games -> 6 other games
    y[, `:=`(k = k, rep = r)]
    pairs2[[length(pairs2) + 1]] <- y
  }
}
d1 <- rbindlist(d1); pairs2 <- rbindlist(pairs2)
rel <- melt(d1, id.vars = c("position", "k", "rep", "n"), variable.name = "metric", value.name = "r")[
  , .(r = mean(r, na.rm = TRUE), n = mean(n)), by = .(position, k, metric)]
cat("==== D1 split-half reliability r(metric in k games, same metric in k other games), mean of", R, "draws\n")
print(dcast(rel[k %in% c(2, 4, 8)], position + metric ~ k, value.var = "r")[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 2) else v)])

val <- pairs2[, c(list(n = .N), lapply(setNames(metrics, metrics), function(m)
  suppressWarnings(cor(get(paste0(m, "_A")), fp_g_B, use = "complete.obs")))), by = .(position, k, rep)]
val <- melt(val, id.vars = c("position", "k", "rep", "n"), variable.name = "metric", value.name = "r")[
  , .(r = mean(r, na.rm = TRUE)), by = .(position, k, metric)]
cat("\n==== D2 predictive validity r(metric in k random games, fp/game in 6 other games)\n")
print(dcast(val[k %in% c(2, 4, 8)], position + metric ~ k, value.var = "r")[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 2) else v)])
# CI on the headline difference xfp_g vs fp_g, one draw per k, players resampled
ci2 <- pairs2[rep == 1, {
  u <- unique(ps); idx <- split(seq_len(.N), ps)
  s <- replicate(B, { j <- unlist(idx[sample(u, length(u), TRUE)], use.names = FALSE)
    cor(xfp_g_A[j], fp_g_B[j]) - cor(fp_g_A[j], fp_g_B[j]) })
  .(diff = cor(xfp_g_A, fp_g_B) - cor(fp_g_A, fp_g_B), lo = quantile(s, .025), hi = quantile(s, .975), n = .N)
}, by = .(position, k)]
cat("\n   xFP/g minus FP/g, predictive r (one draw, 95% CI by player bootstrap)\n")
print(ci2[k %in% c(1, 2, 4, 8)][order(position, k), lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])

# ---- D3 time order ---------------------------------------------------------------
d3 <- rbindlist(lapply(c(2, 4, 8), function(k) {
  A <- agg(p[week <= k]); Bs <- agg(p[week > k])
  x <- merge(A, Bs, by = c("ps", "position"), suffixes = c("_A", "_B"))[games_A >= ceiling(.6 * k) & games_B >= 5]
  x[, {
    ci <- quantile(replicate(B, { j <- sample.int(.N, replace = TRUE); cor(xfp_g_A[j], fp_g_B[j]) - cor(fp_g_A[j], fp_g_B[j]) }), c(.025, .975))
    .(k = k, n = .N, r_fp = cor(fp_g_A, fp_g_B), r_xfp = cor(xfp_g_A, fp_g_B), r_opp = cor(opp_share_A, fp_g_B, use = "complete.obs"),
      diff = cor(xfp_g_A, fp_g_B) - cor(fp_g_A, fp_g_B), lo = ci[1], hi = ci[2])
  }, by = position]
}))
cat("\n==== D3 first k weeks -> rest of season\n")
print(d3[order(position, k), lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])

# ---- D4 season to season ------------------------------------------------------
sz <- agg(p, by = c("season", "gsis_id", "position"))[games >= 8]
tm <- p[, .(team = names(which.max(table(team)))), by = .(season, gsis_id)]
sz <- merge(sz, tm, by = c("season", "gsis_id"))
nx <- copy(sz)[, season := season - 1L]
d4x <- merge(sz, nx, by = c("season", "gsis_id", "position"), suffixes = c("_t", "_t1"))
d4x[, moved := team_t != team_t1]
d4 <- d4x[, .(n = .N, r_fp = cor(fp_g_t, fp_g_t1), r_xfp = cor(xfp_g_t, fp_g_t1),
              r_opp = cor(opp_share_t, fp_g_t1), r_fpoe_persist = cor(fpoe_g_t, fpoe_g_t1),
              r_fpoe_snap_persist = cor(fpoe_snap_t, fpoe_snap_t1)), by = .(position, moved)][order(position, moved)]
cat("\n==== D4 season t -> season t+1 (>= 8 games each)\n")
print(d4[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])

# ---- D5 subsets ---------------------------------------------------------------
top_cut <- c(QB = 12, RB = 24, WR = 36, TE = 12)
pr <- unique(p[, .(ps, pre_rank)])[, .(pre_rank = min(pre_rank, na.rm = TRUE)), by = ps]
pr[!is.finite(pre_rank), pre_rank := NA]
d5d <- merge(pairs2[k %in% c(4, 8)], pr, by = "ps", all.x = TRUE)
d5d[, `:=`(tier = fifelse(!is.na(pre_rank) & pre_rank <= top_cut[position], "preseason top", "rest"),
           usage = fifelse(snap_pct_A >= 0.6, "snap share >= 60%", "snap share < 60%"))]
d5 <- rbind(
  d5d[, .(subset = tier, n = .N / R, r_fp = cor(fp_g_A, fp_g_B), r_xfp = cor(xfp_g_A, fp_g_B),
          r_xfp_snap = cor(xfp_snap_A, fp_g_B), r_opp = cor(opp_share_A, fp_g_B, use = "complete.obs")), by = .(position, k, tier)][, tier := NULL],
  d5d[, .(subset = usage, n = .N / R, r_fp = cor(fp_g_A, fp_g_B), r_xfp = cor(xfp_g_A, fp_g_B),
          r_xfp_snap = cor(xfp_snap_A, fp_g_B), r_opp = cor(opp_share_A, fp_g_B, use = "complete.obs")), by = .(position, k, usage)][, usage := NULL])
cat("\n==== D5 subsets (k random games -> 6 other games)\n")
print(d5[order(position, k, subset), lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])

# ---- D6 shrinkage rule: fit 2022-23, test 2024-25 ------------------------------
pairs2[, season := as.integer(substr(ps, 1, 4))]
d6 <- pairs2[rep <= 5, {
  tr <- .SD[season <= 2023]; te <- .SD[season >= 2024]
  m_both <- lm(fp_g_B ~ xfp_g_A + fpoe_g_A, tr); m_fp <- lm(fp_g_B ~ fp_g_A, tr); m_x <- lm(fp_g_B ~ xfp_g_A, tr)
  rmse <- function(m) sqrt(mean((te$fp_g_B - predict(m, te))^2))
  .(n_test = nrow(te) / 5, w_fpoe = coef(m_both)[["fpoe_g_A"]] / coef(m_both)[["xfp_g_A"]],
    rmse_fp = rmse(m_fp), rmse_xfp = rmse(m_x), rmse_blend = rmse(m_both))
}, by = .(position, k)]
cat("\n==== D6 blend fp_B ~ xfp_A + fpoe_A (fit 2022-23, test 2024-25); w_fpoe = weight on over-performance relative to xFP\n")
print(d6[k %in% c(1, 2, 4, 8)][order(position, k), lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])

saveRDS(list(rel = rel, val = val, ci2 = ci2, d3 = d3, d4 = d4, d5 = d5, d6 = d6, R = R), file.path(S, "ep_stability.rds"))
cat("\nsaved dev/ep/ep_stability.rds\n")
