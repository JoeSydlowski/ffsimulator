# Do RBs who under-perform on yardage lose their jobs?
#   Rscript dev/ep/ep_rb_bench.R   (needs dev/ep/ep_panel.rds and ep_panel_2026.rds)
# History (2022-25): every week t, trailing 4 weeks (t-3..t) -> next 4 weeks (t+1..t+4).
# Efficiency = rushing yards over expected per carry (RYOE/att) and receiving yards
# over expected per game; TD over expected as the contrast. Role = carry share, snap
# share, expected points. "Benched" = next-month carry share below 60% of trailing.
# Controls: the trailing role itself (a big role can only shrink) and trailing xFP.
suppressPackageStartupMessages(library(data.table))
set.seed(5)
S <- here::here("dev", "ep")
B <- 400
mk <- function(p, t, k = 4) {
  tr <- p[week > t - k & week <= t & position == "RB", .(
    g_tr = .N, carries_tr = sum(carries), ryoe_att = sum(rush_yds_oe) / pmax(sum(carries), 1),
    recyoe_g = mean(rec_yds_oe), tdoe_g = mean(rush_td_oe + rec_td_oe),
    carry_share_tr = sum(carries) / pmax(sum(team_carries), 1), snap_tr = mean(snap_pct), xfp_tr = mean(xfp),
    fp_tr = mean(fp), team = names(which.max(table(team)))), by = gsis_id]
  tr[]
}
p <- readRDS(file.path(S, "ep_panel.rds"))[season >= 2022]
p[is.na(snap_pct), snap_pct := 0]
rows <- list()
for (s in 2022:2025) for (t in 4:14) {
  ps <- p[season == s]
  tr <- mk(ps, t)[g_tr >= 3 & carries_tr >= 20]
  fu <- ps[week > t & week <= t + 4 & position == "RB", .(g_fu = .N,
          carry_share_fu = sum(carries) / pmax(sum(team_carries), 1), snap_fu = mean(snap_pct), xfp_fu = mean(xfp)), by = gsis_id]
  x <- merge(tr, fu[g_fu >= 3], by = "gsis_id")
  if (nrow(x)) rows[[length(rows) + 1]] <- x[, `:=`(season = s, week = t)]
}
d <- rbindlist(rows)
d[, `:=`(d_carry = carry_share_fu - carry_share_tr, d_snap = snap_fu - snap_tr, xfp_kept = xfp_fu / xfp_tr,
         benched = carry_share_fu < 0.6 * carry_share_tr)]
cat("RB anchors (>= 20 carries in trailing 4 weeks, >= 3 games in next 4):", nrow(d), " players:", uniqueN(d$gsis_id), "\n")
boot <- function(z, f) {
  u <- unique(z$gsis_id); idx <- split(seq_len(nrow(z)), z$gsis_id)
  quantile(replicate(B, f(z[unlist(idx[sample(u, length(u), TRUE)], use.names = FALSE)])), c(.025, .975), na.rm = TRUE)
}

cat("\n==== 1. by trailing RYOE/carry quintile: what happens to the role next month\n")
d[, q := cut(frank(ryoe_att, ties.method = "random") / .N, c(0, .2, .4, .6, .8, 1),
             labels = c("Q1 worst", "Q2", "Q3", "Q4", "Q5 best"))]
print(d[, .(n = .N, ryoe_att = mean(ryoe_att), carry_share_tr = mean(carry_share_tr), carry_share_next = mean(carry_share_fu),
            d_snap = mean(d_snap), xfp_kept = mean(xfp_kept), benched = mean(benched)), by = q][order(q)][
  , lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])

cat("\n==== 2. regressions with role controls (standardised efficiency: per 1 SD)\n")
d[, `:=`(z_ryoe = (ryoe_att - mean(ryoe_att)) / sd(ryoe_att), z_rec = (recyoe_g - mean(recyoe_g)) / sd(recyoe_g),
         z_td = (tdoe_g - mean(tdoe_g)) / sd(tdoe_g))]
f_c <- function(z) coef(lm(d_carry ~ z_ryoe + z_rec + z_td + carry_share_tr + snap_tr + xfp_tr, z))
f_b <- function(z) coef(glm(benched ~ z_ryoe + z_rec + z_td + carry_share_tr + snap_tr + xfp_tr, binomial, z))
co_c <- f_c(d); co_b <- f_b(d)
cat("   change in carry share (percentage points) per 1 SD of trailing efficiency:\n")
for (nm in c("z_ryoe", "z_rec", "z_td")) { ci <- boot(d, function(z) f_c(z)[[nm]])
  cat(sprintf("   %-7s %+.2f pp [%+.2f, %+.2f]\n", nm, 100 * co_c[[nm]], 100 * ci[1], 100 * ci[2])) }
cat("   odds ratio of being benched (carry share < 60% of trailing) per 1 SD better efficiency:\n")
for (nm in c("z_ryoe", "z_rec", "z_td")) { ci <- boot(d, function(z) f_b(z)[[nm]])
  cat(sprintf("   %-7s OR %.2f [%.2f, %.2f]\n", nm, exp(co_b[[nm]]), exp(ci[1]), exp(ci[2]))) }
base_rate <- mean(d$benched)
cat(sprintf("   base benched rate: %.3f\n", base_rate))
# lead-back starters only: the job is the thing at stake
st <- d[carry_share_tr >= 0.45]
co_s <- f_b(st); ci_s <- boot(st, function(z) f_b(z)[["z_ryoe"]])
cat(sprintf("   lead backs only (trailing carry share >= 45%%, n=%d, benched rate %.3f): RYOE OR %.2f [%.2f, %.2f]\n",
            nrow(st), mean(st$benched), exp(co_s[["z_ryoe"]]), exp(ci_s[1]), exp(ci_s[2])))
st[, qs := cut(frank(ryoe_att, ties.method = "random") / .N, c(0, .2, .8, 1), labels = c("bottom 20%", "middle", "top 20%"))]
print(st[, .(n = .N, ryoe_att = round(mean(ryoe_att), 2), benched = round(mean(benched), 3), carry_share_next = round(mean(carry_share_fu), 3)), by = qs][order(qs)])

# fitted benching model, for scoring 2026
fit <- glm(benched ~ z_ryoe + z_rec + z_td + carry_share_tr + snap_tr + xfp_tr, binomial, d)
mu <- d[, .(m_r = mean(ryoe_att), s_r = sd(ryoe_att), m_c = mean(recyoe_g), s_c = sd(recyoe_g), m_t = mean(tdoe_g), s_t = sd(tdoe_g))]

# ---- 2026 so far ------------------------------------------------------------------
p26 <- readRDS(file.path(S, "ep_panel_2026.rds"))
p26[is.na(snap_pct), snap_pct := 0]
wk <- max(p26$week)
season_rb <- p26[position == "RB", .(name = name[1], team = names(which.max(table(team))), games = .N, carries = sum(carries),
  ryoe = sum(rush_yds_oe), ryoe_att = sum(rush_yds_oe) / pmax(sum(carries), 1), recyoe = sum(rec_yds_oe),
  yds_oe_total = sum(rush_yds_oe + rec_yds_oe), tdoe = sum(rush_td_oe + rec_td_oe),
  carry_share = sum(carries) / pmax(sum(team_carries), 1), snap = mean(snap_pct), xfp_g = mean(xfp), fp_g = mean(fp)), by = gsis_id]
cur <- mk(p26, wk)[g_tr >= 3 & carries_tr >= 20]
cur[, `:=`(z_ryoe = (ryoe_att - mu$m_r) / mu$s_r, z_rec = (recyoe_g - mu$m_c) / mu$s_c, z_td = (tdoe_g - mu$m_t) / mu$s_t)]
cur[, p_bench := predict(fit, cur, type = "response")]
out26 <- merge(season_rb[carries >= 30], cur[, .(gsis_id, carry_share_last4 = carry_share_tr, p_bench)], by = "gsis_id", all.x = TRUE)
cat(sprintf("\n==== 3. 2026 through week %d: RBs (>= 30 carries) most below expected on yardage\n", wk))
cat("   yds_oe_total = rushing + receiving yards over expected (season); p_bench = model's chance carry share falls below 60% of the last 4 weeks' over the next 4\n")
print(out26[order(yds_oe_total)][1:15, .(name, team, games, carries, ryoe = round(ryoe), ryoe_att = round(ryoe_att, 2),
  recyoe = round(recyoe), tdoe = round(tdoe, 1), carry_share = round(carry_share, 2), snap = round(snap, 2),
  fp_g = round(fp_g, 1), xfp_g = round(xfp_g, 1), p_bench = round(p_bench, 2))])
cat("\n   most ABOVE expected, for contrast:\n")
print(out26[order(-yds_oe_total)][1:8, .(name, team, carries, ryoe = round(ryoe), ryoe_att = round(ryoe_att, 2), recyoe = round(recyoe),
  carry_share = round(carry_share, 2), fp_g = round(fp_g, 1), xfp_g = round(xfp_g, 1), p_bench = round(p_bench, 2))])
saveRDS(list(hist = d, fit = fit, out26 = out26, week = wk), file.path(S, "ep_rb_bench.rds"))
