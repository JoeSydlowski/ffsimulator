# RB benching, part 2: rushing vs receiving efficiency, together or separately, and
# own efficiency vs the teammate's (is it the back or the situation?).
#   Rscript dev/ep/ep_rb_bench2.R   (needs dev/ep/ep_panel.rds)
# Same setup as ep_rb_bench.R: every week t of 2022-25, trailing weeks t-3..t ->
# next weeks t+1..t+4, RBs with >= 20 trailing carries and >= 3 of the next 4 games.
#   ryoe_att  = rushing yards over expected per carry
#   recoe_tgt = receiving yards over expected per target (0 targets -> NA)
#   tm_ryoe   = the team's OTHER RBs' rushing yards over expected per carry, same window
#   team_ryoe = everyone on the team (incl. QB scrambles) - the "situation"
suppressPackageStartupMessages(library(data.table))
set.seed(9)
S <- here::here("dev", "ep")
B <- 400
p <- readRDS(file.path(S, "ep_panel.rds"))[season >= 2022]
p[is.na(snap_pct), snap_pct := 0]

rows <- list()
for (s in 2022:2025) for (t in 4:14) {
  w <- p[season == s & week > t - 4 & week <= t]
  rb <- w[position == "RB", .(g_tr = .N, carries = sum(carries), targets = sum(targets),
    ryoe = sum(rush_yds_oe), recoe = sum(rec_yds_oe), tdoe_g = mean(rush_td_oe + rec_td_oe),
    team_carries = sum(team_carries), snap_tr = mean(snap_pct), xfp_tr = mean(xfp),
    team = names(which.max(table(team)))), by = gsis_id]
  # team-level rushing efficiency (all positions) and the RB room total
  tmall <- w[, .(team_ryoe_yds = sum(rush_yds_oe), team_car = sum(carries)), by = team]
  room <- rb[, .(room_ryoe = sum(ryoe), room_car = sum(carries)), by = team]
  rb <- merge(merge(rb, tmall, by = "team"), room, by = "team")
  rb[, `:=`(tm_car = room_car - carries, tm_ryoe_yds = room_ryoe - ryoe)]
  rb <- rb[g_tr >= 3 & carries >= 20]
  fu <- p[season == s & week > t & week <= t + 4 & position == "RB",
          .(g_fu = .N, carry_share_fu = sum(carries) / pmax(sum(team_carries), 1)), by = gsis_id]
  x <- merge(rb, fu[g_fu >= 3], by = "gsis_id")
  if (nrow(x)) rows[[length(rows) + 1]] <- x[, `:=`(season = s, week = t)]
}
d <- rbindlist(rows)
d[, `:=`(ryoe_att = ryoe / carries, recoe_tgt = fifelse(targets >= 5, recoe / targets, NA_real_),
         carry_share_tr = carries / pmax(team_carries, 1),
         tm_ryoe = fifelse(tm_car >= 10, tm_ryoe_yds / tm_car, NA_real_),
         team_ryoe = team_ryoe_yds / pmax(team_car, 1))]
d[, `:=`(benched = carry_share_fu < 0.6 * carry_share_tr, d_carry = carry_share_fu - carry_share_tr)]
z <- function(v) (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE)
d[, `:=`(z_rush = z(ryoe_att), z_rec = z(recoe_tgt), z_tm = z(tm_ryoe), z_team = z(team_ryoe), z_td = z(tdoe_g),
         z_rel = z(ryoe_att - tm_ryoe))]
cat("RB anchors:", nrow(d), " with >= 5 targets:", sum(!is.na(d$recoe_tgt)), " with a teammate RB (>= 10 carries):", sum(!is.na(d$tm_ryoe)), "\n")
boot <- function(zz, f) {
  u <- unique(zz$gsis_id); idx <- split(seq_len(nrow(zz)), zz$gsis_id)
  quantile(replicate(B, f(zz[unlist(idx[sample(u, length(u), TRUE)], use.names = FALSE)])), c(.025, .975), na.rm = TRUE)
}
ctrl <- "+ carry_share_tr + snap_tr + xfp_tr"
report <- function(dd, form, terms, label) {
  fb <- function(zz) coef(glm(as.formula(paste("benched ~", form, ctrl)), binomial, zz))
  fc <- function(zz) coef(lm(as.formula(paste("d_carry ~", form, ctrl)), zz))
  cb <- fb(dd); cc <- fc(dd)
  cat(sprintf("\n-- %s  (n = %d, benched rate %.3f)\n", label, nrow(dd), mean(dd$benched)))
  for (nm in terms) {
    ib <- boot(dd, function(zz) fb(zz)[[nm]]); ic <- boot(dd, function(zz) fc(zz)[[nm]])
    cat(sprintf("   %-12s bench OR %.2f [%.2f, %.2f]   carry share %+.2f pp [%+.2f, %+.2f]\n",
                nm, exp(cb[[nm]]), exp(ib[1]), exp(ib[2]), 100 * cc[[nm]], 100 * ic[1], 100 * ic[2]))
  }
  m <- glm(as.formula(paste("benched ~", form, ctrl)), binomial, dd)
  invisible(m)
}

cat("\n==== 1. are rushing and receiving efficiency the same skill?\n")
r <- d[!is.na(recoe_tgt)]
cat(sprintf("   corr(rush YOE/carry, rec YOE/target) = %+.3f  (Spearman %+.3f), n = %d\n",
            cor(r$ryoe_att, r$recoe_tgt), cor(r$ryoe_att, r$recoe_tgt, method = "spearman"), nrow(r)))

cat("\n==== 2. rushing vs receiving, alone and together (per 1 SD better; OR < 1 = less likely benched)\n")
report(r, "z_rush", "z_rush", "rushing only")
report(r, "z_rec", "z_rec", "receiving only")
report(r, "z_rush + z_rec", c("z_rush", "z_rec"), "both")
report(r, "z_rush * z_rec", c("z_rush", "z_rec", "z_rush:z_rec"), "both + interaction (bad at both = worse than the sum?)")
r[, combo := fifelse(ryoe_att < quantile(ryoe_att, .33), "rush bad", fifelse(ryoe_att > quantile(ryoe_att, .67), "rush good", "rush mid"))]
r[, combo2 := fifelse(recoe_tgt < quantile(recoe_tgt, .33), "rec bad", fifelse(recoe_tgt > quantile(recoe_tgt, .67), "rec good", "rec mid"))]
cat("\n   benched rate by tercile of each (rows = rushing, cols = receiving; n in brackets):\n")
tab <- r[, .(rate = sprintf("%.3f [%d]", mean(benched), .N)), by = .(combo, combo2)]
print(dcast(tab, factor(combo, c("rush bad", "rush mid", "rush good")) ~ factor(combo2, c("rec bad", "rec mid", "rec good")), value.var = "rate"))

cat("\n==== 3. the back or the situation? own vs teammate vs whole team (RBs with a teammate RB)\n")
tmd <- d[!is.na(tm_ryoe)]
cat(sprintf("   corr(own rush YOE/carry, teammate's) = %+.3f   corr(own, team-wide) = %+.3f\n",
            cor(tmd$ryoe_att, tmd$tm_ryoe), cor(tmd$ryoe_att, tmd$team_ryoe)))
m_own <- report(tmd, "z_rush", "z_rush", "own only")
m_both <- report(tmd, "z_rush + z_tm", c("z_rush", "z_tm"), "own + teammate (teammate OR > 1 = his efficiency takes your carries)")
m_team <- report(tmd, "z_rush + z_team", c("z_rush", "z_team"), "own + team-wide situation")
m_rel <- report(tmd, "z_rel", "z_rel", "relative only (own - teammate)")
cv <- function(form) {        # leave-one-season-out log loss
  ll <- unlist(lapply(2022:2025, function(s) {
    m <- glm(as.formula(paste("benched ~", form, ctrl)), binomial, tmd[season != s])
    pp <- pmin(pmax(predict(m, tmd[season == s], type = "response"), 1e-4), 1 - 1e-4)
    y <- tmd[season == s]$benched
    -(y * log(pp) + (1 - y) * log(1 - pp))
  }))
  mean(ll)
}
cat("\n   leave-one-season-out log loss (lower = better):\n")
for (f in c("1", "z_rush", "z_rel", "z_rush + z_tm", "z_rush + z_team"))
  cat(sprintf("   %-18s %.4f\n", f, cv(f)))
cat("\n   lead backs vs committee backs (relative efficiency):\n")
report(tmd[carry_share_tr >= 0.45], "z_rush + z_tm", c("z_rush", "z_tm"), "lead backs (>= 45% carries)")
report(tmd[carry_share_tr < 0.45], "z_rush + z_tm", c("z_rush", "z_tm"), "committee / backup backs (< 45%)")
saveRDS(d, file.path(S, "ep_rb_bench2.rds"))
