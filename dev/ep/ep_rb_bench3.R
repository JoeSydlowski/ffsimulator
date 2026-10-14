# RB benching on 2012-2025 with injury-driven role loss removed.
#   Rscript dev/ep/ep_rb_bench3.R   (needs dev/ep/ep_panel_long.rds: FFS_EP_SEASONS=2012:2025 FFS_EP_OUT=ep_panel_long.rds)
# Setup as ep_rb_bench2.R (trailing weeks t-3..t -> next 4, >= 20 trailing carries,
# >= 3 of the next 4 games). 2012-2021 is in-sample for the ffopportunity models,
# 2022-25 out of sample - both reported.
# Injury filters (nflverse weekly injury reports):
#   none      - everything
#   own       - drop anchors where the back was Questionable/Doubtful/Out in week t
#               or any of weeks t+1..t+4 (his role fell because he was hurt)
#   own+mate  - also drop anchors where a teammate RB was Out/Doubtful during the
#               trailing weeks (a starter coming back looks like a benching)
suppressPackageStartupMessages(library(data.table))
set.seed(13)
S <- here::here("dev", "ep")
B <- as.integer(Sys.getenv("FFS_EP_BOOT", "300"))
p <- readRDS(file.path(S, "ep_panel_long.rds"))
p[is.na(snap_pct), snap_pct := 0]
SEAS <- sort(unique(p$season))
inj <- as.data.table(nflreadr::load_injuries(seasons = SEAS))[game_type == "REG" & !is.na(gsis_id),
  .(season = as.integer(season), week = as.integer(week), gsis_id, status = report_status)]
inj <- unique(inj, by = c("season", "week", "gsis_id"))
hurt <- inj[status %in% c("Questionable", "Doubtful", "Out")]
out_ <- inj[status %in% c("Doubtful", "Out")]

rows <- list()
K <- as.integer(Sys.getenv("FFS_EP_K", "4"))   # trailing window length (games)
for (s in SEAS) for (t in K:14) {
  w <- p[season == s & week > t - K & week <= t]
  rb <- w[position == "RB", .(g_tr = .N, carries = sum(carries), targets = sum(targets),
    ryoe = sum(rush_yds_oe), recoe = sum(rec_yds_oe), team_carries = sum(team_carries),
    snap_tr = mean(snap_pct), xfp_tr = mean(xfp), team = names(which.max(table(team)))), by = gsis_id]
  room <- rb[, .(room_ryoe = sum(ryoe), room_car = sum(carries)), by = team]
  rb <- merge(rb, room, by = "team")
  rb[, `:=`(tm_car = room_car - carries, tm_ryoe_yds = room_ryoe - ryoe)]
  # teammate RBs who were Out/Doubtful during the trailing window (all RBs on the roster that season)
  roster <- unique(p[season == s & position == "RB", .(gsis_id, team)])
  mate_out <- merge(out_[season == s & week > t - K & week <= t, .(gsis_id)], roster, by = "gsis_id")[, .(mate_out = TRUE), by = team]
  rb <- merge(rb, unique(mate_out), by = "team", all.x = TRUE)[is.na(mate_out), mate_out := FALSE]
  # own injury flag: hurt in week t or t+1..t+4
  own_h <- unique(hurt[season == s & week >= t & week <= t + 4, .(gsis_id, own_hurt = TRUE)])
  rb <- merge(rb, own_h, by = "gsis_id", all.x = TRUE)[is.na(own_hurt), own_hurt := FALSE]
  rb <- rb[g_tr >= K - 1 & carries >= 20]
  fu <- p[season == s & week > t & week <= t + 4 & position == "RB",
          .(g_fu = .N, carry_share_fu = sum(carries) / pmax(sum(team_carries), 1)), by = gsis_id]
  x <- merge(rb, fu[g_fu >= 3], by = "gsis_id")
  if (nrow(x)) rows[[length(rows) + 1]] <- x[, `:=`(season = s, week = t)]
}
d <- rbindlist(rows)
d[, `:=`(ryoe_att = ryoe / carries, recoe_tgt = fifelse(targets >= 5, recoe / targets, NA_real_),
         carry_share_tr = carries / pmax(team_carries, 1), tm_ryoe = fifelse(tm_car >= 10, tm_ryoe_yds / (tm_car + 30), NA_real_))]  # shrunk: 30 phantom carries at 0 YOE, so one long run on 2 carries cannot make a room look efficient
d[, `:=`(benched = carry_share_fu < 0.6 * carry_share_tr, d_carry = carry_share_fu - carry_share_tr,
         era = fifelse(season <= 2021, "2013-21 (in-sample)", "2022-25 (out of sample)"))]
z <- function(v) (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE)
d[, `:=`(z_rush = z(ryoe_att), z_rec = z(recoe_tgt), z_tm = z(tm_ryoe))]
d[, pid := gsis_id]
cat("anchors:", nrow(d), "  players:", uniqueN(d$gsis_id), "  seasons:", paste(range(d$season), collapse = "-"), "\n")
cat(sprintf("flagged: own injury %.1f%%, teammate RB out %.1f%%\n", 100 * mean(d$own_hurt), 100 * mean(d$mate_out)))
cat("benched rate by injury flag:\n"); print(d[, .(n = .N, benched = round(mean(benched), 3)), by = .(own_hurt, mate_out)][order(own_hurt, mate_out)])

boot <- function(zz, f) {
  u <- unique(zz$pid); idx <- split(seq_len(nrow(zz)), zz$pid)
  quantile(replicate(B, f(zz[unlist(idx[sample(u, length(u), TRUE)], use.names = FALSE)])), c(.025, .975), na.rm = TRUE)
}
ctrl <- "+ carry_share_tr + snap_tr + xfp_tr"
est <- function(dd, form, term, what = c("or", "pp")) {
  what <- match.arg(what)
  f <- if (what == "or") function(zz) coef(glm(as.formula(paste("benched ~", form, ctrl)), binomial, zz))[[term]]
       else function(zz) coef(lm(as.formula(paste("d_carry ~", form, ctrl)), zz))[[term]]
  e <- f(dd); ci <- boot(dd, f)
  if (what == "or") sprintf("%.2f [%.2f, %.2f]", exp(e), exp(ci[1]), exp(ci[2]))
  else sprintf("%+.2f [%+.2f, %+.2f]", 100 * e, 100 * ci[1], 100 * ci[2])
}
filters <- list(none = quote(TRUE), own = quote(!own_hurt), `own+mate` = quote(!own_hurt & !mate_out))
res <- list()
for (fn in names(filters)) for (er in c("all 2013-25", "2013-21 (in-sample)", "2022-25 (out of sample)")) {
  keep_rows <- rep_len(eval(filters[[fn]], d), nrow(d))
  dd <- d[keep_rows]
  if (er != "all 2013-25") dd <- dd[era == er]
  r <- dd[!is.na(recoe_tgt)]; tm <- dd[!is.na(tm_ryoe)]
  res[[length(res) + 1]] <- data.table(
    filter = fn, era = er, n = nrow(dd), benched = round(mean(dd$benched), 3),
    rush_OR = est(r, "z_rush + z_rec", "z_rush"), rec_OR = est(r, "z_rush + z_rec", "z_rec"),
    rush_pp = est(r, "z_rush + z_rec", "z_rush", "pp"),
    mate_pp = est(tm, "z_rush + z_tm", "z_tm", "pp"),
    lead_rush_OR = est(dd[carry_share_tr >= 0.45 & !is.na(recoe_tgt)], "z_rush + z_rec", "z_rush"),
    cmte_rush_OR = est(dd[carry_share_tr < 0.45 & !is.na(recoe_tgt)], "z_rush + z_rec", "z_rush"))
  cat(".")
}
res <- rbindlist(res)
cat("\n\nper 1 SD better efficiency. OR = odds of losing 40%+ of carry share next month; pp = change in carry share.\n")
cat("rush/rec from a joint model with both; mate_pp = effect of the TEAMMATE RB being 1 SD more efficient on YOUR carry share\n\n")
print(res[, .(filter, era, n, benched, rush_OR, rec_OR, rush_pp)])
cat("\n"); print(res[, .(filter, era, mate_pp, lead_rush_OR, cmte_rush_OR)])

cat("\nbenched rate by trailing rush-efficiency quintile, injury-filtered (own+mate), all seasons:\n")
dd <- d[!own_hurt & !mate_out]
dd[, q := cut(frank(ryoe_att, ties.method = "random") / .N, c(0, .2, .4, .6, .8, 1), labels = c("Q1 worst", "Q2", "Q3", "Q4", "Q5 best"))]
print(dd[, .(n = .N, ryoe_att = round(mean(ryoe_att), 2), benched = round(mean(benched), 3),
             lead_benched = round(mean(benched[carry_share_tr >= .45]), 3), cmte_benched = round(mean(benched[carry_share_tr < .45]), 3)), by = q][order(q)])
saveRDS(list(d = d, res = res, K = K), file.path(S, if (K == 4) "ep_rb_bench3.rds" else sprintf("ep_rb_bench3_k%d.rds", K)))
