# Phase 1 - three more angles on stacking, 5 seasons:
#   (A) QB-WR vs QB-TE : does the stack position matter? (article: WR1-WR2 +0.16,
#       TE-WR2 negative)
#   (B) by QB draft tier : is stacking's effect bigger for elite (early-ADP) QBs?
#   (C) bring-back / game stack : within own-stacked entries, does ALSO rostering
#       the QB's WEEK-15 opponent's pass-catcher raise the week-15 ceiling?
# All within-QB. Advance lift uses regular-season pods; ceiling (sd_ratio) uses
# the single playoff week (rd2 = wk15).

source("R/stack_lib.R")

q95  <- function(x) as.numeric(quantile(x, .95, names = FALSE))
qadp <- function(fact) fact[pos == "QB" & !is.na(adp), .(qb_adp = median(adp)), by = .(qb = pkey)]

perqb_adv <- function(fact, catch_pos, min_side = 100L) {
  em <- unique(fact[, .(entry_id, draft_id, roster_points)])
  em[, adv := as.integer(frank(-roster_points, ties.method = "first") <= 2L), by = draft_id]
  q <- merge(.qb_stack(fact, catch_pos), em[, .(entry_id, adv)], by = "entry_id")
  w <- q[, .(n_y = sum(stacked), n_n = sum(!stacked),
             adv_y = mean(adv[stacked]), adv_n = mean(adv[!stacked])), by = qb]
  w <- merge(w, qadp(fact), by = "qb", all.x = TRUE)
  w <- w[n_y >= min_side & n_n >= min_side]
  w[, `:=`(lift = adv_y - adv_n, wt = pmin(n_y, n_n))][]
}

perqb_week <- function(fact, catch_pos, min_side = 80L) {
  score <- unique(fact[, .(entry_id, s = roster_points)])
  q <- merge(.qb_stack(fact, catch_pos), score, by = "entry_id")
  w <- suppressWarnings(q[, .(n_y = sum(stacked), n_n = sum(!stacked),
        sd_y = sd(s[stacked]), sd_n = sd(s[!stacked])), by = qb])
  w <- merge(w, qadp(fact), by = "qb", all.x = TRUE)
  w <- w[n_y >= min_side & n_n >= min_side]
  w[, `:=`(sd_ratio = sd_y / sd_n, wt = pmin(n_y, n_n))][]
}

# (C) within own-stacked entries, does adding the QB's wk15 opponent's catcher help?
bringback <- function(fact, season, min_side = 40L) {
  sched <- as.data.table(nflreadr::load_schedules(season))[week == 15]
  opp <- rbind(sched[, .(team = home_team, oppo = away_team)],
               sched[, .(team = away_team, oppo = home_team)])
  score <- unique(fact[, .(entry_id, s = roster_points)])
  qb <- unique(fact[pos == "QB" & !is.na(nfl_team), .(entry_id, qb = pkey, qb_team = nfl_team)])
  qb <- merge(qb, opp, by.x = "qb_team", by.y = "team", all.x = TRUE)
  catch <- unique(fact[pos %in% c("WR","TE") & !is.na(nfl_team), .(entry_id, cteam = nfl_team)])
  j <- merge(qb, catch, by = "entry_id", allow.cartesian = TRUE)
  fl <- j[, .(own = any(cteam == qb_team), bb = any(cteam == oppo & !is.na(oppo))),
          by = .(entry_id, qb)]
  fl <- merge(fl, score, by = "entry_id")
  os <- fl[own == TRUE]                       # already same-team stacked
  w <- suppressWarnings(os[, .(n_bb = sum(bb), n_no = sum(!bb),
        s_bb = mean(s[bb]), s_no = mean(s[!bb]),
        sd_bb = sd(s[bb]), sd_no = sd(s[!bb])), by = qb])
  w <- w[n_bb >= min_side & n_no >= min_side]
  w[, `:=`(lift = s_bb - s_no, sd_ratio = sd_bb / sd_no, wt = pmin(n_bb, n_no))]
  data.table(season = season, n_qb = nrow(w),
             wk15_lift = round(weighted.mean(w$lift, w$wt), 2),
             sd_ratio  = round(weighted.mean(w$sd_ratio, w$wt), 3))
}

cfg <- list(
  list(y = 2021L, kind = "thin", reg = "data/parquet/2021_BBMII.parquet",  wk = "data/raw/bbm2021_rd2.csv"),
  list(y = 2022L, kind = "thin", reg = "data/parquet/2022_BBMIII.parquet", wk = sprintf("data/raw/bbm2022_rd2_%02d.csv", 0:2)),
  list(y = 2023L, kind = "rich", reg = "data/parquet/2023_BBMIV",   wk = "data/parquet/2023_BBMIV_rd2"),
  list(y = 2024L, kind = "rich", reg = "data/parquet/2024_BBMV",    wk = "data/parquet/2024_BBMV_rd2"),
  list(y = 2025L, kind = "rich", reg = "data/parquet/2025_BBMVI",   wk = "data/parquet/2025_BBMVI_rd2")
)
reg_prep <- function(c) if (c$kind == "rich") prep_rich(c$reg, c$y) else prep_thin(c$reg, c$y)
wk_prep  <- function(c) if (c$kind == "rich") prep_rich(c$wk, c$y)  else prep_thin_raw(c$wk, c$y)

A_wr_te <- list(); B_tier <- list(); C_bb <- list()
wm <- function(x, w) round(weighted.mean(x, w), 4)

for (c in cfg) {
  cat(sprintf("[year] %d\n", c$y))
  # regular season: advance lift by catch pos + qb_adp for tiers
  fr <- reg_prep(c)$fact
  aw <- perqb_adv(fr, "WR"); at <- perqb_adv(fr, "TE"); aa <- perqb_adv(fr, c("WR","TE"))
  # single week: sd_ratio by catch pos + qb_adp for tiers
  fw <- wk_prep(c)$fact
  ww <- perqb_week(fw, "WR"); wt <- perqb_week(fw, "TE"); wa <- perqb_week(fw, c("WR","TE"))

  A_wr_te[[as.character(c$y)]] <- data.table(
    season = c$y,
    adv_lift_WR = wm(aw$lift, aw$wt), adv_lift_TE = wm(at$lift, at$wt),
    sd_ratio_WR = round(weighted.mean(ww$sd_ratio, ww$wt), 3),
    sd_ratio_TE = round(weighted.mean(wt$sd_ratio, wt$wt), 3))

  # ADP tiers from the QB set (terciles of qb_adp), applied to advance + sd_ratio
  aa[, tier := cut(qb_adp, quantile(qb_adp, 0:3/3, na.rm = TRUE),
                   include.lowest = TRUE, labels = c("elite","mid","late"))]
  wa <- merge(wa, unique(aa[, .(qb, tier)]), by = "qb", all.x = TRUE)
  bt <- merge(
    aa[!is.na(tier), .(adv_lift = wm(lift, wt), qb_adp = round(mean(qb_adp))), by = tier],
    wa[!is.na(tier), .(sd_ratio = round(weighted.mean(sd_ratio, wt), 3)), by = tier], by = "tier")
  bt[, season := c$y]; B_tier[[as.character(c$y)]] <- bt

  C_bb[[as.character(c$y)]] <- bringback(fw, c$y)
  rm(fr, fw); gc(FALSE)
}

cat("\n===== (A) QB-WR vs QB-TE stacks =====\n"); print(rbindlist(A_wr_te))
cat("\n===== (B) stacking by QB draft tier (elite=earliest ADP) =====\n")
print(rbindlist(B_tier)[order(season, tier)])
cat("\n===== (C) bring-back: own-stack + wk15-opponent catcher, within QB =====\n")
print(rbindlist(C_bb))
cat("\nadv_lift>0 or sd_ratio>1 => that stacking form adds advancement / weekly ceiling\n")
