# =====================================================================
# exchange_rate_study.R - what is 1% of playoff probability worth, in
# dynasty-value points, and how much should PROJECTED future value be
# discounted? Derives the trade-score exchange rate R = future-value
# points per +1% playoff = k_P / delta, plus the auxiliary rates the
# deal builder's screen needs (wins<->playoff). Posture/leverage-aware.
#
# All inputs are SAVED league reports + the dynasty value-calibration -
# no new simulations. Runs in seconds.
#
#   k_P            value pts per +1% playoff  (win-now price of playoff)
#   win_to_playoff +playoff% per +1 h2h win   (screen currency)
#   delta_stat     realized/projected future-value reliability (<1)
#   delta_risk     win-now time-preference haircut, by posture
#   R = k_P / (delta_stat * delta_risk)   future pts per +1% playoff
#
# Outputs: dev/validate_outputs/exchange_rate_study.{csv,txt}
# Usage:   Rscript dev/suite/exchange_rate_study.R
# =====================================================================

suppressMessages({ library(data.table); library(here) })

MY_TEAM  <- Sys.getenv("FFS_MY_TEAM", "sox05syd")
LG <- list(
  jon = "1359546500786434048",   # superflex, WR-stacked (this session)
  jml = "1326464763936403456"    # superflex, 2QB
)
sim_root <- here::here("dev", "league_sims")
vo       <- here::here("dev", "validate_outputs")

# newest report folder for a league that has the sheets we need
latest_report <- function(id, need = "roster.csv") {
  fs <- Sys.glob(file.path(sim_root, id, "*", need))
  if (!length(fs)) return(NA_character_)
  dirname(fs[order(file.info(fs)$mtime, decreasing = TRUE)][1])
}

## ---- gather per-league inputs --------------------------------------------------
rd <- function(d, f, ...) { p <- file.path(d, f); if (file.exists(p)) fread(p, ...) else NULL }
pooled_kp <- list(); pooled_odds <- list(); posture <- list(); posmix <- list()
for (lg in names(LG)) {
  d <- latest_report(LG[[lg]])
  if (is.na(d)) { message("no report for ", lg); next }
  message(lg, ": ", d)
  ros <- rd(d, "roster.csv"); tg <- rd(d, "targets.csv"); od <- rd(d, "playoff_odds.csv")

  # k_P rows: win-now value vs playoff-added, MY team's exchange rate. roster =
  # leave-one-out playoff_add (all confirmed on the standings sim); targets =
  # confirmed-only playoff_delta_you (n=60 rows are too noisy for a price line).
  kp <- rbindlist(list(
    if (!is.null(ros)) ros[win_now_value > 0 & is.finite(playoff_add) & playoff_add > 0,
        .(league = lg, src = "roster", pos, win_now_value, playoff = playoff_add)],
    if (!is.null(tg)) tg[win_now_value > 0 & is.finite(playoff_delta_you) & playoff_delta_you > 0 &
        (is.null(tg$confirmed) | confirmed == TRUE),
        .(league = lg, src = "target", pos, win_now_value, playoff = playoff_delta_you)]
  ), use.names = TRUE, fill = TRUE)
  pooled_kp[[lg]] <- kp

  # wins<->playoff and champ leverage: cross-sectional over the 12 franchises
  if (!is.null(od)) { od[, league := lg]; pooled_odds[[lg]] <- od
    me <- od[franchise_name == MY_TEAM]
    if (nrow(me)) posture[[lg]] <- data.table(league = lg, my_playoff = me$playoff_pct[1],
                                              my_champ = me$champion_pct[1]) }
  # my roster position-value mix (to blend the per-position future reliability)
  if (!is.null(ros)) posmix[[lg]] <- ros[cur_value > 0, .(league = lg, pos, cur_value)]
}
kp   <- rbindlist(pooled_kp, use.names = TRUE, fill = TRUE)
odds <- rbindlist(pooled_odds, use.names = TRUE, fill = TRUE)
post <- rbindlist(posture, use.names = TRUE, fill = TRUE)
pmix <- rbindlist(posmix, use.names = TRUE, fill = TRUE)

## ---- 1. k_P: value points per +1% playoff --------------------------------------
# playoff is a FRACTION (0.19 = 19%), so slope is per 1.0 playoff; /100 = per 1%.
fit_slope <- function(dt) if (nrow(dt) >= 8) {
  m <- lm(win_now_value ~ playoff, dt); s <- summary(m)
  data.table(n = nrow(dt), slope_per1 = coef(m)[2] / 100,
             se_per1 = s$coefficients[2, 2] / 100, r2 = s$r.squared,
             med_ratio = median(dt$win_now_value / (100 * dt$playoff)))
} else data.table(n = nrow(dt), slope_per1 = NA, se_per1 = NA, r2 = NA, med_ratio = NA)

kp_overall <- fit_slope(kp)[, grp := "pooled"]
kp_league  <- kp[, fit_slope(.SD), by = league][, grp := "league"]
kp_pos     <- kp[, fit_slope(.SD), by = pos][, grp := "pos"]
kP <- kp_overall$slope_per1                      # headline marginal k_P
kP_lo <- kP - 1.96 * kp_overall$se_per1; kP_hi <- kP + 1.96 * kp_overall$se_per1

## ---- 2. wins<->playoff (screen currency) + champ leverage -----------------------
w2p <- lm(playoff_pct ~ mean_wins, odds)             # playoff FRACTION per win
win_to_playoff <- coef(w2p)[2] * 100                 # +playoff % per +1 win
champ_lev <- lm(champion_pct ~ playoff_pct, odds)    # title equity per berth equity
# leverage check: does the k_P price line steepen for a bubble team vs a safer one?
lev <- merge(kp_league[, .(league, kP_league = slope_per1)],
             post[, .(league, my_playoff)], by = "league", all.x = TRUE)

## ---- 3. delta_stat: reliability of PROJECTED future value -----------------------
# The dynasty value-calibration slope = lm(actual_value_move ~ predicted_move):
# <1 means projected moves are too extreme (attenuation), so a projected future
# GAIN realizes only ~slope of face. Position-specific; blend by my roster's
# position-value mix. Superflex primary (both leagues are SF); 1qb as fallback.
cal <- fread(file.path(vo, "dynasty_calibration_value_calib_1qb.csv"))
relslope <- cal[level == "pos_surv" & format == "superflex", .(pos, delta_pos = pmin(pmax(slope, 0), 1),
                                                               cover80 = val_cover80)]
wts <- pmix[, .(w = sum(cur_value)), by = pos]
relblend <- merge(relslope, wts, by = "pos", all.x = TRUE)[!is.na(w)]
delta_stat <- relblend[, sum(delta_pos * w) / sum(w)]     # roster-value-weighted reliability

## ---- 4. delta_risk(posture) + R -------------------------------------------------
# Stated win-now time-preference haircut on (already reliability-adjusted) future
# value. A contender values future value least; a rebuilder ~at face. EXPLICIT
# assumption - swept in the sensitivity block below.
delta_risk <- c(contend = 0.65, bubble = 0.80, rebuild = 1.00)
posture_of <- function(p) fifelse(p >= 0.55, "contend", fifelse(p >= 0.35, "bubble", "rebuild"))
R_of <- function(kp, ds, dr) kp / (ds * dr)          # future pts per +1% playoff

Rtab <- rbindlist(lapply(names(delta_risk), function(ps)
  data.table(posture = ps, delta_risk = delta_risk[[ps]],
             R = R_of(kP, delta_stat, delta_risk[[ps]]))))

## ---- 5. sensitivity -------------------------------------------------------------
sens <- CJ(kP = c(kP_lo, kP, kP_hi),
           delta_stat = c(delta_stat * 0.75, delta_stat, min(1, delta_stat * 1.25)),
           delta_risk = unname(delta_risk))[, R := R_of(kP, delta_stat, delta_risk)]

## ---- outputs --------------------------------------------------------------------
metrics <- rbindlist(list(
  data.table(metric = "k_P_per1pct", value = round(kP, 1), note = sprintf("95%% CI %.0f-%.0f; r2=%.2f; n=%d", kP_lo, kP_hi, kp_overall$r2, kp_overall$n)),
  data.table(metric = "win_to_playoff_pct", value = round(win_to_playoff, 2), note = "+playoff% per +1 h2h win (screen)"),
  data.table(metric = "champ_per_playoff", value = round(coef(champ_lev)[2], 3), note = "title equity per berth equity"),
  data.table(metric = "delta_stat_blend", value = round(delta_stat, 3), note = "roster-value-weighted future reliability (SF)"),
  rbindlist(lapply(names(delta_risk), function(p) data.table(metric = paste0("delta_risk_", p), value = delta_risk[[p]], note = "stated"))),
  Rtab[, .(metric = paste0("R_", posture), value = round(R), note = "future pts per +1% playoff")]
), use.names = TRUE, fill = TRUE)
fwrite(metrics, file.path(vo, "exchange_rate_study.csv"))
fwrite(sens,    file.path(vo, "exchange_rate_study_sensitivity.csv"))

sink(file.path(vo, "exchange_rate_study.txt"))
cat("=== EXCHANGE-RATE STUDY  (", format(Sys.Date()), ") ===\n\n")
cat("R = future-value points per +1% playoff = k_P / (delta_stat * delta_risk)\n\n")
cat("-- k_P: win-now value points per +1% playoff --\n"); print(rbind(kp_overall, kp_league, kp_pos, fill = TRUE))
cat("\n-- wins<->playoff & champ leverage --\n")
cat(sprintf("win_to_playoff = %+.2f%% playoff per +1 win\n", win_to_playoff))
cat(sprintf("champ_per_playoff slope = %.3f (title equity per unit berth equity)\n", coef(champ_lev)[2]))
cat("leverage check (k_P slope vs my baseline playoff, per league):\n"); print(lev)
cat("\n-- delta_stat: future-value reliability (superflex pos_surv, blended by my roster) --\n")
print(relblend); cat(sprintf("blended delta_stat = %.3f\n", delta_stat))
cat("\n-- posture & resulting R --\n"); print(post); print(Rtab)
cat("\n-- sensitivity (R across k_P CI x delta_stat +-25%% x posture) --\n")
cat(sprintf("R range: %.0f - %.0f ; central (bubble) = %.0f\n",
            min(sens$R), max(sens$R), Rtab[posture == "bubble", R]))
cat("\nRECOMMENDED DEFAULTS to bake into ffs_build_trades:\n")
cat(sprintf("  k_P = %.0f value pts / 1%% playoff\n", kP))
cat(sprintf("  win_to_playoff = %.3f playoff-frac / win\n", coef(w2p)[2]))
cat(sprintf("  delta_stat (SF, by pos) = %s\n", paste(sprintf("%s:%.2f", relslope$pos, relslope$delta_pos), collapse=" ")))
cat(sprintf("  delta_risk = contend %.2f / bubble %.2f / rebuild %.2f\n", delta_risk[1], delta_risk[2], delta_risk[3]))
cat(sprintf("  => R(bubble) = %.0f future pts / 1%% playoff\n", Rtab[posture=="bubble", R]))
sink()

cat(readLines(file.path(vo, "exchange_rate_study.txt")), sep = "\n")
