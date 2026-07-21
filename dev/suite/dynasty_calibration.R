# Point-prediction calibration of the dynasty transition model, by position
# and direction, across all backtest holdouts - and the per-position linear
# recalibration (move_slope/move_bias) it implies.
#
# Reads one or more per-player backtest outputs (dev/suite/dynasty_backtest.R
# players csvs) and reports:
#
# RANK space (survivors, positional-rank moves, improvement-positive signs):
#   slope      lm(actual_move ~ predicted_move) - 1 = perfectly scaled,
#              < 1 = predicted magnitudes too extreme
#   bias       mean(actual - predicted move); > 0 = model too bearish
#   disp_ratio sd(pred)/sd(act)
#   direction  split by SIGN of the predicted move (riser cells get thin)
#
# FIT: per-position linear map actual ~ a + b * predicted (delta space,
#   decline-positive - the .ffs_draw_transition move_slope/move_bias
#   convention), formats combined; leave-one-holdout-out (LOHO) stability,
#   and an ANALYTIC LOHO evaluation of the corrected predictions (centers
#   and intervals shift by the same per-player delta, so corrected
#   slope/bias/mae/cover80 are computable without re-running draws).
#   Residuals by rank band + direction test whether one line per position
#   is enough.
#
# VALUE space (when the csv carries val_* columns): the rank move mapped
#   through the synthetic value curve on the season-Y ladder - what
#   exp_change readers actually see. Material players only (cur_val >= 150)
#   for ratio metrics; $-space regression for the stud-weighted view.
#
# Usage: Rscript dev/suite/dynasty_calibration.R tag1 [tag2 ...]
#   (default: calib_1qb calib_sflx)
# Writes dev/validate_outputs/dynasty_calibration_{rank,value,fit}_<label>.csv
# where <label> = first tag.

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
tags <- if (length(args) >= 1) args else c("calib_1qb", "calib_sflx")
out_dir <- file.path("dev", "validate_outputs")
players <- rbindlist(lapply(tags, function(tg)
  fread(file.path(out_dir, paste0("dynasty_backtest_players_", tg, ".csv")))),
  use.names = TRUE, fill = TRUE)
label <- tags[[1]]

surv <- players[actual_exit == FALSE & n_surv_draws > 10 & !is.na(actual_rank)]
# improvement-positive sign convention: +5 = climbed 5 positional ranks
surv[, `:=`(pred_move = pos_rank - med_rank,
            act_move  = pos_rank - actual_rank)]
surv[, direction := fifelse(pred_move > 0, "pred_riser", "pred_decliner")]

calib <- function(dt, pred = "pred_move", act = "act_move") {
  x <- dt[[pred]]; y <- dt[[act]]
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  n <- length(x)
  if (n < 3 || stats::var(x) == 0) {
    return(list(n = n, slope = NA_real_, slope_se = NA_real_, bias = NA_real_,
                disp_ratio = NA_real_, cor = NA_real_,
                mean_pred = NA_real_, mean_act = NA_real_))
  }
  fit <- stats::lm(y ~ x)
  sm <- summary(fit)$coefficients
  list(
    n = n,
    slope = round(sm[2, 1], 3),
    slope_se = round(sm[2, 2], 3),
    bias = round(mean(y - x), 2),
    disp_ratio = round(stats::sd(x) / stats::sd(y), 3),
    cor = round(stats::cor(x, y), 3),
    mean_pred = round(mean(x), 2),
    mean_act = round(mean(y), 2)
  )
}

## ---- rank space -------------------------------------------------------------------
cat("==== RANK space, POOLED (all holdouts): by format x pos ====\n")
by_pos <- surv[, c(calib(.SD), list(cover80 = round(mean(cover80), 3),
                                    mae = round(mean(abs_err), 2))),
               by = list(format, pos)][order(format, pos)]
print(by_pos)

cat("\n==== RANK space: by format x pos x predicted direction ====\n")
by_dir <- surv[, calib(.SD), by = list(format, pos, direction)][order(format, pos, direction)]
by_dir[, thin := fifelse(n < 50, "*THIN*", "")]
print(by_dir)

cat("\n==== RANK space: per-holdout slope/bias pooled over positions ====\n")
by_hold <- surv[, calib(.SD), by = list(format, holdout)][order(format, holdout)]
print(by_hold)

## ---- fit the per-position linear correction (delta space) -------------------------
# delta space (decline-positive) so (a, b) drop straight into
# move_bias/move_slope. Formats combined: superflex borrows the 1qb movement
# pool, per-format slopes agree within ~1.5 se, and shared constants mirror
# the shared disp_factor.
surv[, `:=`(pred_delta = med_rank - pos_rank, act_delta = actual_rank - pos_rank,
            material = cur_val >= 150)]
fit_fun <- function(dt) dt[, {
  f <- stats::lm(act_delta ~ pred_delta)
  list(n = .N, b = round(coef(f)[[2]], 3), a = round(coef(f)[[1]], 2),
       b_se = round(summary(f)$coefficients[2, 2], 3))
}, by = pos]
cat("\n==== FIT: actual_delta ~ a + b * pred_delta, formats combined ====\n")
fit_all <- fit_fun(surv)
print(fit_all)
# the SHIPPED constants come from the MATERIAL subset (cur_val >= 150, the
# trade-tool floor): the all-player fit is dominated by deep players whose
# larger positional drift drags the intercept up and over-corrects the
# material ranks users actually trade (top-48 residuals +4..+7)
cat("\n==== FIT on MATERIAL players only (shipped-constant source) ====\n")
fit_pos <- fit_fun(surv[material == TRUE])
print(fit_pos)

cat("\n==== FIT: LOHO stability (b and a refit excluding each holdout) ====\n")
holds <- surv[, unique(paste(format, holdout))]
if (length(holds) < 2) holds <- character(0)  # single holdout: no LOHO folds
loho <- rbindlist(lapply(holds, function(h) {
  tr <- surv[material == TRUE & paste(format, holdout) != h]
  tr[, {
    f <- stats::lm(act_delta ~ pred_delta)
    list(excl = h, b = round(coef(f)[[2]], 3), a = round(coef(f)[[1]], 2))
  }, by = pos]
}))
if (nrow(loho)) {
  print(dcast(loho, pos ~ excl, value.var = "b"))
  print(loho[, list(b_min = min(b), b_max = max(b),
                    a_min = min(a), a_max = max(a)), by = pos])
} else cat("(single holdout - skipped)\n")

## ---- analytic LOHO evaluation of the corrected predictions ------------------------
# corrected center = a + b * pred_delta; the whole predictive distribution
# shifts by shift = (a + (b-1) * pred_delta), so corrected q10/q90/med all
# move together (dispersion untouched - it belongs to disp_factor).
surv[, shift := NA_real_]
if (length(holds)) {
  for (h in holds) {
    f <- surv[material == TRUE & paste(format, holdout) != h][
      , as.list(coef(stats::lm(act_delta ~ pred_delta))), by = pos]
    setnames(f, c("pos", "a_l", "b_l"))
    surv[paste(format, holdout) == h,
         shift := f$a_l[match(pos, f$pos)] + (f$b_l[match(pos, f$pos)] - 1) * pred_delta]
  }
} else {
  # single holdout: in-sample fit (smoke runs only)
  surv[fit_pos, shift := i.a + (i.b - 1) * pred_delta, on = "pos"]
}
surv[, `:=`(med_c = med_rank + shift, q10_c = q10 + shift, q90_c = q90 + shift)]
surv[, `:=`(pred_move_c = pos_rank - med_c,
            cover80_c = actual_rank >= q10_c & actual_rank <= q90_c,
            abs_err_c = abs(med_c - actual_rank))]
cat("\n==== LOHO-corrected (analytic): by format x pos ====\n")
by_pos_c <- surv[, c(calib(.SD, pred = "pred_move_c"),
                     list(cover80 = round(mean(cover80_c), 3),
                          mae = round(mean(abs_err_c), 2))),
                 by = list(format, pos)][order(format, pos)]
print(by_pos_c)
cat("\n==== LOHO-corrected: by pos x material (formats pooled) ====\n")
print(surv[, c(calib(.SD, pred = "pred_move_c"),
               list(cover80_base = round(mean(cover80), 3),
                    cover80 = round(mean(cover80_c), 3),
                    mae_base = round(mean(abs_err), 2),
                    mae = round(mean(abs_err_c), 2))),
           by = list(pos, material)][order(pos, -material)])
cat("\n==== LOHO-corrected: by direction (of the ORIGINAL prediction) ====\n")
by_dir_c <- surv[, calib(.SD, pred = "pred_move_c"), by = list(format, pos, direction)][
  order(format, pos, direction)]
print(by_dir_c)

cat("\n==== residual structure: mean corrected residual by pos_rank band ====\n")
surv[, resid_c := act_move - pred_move_c]
surv[, rank_band := cut(pos_rank, c(0, 6, 12, 24, 48, 999),
                        labels = c("1-6", "7-12", "13-24", "25-48", "49+"))]
print(dcast(surv[, list(r = round(mean(resid_c), 1), n = .N), by = list(pos, rank_band)],
            pos ~ rank_band, value.var = "r"))

## ---- value space ------------------------------------------------------------------
has_val <- "val_mean" %in% names(players)
if (has_val) {
  cat("\n==== VALUE space (synthetic curve, season-Y ladder) ====\n")
  # material players only for ratio metrics: tiny cur_val explodes exp_change
  # (trade_intel floors ratios at cur_value >= 150 for the same reason)
  sv <- surv[cur_val >= 150]
  sv[, `:=`(exp_pred_surv = val_mean_surv / cur_val - 1,
            exp_act = actual_val / cur_val - 1)]
  cat("\n-- survivors, exp_change space (cur_val >= 150), by format x pos --\n")
  vp <- sv[, c(calib(.SD, pred = "exp_pred_surv", act = "exp_act"),
               list(val_cover80 = round(mean(val_cover80), 3),
                    val_pit = round(mean(val_pit), 3))),
           by = list(format, pos)][order(format, pos)]
  print(vp)
  cat("\n-- survivors, exp_change space, by direction of predicted value move --\n")
  sv[, vdirection := fifelse(exp_pred_surv > 0, "pred_riser", "pred_decliner")]
  vd <- sv[, calib(.SD, pred = "exp_pred_surv", act = "exp_act"),
           by = list(format, pos, vdirection)][order(format, pos, vdirection)]
  vd[, thin := fifelse(n < 50, "*THIN*", "")]
  print(vd[])
  cat("\n-- survivors, $-space (stud-weighted): d_value act ~ pred, by format x pos --\n")
  sv[, `:=`(dval_pred = val_mean_surv - cur_val, dval_act = actual_val - cur_val)]
  print(sv[, calib(.SD, pred = "dval_pred", act = "dval_act"),
           by = list(format, pos)][order(format, pos)])
  cat("\n-- ALL material players incl exits (user-facing exp_change) --\n")
  al <- players[cur_val >= 150]
  print(al[, calib(.SD, pred = "exp_change_pred", act = "exp_change_act"),
           by = list(format, pos)][order(format, pos)])
  cat("\n-- per-holdout stability, survivors exp_change slope --\n")
  vh <- sv[, calib(.SD, pred = "exp_pred_surv", act = "exp_act"),
           by = list(format, holdout)][order(format, holdout)]
  print(vh)
  add_level <- function(dt, lev) { d <- copy(dt); d[, level := lev]; d[] }
  fwrite(rbindlist(list(add_level(vp, "pos_surv"), add_level(vd, "pos_dir"),
                        add_level(vh, "holdout")), use.names = TRUE, fill = TRUE),
         file.path(out_dir, paste0("dynasty_calibration_value_", label, ".csv")))
}

## ---- write ------------------------------------------------------------------------
add_level2 <- function(dt, lev) { d <- copy(dt); d[, level := lev]; d[] }
fwrite(rbindlist(list(add_level2(by_pos, "pos_base"),
                      add_level2(by_pos_c, "pos_loho_corrected"),
                      add_level2(by_dir, "dir_base"),
                      add_level2(by_dir_c, "dir_loho_corrected")),
                 use.names = TRUE, fill = TRUE),
       file.path(out_dir, paste0("dynasty_calibration_rank_", label, ".csv")))
fwrite(rbindlist(list(add_level2(fit_pos, "pooled"),
                      if (nrow(loho)) add_level2(loho, "loho")),
                 use.names = TRUE, fill = TRUE),
       file.path(out_dir, paste0("dynasty_calibration_fit_", label, ".csv")))
cat("\nwrote dynasty_calibration_{rank,value,fit}_", label, ".csv\nDONE\n", sep = "")
