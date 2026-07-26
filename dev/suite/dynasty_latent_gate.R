# Compare move_space variants at the APEX and across rank bands, straight from
# the backtest player CSVs (cover80/abs_err are transform-agnostic - they only
# test whether actual_rank fell in the predicted [q10,q90] rank interval, so
# rank/log/probit runs are directly comparable). Also fits the implied
# latent-space move_slope/move_bias per position so a corrected run can be set up.
#
# Usage: Rscript dev/suite/dynasty_latent_gate.R <tag1> [tag2 ...]
#   tags are FFS_DYN_TAG values, e.g. q_rank q_log q_probit
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
tags <- if (length(args)) args else c("q_rank", "q_log", "q_probit")
out_dir <- file.path("dev", "validate_outputs")

band_of <- function(r) cut(r, c(0, 6, 12, 24, 48, 999),
                           labels = c("1-6", "7-12", "13-24", "25-48", "49+"))

read_tag <- function(tg) {
  f <- file.path(out_dir, paste0("dynasty_backtest_players_", tg, ".csv"))
  if (!file.exists(f)) { cat("MISSING:", f, "\n"); return(NULL) }
  d <- fread(f)
  d <- d[actual_exit == FALSE & n_surv_draws > 10 & !is.na(actual_rank)]
  d[, `:=`(tag = tg, band = band_of(pos_rank),
           pred_delta = med_rank - pos_rank, act_delta = actual_rank - pos_rank)]
  d
}

all <- rbindlist(lapply(tags, read_tag), use.names = TRUE, fill = TRUE)
if (!nrow(all)) stop("no data")

cat("==== survivors: cover80 / MAE / mean deltas by tag x pos_rank band ====\n")
tab <- all[, list(n = .N,
                  cover80 = round(mean(cover80), 3),
                  mae = round(mean(abs_err), 2),
                  pred_d = round(mean(pred_delta), 2),
                  act_d = round(mean(act_delta), 2),
                  pileup1 = round(mean(med_rank <= 1), 3)),
           by = list(tag, band)][order(band, tag)]
print(tab)

cat("\n==== APEX focus (pos_rank 1-6): by tag x pos ====\n")
apex <- all[pos_rank <= 6, list(n = .N,
                  cover80 = round(mean(cover80), 3),
                  mae = round(mean(abs_err), 2),
                  pred_d = round(mean(pred_delta), 2),
                  act_d = round(mean(act_delta), 2)),
           by = list(tag, pos)][order(pos, tag)]
print(apex)

cat("\n==== pooled cover80 / MAE by tag (all survivors, material cur_val>=150) ====\n")
print(all[cur_val >= 150, list(n = .N, cover80 = round(mean(cover80), 3),
                               mae = round(mean(abs_err), 2)), by = tag][order(tag)])

# value-space apex read: is next_value less conservative? exp_change on the
# synthetic curve, material apex players.
cat("\n==== value-space apex (pos_rank<=6, cur_val>=150): median exp_change pred vs act ====\n")
va <- all[pos_rank <= 6 & cur_val >= 150]
va[, `:=`(exp_pred = val_mean / cur_val - 1, exp_act = actual_val / cur_val - 1)]
print(va[, list(n = .N, med_exp_pred = round(median(exp_pred), 3),
                med_exp_act = round(median(exp_act), 3),
                val_cover80 = round(mean(val_cover80), 3)), by = tag][order(tag)])

## ---- latent-space move_slope/move_bias fit -------------------------------------
# For a RAW (corrections-off) latent run, fit act_delta ~ a + b*pred_delta IN THE
# TAG'S TRANSFORM space (T is monotone so T(med_rank) is the latent draw center).
# (a,b) -> (move_bias, move_slope) to plug into a corrected run of that space.
# Space is inferred from the tag suffix: *_log -> log, *_probit -> probit.
space_of <- function(tg) if (grepl("probit", tg)) "probit" else if (grepl("log", tg)) "log" else "rank"
Tf <- function(r, space, d) switch(space,
  rank = r, log = log(r),
  probit = stats::qnorm(pmin(pmax(r / (d + 1), 1e-6), 1 - 1e-6)))
cat("\n==== implied latent move_slope/move_bias (material, per tag x pos) ====\n")
for (tg in tags) {
  sp <- space_of(tg)
  if (sp == "rank") next
  d <- all[tag == tg & cur_val >= 150]
  if (!nrow(d)) next
  # per-position depth for probit = deepest pos_rank seen for that pos in the run
  depth <- all[tag == tg, list(depth = max(pos_rank)), by = pos]
  d <- merge(d, depth, by = "pos")
  d[, `:=`(pd = Tf(med_rank, sp, depth) - Tf(pos_rank, sp, depth),
           ad = Tf(actual_rank, sp, depth) - Tf(pos_rank, sp, depth))]
  fit <- d[, {
    f <- stats::lm(ad ~ pd)
    list(n = .N, move_slope = round(coef(f)[[2]], 3), move_bias = round(coef(f)[[1]], 3),
         b_se = round(summary(f)$coefficients[2, 2], 3))
  }, by = pos][order(pos)]
  cat("--", tg, "(", sp, ") --\n"); print(fit)
}
