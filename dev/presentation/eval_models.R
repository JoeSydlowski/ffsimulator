# Out-of-sample comparison of the dynasty transition model against baselines, on
# the same 11 rolling-origin holdouts (train on transitions finished by season Y,
# predict Y -> Y+1, grade against the real Y+1 FantasyPros dynasty ranks).
#   Rscript dev/presentation/eval_models.R
# Reads the backtest player files written by dev/suite/dynasty_backtest.R
# (tags below) and writes dev/presentation/eval.rds for the deck.
suppressPackageStartupMessages(library(data.table))
devtools::load_all(here::here(), quiet = TRUE)
V <- Sys.getenv("FFS_EVAL_DIR", here::here("dev", "validate_outputs"))
S <- here::here("dev", "presentation")
scoring <- readRDS(Sys.getenv("FFS_DYN_SCORING",
                              "C:/Github/ffsimulator/dev/validate_outputs/scoring_history_2012_2025.rds"))
tags <- c(pre_base = "Model, preseason", pre_gap05 = "Model + gap (h=0.5), preseason",
          pre_gap10 = "Model + gap (h=1.0), preseason", act_base = "Model, actual season (hindsight)")
tags <- tags[file.exists(file.path(V, paste0("dynasty_backtest_players_", names(tags), ".csv")))]
read_bt <- function(tg) {
  p <- fread(file.path(V, paste0("dynasty_backtest_players_", tg, ".csv")))
  p[, fantasypros_id := as.character(fantasypros_id)]
  if (!"val_med" %in% names(p)) p[, val_med := NA_real_]
  p[, .(format, holdout, fantasypros_id, player_name, pos, age, pos_rank, redraft_rank, q, cur_val,
        actual_rank, actual_exit, actual_val, med_rank, q10, q90, p_exit, val_med, method = tags[[tg]])]
}
bt <- rbindlist(lapply(names(tags), read_bt), fill = TRUE)
key <- unique(bt[method == tags[[1]], .(format, holdout, fantasypros_id, player_name, pos, age, pos_rank,
                                        redraft_rank, q, cur_val, actual_rank, actual_exit, actual_val)])

# ---- baselines, refit per holdout on transitions into seasons <= Y -------------
pools <- list(`1qb` = .ffs_dynasty_transition_pools(scoring, format = "1qb")$transitions,
              superflex = .ffs_dynasty_transition_pools(scoring, format = "superflex")$transitions)
band <- function(age, rank) list(
  ab = cut(age, c(0, 23, 26, 29, 99), labels = FALSE),
  rb = cut(rank, c(0, 6, 12, 24, 48, Inf), labels = FALSE))
baseline <- function(fmt, Y) {
  # movement pool mirrors the model: superflex borrows the 1qb movement history
  tr_m <- pools[["1qb"]][season + 1 <= Y & !is.na(age)]
  tr_e <- pools[[fmt]][season + 1 <= Y & !is.na(age)]
  tg <- key[format == fmt & holdout == Y]
  tr_m[, c("ab", "rb") := band(age, pos_rank)]; tr_e[, c("ab", "rb") := band(age, pos_rank)]
  tg[, c("ab", "rb") := band(age, pos_rank)]
  tg[, redraft_gap := fifelse(!is.na(redraft_rank) & redraft_rank > 0, log(pos_rank / redraft_rank), NA_real_)]
  surv <- tr_m[exited == FALSE][, mv := log(next_pos_rank / pos_rank)]
  out <- list()
  # B0: no change; exit = training base rate by position
  er <- tr_e[, .(p_exit = mean(exited)), by = pos]
  out$b0 <- merge(tg, er, by = "pos", all.x = TRUE)[, `:=`(med_rank = pos_rank, q10 = pos_rank, q90 = pos_rank, method = "No change")]
  # B1: lookup table, pos x age band x rank band (empirical quantiles of the log move)
  cell <- surv[, .(m10 = quantile(mv, .1), m50 = quantile(mv, .5), m90 = quantile(mv, .9), n = .N), by = .(pos, ab, rb)]
  ecell <- tr_e[, .(p_exit = mean(exited)), by = .(pos, ab, rb)]
  b1 <- merge(merge(tg, cell, by = c("pos", "ab", "rb"), all.x = TRUE), ecell, by = c("pos", "ab", "rb"), all.x = TRUE)
  b1[is.na(m50), `:=`(m10 = 0, m50 = 0, m90 = 0)]; b1[is.na(p_exit), p_exit := mean(tr_e$exited)]
  out$b1 <- b1[, `:=`(med_rank = pos_rank * exp(m50), q10 = pos_rank * exp(m10), q90 = pos_rank * exp(m90), method = "Age x rank lookup")]
  # B2 / B3: linear model in log move (+ redraft gap), residual quantiles; logistic exit
  surv[, redraft_gap := fifelse(!is.na(redraft_rank) & redraft_rank > 0, log(pos_rank / redraft_rank), NA_real_)]
  tr_e[, redraft_gap := fifelse(!is.na(redraft_rank) & redraft_rank > 0, log(pos_rank / redraft_rank), NA_real_)]
  for (with_gap in c(FALSE, TRUE)) {
    f_m <- mv ~ pos * (log(pos_rank) + age + I(age^2))
    f_e <- exited ~ pos * (log(pos_rank) + age + I(age^2))
    if (with_gap) { f_m <- update(f_m, . ~ . + redraft_gap); f_e <- update(f_e, . ~ . + redraft_gap) }
    sm <- if (with_gap) surv[!is.na(redraft_gap)] else surv
    se <- if (with_gap) tr_e[!is.na(redraft_gap)] else tr_e
    lm_m <- lm(f_m, sm); gl_e <- glm(f_e, binomial, se)
    r <- residuals(lm_m)
    t2 <- copy(tg)
    if (with_gap) t2[is.na(redraft_gap), redraft_gap := 0]
    mu <- predict(lm_m, t2)
    t2[, `:=`(med_rank = pos_rank * exp(mu + quantile(r, .5)), q10 = pos_rank * exp(mu + quantile(r, .1)),
              q90 = pos_rank * exp(mu + quantile(r, .9)), p_exit = predict(gl_e, t2, type = "response"),
              method = if (with_gap) "Linear + redraft gap" else "Linear (pos, rank, age)")]
    out[[if (with_gap) "b3" else "b2"]] <- t2
  }
  rbindlist(out, fill = TRUE)
}
grid <- unique(key[, .(format, holdout)])
base <- rbindlist(lapply(seq_len(nrow(grid)), function(i) baseline(grid$format[i], grid$holdout[i])), fill = TRUE)
cols <- c(names(key), "med_rank", "q10", "q90", "p_exit", "method")
all <- rbind(bt[, intersect(c(cols, "val_med"), names(bt)), with = FALSE], base[, cols, with = FALSE], fill = TRUE)

# ---- scoring -----------------------------------------------------------------
pin <- function(y, q, tau) { u <- y - q; pmax(tau * u, (tau - 1) * u) }
all[, `:=`(y = log(actual_rank), l10 = log(pmax(q10, 1)), l50 = log(pmax(med_rank, 1)), l90 = log(pmax(q90, 1)))]
sv <- all[actual_exit == FALSE & !is.na(med_rank)]
score <- function(d) d[, .(
  n = .N,
  mae_rank = mean(abs(actual_rank - med_rank)),
  pinball = mean((pin(y, l10, .1) + pin(y, l50, .5) + pin(y, l90, .9)) / 3),
  cover80 = mean(actual_rank >= q10 & actual_rank <= q90),
  ic_move = cor(log(med_rank / pos_rank), log(actual_rank / pos_rank), method = "spearman")
), by = method]
surv_score <- score(sv)
ex <- all[!is.na(p_exit)]
auc <- function(p, y) { r <- rank(p); n1 <- sum(y); n0 <- length(y) - n1; (sum(r[y]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
exit_score <- ex[, .(brier = mean((p_exit - actual_exit)^2), auc = auc(p_exit, actual_exit)), by = method]
overall <- merge(surv_score, exit_score, by = "method")
# per holdout (stability: how often does each method beat the lookup table?)
per_h <- sv[, .(pinball = mean((pin(y, l10, .1) + pin(y, l50, .5) + pin(y, l90, .9)) / 3),
                ic_move = cor(log(med_rank / pos_rank), log(actual_rank / pos_rank), method = "spearman")),
            by = .(method, format, holdout)]
ref <- per_h[method == "Age x rank lookup", .(format, holdout, ref_pin = pinball, ref_ic = ic_move)]
per_h <- merge(per_h, ref, by = c("format", "holdout"))
wins <- per_h[, .(beats_lookup_pinball = sum(pinball < ref_pin), beats_lookup_ic = sum(ic_move > ref_ic), of = .N), by = method]
overall <- merge(overall, wins, by = "method")
cat("\n==== out-of-sample, 11 holdouts ====\n")
print(overall[order(pinball), lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

# ---- the thesis: players projected to rise / fall -----------------------------
# preseason model + best gap arm: decile of predicted value change vs realized
dec <- all[!is.na(val_med) & cur_val >= 150]
if (nrow(dec)) dec <- dec[
  , `:=`(pred = val_med / cur_val - 1, real = actual_val / cur_val - 1)][
  , dec := cut(frank(pred, ties.method = "random"), 10, labels = FALSE), by = method][
  , .(n = .N, pred = median(pred), real = median(real), share_up = mean(real > 0)), by = .(method, dec)]
ic_val <- all[!is.na(val_med) & cur_val >= 150,
              .(ic_value = cor(val_med / cur_val, actual_val / cur_val, method = "spearman"),
                ic_age_only = cor(-age, actual_val / cur_val, method = "spearman"), n = .N), by = method]
cat("\n==== value-change ranking skill (material players) ====\n"); print(ic_val)

# ---- the efficiency assumption: is out-of-sample Q uniform? -------------------
# score every redraft-ranked season (pos rank <= 60) against ONLY earlier
# seasons' outcomes at the same rank (+/-3), i.e. what you'd have known in August
dp_id <- as.data.table(ffscrapr::dp_playerids())[!is.na(gsis_id) & !is.na(fantasypros_id), .(fantasypros_id = as.character(fantasypros_id), gsis_id)]
tot <- merge(as.data.table(scoring)[week %in% 1:14 & !is.na(gsis_id), .(points = sum(points)), by = .(season, gsis_id)],
             dp_id, by = "gsis_id")[, .(total = sum(points)), by = .(season, fantasypros_id)]
rd <- as.data.table(fp_rankings_history())[page_pos == pos, .(season, fantasypros_id = as.character(fantasypros_id), pos, redraft_rank = rank)]
rd <- merge(rd[redraft_rank <= 60 & pos %in% c("QB", "RB", "WR", "TE")], tot, by = c("season", "fantasypros_id"), all.x = TRUE)
rd[is.na(total), total := 0]
qq <- rbindlist(lapply(2016:2025, function(Y) {
  tgt <- rd[season == Y]; pool <- rd[season < Y]
  tgt[, q := vapply(seq_len(.N), function(i) {
    p <- pool[pos == tgt$pos[i] & abs(redraft_rank - tgt$redraft_rank[i]) <= 3]$total
    if (length(p) < 10) NA_real_ else (sum(p < tgt$total[i]) + 0.5 * sum(p == tgt$total[i])) / length(p)
  }, numeric(1))]
}))[!is.na(q)]
qq[, tier := cut(redraft_rank, c(0, 6, 12, 24, 48, Inf), labels = c("1-6", "7-12", "13-24", "25-48", "49+"))]
q_tier <- qq[, .(n = .N, mean_q = mean(q), share_top_half = mean(q > .5)), by = .(pos, tier)][order(pos, tier)]
q_hist <- qq[, .(n = .N), by = .(bin = pmin(floor(q * 10), 9))][order(bin)][, share := n / sum(n)]
cat("\n==== Q uniformity (expect 0.10 per decile) ====\n"); print(q_hist)
cat("KS vs uniform:", round(suppressWarnings(ks.test(qq$q, "punif"))$statistic, 3), "\n")

saveRDS(list(overall = overall, per_h = per_h, dec = dec, ic_val = ic_val, q_tier = q_tier, q_hist = q_hist,
             q_raw = qq[, .(season, pos, redraft_rank, q)]), file.path(S, "eval.rds"))
cat("saved eval.rds\n")
