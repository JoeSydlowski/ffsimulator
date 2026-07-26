#!/usr/bin/env Rscript
# =====================================================================
# pick_value_study.R  -  Phase 1 of the rookie-pick valuation work.
#
# Builds an EMPIRICAL, hit-rate-based rookie-pick value curve:
#   slot (1.01, 1.02, ...) -> distribution of realized forward dynasty
#   value 1/2/3 seasons later, aggregated across historical classes.
#
# Method (all from data already in the repo, no external ADP scrape):
#   * A rookie class = players in their FIRST season of fp_dynasty_history().
#   * Implied rookie-draft slot = order within the class by debut dynasty
#     rank (1.01 = the top-ranked rookie, etc.), mapped to 12-team rounds.
#   * Forward value = the player's realized dynasty VALUE h seasons later
#     on a season-invariant synthetic value scale (10000*exp(-0.023*rank),
#     the same curve ffs_dynasty_outlook falls back to). Falling out of the
#     rankings ("exit") contributes value 0, so a slot's mean IS its
#     hit-rate-weighted EV -- busts are priced in.
#
# Validation:
#   * Leave-one-class-out: does debut-rank slotting predict forward value
#     out of sample? (rank correlation + coverage of the p10-p90 band)
#   * Market cross-check: compare the empirical EV curve to the live
#     FantasyCalc pick curve (fc_dynasty_values rows with pos == "PICK").
#
# Outputs:
#   dev/data/pick_value_curve.csv           - the curve (both formats/horizons)
#   dev/validate_outputs/pick_value_study.txt - console writeup
#   dev/validate_outputs/pick_value_market.csv - empirical-vs-market join
# =====================================================================

suppressMessages({
  library(data.table)
  pkgload::load_all(".", quiet = TRUE)
})

set.seed(1)
HORIZONS   <- 1:3
PRIMARY_H  <- 1L
PICK_MAX   <- 60L        # model overall rookie picks 1..60 (covers 4 rounds up
                         # to a 14-team league); curve is keyed on the RAW
                         # overall pick number so it ports to any league size.
LABEL_TEAMS <- 12L       # ONLY for the human-readable round.slot label / the FC
                         # market cross-check (FantasyCalc prices a 12-team draft)
SCRAPE_FC  <- as.integer(Sys.getenv("FFS_PICK_FC", "1")) == 1

# season-invariant value scale: same synthetic curve ffs_dynasty_outlook uses
# when no market values are supplied. Using ONE fixed monotone map across all
# seasons keeps cross-class forward-value comparisons consistent.
val_curve <- function(rank) 10000 * exp(-0.023 * rank)

# position-startable thresholds (12-team, dynasty): "hit" = a returnable
# starter-quality asset, "stud" = a positional cornerstone. Defined on forward
# POSITIONAL rank so they mean the same thing across positions.
HIT_POSRANK  <- c(QB = 12, RB = 24, WR = 30, TE = 12)
STUD_POSRANK <- c(QB = 6,  RB = 8,  WR = 10, TE = 4)

# 12-team round.slot label for a raw overall pick number (display only)
slot_label <- function(pick, teams = LABEL_TEAMS) {
  rnd <- ((pick - 1L) %/% teams) + 1L
  inr <- ((pick - 1L) %%  teams) + 1L
  sprintf("%d.%02d", rnd, inr)
}

# smooth + strictly-monotone-non-increasing fit of value vs overall pick number.
# loess on log1p removes small-n jitter; isotonic regression then guarantees a
# later pick is never worth more than an earlier one (e.g. 2.06 <= 2.05). Assumes
# `pick` ascending (the caller sorts).
smooth_monotone <- function(pick, y, span = 0.4) {
  if (length(y) < 4) return(as.numeric(y))
  sm <- tryCatch(
    expm1(stats::predict(stats::loess(log1p(y) ~ pick, span = span, degree = 1))),
    error = function(e) as.numeric(y))
  sm[!is.finite(sm) | sm < 0] <- 0
  as.numeric(-stats::isoreg(pick, -sm)$yf)   # non-increasing in ascending pick
}

# ---------------------------------------------------------------------
# 1. Build the per-rookie forward-value table for one format
# ---------------------------------------------------------------------
rookie_forward <- function(fmt) {
  dyn <- as.data.table(fp_dynasty_history())[format == fmt,
           .(season, fantasypros_id, player_name, pos, rank, pos_rank, age)]
  dyn <- unique(dyn, by = c("season", "fantasypros_id"))

  # rookie = first season we ever see the player in the dynasty rankings.
  # (dp_playerids draft_year lags the newest classes; first-seen season is the
  #  robust fallback the transition pools already lean on.)
  first <- dyn[, .(first_season = min(season)), by = fantasypros_id]
  dyn   <- merge(dyn, first, by = "fantasypros_id")
  rook  <- dyn[season == first_season]

  # implied rookie-pick number = order within the class by debut dynasty rank.
  # RAW overall pick number (1 = top rookie) so the curve ports to any league
  # size; the round.slot label is display-only.
  rook[, pick := frank(rank, ties.method = "first"), by = season]
  rook[, slot := slot_label(pick)]
  rook <- rook[pick <= PICK_MAX]

  maxseason <- max(dyn$season)
  fwd <- dyn[, .(fantasypros_id, tgt = season, fwd_rank = rank, fwd_posrank = pos_rank)]

  out <- rbindlist(lapply(HORIZONS, function(h) {
    r <- rook[season + h <= maxseason]              # only classes with a realized +h
    if (!nrow(r)) return(NULL)
    m <- merge(r[, .(fantasypros_id, season, pick, slot, pos, player_name)],
               fwd, by = "fantasypros_id", all.x = TRUE, allow.cartesian = TRUE)
    m <- m[is.na(tgt) | tgt == season + h]
    # collapse to one row/rookie: the +h ranking if it exists, else exit
    g <- m[, .(fwd_rank    = if (all(is.na(fwd_rank)))    NA_real_ else fwd_rank[!is.na(fwd_rank)][1],
               fwd_posrank = if (all(is.na(fwd_posrank))) NA_real_ else fwd_posrank[!is.na(fwd_posrank)][1]),
           by = .(fantasypros_id, player_name, season, pick, slot, pos)]
    g[, `:=`(
      horizon = h,
      exited  = is.na(fwd_rank),
      fwd_val = fifelse(is.na(fwd_rank), 0, val_curve(fwd_rank)),
      hit     = !is.na(fwd_posrank) & fwd_posrank <= HIT_POSRANK[pos],
      stud    = !is.na(fwd_posrank) & fwd_posrank <= STUD_POSRANK[pos]
    )]
    g[]
  }), fill = TRUE)
  out[, format := fmt]
  out[]
}

# ---------------------------------------------------------------------
# 2. Aggregate to the per-pick curve (raw overall pick number) + smooth
# ---------------------------------------------------------------------
curve_from <- function(rf, by_cols = c("format", "horizon", "pick"), smooth = TRUE) {
  agg <- rf[, {
    .(n         = .N,
      mean_raw  = mean(fwd_val),
      med_raw   = as.numeric(median(fwd_val)),
      p10_raw   = as.numeric(quantile(fwd_val, .10)),
      p90_raw   = as.numeric(quantile(fwd_val, .90)),
      p_hit     = mean(hit),
      p_stud    = mean(stud),
      p_exit    = mean(exited))
  }, by = by_cols]
  data.table::setorderv(agg, intersect(c("format", "horizon", "pick"), names(agg)))

  # smooth + enforce monotone-decreasing value within each format x horizon
  if (smooth && "pick" %in% names(agg)) {
    grp <- intersect(c("format", "horizon"), names(agg))
    agg[, `:=`(
      mean_val = smooth_monotone(pick, mean_raw),
      med_val  = smooth_monotone(pick, med_raw),
      p10_val  = smooth_monotone(pick, p10_raw),
      p90_val  = smooth_monotone(pick, p90_raw)
    ), by = grp]
  } else {
    agg[, `:=`(mean_val = mean_raw, med_val = med_raw,
               p10_val = p10_raw, p90_val = p90_raw)]
  }
  agg[]
}

rf1 <- rookie_forward("1qb")
rfs <- rookie_forward("superflex")
rf  <- rbind(rf1, rfs)
curve <- curve_from(rf)
curve[, slot := slot_label(pick)]   # 12-team display label

dir.create("dev/data", showWarnings = FALSE, recursive = TRUE)
fwrite(curve[, .(format, horizon, pick, slot, n,
                 mean_val = round(mean_val), med_val = round(med_val),
                 p10_val = round(p10_val), p90_val = round(p90_val),
                 mean_raw = round(mean_raw), med_raw = round(med_raw),
                 p_hit = round(p_hit, 3), p_stud = round(p_stud, 3),
                 p_exit = round(p_exit, 3))],
       "dev/data/pick_value_curve.csv")

# ---------------------------------------------------------------------
# 3. Leave-one-class-out validation (primary horizon)
# ---------------------------------------------------------------------
loo_validate <- function(rf, fmt, h = PRIMARY_H) {
  d <- rf[format == fmt & horizon == h]
  seasons <- sort(unique(d$season))
  res <- rbindlist(lapply(seasons, function(s) {
    train <- d[season != s]
    test  <- d[season == s]
    if (!nrow(test) || !nrow(train)) return(NULL)
    tc <- curve_from(train, by_cols = c("pick"))[, .(pick, pred = mean_val,
                                                     p10 = p10_val, p90 = p90_val)]
    m  <- merge(test, tc, by = "pick")
    m[, .(season = s, fantasypros_id, pick, actual = fwd_val, pred, p10, p90)]
  }))
  list(
    n        = nrow(res),
    spearman = suppressWarnings(cor(res$pred, res$actual, method = "spearman")),
    pearson  = suppressWarnings(cor(res$pred, res$actual)),
    cover80  = mean(res$actual >= res$p10 & res$actual <= res$p90),
    res      = res
  )
}
loo1 <- loo_validate(rf, "1qb")
loos <- loo_validate(rf, "superflex")

# ---------------------------------------------------------------------
# 4. Market cross-check vs live FantasyCalc pick curve
# ---------------------------------------------------------------------
parse_fc_picks <- function(num_qbs) {
  fc <- tryCatch(fc_dynasty_values(num_qbs = num_qbs), error = function(e) NULL)
  if (is.null(fc)) return(NULL)
  fc <- as.data.table(fc)[pos == "PICK"]
  # only explicit "<round>.<slot>" labels (skip generic "2026 1st" mids)
  rx   <- regexpr("[1-9]\\.[0-9]{2}", fc$player_name)
  keep <- rx != -1L
  fc   <- fc[keep]
  fc[, slot := regmatches(player_name, regexpr("[1-9]\\.[0-9]{2}", player_name))]
  # FantasyCalc prices a 12-team draft -> raw overall pick number
  rnd <- as.integer(sub("\\..*$", "", fc$slot))
  inr <- as.integer(sub("^.*\\.", "", fc$slot))
  fc[, pick := (rnd - 1L) * LABEL_TEAMS + inr]
  fmt <- if (num_qbs > 1) "superflex" else "1qb"
  fc[, .(format = fmt, pick, slot, fc_value = value)]
}

market <- NULL
if (SCRAPE_FC) {
  market <- rbind(parse_fc_picks(1), parse_fc_picks(2))
}

market_join <- NULL
if (!is.null(market) && nrow(market)) {
  emp <- curve[horizon == PRIMARY_H, .(format, pick, emp_ev = mean_val)]
  market_join <- merge(emp, market, by = c("format", "pick"))
  # normalise each curve to its own pick 1 so shapes compare on one scale
  market_join[, `:=`(
    emp_idx = emp_ev / emp_ev[pick == 1L],
    mkt_idx = fc_value / fc_value[pick == 1L]
  ), by = format]
  market_join[, resid_idx := emp_idx - mkt_idx]  # >0 = hit-rate EV richer than market
  data.table::setorder(market_join, format, pick)
  fwrite(market_join, "dev/validate_outputs/pick_value_market.csv")
}

# ---------------------------------------------------------------------
# 5. Writeup
# ---------------------------------------------------------------------
dir.create("dev/validate_outputs", showWarnings = FALSE, recursive = TRUE)
sink("dev/validate_outputs/pick_value_study.txt", split = TRUE)

cat("=== ROOKIE PICK VALUE STUDY (Phase 1) ===\n")
cat("built", format(Sys.Date()), "| value scale = synthetic 10000*exp(-0.023*rank)\n")
cat("rookie = first fp_dynasty_history season; slot = debut-rank order within class\n\n")

for (fmt in c("1qb", "superflex")) {
  cat(sprintf("---- %s : forward value by slot (horizon +%d) ----\n", fmt, PRIMARY_H))
  print(curve[format == fmt & horizon == PRIMARY_H,
              .(slot, n, mean_val = round(mean_val), med_val = round(med_val),
                p10 = round(p10_val), p90 = round(p90_val),
                p_hit = round(p_hit, 2), p_stud = round(p_stud, 2))][1:min(.N, 48)])
  cat("\n")
}

cat("---- leave-one-class-out (horizon +1) ----\n")
cat(sprintf("1qb:       n=%d  spearman=%.2f  pearson=%.2f  cover80=%.2f\n",
            loo1$n, loo1$spearman, loo1$pearson, loo1$cover80))
cat(sprintf("superflex: n=%d  spearman=%.2f  pearson=%.2f  cover80=%.2f\n\n",
            loos$n, loos$spearman, loos$pearson, loos$cover80))

if (!is.null(market_join) && nrow(market_join)) {
  cat("---- empirical hit-rate EV vs FantasyCalc market (normalised to 1.01) ----\n")
  cat("resid_idx > 0  => hit-rate EV is RICHER than the market prices the slot\n")
  for (fmt in c("1qb", "superflex")) {
    mj <- market_join[format == fmt][order(pick)]
    if (!nrow(mj)) next
    cat(sprintf("\n[%s]  shape corr(emp_idx, mkt_idx) = %.3f\n", fmt,
                suppressWarnings(cor(mj$emp_idx, mj$mkt_idx))))
    print(mj[, .(pick, slot, emp_ev = round(emp_ev), fc_value = round(fc_value),
                 emp_idx = round(emp_idx, 2), mkt_idx = round(mkt_idx, 2),
                 resid = round(resid_idx, 2))][1:min(.N, 24)])
  }
} else {
  cat("(FantasyCalc market cross-check skipped or unavailable)\n")
}

sink()
cat("\nwrote dev/data/pick_value_curve.csv and dev/validate_outputs/pick_value_study.txt\n")
