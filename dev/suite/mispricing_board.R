# Dynasty mispricing board - where the redraft board has moved and the dynasty
# board has not yet.
#
#   Rscript dev/suite/mispricing_board.R
#   FFS_MB_WINDOW=4 FFS_MB_MIN_VALUE=1000 Rscript dev/suite/mispricing_board.R
#
# dev/suite/leadlag_study.R establishes the edge this board trades: on 323 weekly
# snapshots since 2020, a player's redraft percentile move over the prior h weeks
# predicts his DYNASTY percentile move over the next h weeks, partial of the
# dynasty board's own momentum. In-season IC ~ +0.23..+0.27, positive in every
# season, decile spread positive in 85-96% of anchors, all six format x horizon
# cells through the gate.
#
# The signal is ASYMMETRIC. Almost all of the spread is fallers: redraft fallers
# lose ~1.5 dynasty percentile points over the next month while risers gain
# ~nothing. So this board leads with SELL/FADE, which is also where the repo's
# older dynasty work found its firmest signal.
#
#   fast  = improvement in redraft percentile over the window   (+ = stock up)
#   slow  = improvement in dynasty percentile over the window
#   score = z(fast) - z(slow)
#           >0  redraft moved up more than dynasty has     -> BUY candidate
#           <0  redraft fell more than dynasty has priced  -> SELL / FADE
#
# Both legs are measured, neither assumed zero: "the price hasn't moved" is an
# assertion that sits well inside weekly noise, so we compare like with like.
#
# No league simulation, no roster context - this is the market-wide view. Cross
# it with a trade_intel run when you want to know if a name helps YOUR team.
#
# Outputs to dev/league_sims/mispricing/<date>/:
#   board_<format>.csv   every priced player, scored
#   history.csv          appended each run - the pre-registered forward test:
#                        today's score vs what the dynasty board does next

library(data.table)

ECR_ARCHIVE_SOURCE_ONLY <- TRUE
source(here::here("dev", "suite", "ecr_archive.R"))
MARKET_SNAPSHOT_SOURCE_ONLY <- TRUE
source(here::here("dev", "suite", "market_snapshot.R"))
devtools::load_all(here::here(), quiet = TRUE)

config <- list(
  window = as.integer(Sys.getenv("FFS_MB_WINDOW", "4")),
  tol_days = as.integer(Sys.getenv("FFS_MB_TOL", "5")),
  # the noise floor. Sub-$1000 dynasty values swing tens of percent on nothing;
  # scoring them just fills the board with backup-QB churn
  min_value = as.numeric(Sys.getenv("FFS_MB_MIN_VALUE", "1000")),
  tier = as.numeric(Sys.getenv("FFS_MB_TIER", "0.5")),
  established = as.numeric(Sys.getenv("FFS_MB_ESTABLISHED", "2500")),
  # slope of forward dynasty move on the fast signal, median across anchors in
  # dev/validate_outputs/leadlag_study_*.txt. Already an attenuated (realised)
  # coefficient, so it is a shrunk forecast, not an extrapolation
  slope = as.numeric(Sys.getenv("FFS_MB_SLOPE", "0.06")),
  scrape = !identical(Sys.getenv("FFS_MB_SCRAPE"), "0")
)
formats <- list(`1qb` = list(page = "dynasty-overall", num_qbs = 1),
                superflex = list(page = "dynasty-op", num_qbs = 2))

out <- here::here("dev", "league_sims", "mispricing", format(Sys.Date()))
dir.create(out, recursive = TRUE, showWarnings = FALSE)

message("reading ecr panel @ ", Sys.time())
panel <- ecr_panel_read(pages = c("dynasty-overall", "dynasty-op", "redraft-overall"))
asof <- max(panel$scrape_date)
message("archive as of ", asof, " (refresh with FFS_ECR_REFRESH=1 dev/suite/ecr_archive.R)")

.near <- function(dates, target, tol) {
  i <- which.min(abs(as.numeric(dates - target)))
  if (length(i) && abs(as.numeric(dates[i] - target)) <= tol) dates[i] else as.Date(NA)
}

# per-age-band (b0, b1) from the newest lead-lag run, so the board and the study
# never drift apart. Falls back to a flat slope with no age drift.
drift_fit <- local({
  f <- sort(Sys.glob(here::here("dev", "validate_outputs", "leadlag_ageband_*.csv")),
            decreasing = TRUE)
  fb <- data.table(age_band = c("<=23", "24-26", "27-29", "30+"),
                   b0 = 0, b1 = config$slope)
  if (!length(f)) { message("no lead-lag age fit found - using a flat slope"); return(fb) }
  d <- data.table::fread(f[1])
  if (!all(c("age_band", "b0", "b1", "inseason") %in% names(d))) return(fb)
  d <- d[inseason == TRUE & is.finite(b0) & is.finite(b1)]
  if (!nrow(d)) return(fb)
  message("age drift from ", basename(f[1]))
  # keep the horizon: b0 is a drift accumulated over h weeks, so applying an
  # 8-week baseline to a 1-week window would forecast an eight-fold decline
  d[, list(b0 = stats::median(b0), b1 = stats::median(b1)), by = list(age_band, h)]
})

build_board <- function(fmt_name) {
  spec <- formats[[fmt_name]]
  dyn <- panel[page_type == spec$page]
  rdr <- panel[page_type == "redraft-overall"]
  dates <- sort(unique(dyn$scrape_date))
  t0 <- max(dates)

  # the redraft page flips from the preseason draft board to the rest-of-season
  # board every September, over a different (smaller) universe. A window that
  # straddles it compares two different questions, so shrink back to the longest
  # span that stays on one page and say so, rather than quietly crossing it.
  page_now <- rdr[scrape_date == t0, fp_page[1]]
  same_page <- rdr[fp_page == page_now, sort(unique(scrape_date))]
  want <- t0 - 7L * config$window
  cand <- dates[dates %in% same_page & dates < t0]
  if (!length(cand)) stop("no earlier snapshot on page ", page_now)
  tm <- .near(cand, want, config$tol_days)
  if (is.na(tm)) tm <- min(cand[cand >= want], na.rm = TRUE)
  if (is.na(tm) || is.infinite(as.numeric(tm))) tm <- min(cand)

  # COMMON-UNIVERSE RANKING: rank/N only compares between snapshots holding the
  # same players. The 2026-09-11 switch dropped 121 players, which inflated every
  # survivor's percentile in proportion to his rank and made risers look like
  # fallers (Kincaid: redraft ecr 113.7 -> 105.7, a real rise, scored as a fall).
  ids <- Reduce(intersect, list(
    dyn[scrape_date == t0, fantasypros_id], dyn[scrape_date == tm, fantasypros_id],
    rdr[scrape_date == t0, fantasypros_id], rdr[scrape_date == tm, fantasypros_id]))
  pctize <- function(src, when, col) {
    x <- src[scrape_date == when & fantasypros_id %chin% ids]
    x[, (col) := data.table::frank(ecr, ties.method = "average") / .N]
    x[, c("fantasypros_id", col), with = FALSE]
  }
  b <- merge(dyn[scrape_date == t0 & fantasypros_id %chin% ids,
                 list(fantasypros_id, player, pos, team, dyn_ecr = ecr)],
             pctize(dyn, t0, "dyn_pct"), by = "fantasypros_id")
  b <- merge(b, pctize(dyn, tm, "dyn_pct_prev"), by = "fantasypros_id")
  b <- merge(b, pctize(rdr, t0, "rdr_pct"), by = "fantasypros_id")
  b <- merge(b, pctize(rdr, tm, "rdr_pct_prev"), by = "fantasypros_id")

  # + = his stock improved over the window
  b[, fast := -(rdr_pct - rdr_pct_prev)]
  b[, slow := -(dyn_pct - dyn_pct_prev)]

  # market price, for the noise floor, the $ translation and the age column
  fc <- tryCatch({
    if (config$scrape) {
      data.table::as.data.table(fc_dynasty_values(num_qbs = spec$num_qbs, num_teams = 12, ppr = 1))
    } else {
      mk <- market_panel_read(src = "fc")
      mk <- mk[format == fmt_name & snapshot_date == max(snapshot_date)]
      data.table::setnames(mk, "player_name", "player_name", skip_absent = TRUE)[]
    }
  }, error = function(e) { message("no FantasyCalc values: ", conditionMessage(e)); NULL })
  if (!is.null(fc) && nrow(fc)) {
    fc <- fc[!is.na(fantasypros_id),
             list(fantasypros_id = as.character(fantasypros_id),
                  age, cur_value = value, trend_30day, redraft_value)]
    b <- merge(b, unique(fc, by = "fantasypros_id"), by = "fantasypros_id", all.x = TRUE)
  } else {
    b[, c("age", "cur_value", "trend_30day", "redraft_value") := NA_real_]
  }

  # the market's own value ladder, built on the FULL priced universe before any
  # filtering, and monotone by construction: rank everyone by value, then read
  # value off that ladder at a player's dynasty percentile. Fitting value on
  # dyn_pct directly is not monotone - the two disagree, which is the whole
  # premise of this board - and a locally upward stretch flips the sign of every
  # dollar estimate near it
  curve <- tryCatch({
    v <- b[!is.na(cur_value) & cur_value > 0]
    if (nrow(v) < 20) NULL else {
      vv <- sort(v$cur_value, decreasing = TRUE)
      stats::approxfun(seq_along(vv) / length(vv), vv, rule = 2)
    }
  }, error = function(e) NULL)

  # unpriced players are id-crosswalk misses, not opportunities: without a price
  # there is nothing to call cheap and no way to size the trade
  b <- b[!is.na(cur_value) & cur_value >= config$min_value & dyn_pct <= config$tier]
  if (!nrow(b)) return(b)

  z <- function(x) { s <- stats::sd(x, na.rm = TRUE); if (!is.finite(s) || s == 0) return(x * 0); (x - mean(x, na.rm = TRUE)) / s }
  b[, score := z(fast) - z(slow)]

  # what the study says happens next, and what that is worth in dollars.
  # AGE MATTERS TO THE LEVEL, not just the ranking. The study's per-band fit is
  # dyn_fwd ~ b0 + b1*fast: b1 (the response to the fast signal) is similar at
  # every age, but b0 (baseline decay) climbs +0.005 at <=23 to +0.016 at 30+,
  # and the riser half flips sign around 27 - under 27 a redraft riser gains
  # dynasty ground, over 27 he still loses it, just more slowly. Using b1 alone
  # predicts an ageing riser improves, which he does not.
  b[, age_band := cut(age, c(-Inf, 23, 26, 29, Inf),
                      labels = c("<=23", "24-26", "27-29", "30+"))]
  # match the fit to the window we could actually measure, then pro-rate the
  # baseline to its length: b1 is a response ratio and travels; b0 is a rate
  wk <- max(1 / 7, as.numeric(t0 - tm) / 7)
  hfit <- unique(drift_fit$h)[which.min(abs(unique(drift_fit$h) - wk))]
  b <- merge(b, drift_fit[h == hfit, list(age_band, b0, b1)], by = "age_band", all.x = TRUE)
  b[is.na(b1), b1 := config$slope][is.na(b0), b0 := 0]
  b[, b0 := b0 * wk / hfit]
  b[, exp_pct_move := b0 + b1 * (rdr_pct - rdr_pct_prev)]
  b[, exp_value_chg := if (is.null(curve)) NA_real_ else
    curve(pmin(1, pmax(0, dyn_pct + exp_pct_move))) - curve(dyn_pct)]

  # a BUY has to clear both bars: the redraft board moved up more than dynasty
  # did (score), AND the player is actually expected to gain dynasty ground once
  # his age band's baseline decay is charged. An ageing riser passes the first
  # and fails the second - that is the Derrick Henry case, and the dynasty board
  # is right not to follow him.
  b[, verdict := data.table::fcase(
    score <= -1, "SELL / FADE",
    score >= 1 & exp_pct_move < 0, "BUY",
    score >= 1, "rising, below drift",
    default = "hold")]
  # the market marking him down already is confirmation, not a reason to skip
  b[, price_moving := !is.na(trend_30day) & !is.na(cur_value) &
      abs(trend_30day) > 0.10 * cur_value]

  b[, format := fmt_name][, asof := t0][, window_from := tm]
  data.table::setorder(b, score)
  b[, list(format, asof, window_from, player, pos, team, age, age_band, cur_value,
           dyn_ecr, dyn_pct, dyn_pct_prev, rdr_pct, rdr_pct_prev,
           fast, slow, score, b0, b1, exp_pct_move, exp_value_chg, verdict,
           trend_30day, price_moving, redraft_value, fantasypros_id)]
}

boards <- list()
for (f in names(formats)) {
  message("building ", f, " @ ", Sys.time())
  bd <- build_board(f)
  if (!nrow(bd)) { message("  nothing to score"); next }
  boards[[f]] <- bd
  data.table::fwrite(bd, file.path(out, paste0("board_", f, ".csv")))
}
stopifnot(length(boards) > 0)
all_b <- data.table::rbindlist(boards)

# the pre-registered forward test: freeze today's scores, score them later
hist_path <- here::here("dev", "league_sims", "mispricing", "history.csv")
h <- all_b[, list(generated = as.character(Sys.Date()), format,
                  asof = as.character(asof), fantasypros_id,
                  player, pos, dyn_pct, fast, slow, score, verdict)]
if (file.exists(hist_path)) {
  # dates as character on both sides: fread turns them into IDate, and rbind on
  # a character/IDate pair is an error rather than a coercion
  old <- data.table::fread(hist_path, colClasses = list(
    character = c("fantasypros_id", "generated", "asof")))
  old <- old[generated != as.character(Sys.Date())]
  h <- rbind(old, h, fill = TRUE)
}
data.table::fwrite(h, hist_path)

## ---- narrative ---------------------------------------------------------------
cat("\ndynasty mispricing board  ", format(Sys.Date()), "\n")
wk_used <- round(as.numeric(max(all_b$asof) - min(all_b$window_from)) / 7, 1)
cat("window: ", wk_used, " weeks  (", as.character(min(all_b$window_from)), " -> ",
    as.character(max(all_b$asof)), ")   floor: $", config$min_value, "\n", sep = "")
if (wk_used < config$window) {
  cat("  asked for ", config$window, " weeks; the redraft board switched to its\n",
      "  rest-of-season page on ", as.character(min(all_b$window_from)),
      " and a window may not cross that.\n",
      "  The span widens by a week per snapshot from here.\n", sep = "")
}
cat("score = z(redraft move) - z(dynasty move).  negative = the redraft board has\n")
cat("marked him down and the dynasty board has not caught up yet.\n")

for (f in names(boards)) {
  bd <- boards[[f]]
  cat("\n== ", f, "  (", nrow(bd), " scored)\n", sep = "")
  show <- function(d, title) {
    if (!nrow(d)) return(invisible())
    cat("  ", title, "\n", sep = "")
    for (i in seq_len(nrow(d))) with(d[i], cat(sprintf(
      "    %-22s %-3s %5s  score %+5.2f  exp %+5.0f$  %s\n",
      player, pos, ifelse(is.na(cur_value), "-", format(round(cur_value))),
      score, ifelse(is.na(exp_value_chg), 0, exp_value_chg),
      ifelse(price_moving, "(price already moving)", ""))))
  }
  show(head(bd[verdict == "SELL / FADE"], 10), "SELL / FADE - redraft fell, dynasty has not")
  show(head(bd[verdict == "BUY"][order(-score)], 10), "BUY - redraft rose, dynasty has not")
  # the cheap end of the board moves most, so it crowds out the names that are
  # actually worth trading; show the established tier on its own
  est <- bd[cur_value >= config$established]
  show(head(est[order(score)], 6), paste0("established (>= $", config$established, ") - fading"))
  show(head(est[order(-score)], 6), paste0("established (>= $", config$established, ") - rising"))
}

watch <- c("Parker Washington", "Rome Odunze", "Luther Burden", "Marvin Harrison",
           "Brian Thomas", "Roman Wilson")
cat("\n== names you asked about\n")
for (nm in watch) {
  r <- all_b[format == "1qb" & grepl(nm, player, fixed = TRUE)]
  if (!nrow(r)) { cat(sprintf("    %-22s not scored (below the $%s floor or outside the top %.0f%%)\n",
                              nm, config$min_value, 100 * config$tier)); next }
  with(r[1], cat(sprintf("    %-22s %-3s %5s  fast %+.3f  slow %+.3f  score %+5.2f  %s\n",
                         player, pos, format(round(cur_value)), fast, slow, score, verdict)))
}
cat("\nwrote ", out, "\n", sep = "")
