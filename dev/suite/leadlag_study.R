# Do redraft rankings lead the dynasty market? - gated study
#
#   Rscript dev/suite/leadlag_study.R
#   FFS_LL_HORIZONS=2,4,8 FFS_LL_TIER=0.33 Rscript dev/suite/leadlag_study.R
#
# The thesis: current-week / rest-of-season rankings react faster to what a
# player is actually doing than dynasty rankings do, so a player whose redraft
# stock has moved is a player whose dynasty price is about to move.
#
# dev/suite/ecr_archive.R makes this testable: 323 weekly snapshots of BOTH the
# dynasty boards (1QB `dynasty-overall`, superflex `dynasty-op`) and the redraft
# board, 2020-10-16 .. 2026-09-18, on one id. For every anchor date t we measure
#
#   fast     = redraft percentile move over the PRIOR h weeks
#   dyn_past = dynasty percentile move over the same prior window
#   dyn_fwd  = dynasty percentile move over the NEXT h weeks      <- the outcome
#
# and ask whether `fast` predicts `dyn_fwd` once the dynasty board's own recent
# move is partialled out.
#
# Percentiles are re-ranked on the players COMMON to the two snapshots being
# compared, and a window is discarded if it straddles the September switch from
# the preseason draft board to the rest-of-season board. Both matter: the boards
# change size and composition, and a stored rank/N differenced across that moves
# for players who did not (it scored Dalton Kincaid as a faller in the week his
# redraft ecr went 113.7 -> 105.7).
#
# WHAT THIS DOES AND DOES NOT CLAIM. It measures ECR leading ECR - one analyst
# consensus predicting another. It is a FORECAST claim about rankings. Whether
# the trade price you actually pay lags the same way is a different question:
# there is no historical archive of trade prices, so it can only be answered
# forward, off the panel dev/suite/market_snapshot.R is now accumulating.
#
# Outputs to dev/validate_outputs/:
#   leadlag_anchors_<tag>.csv   one row per anchor date x horizon x format
#   leadlag_buckets_<tag>.csv   forward move by fast-signal quintile
#   leadlag_ageband_<tag>.csv   the same, split by age band
#   leadlag_study_<tag>.txt     the verdict, in words

library(data.table)

ECR_ARCHIVE_SOURCE_ONLY <- TRUE
source(here::here("dev", "suite", "ecr_archive.R"))
MARKET_SNAPSHOT_SOURCE_ONLY <- TRUE
source(here::here("dev", "suite", "market_snapshot.R"))

config <- list(
  horizons = as.integer(strsplit(Sys.getenv("FFS_LL_HORIZONS", "2,4,8"), ",")[[1]]),
  # the tradeable universe: the top third of the dynasty board. Deep-bench ranks
  # are noise - a 400th-ranked player moving 50 spots is not a trade
  tier = as.numeric(Sys.getenv("FFS_LL_TIER", "0.33")),
  tol_days = as.integer(Sys.getenv("FFS_LL_TOL", "5")),
  min_n = as.integer(Sys.getenv("FFS_LL_MIN_N", "50")),
  boot = as.integer(Sys.getenv("FFS_LL_BOOT", "2000")),
  tag = Sys.getenv("FFS_LL_TAG", format(Sys.Date()))
)
formats <- list(`1qb` = "dynasty-overall", superflex = "dynasty-op")
out_dir <- here::here("dev", "validate_outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("reading ecr panel @ ", Sys.time())
panel <- ecr_panel_read(pages = c("dynasty-overall", "dynasty-op", "redraft-overall"))

## ---- ages, for the age-band split -------------------------------------------
ages <- tryCatch({
  ids <- data.table::as.data.table(ffscrapr::dp_playerids())
  ids <- ids[!is.na(fantasypros_id) & !is.na(birthdate),
             list(fantasypros_id = as.character(fantasypros_id),
                  birthdate = as.Date(birthdate))]
  unique(ids, by = "fantasypros_id")
}, error = function(e) { message("no birthdates: ", conditionMessage(e)); NULL })

## ---- helpers ----------------------------------------------------------------
# nearest snapshot to a target date, or NA when the archive has a gap there
.near <- function(dates, target, tol) {
  i <- which.min(abs(as.numeric(dates - target)))
  if (length(i) && abs(as.numeric(dates[i] - target)) <= tol) dates[i] else as.Date(NA)
}

# season an anchor belongs to: Jan/Feb sit with the season just finished
.season_of <- function(d) {
  as.integer(format(d, "%Y")) - (as.integer(format(d, "%m")) <= 2L)
}

.safe_cor <- function(x, y) {
  if (length(x) < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) return(NA_real_)
  suppressWarnings(stats::cor(x, y, method = "spearman"))
}

# overlapping windows: anchors h weeks wide sampled weekly share ~h-1 weeks of
# data, so the naive t-stat is inflated. Resample contiguous BLOCKS of anchors
# instead, which keeps that dependence intact.
.block_boot_t <- function(x, block, n_boot) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 5) return(c(mean = mean(x), se = NA_real_, t = NA_real_))
  block <- max(1L, min(as.integer(block), n))
  nb <- ceiling(n / block)
  starts <- seq_len(n - block + 1L)
  mus <- vapply(seq_len(n_boot), function(i) {
    idx <- unlist(lapply(sample(starts, nb, replace = TRUE), function(s) s:(s + block - 1L)))
    mean(x[idx[seq_len(n)]])
  }, numeric(1))
  se <- stats::sd(mus)
  c(mean = mean(x), se = se, t = if (is.finite(se) && se > 0) mean(x) / se else NA_real_)
}

## ---- per-anchor measurement --------------------------------------------------
measure <- function(dyn_page, h) {
  dyn <- panel[page_type == dyn_page,
               list(fantasypros_id, pos, dt = scrape_date, e = ecr, page = fp_page)]
  rdr <- panel[page_type == "redraft-overall",
               list(fantasypros_id, dt = scrape_date, e = ecr, page = fp_page)]
  dates <- sort(unique(dyn$dt))
  lag_days <- 7L * h
  # which FantasyPros page served each redraft snapshot, so a window can refuse
  # to straddle the September draft-board -> rest-of-season switch
  rdr_page <- unique(rdr[, list(dt, page)])

  anchors <- list(); buckets <- list(); ageband <- list()
  for (t0 in dates) {
    t0 <- as.Date(t0, origin = "1970-01-01")
    tm <- .near(dates, t0 - lag_days, config$tol_days)
    tp <- .near(dates, t0 + lag_days, config$tol_days)
    if (is.na(tm) || is.na(tp) || tm >= t0 || tp <= t0) next

    # the draft board and the rest-of-season board are different questions over
    # different universes; a move measured across the switch is an artefact
    pg <- rdr_page[dt %in% c(tm, t0), unique(page)]
    if (length(pg) != 1L) next

    # COMMON-UNIVERSE RANKING. rank/N is only comparable between two snapshots
    # that contain the same players: when the board sheds its deep end, every
    # survivor's percentile inflates in proportion to his rank, and improving
    # players read as decliners (Dalton Kincaid, 2026-09-18: redraft ecr 113.7
    # -> 105.7, a real rise, scored as a fall). So re-rank on the intersection.
    ids <- Reduce(intersect, list(dyn[dt == t0, fantasypros_id], dyn[dt == tm, fantasypros_id],
                                  rdr[dt == t0, fantasypros_id], rdr[dt == tm, fantasypros_id]))
    if (length(ids) < config$min_n) next
    pctize <- function(src, when, col) {
      x <- src[dt == when & fantasypros_id %chin% ids]
      x[, (col) := data.table::frank(e, ties.method = "average") / .N]
      x[, c("fantasypros_id", col), with = FALSE]
    }
    a <- merge(dyn[dt == t0 & fantasypros_id %chin% ids, list(fantasypros_id, pos)],
               pctize(dyn, t0, "d0"), by = "fantasypros_id")
    a <- merge(a, pctize(dyn, tm, "dm"), by = "fantasypros_id")
    a <- merge(a, pctize(rdr, t0, "r0"), by = "fantasypros_id")
    a <- merge(a, pctize(rdr, tm, "rm"), by = "fantasypros_id")

    # the forward snapshot is ranked on the same universe, so the outcome is on
    # the same scale; players who leave the board entirely are the sharpest
    # decliners, so they are an outcome (worst percentile), never a dropped row
    fwd <- dyn[dt == tp & fantasypros_id %chin% ids]
    fwd[, dp := data.table::frank(e, ties.method = "average") / .N]
    a <- merge(a, fwd[, list(fantasypros_id, dp)], by = "fantasypros_id", all.x = TRUE)
    a[, exited := is.na(dp)]
    a[is.na(dp), dp := 1.0]
    a <- a[d0 <= config$tier]
    if (nrow(a) < config$min_n) next

    a[, fast := r0 - rm][, dyn_past := d0 - dm][, dyn_fwd := dp - d0]
    # partial out the dynasty board's own recent move, in rank space
    rf <- stats::resid(stats::lm(rank(fast) ~ rank(dyn_past), a))
    ry <- stats::resid(stats::lm(rank(dyn_fwd) ~ rank(dyn_past), a))

    a[, q := cut(data.table::frank(fast) / .N, c(0, .2, .8, 1),
                 labels = c("risers", "mid", "fallers"))]
    bk <- a[, list(t0 = t0, h = h, dyn_page = dyn_page, n = .N,
                   inseason = as.integer(format(t0, "%m")) %in% 9:12,
                   fwd = mean(dyn_fwd), exit = mean(exited)), by = q]

    pos_ic <- a[pos %chin% c("QB", "RB", "WR", "TE"),
                list(ic = .safe_cor(fast, dyn_fwd)), by = pos]

    # age split: does the dynasty board follow the redraft board for an ageing
    # player, or is it right not to? A 33-year-old RB can top the redraft board
    # all season without the dynasty board owing him anything, in which case
    # "dynasty hasn't caught up" is the wrong reading of the same number.
    agerows <- NULL
    if (!is.null(ages)) {
      aa <- merge(a, ages, by = "fantasypros_id", all.x = TRUE)
      aa[, age := as.numeric(difftime(t0, birthdate, units = "days")) / 365.25]
      aa[, age_band := cut(age, c(-Inf, 23, 26, 29, Inf),
                           labels = c("<=23", "24-26", "27-29", "30+"))]
      agerows <- aa[!is.na(age_band) & .N >= 20, {
        # risers/fallers split WITHIN the band, so the comparison is like for like
        qq <- cut(data.table::frank(fast) / .N, c(0, .2, .8, 1),
                  labels = c("risers", "mid", "fallers"))
        # b0 is the band's BASELINE drift - where a player of this age goes when
        # the fast signal is flat - and b1 the response to it. The board needs
        # both: a 32-year-old's ranking decays whatever redraft says, so scoring
        # him off b1 alone predicts an improvement that never comes.
        fit <- tryCatch(stats::coef(stats::lm(dyn_fwd ~ fast)), error = function(e) c(NA, NA))
        list(t0 = t0, h = h, dyn_page = dyn_page,
             inseason = as.integer(format(t0, "%m")) %in% 9:12, n = .N,
             ic = .safe_cor(fast, dyn_fwd),
             b0 = fit[[1]], b1 = if (length(fit) > 1) fit[[2]] else NA_real_,
             fwd_all = mean(dyn_fwd, na.rm = TRUE),
             fwd_risers = mean(dyn_fwd[qq == "risers"], na.rm = TRUE),
             fwd_fallers = mean(dyn_fwd[qq == "fallers"], na.rm = TRUE))
      }, by = age_band]
    }
    anchors[[length(anchors) + 1L]] <- data.table(
      t0 = t0, h = h, dyn_page = dyn_page, n = nrow(a),
      season = .season_of(t0),
      inseason = as.integer(format(t0, "%m")) %in% 9:12,
      ic_raw = .safe_cor(a$fast, a$dyn_fwd),
      ic_partial = .safe_cor(rf, ry),
      ic_mom = .safe_cor(a$dyn_past, a$dyn_fwd),
      slope = tryCatch(stats::coef(stats::lm(dyn_fwd ~ fast, a))[["fast"]], error = function(e) NA_real_),
      ic_QB = pos_ic[pos == "QB", ic][1], ic_RB = pos_ic[pos == "RB", ic][1],
      ic_WR = pos_ic[pos == "WR", ic][1], ic_TE = pos_ic[pos == "TE", ic][1]
    )
    buckets[[length(buckets) + 1L]] <- bk
    if (!is.null(agerows) && nrow(agerows)) ageband[[length(ageband) + 1L]] <- agerows
  }
  list(anchors = data.table::rbindlist(anchors),
       ageband = data.table::rbindlist(ageband),
       buckets = data.table::rbindlist(buckets))
}

message("measuring @ ", Sys.time())
res <- list()
for (fmt in names(formats)) {
  for (h in config$horizons) {
    message("  ", fmt, " h=", h, "w @ ", Sys.time())
    m <- measure(formats[[fmt]], h)
    if (!nrow(m$anchors)) next
    m$anchors[, fmt := fmt]; m$buckets[, fmt := fmt]
    if (nrow(m$ageband)) m$ageband[, fmt := fmt]
    res[[paste(fmt, h)]] <- m
  }
}
anchors <- data.table::rbindlist(lapply(res, `[[`, "anchors"))
buckets <- data.table::rbindlist(lapply(res, `[[`, "buckets"))
ageband <- data.table::rbindlist(lapply(res, `[[`, "ageband"), fill = TRUE)
stopifnot(nrow(anchors) > 0)

## ---- value translation, for the P&L line only --------------------------------
# percentile moves are the inference; dollars are the sanity check. The ladder is
# the live FantasyCalc cross-section, same construction ffs_dynasty_outlook uses.
value_curve <- tryCatch({
  mk <- market_panel_read(src = "fc")
  mk <- mk[snapshot_date == max(snapshot_date) & format == "1qb" & !is.na(value)]
  mk[, p := data.table::frank(-value, ties.method = "average") / .N]
  stats::approxfun(mk$p, mk$value, rule = 2)
}, error = function(e) NULL)

## ---- verdict -----------------------------------------------------------------
sink_path <- file.path(out_dir, paste0("leadlag_study_", config$tag, ".txt"))
con <- file(sink_path, open = "wt")
say <- function(...) { cat(..., "\n", sep = ""); cat(..., "\n", sep = "", file = con) }

say("lead-lag study  ", as.character(Sys.Date()))
say("universe: dynasty pct <= ", config$tier, "   anchors: ", nrow(anchors),
    "   archive: ", as.character(min(anchors$t0)), " .. ", as.character(max(anchors$t0)))
say("")
say("CLAIM: this is redraft ECR predicting dynasty ECR. It is a forecast claim")
say("about rankings, not proof that the trade price under-reacts - no archive of")
say("trade prices exists, so that can only be settled forward.")

for (f_ in names(formats)) for (h_ in config$horizons) {
  A <- anchors[fmt == f_ & h == h_]
  if (!nrow(A)) next
  # blocks the width of the overlap: h-week windows sampled weekly
  b <- .block_boot_t(A$ic_partial, block = h, n_boot = config$boot)
  ins <- A[inseason == TRUE]
  bi <- .block_boot_t(ins$ic_partial, block = h, n_boot = config$boot)
  say("")
  say("== ", f_, "  h=", h_, " weeks  (", nrow(A), " anchors, median n=", median(A$n), ")")
  say("   IC raw            ", sprintf("%+.3f", mean(A$ic_raw, na.rm = TRUE)))
  say("   IC partial        ", sprintf("%+.3f  block-boot t=%.1f", b[["mean"]], b[["t"]]))
  say("   IC partial in-szn ", sprintf("%+.3f  block-boot t=%.1f  (%d anchors)",
                                       bi[["mean"]], bi[["t"]], nrow(ins)))
  say("   dynasty own momentum -> fwd ", sprintf("%+.3f", mean(A$ic_mom, na.rm = TRUE)))
  say("   by position  QB ", sprintf("%+.3f", mean(A$ic_QB, na.rm = TRUE)),
      "  RB ", sprintf("%+.3f", mean(A$ic_RB, na.rm = TRUE)),
      "  WR ", sprintf("%+.3f", mean(A$ic_WR, na.rm = TRUE)),
      "  TE ", sprintf("%+.3f", mean(A$ic_TE, na.rm = TRUE)))

  # leave-one-season-out: every season must pull its weight, not just the pooled mean
  per <- A[, list(ic = mean(ic_partial, na.rm = TRUE), k = .N), by = season][order(season)]
  say("   per season: ", paste(sprintf("%d %+.3f%s", per$season, per$ic,
                                       ifelse(per$k < 3, "*", "")), collapse = "  "),
      if (any(per$k < 3)) "   (* fewer than 3 anchors - not counted)" else "")
  pc <- per[k >= 3]
  say("   seasons positive: ", sum(pc$ic > 0, na.rm = TRUE), "/", nrow(pc))

  B <- buckets[fmt == f_ & h == h_]
  # t0 alone on the LHS: `n` is the per-bucket count, so casting on it would
  # split each anchor into three one-bucket rows and every spread would be NA
  w <- data.table::dcast(B, t0 ~ q, value.var = "fwd")
  w[, spread := fallers - risers]
  ws <- w[t0 %in% ins$t0]
  say("   fwd move  risers ", sprintf("%+.4f", mean(w$risers, na.rm = TRUE)),
      "  fallers ", sprintf("%+.4f", mean(w$fallers, na.rm = TRUE)),
      "  spread ", sprintf("%+.4f", mean(w$spread, na.rm = TRUE)))
  say("   spread > 0 in ", sum(w$spread > 0, na.rm = TRUE), "/", nrow(w), " anchors",
      " (in-season ", sum(ws$spread > 0, na.rm = TRUE), "/", nrow(ws), ")")
  E <- B[, list(exit = mean(exit, na.rm = TRUE)), by = q]
  say("   exit rate  ", paste(sprintf("%s %.3f", E$q, E$exit), collapse = "  "))

  if (!is.null(value_curve) && nrow(ins)) {
    # a top-third player sits near pct .17; price the average bucket move there
    p0 <- config$tier / 2
    d_r <- value_curve(p0 + mean(ws$risers, na.rm = TRUE)) - value_curve(p0)
    d_f <- value_curve(p0 + mean(ws$fallers, na.rm = TRUE)) - value_curve(p0)
    say("   in-season $ at a median top-third player: risers ", sprintf("%+.0f", d_r),
        "  fallers ", sprintf("%+.0f", d_f), "  gap ", sprintf("%.0f", d_r - d_f))
  }
}

## ---- gate --------------------------------------------------------------------
say("")
## ---- age split ---------------------------------------------------------------
# does the dynasty board owe an ageing player the same follow-through?
if (nrow(ageband)) {
  say("")
  say("== AGE SPLIT (in-season, all horizons pooled)")
  say("   fwd = mean forward dynasty percentile move, + = his ranking got WORSE")
  ab <- ageband[inseason == TRUE, list(
    anchors = .N, n = as.integer(median(n)),
    ic = mean(ic, na.rm = TRUE),
    drift = mean(fwd_all, na.rm = TRUE),
    risers = mean(fwd_risers, na.rm = TRUE),
    fallers = mean(fwd_fallers, na.rm = TRUE)
  ), by = list(fmt, age_band)][order(fmt, age_band)]
  ab[, spread := fallers - risers]
  for (i in seq_len(nrow(ab))) {
    g <- ab[i]
    say(sprintf("   %-9s %-6s n~%3d  IC %+.3f  drift %+.4f  risers %+.4f  fallers %+.4f  spread %+.4f",
                g$fmt, g$age_band, g$n, g$ic, g$drift, g$risers, g$fallers, g$spread))
  }
  say("")
  say("   Read the 30+ row carefully: a weak or negative spread there means the")
  say("   dynasty board does NOT follow redraft for older players - and is right")
  say("   not to. For them 'redraft rose, dynasty has not' is not a buy signal.")
}

say("")
say("== GATE (in-season, partial IC, per format x horizon)")
gate <- anchors[inseason == TRUE, {
  # a season represented by one or two overlapping windows is a single noisy
  # cross-section, not evidence: the current season always looks like this
  # early on (2026 had exactly one usable anchor, at -0.32) and would otherwise
  # count as a failing season
  per <- .SD[, list(ic = mean(ic_partial, na.rm = TRUE), k = .N), by = season][k >= 3]
  bb <- .block_boot_t(ic_partial, block = h[1], n_boot = config$boot)
  list(seasons = nrow(per), seasons_pos = sum(per$ic > 0, na.rm = TRUE),
       ic = bb[["mean"]], t = bb[["t"]])
}, by = list(fmt, h)]
sp <- buckets[inseason == TRUE, {
  w <- data.table::dcast(.SD, t0 ~ q, value.var = "fwd")
  list(spread_pos = mean(w$fallers - w$risers > 0, na.rm = TRUE))
}, by = list(fmt, h)]
gate <- merge(gate, sp, by = c("fmt", "h"))
gate[, pass := seasons_pos >= pmax(1L, seasons - 1L) & t > 2 & spread_pos >= 0.8]
for (i in seq_len(nrow(gate))) {
  g <- gate[i]
  say(sprintf("   %-9s h=%dw  seasons %d/%d positive  IC %+.3f (t=%.1f)  spread>0 %.0f%%  -> %s",
              g$fmt, g$h, g$seasons_pos, g$seasons, g$ic, g$t, 100 * g$spread_pos,
              if (isTRUE(g$pass)) "PASS" else "FAIL"))
}
say("")
say("gate = every season but at most one positive, block-boot t > 2, decile")
say("spread positive in >= 80% of anchors. The archive covers ",
    length(unique(anchors$season)), " seasons, so")
say("'>= 8 of 11' from the annual-data plan becomes 'all but one' here.")

# attenuation: fit on the other seasons, check the slope on the held-out one
att <- anchors[is.finite(slope), list(slope = median(slope, na.rm = TRUE)), by = list(fmt, h)]
say("")
say("median in-anchor slope of dyn_fwd on fast (shrink any shipped coefficient by it):")
for (i in seq_len(nrow(att))) say(sprintf("   %-9s h=%dw  %.3f", att$fmt[i], att$h[i], att$slope[i]))

close(con)
data.table::fwrite(anchors, file.path(out_dir, paste0("leadlag_anchors_", config$tag, ".csv")))
data.table::fwrite(buckets, file.path(out_dir, paste0("leadlag_buckets_", config$tag, ".csv")))
if (nrow(ageband)) data.table::fwrite(ageband, file.path(out_dir, paste0("leadlag_ageband_", config$tag, ".csv")))
message("wrote ", sink_path, " @ ", Sys.time())
