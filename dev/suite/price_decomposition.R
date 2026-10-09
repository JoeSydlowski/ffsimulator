# Does the dynasty market pay for EXPECTED points or for REALISED points?
#
#   Rscript dev/suite/price_decomposition.R
#   FFS_PD_H=8 Rscript dev/suite/price_decomposition.R
#
# The paradox this resolves. FPOE persists at ~4% (dev/suite/fpoe_persistence.R),
# so a hot start is almost all noise - yet the dynasty market pays up for it.
# That should be an edge. But dev/suite/ep_gap_study.R found the opposite of an
# edge: underperformers keep falling, over-performers keep rising.
#
# The missing piece is that FPOE EARNS ROLE. Over-performing your expectation
# gets you more opportunity (gap -> change in EP is about -0.15, i.e. FPOE ->
# change in EP is about +0.15), so a price rise after a hot start is not
# obviously irrational - it may be pricing the role the player just won.
#
# So the question is not "does the market react to noise" but "does it react to
# noise BY MORE THAN the role that noise earns". Two regressions answer it:
#
#   1. CONTEMPORANEOUS  dyn_pct(start -> w) ~ z(EP/g) + z(FPOE/g)
#      What did the market actually pay for - opportunity, or points?
#
#   2. FORWARD          dyn_pct(w -> w+H) ~ z(EP/g) + z(FPOE/g) + price move so far
#      Does the FPOE-driven part of the price UNWIND? A negative FPOE coefficient
#      here is the edge: the market overpaid for variance and takes it back. A
#      zero or positive one means the price move was earned, and there is no
#      trade - which is what the earlier gate implied.
#
# Output: dev/validate_outputs/price_decomposition_<tag>.txt

library(data.table)
ECR_ARCHIVE_SOURCE_ONLY <- TRUE
source(here::here("dev", "suite", "ecr_archive.R"))
suppressMessages(devtools::load_all(here::here(), quiet = TRUE))

config <- list(
  seasons = 2020:2025,
  H = as.integer(Sys.getenv("FFS_PD_H", "8")),     # forward weeks
  min_games = as.integer(Sys.getenv("FFS_PD_MING", "3")),
  min_ep = as.numeric(Sys.getenv("FFS_PD_MINEP", "5")),
  tier = as.numeric(Sys.getenv("FFS_PD_TIER", "0.5")),
  boot = as.integer(Sys.getenv("FFS_PD_BOOT", "600")),
  tag = Sys.getenv("FFS_PD_TAG", format(Sys.Date()))
)
out_dir <- here::here("dev", "validate_outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

panel <- ecr_panel_read(pages = "dynasty-overall")
sched <- data.table::as.data.table(nflreadr::load_schedules(config$seasons))
wk_end <- sched[!is.na(gameday), list(ended = max(as.Date(gameday))), by = list(season, week)]

opp <- data.table::rbindlist(lapply(config$seasons, function(y) {
  x <- tryCatch(data.table::as.data.table(
    nflreadr::load_ff_opportunity(y, stat_type = "weekly")), error = function(e) NULL)
  if (is.null(x) || !nrow(x)) return(NULL)
  x[position %chin% c("QB", "RB", "WR", "TE"),
    list(season = y, week, gsis_id = player_id, pos = position,
         ep = total_fantasy_points_exp, fp = total_fantasy_points)]
}), fill = TRUE)
opp <- opp[!is.na(ep)]
opp <- merge(opp, wk_end, by = c("season", "week"))
ids <- data.table::as.data.table(nflreadr::load_ff_playerids())
ids <- unique(ids[!is.na(fantasypros_id) & !is.na(gsis_id),
                  list(gsis_id = as.character(gsis_id),
                       fantasypros_id = as.character(fantasypros_id))], by = "gsis_id")
opp[, gsis_id := as.character(gsis_id)]
opp <- merge(opp, ids, by = "gsis_id")

dyn <- panel[, list(fantasypros_id, dt = scrape_date, e = ecr)]
dates <- sort(unique(dyn$dt))
.near <- function(dd, target, tol = 6) {
  i <- which.min(abs(as.numeric(dd - target)))
  if (length(i) && abs(as.numeric(dd[i] - target)) <= tol) dd[i] else as.Date(NA)
}
# common-universe percentile between two snapshots, per the archive's rules
.pair <- function(t1, t2) {
  ids_c <- intersect(dyn[dt == t1, fantasypros_id], dyn[dt == t2, fantasypros_id])
  a <- dyn[dt == t1 & fantasypros_id %chin% ids_c]
  a[, p1 := data.table::frank(e, ties.method = "average") / .N]
  b <- dyn[dt == t2 & fantasypros_id %chin% ids_c]
  b[, p2 := data.table::frank(e, ties.method = "average") / .N]
  merge(a[, list(fantasypros_id, p1)], b[, list(fantasypros_id, p2)], by = "fantasypros_id")
}

rows <- list()
for (s in config$seasons) {
  sd <- dates[as.integer(format(dates, "%Y")) == s &
                as.integer(format(dates, "%m")) %in% 9:12]
  if (length(sd) < 3) next
  t_start <- min(sd)                       # the season's first dynasty snapshot
  for (t0 in sd) {
    t0 <- as.Date(t0, origin = "1970-01-01")
    if (t0 <= t_start) next
    tf <- .near(dates, t0 + 7L * config$H)
    if (is.na(tf) || tf <= t0) next

    # season to date, through the last game that finished before the anchor
    std <- opp[season == s & ended < t0,
               list(g = .N, ep_g = mean(ep), fp_g = mean(fp), fpoe_g = mean(fp - ep)),
               by = list(fantasypros_id, pos)]
    std <- std[g >= config$min_games & ep_g >= config$min_ep]
    if (nrow(std) < 40) next

    now <- .pair(t_start, t0)              # market move so far, this season
    fwd <- .pair(t0, tf)                   # market move from here
    m <- merge(std, now[, list(fantasypros_id, p_start = p1, p_now = p2)], by = "fantasypros_id")
    m <- merge(m, fwd[, list(fantasypros_id, p_fwd = p2)], by = "fantasypros_id")
    m <- m[p_now <= config$tier]
    if (nrow(m) < 40) next
    m[, `:=`(mkt_so_far = p_now - p_start,   # + = ranking got WORSE
             mkt_fwd = p_fwd - p_now,
             season = s, t0 = t0)]
    rows[[length(rows) + 1L]] <- m
  }
}
p <- data.table::rbindlist(rows, fill = TRUE)
p[, cl := paste0(fantasypros_id, "_", season)]
# standardise within anchor so coefficients are comparable across dates
p[, `:=`(z_ep = scale(ep_g)[, 1], z_fpoe = scale(fpoe_g)[, 1],
         z_fp = scale(fp_g)[, 1]), by = t0]
p <- p[is.finite(z_ep) & is.finite(z_fpoe)]
cat("rows:", nrow(p), "| anchors:", uniqueN(p$t0), "| seasons:", uniqueN(p$season), "\n")

.boot <- function(d, form, term, n_boot) {
  idx <- split(seq_len(nrow(d)), d$cl); cls <- names(idx)
  vapply(seq_len(n_boot), function(i) {
    dd <- d[unlist(idx[sample(cls, length(cls), replace = TRUE)], use.names = FALSE)]
    tryCatch(stats::coef(stats::lm(form, dd))[[term]], error = function(e) NA_real_)
  }, numeric(1))
}

say_lines <- character(0)
say <- function(...) { l <- paste0(...); cat(l, "\n"); say_lines <<- c(say_lines, l) }
say("price decomposition  ", as.character(Sys.Date()))
say("dynasty percentile moves: + = his ranking got WORSE")
say("rows ", nrow(p), " | anchors ", uniqueN(p$t0), " | seasons ",
    min(p$season), "-", max(p$season), " | forward horizon ", config$H, " weeks")

## 1. what did the market pay for? ---------------------------------------------
m1 <- stats::lm(mkt_so_far ~ z_ep + z_fpoe, p)
b1 <- .boot(p, mkt_so_far ~ z_ep + z_fpoe, "z_fpoe", config$boot)
say("")
say("== 1. CONTEMPORANEOUS: what moved the price this season so far?")
say(sprintf("   z(EP/g)    %+.5f     (more opportunity -> ranking improves)", stats::coef(m1)[["z_ep"]]))
say(sprintf("   z(FPOE/g)  %+.5f     boot 95%% [%+.5f, %+.5f]",
            stats::coef(m1)[["z_fpoe"]],
            stats::quantile(b1, .025, na.rm = TRUE), stats::quantile(b1, .975, na.rm = TRUE)))
say(sprintf("   ratio |FPOE| / |EP| = %.2f  <- 0 means the market ignores luck, 1 means it treats",
            abs(stats::coef(m1)[["z_fpoe"]]) / abs(stats::coef(m1)[["z_ep"]])))
say("                                    a lucky point exactly like an earned one")

## 2. does the FPOE-driven part unwind? -----------------------------------------
m2 <- stats::lm(mkt_fwd ~ z_ep + z_fpoe + mkt_so_far, p)
b2 <- .boot(p, mkt_fwd ~ z_ep + z_fpoe + mkt_so_far, "z_fpoe", config$boot)
say("")
say("== 2. FORWARD ", config$H, "w: does the luck-driven price move come back?")
say(sprintf("   z(FPOE/g)  %+.5f     boot 95%% [%+.5f, %+.5f]",
            stats::coef(m2)[["z_fpoe"]],
            stats::quantile(b2, .025, na.rm = TRUE), stats::quantile(b2, .975, na.rm = TRUE)))
say(sprintf("   z(EP/g)    %+.5f", stats::coef(m2)[["z_ep"]]))
say("   POSITIVE here = the hot player's ranking gets WORSE later = the market")
say("   overpaid and takes it back = a tradeable fade. Negative or zero = the")
say("   price move was earned and there is nothing to trade.")

## 3. does FPOE earn role? -------------------------------------------------------
roles <- list()
for (s in config$seasons) {
  # `anch`, not `t0`: p has a column called t0, and comparing a column to a
  # same-named variable is the data.table NSE trap this repo has been bitten by
  # before - it either errors or, worse, silently no-ops the filter
  for (anch in unique(p[season == s]$t0)) {
    anch <- as.Date(anch, origin = "1970-01-01")
    aft <- opp[season == s & ended >= anch, list(g2 = .N, ep2 = mean(ep)),
               by = fantasypros_id][g2 >= 3]
    cur <- p[season == s & t0 == anch]
    if (!nrow(cur) || !nrow(aft)) next
    roles[[length(roles) + 1L]] <- merge(cur[, list(fantasypros_id, z_fpoe, z_ep, ep_g, cl)],
                                         aft, by = "fantasypros_id")
  }
}
r <- data.table::rbindlist(roles, fill = TRUE)
if (nrow(r) > 200) {
  r[, d_ep := ep2 - ep_g]
  m3 <- stats::lm(d_ep ~ z_fpoe + z_ep, r)
  say("")
  say("== 3. does over-performing EARN opportunity?  (n = ", nrow(r), ")")
  say(sprintf("   z(FPOE/g) -> change in EP/g  %+.4f", stats::coef(m3)[["z_fpoe"]]))
  say("   positive = yes, the coach rewards production with more work - which is")
  say("   what would make a price rise after a hot start legitimate")
}

## 4. is a given player priced at his EP level or his FP level? ------------------
# The direct version of "has his price risen to his expected points or his actual
# points?" - fit the cross-sectional price curve against each separately, then
# read where the player actually sits between the two. Avoids the level-vs-
# surprise apples-and-oranges problem in section 1 entirely.
# INVERT the price curve instead of interpolating between two nearly identical
# ones. EP and FP are ~0.9 correlated across players, so "% of the way from
# EP-implied price to FP-implied price" has a near-zero denominator and explodes
# (it produced 248%, -399%, -907%). Asking what EP the market's pricing IMPLIES
# is well conditioned, and it is the same question: if implied EP sits at his
# actual FP, the market is pricing his hot scoring as his true level.
watch <- strsplit(Sys.getenv("FFS_PD_WATCH",
  "Kincaid,Nabers,CeeDee Lamb,Mahomes,Blake Corum,Stowers"), ",")[[1]]
cur_season <- as.integer(Sys.getenv("FFS_PD_CUR", "2026"))
cs <- tryCatch(data.table::as.data.table(
  nflreadr::load_ff_opportunity(cur_season, stat_type = "weekly")), error = function(e) NULL)
if (!is.null(cs) && nrow(cs)) {
  cs <- cs[position %chin% c("QB", "RB", "WR", "TE"),
           list(gsis_id = as.character(player_id), pos = position,
                ep = total_fantasy_points_exp, fp = total_fantasy_points)]
  cs <- cs[, list(g = .N, ep_g = mean(ep), fp_g = mean(fp), fpoe_g = mean(fp - ep)),
           by = list(gsis_id, pos)]
  cs <- merge(cs, ids, by = "gsis_id")
  last_dt <- max(dyn$dt)
  now <- dyn[dt == last_dt]
  now[, pct := data.table::frank(e, ties.method = "average") / .N]
  nm <- unique(panel[, list(fantasypros_id, player, pos_p = pos)])
  cc <- merge(cs, now[, list(fantasypros_id, pct)], by = "fantasypros_id")
  cc <- merge(cc, nm, by = "fantasypros_id", all.x = TRUE)
  cc <- cc[g >= 2 & pct <= 0.6]
  if (nrow(cc) > 60) {
    # market-implied EP: invert pct ~ ep_g over the priced universe
    fit <- stats::lm(pct ~ ep_g, cc)
    b0 <- stats::coef(fit)[[1]]; b1 <- stats::coef(fit)[[2]]
    cc[, implied_ep := (pct - b0) / b1]
    say("")
    say("== 4. WHAT EP DOES HIS PRICE IMPLY?  (", as.character(last_dt),
        ", season ", cur_season, ", n = ", nrow(cc), ")")
    say("   if implied EP is near his FP/g, the market is pricing the hot scoring")
    say("   as his true level; if near his EP/g, it is pricing the opportunity")
    for (w in watch) {
      x <- cc[grepl(trimws(w), player, fixed = TRUE)]
      if (!nrow(x)) { say(sprintf("   %-20s not in the priced universe", trimws(w))); next }
      x <- x[1]
      say(sprintf("   %-20s EP/g %5.1f | FP/g %5.1f | price implies EP %5.1f",
                  x$player, x$ep_g, x$fp_g, x$implied_ep))
    }
  }
}

writeLines(say_lines, file.path(out_dir, paste0("price_decomposition_", config$tag, ".txt")))
message("wrote @ ", Sys.time())
