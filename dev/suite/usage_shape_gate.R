# Does usage SHAPE condition how much a cold stretch costs a player's ROLE?
#
#   Rscript dev/suite/usage_shape_gate.R
#   FFS_US_POS=RB FFS_US_SEASONS=2009:2025 Rscript dev/suite/usage_shape_gate.R
#
# THE PRE-REGISTERED HYPOTHESIS
#   The market's markdown for a cold stretch is archetype-invariant, while the
#   role consequence is archetype-varying. This script tests only the SECOND
#   half - whether role consequence varies with shape - because if it does not,
#   there is nothing for the market to be wrong about and the market layer
#   should never be built. It touches NO dynasty data by design.
#
# WHY THIS EXISTS. dev/suite/ep_gap_study.R found that scoring below expected
# points predicts the dynasty ranking getting WORSE (incremental IC -0.14/-0.175,
# 0/6 seasons positive). The mechanism run alongside it showed production
# regresses UP (+0.23) while opportunity erodes (-0.15) - and that the erosion is
# FLAT across EP-rank tiers (star -0.162, fringe -0.147). So volume tier is the
# wrong conditioner. This asks whether usage SHAPE is the right one.
#
# ESTIMAND, per player i, season s, anchor week w:
#   R = EPpg(w+1..w+4) - EPpg(w-4..w-1)     EP per TEAM game, inactive = 0
#   G = gap (expected minus actual points per team game) over w-4..w-1
#   S = shape, measured in season s-1, percentile-ranked within position-season,
#       residualised on volume
#   R ~ a_{team x season} + b*G + c*EP_lvl + d*S + theta*(G x S) + lambda*(G x EP_lvl)
#
#   theta is the target. LAG STRUCTURE IS THE POINT: usage at t is caused by
#   efficiency at t-1, so a shape label measured during or after the gap window
#   is mechanically entangled with it. S comes from the PRIOR season, always.
#
# WHY NOT CLUSTERING. Unsupervised mixtures optimise density separation; the
# question is slope heterogeneity. s(S, by = G) IS the slope-as-a-function-of-
# shape curve, with no k to defend and no stability gate to pass.
#
# Outputs to dev/validate_outputs/:
#   usage_shape_rows_<tag>.csv    the panel
#   usage_shape_gate_<tag>.txt    the verdict

library(data.table)
suppressMessages(devtools::load_all(here::here(), quiet = TRUE))
stopifnot(requireNamespace("mgcv", quietly = TRUE))

config <- list(
  pos = strsplit(Sys.getenv("FFS_US_POS", "RB,WR"), ",")[[1]],
  seasons = 2009:2025,
  L = as.integer(Sys.getenv("FFS_US_L", "4")),      # gap window, weeks
  H = as.integer(Sys.getenv("FFS_US_H", "4")),      # outcome window, weeks
  min_team_games = as.integer(Sys.getenv("FFS_US_MIN_G", "4")),
  min_ep = as.numeric(Sys.getenv("FFS_US_MIN_EP", "3")),
  # prior_season = shape from s-1 (cleanest lag, but a noisy proxy for the
  # current role); early_season = weeks 1-4 of s with anchors at w>=9, still
  # strictly before the gap window; contemporaneous = shape from the gap window
  # itself, which is the CONTAMINATED version and exists only as a diagnostic
  shape_src = Sys.getenv("FFS_US_SHAPE", "prior_season"),
  # WHICH ROLE AXIS. "tilt" = receiving-weighted vs ground-weighted, the right
  # axis for RBs. For WRs it is degenerate: they barely rush, so percentile-
  # ranking a near-constant rush share spreads ties arbitrarily and injects noise
  # (measured: cor(S, target share) = 0.80 for RB but 0.14 for WR). "adot" =
  # depth of target, the genuinely orthogonal receiver role axis (deep threat vs
  # possession), near-independent of tilt at rho 0.13.
  axis = Sys.getenv("FFS_US_AXIS", "auto"),
  n_perm = as.integer(Sys.getenv("FFS_US_PERM", "200")),
  boot = as.integer(Sys.getenv("FFS_US_BOOT", "2000")),
  tag = Sys.getenv("FFS_US_TAG", format(Sys.Date()))
)
out_dir <- here::here("dev", "validate_outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

## ---- load opportunity, all seasons -------------------------------------------
message("loading opportunity ", min(config$seasons), "-", max(config$seasons), " @ ", Sys.time())
opp <- data.table::rbindlist(lapply(config$seasons, function(y) {
  x <- tryCatch(data.table::as.data.table(
    nflreadr::load_ff_opportunity(y, stat_type = "weekly")), error = function(e) NULL)
  if (is.null(x) || !nrow(x)) return(NULL)
  x[position %chin% c("RB", "WR", "TE"),
    list(season = y, week, gsis_id = player_id, pos = position, team = posteam,
         ep = total_fantasy_points_exp, fp = total_fantasy_points,
         rush_att = rush_attempt, rush_att_tm = rush_attempt_team,
         rec_att = rec_attempt, rec_att_tm = rec_attempt_team,
         air = rec_air_yards)]
}), fill = TRUE)
opp <- opp[!is.na(ep)]
cat("opportunity rows:", nrow(opp), "\n")

# team game weeks: the denominator for "per TEAM game". A week the team played
# and the player did not is a real zero, not a missing value - the same lesson
# ep_gap_study.R learned about load_ff_opportunity's absent rows.
team_weeks <- unique(opp[, list(season, team, week)])

## ---- shape, measured in season s-1 -------------------------------------------
# Shares are computed against team denominators, then percentile-ranked WITHIN
# position-season: the league drifts (RB target share and pass rate both trend
# up), so raw-unit shape changes because the league changed, not the player.
shape_window <- switch(config$shape_src,
  prior_season      = function(d, w) d,                       # whole of s-1
  early_season      = function(d, w) d[week <= 4L],           # weeks 1-4 of s
  contemporaneous   = function(d, w) d,                       # handled per-anchor
  stop("bad FFS_US_SHAPE"))

shape_base <- if (config$shape_src == "early_season") opp[week <= 4L] else opp
shape_src <- shape_base[, list(
  g = .N,
  rush_sh = sum(rush_att, na.rm = TRUE) / pmax(sum(rush_att_tm, na.rm = TRUE), 1),
  tgt_sh  = sum(rec_att, na.rm = TRUE)  / pmax(sum(rec_att_tm, na.rm = TRUE), 1),
  adot    = sum(air, na.rm = TRUE) / pmax(sum(rec_att, na.rm = TRUE), 1),
  vol     = (sum(rush_att, na.rm = TRUE) + sum(rec_att, na.rm = TRUE)) / .N,
  ep_prior = mean(ep, na.rm = TRUE)
), by = list(season, gsis_id, pos)]
shape_src <- shape_src[g >= if (config$shape_src == "early_season") 3L else 6L]

pctr <- function(x) data.table::frank(x, ties.method = "average", na.last = "keep") / sum(!is.na(x))
shape_src[, `:=`(rush_p = pctr(rush_sh), tgt_p = pctr(tgt_sh),
                 adot_p = pctr(adot), vol_p = pctr(vol)), by = list(season, pos)]

# the scalar shape axis: receiving tilt in RANK space. Positive = his role is
# receiving-weighted relative to peers; negative = ground-weighted. Interpretable,
# and it generalises the crude volume x tilt buckets that motivated this.
.axis_for <- function(po) if (config$axis != "auto") config$axis else
  if (po == "RB") "tilt" else "adot"
shape_src[, S_raw := if (.axis_for(pos[1]) == "adot") adot_p else tgt_p - rush_p,
          by = pos]
# residualise on volume so the axis is unambiguously SHAPE, not size
shape_src[, S := {
  f <- tryCatch(stats::resid(stats::lm(S_raw ~ vol_p)), error = function(e) S_raw - mean(S_raw))
  as.numeric(f)
}, by = list(season, pos)]
# prior_season -> the label applies to the FOLLOWING season; early_season -> it
# applies to the same season, and anchors are pushed to w >= 9 so the shape
# window (weeks 1-4) never touches the gap window (w-4..w-1)
shape_prior <- shape_src[, list(
  season_next = if (config$shape_src == "prior_season") season + 1L else season,
  gsis_id, pos, S, S_raw, adot_p, rush_p, tgt_p)]
min_anchor_week <- if (config$shape_src == "early_season") 9L else 1L

## ---- build the anchor panel ---------------------------------------------------
message("building panel @ ", Sys.time())
rows <- list()
for (s in config$seasons) {
  d <- opp[season == s]
  if (!nrow(d)) next
  tw <- team_weeks[season == s]
  wks <- sort(unique(d$week))
  for (w in wks) {
    pre_w  <- (w - config$L):(w - 1L)
    post_w <- (w + 1L):(w + config$H)
    if (min(pre_w) < 1L || max(post_w) > max(wks) || w < min_anchor_week) next

    # team games in each window, per team -> the per-team-game denominator
    g_pre  <- tw[week %in% pre_w,  list(gp = .N), by = team]
    g_post <- tw[week %in% post_w, list(gq = .N), by = team]

    pre <- d[week %in% pre_w, list(ep_s = sum(ep, na.rm = TRUE),
                                   fp_s = sum(fp, na.rm = TRUE),
                                   # carried so the contemporaneous DIAGNOSTIC arm
                                   # can build shape from the gap window itself
                                   c_rush = sum(rush_att, na.rm = TRUE),
                                   c_rush_tm = sum(rush_att_tm, na.rm = TRUE),
                                   c_rec = sum(rec_att, na.rm = TRUE),
                                   c_rec_tm = sum(rec_att_tm, na.rm = TRUE)),
             by = list(gsis_id, pos, team)]
    pre <- merge(pre, g_pre, by = "team")
    pre <- pre[gp >= config$min_team_games]
    pre[, `:=`(ep_lvl = ep_s / gp, G = (ep_s - fp_s) / gp)]
    pre <- pre[ep_lvl >= config$min_ep]
    if (!nrow(pre)) next

    post <- d[week %in% post_w, list(ep_q = sum(ep, na.rm = TRUE)),
              by = list(gsis_id, team)]
    post <- merge(post, g_post, by = "team")
    post <- post[gq >= config$min_team_games]
    post[, ep_fwd := ep_q / gq]

    m <- merge(pre, post[, list(gsis_id, ep_fwd)], by = "gsis_id")
    if (!nrow(m)) next
    m[, `:=`(R = ep_fwd - ep_lvl, season = s, week = w)]
    rows[[length(rows) + 1L]] <- m
  }
}
p <- data.table::rbindlist(rows, fill = TRUE)
if (config$shape_src == "contemporaneous") {
  # DIAGNOSTIC ONLY. Shape built from the gap window itself, i.e. entangled with
  # the shock by construction. This is the crude-bucket version that motivated
  # the study; if the effect appears here and nowhere else, the "archetype" was
  # just a restatement of what happened during the cold stretch.
  p[, rush_sh := c_rush / pmax(c_rush_tm, 1)]
  p[, tgt_sh := c_rec / pmax(c_rec_tm, 1)]
  p[, vol_c := (c_rush + c_rec) / pmax(gp, 1)]
  p[, `:=`(rush_p = pctr(rush_sh), tgt_p = pctr(tgt_sh), vol_p = pctr(vol_c)),
    by = list(season, pos)]
  p[, S_raw := tgt_p - rush_p]
  p[, S := as.numeric(tryCatch(stats::resid(stats::lm(S_raw ~ vol_p)),
                               error = function(e) S_raw - mean(S_raw))),
    by = list(season, pos)]
} else {
  p <- merge(p, shape_prior, by.x = c("season", "gsis_id", "pos"),
             by.y = c("season_next", "gsis_id", "pos"))
}
p <- p[pos %chin% config$pos & is.finite(R) & is.finite(G) & is.finite(S)]
p[, team_season := paste0(team, "_", season)]
p[, ply_season := paste0(gsis_id, "_", season)]
cat("panel rows:", nrow(p), "| players:", uniqueN(p$gsis_id),
    "| seasons:", uniqueN(p$season), "\n")
data.table::fwrite(p, file.path(out_dir, paste0("usage_shape_rows_", config$tag, ".csv")))

## ---- helpers ------------------------------------------------------------------
# block bootstrap clustered on (player, season): forward windows overlap heavily,
# so an iid bootstrap would understate every standard error
.boot_theta <- function(dat, form, n_boot) {
  cl <- unique(dat$ply_season)
  idx <- split(seq_len(nrow(dat)), dat$ply_season)
  vapply(seq_len(n_boot), function(i) {
    take <- sample(cl, length(cl), replace = TRUE)
    dd <- dat[unlist(idx[take], use.names = FALSE)]
    tryCatch(stats::coef(stats::lm(form, dd))[["G:S"]], error = function(e) NA_real_)
  }, numeric(1))
}

f_base <- R ~ G + ep_lvl + S + G:S + factor(team_season)
f_full <- R ~ G + ep_lvl + S + G:S + G:ep_lvl + factor(team_season)

say_lines <- character(0)
say <- function(...) { line <- paste0(...); cat(line, "\n"); say_lines <<- c(say_lines, line) }

say("usage-shape gate  ", as.character(Sys.Date()))
say("hypothesis: role consequence of a cold stretch varies with usage SHAPE")
say("shape measured in season s-1, strictly before the gap window (usage at t is")
say("caused by efficiency at t-1, so a contemporaneous label is entangled)")
say("panel: ", nrow(p), " rows, ", uniqueN(p$gsis_id), " players, seasons ",
    min(p$season), "-", max(p$season), ", L=", config$L, "w H=", config$H, "w")

for (po in config$pos) {
  d <- p[pos == po]
  if (nrow(d) < 200) { say("\n== ", po, ": too few rows (", nrow(d), ")"); next }
  m0 <- stats::lm(f_base, d)
  m1 <- stats::lm(f_full, d)
  th0 <- stats::coef(m0)[["G:S"]]
  th1 <- stats::coef(m1)[["G:S"]]
  bs <- .boot_theta(d, f_full, config$boot)
  ci <- stats::quantile(bs, c(0.025, 0.975), na.rm = TRUE)

  say("")
  say("== ", po, "  (n = ", nrow(d), ")")
  say(sprintf("   theta (G x S), base                %+.4f", th0))
  say(sprintf("   theta after G x EP_level enters    %+.4f   <- the decisive control", th1))
  say(sprintf("   block-boot 95%% CI                  [%+.4f, %+.4f]  %s",
              ci[[1]], ci[[2]], if (ci[[1]] * ci[[2]] > 0) "excludes 0" else "INCLUDES 0"))

  # specification sensitivity: team x season FE is ~5.7 rows per level on this
  # panel (490 levels / 2800 RB rows), which is over-parameterised - it absorbs
  # signal and inflates variance, and is why the block-boot CI runs 3-4x the
  # permutation SD. Report the effect with and without it rather than quoting
  # whichever looks cleaner.
  m_nofe <- stats::lm(R ~ G + ep_lvl + S + G:S + G:ep_lvl, d)
  th_nofe <- stats::coef(m_nofe)[["G:S"]]
  se_nofe <- stats::coef(summary(m_nofe))["G:S", "Std. Error"]
  say(sprintf("   theta without team x season FE     %+.4f  (SE %.4f, t %.2f)  [%d FE levels / %d rows]",
              th_nofe, se_nofe, th_nofe / se_nofe, uniqueN(d$team_season), nrow(d)))

  # economic size: predicted R difference between top and bottom shape decile
  # at a 1-SD gap shock
  sd_g <- stats::sd(d$G, na.rm = TRUE)
  s_hi <- stats::quantile(d$S, 0.9, na.rm = TRUE)
  s_lo <- stats::quantile(d$S, 0.1, na.rm = TRUE)
  eff <- th1 * sd_g * (s_hi - s_lo)
  say(sprintf("   decile spread at a 1-SD gap shock  %+.3f EP per team game  (need |.| > 0.5)",
              eff))

  # leave-one-season-out sign stability
  ss <- sort(unique(d$season))
  sgn <- vapply(ss, function(s) {
    tryCatch(sign(stats::coef(stats::lm(f_full, d[season != s]))[["G:S"]]), numeric(1),
             error = function(e) NA_real_)
  }, numeric(1))
  ref <- sign(th1)
  say("   LOSO sign agreement: ", sum(sgn == ref, na.rm = TRUE), "/", length(ss))

  # placebo: the permutation NULL, not a single draw. One permutation carries the
  # same sampling noise as theta itself, so comparing to "~0" by eye is
  # meaningless - what matters is where theta sits in the null distribution.
  perm <- vapply(seq_len(config$n_perm), function(i) {
    dp <- data.table::copy(d)
    dp[, G := sample(G), by = season]
    tryCatch(stats::coef(stats::lm(f_full, dp))[["G:S"]], error = function(e) NA_real_)
  }, numeric(1))
  p_emp <- mean(abs(perm) >= abs(th1), na.rm = TRUE)
  say(sprintf("   PLACEBO permutation null           mean %+.4f  sd %.4f  -> p = %.3f",
              mean(perm, na.rm = TRUE), stats::sd(perm, na.rm = TRUE), p_emp))

  pass <- isTRUE(ci[[1]] * ci[[2]] > 0) &&
    isTRUE(abs(eff) > 0.5) &&
    isTRUE(sum(sgn == ref, na.rm = TRUE) >= ceiling(0.75 * length(ss)))
  say("   VERDICT: ", if (pass) "PASS" else "FAIL")
}

say("")
say("gate = theta CI excludes 0 after the G x EP_level control, decile spread at")
say("a 1-SD shock exceeds 0.5 EP/team game, and LOSO sign agreement >= 75%.")
say("A tiny-but-significant theta is a FAIL: the effect must be tradeable, and")
say("the main effect of shape bought only +0.002 adj R2 in the crude preview.")

writeLines(say_lines, file.path(out_dir, paste0("usage_shape_gate_", config$tag, ".txt")))
message("wrote ", file.path(out_dir, paste0("usage_shape_gate_", config$tag, ".txt")), " @ ", Sys.time())
