# Does scoring OVER your expected points persist? - FPOE persistence study
#
#   Rscript dev/suite/fpoe_persistence.R
#   FFS_FP_MINEP=5 FFS_FP_WINDOW=2 Rscript dev/suite/fpoe_persistence.R
#
# The question: Dalton Kincaid is averaging 13.7 expected points and 20.3 actual
# through 2 games, +6.6 per game over expectation. What does a player like that
# do for the REST OF THE SEASON? That number is the whole argument for whether a
# hot start is a skill signal or a coin flip you are about to pay for.
#
# Design, with the three choices tested rather than assumed:
#   1. THRESHOLD. Without one the sample fills with players who saw two targets
#      and caught a touchdown, whose FPOE is one play. Tested across EP/g floors.
#   2. WHICH TWO GAMES. Weeks 1-2 only (matches the real decision) vs every
#      consecutive 2-game window in a season (more sample, but a player-season
#      contributes many overlapping windows, so SEs must cluster on it).
#   3. WHAT IS "ROS". Forward-only (the tradeable version: what happens NEXT) vs
#      every other game in the season (the trait version: is this a property of
#      the player). The measurement games are always held out of the outcome.
#
# Games are GAMES PLAYED, not calendar weeks, so a bye or an inactive does not
# silently become a zero-FPOE game.
#
# Output: dev/validate_outputs/fpoe_persistence_<tag>.txt

library(data.table)
suppressMessages(devtools::load_all(here::here(), quiet = TRUE))

config <- list(
  seasons = 2009:2025,
  W = as.integer(Sys.getenv("FFS_FP_WINDOW", "2")),      # measurement games
  min_ros = as.integer(Sys.getenv("FFS_FP_MINROS", "4")),
  min_ep = as.numeric(Sys.getenv("FFS_FP_MINEP", "5")),
  boot = as.integer(Sys.getenv("FFS_FP_BOOT", "1000")),
  tag = Sys.getenv("FFS_FP_TAG", format(Sys.Date()))
)
out_dir <- here::here("dev", "validate_outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("loading opportunity @ ", Sys.time())
opp <- data.table::rbindlist(lapply(config$seasons, function(y) {
  x <- tryCatch(data.table::as.data.table(
    nflreadr::load_ff_opportunity(y, stat_type = "weekly")), error = function(e) NULL)
  if (is.null(x) || !nrow(x)) return(NULL)
  x[position %chin% c("QB", "RB", "WR", "TE"),
    list(season = y, week, gsis_id = player_id, pos = position,
         ep = total_fantasy_points_exp, fp = total_fantasy_points)]
}), fill = TRUE)
opp <- opp[!is.na(ep) & !is.na(fp)]
opp[, fpoe := fp - ep]
data.table::setorder(opp, season, gsis_id, week)
opp[, gi := seq_len(.N), by = list(season, gsis_id)]   # game index, games PLAYED
cat("player-games:", nrow(opp), "| seasons:", uniqueN(opp$season), "\n")

## ---- build (measurement window -> outcome) pairs -----------------------------
# anchor at game index k: measurement = games k..k+W-1, outcome = everything after
make_pairs <- function(first_only, forward_only) {
  ks <- if (first_only) 1L else 1:12
  data.table::rbindlist(lapply(ks, function(k) {
    pre <- opp[gi %in% k:(k + config$W - 1L),
               list(n_pre = .N, ep_pre = mean(ep), fp_pre = mean(fp),
                    fpoe_pre = mean(fpoe)), by = list(season, gsis_id, pos)]
    pre <- pre[n_pre == config$W]
    post <- if (forward_only) {
      opp[gi > (k + config$W - 1L),
          list(n_ros = .N, fpoe_ros = mean(fpoe), ep_ros = mean(ep)),
          by = list(season, gsis_id)]
    } else {
      opp[!(gi %in% k:(k + config$W - 1L)),
          list(n_ros = .N, fpoe_ros = mean(fpoe), ep_ros = mean(ep)),
          by = list(season, gsis_id)]
    }
    post <- post[n_ros >= config$min_ros]
    m <- merge(pre, post, by = c("season", "gsis_id"))
    m[, k := k]
    m
  }), fill = TRUE)
}

# cluster bootstrap on player-season: overlapping windows from one player-season
# are not independent observations
.boot_slope <- function(d, n_boot) {
  d[, cl := paste0(gsis_id, "_", season)]
  idx <- split(seq_len(nrow(d)), d$cl)
  cls <- names(idx)
  vapply(seq_len(n_boot), function(i) {
    take <- sample(cls, length(cls), replace = TRUE)
    dd <- d[unlist(idx[take], use.names = FALSE)]
    tryCatch(stats::coef(stats::lm(fpoe_ros ~ fpoe_pre, dd))[[2]], error = function(e) NA_real_)
  }, numeric(1))
}

say_lines <- character(0)
say <- function(...) { l <- paste0(...); cat(l, "\n"); say_lines <<- c(say_lines, l) }

say("FPOE persistence  ", as.character(Sys.Date()))
say("signal: mean (actual - expected) points per game over a ", config$W, "-game window")
say("outcome: mean FPOE per game over the rest of the season, measurement games held out")
say("seasons ", min(config$seasons), "-", max(config$seasons),
    " | EP/g floor ", config$min_ep, " | min ROS games ", config$min_ros)

## ---- design choice 1: threshold ----------------------------------------------
base <- make_pairs(first_only = FALSE, forward_only = TRUE)
say("")
say("== DESIGN 1: does the EP/g floor matter?  (any 2-game window, forward ROS)")
say("   floor      n    slope   r      mean|fpoe_pre|")
for (f in c(0, 3, 5, 8, 12)) {
  d <- base[ep_pre >= f]
  if (nrow(d) < 100) next
  sl <- stats::coef(stats::lm(fpoe_ros ~ fpoe_pre, d))[[2]]
  rr <- stats::cor(d$fpoe_pre, d$fpoe_ros)
  say(sprintf("   %5.0f  %6d  %+.4f  %+.3f   %.2f", f, nrow(d), sl, rr, mean(abs(d$fpoe_pre))))
}

## ---- design choice 2 & 3 -------------------------------------------------------
say("")
say("== DESIGN 2+3: which games, and what counts as ROS  (EP/g floor ", config$min_ep, ")")
say("   window          ROS def     n      slope    boot 95% CI")
grid <- list(
  list(lab = "first 2 games", fo = TRUE,  fw = TRUE),
  list(lab = "any 2 games",   fo = FALSE, fw = TRUE),
  list(lab = "any 2 games",   fo = FALSE, fw = FALSE)
)
main_slope <- NA_real_
for (g in grid) {
  d <- make_pairs(first_only = g$fo, forward_only = g$fw)[ep_pre >= config$min_ep]
  if (nrow(d) < 100) next
  sl <- stats::coef(stats::lm(fpoe_ros ~ fpoe_pre, d))[[2]]
  bs <- .boot_slope(data.table::copy(d), config$boot)
  ci <- stats::quantile(bs, c(0.025, 0.975), na.rm = TRUE)
  say(sprintf("   %-14s  %-10s  %6d  %+.4f   [%+.4f, %+.4f]",
              g$lab, if (g$fw) "forward" else "all other", nrow(d), sl, ci[[1]], ci[[2]]))
  if (g$fo && g$fw) main_slope <- sl
}

## ---- the actual question -------------------------------------------------------
d <- base[ep_pre >= config$min_ep]
d[, bin := cut(fpoe_pre, c(-Inf, -6, -3, -1, 1, 3, 6, Inf),
               labels = c("<-6", "-6..-3", "-3..-1", "-1..1", "1..3", "3..6", ">6"))]
say("")
say("== WHAT A HOT START IS WORTH  (any 2-game window, forward ROS, EP/g >= ",
    config$min_ep, ")")
say("   FPOE/g in the window      n      mean ROS FPOE/g   share still positive")
for (b in levels(d$bin)) {
  x <- d[bin == b]
  if (!nrow(x)) next
  say(sprintf("   %-12s  %10d  %+14.2f   %6.0f%%",
              b, nrow(x), mean(x$fpoe_ros), 100 * mean(x$fpoe_ros > 0)))
}

k <- d[fpoe_pre >= 5 & fpoe_pre <= 8]
if (nrow(k) > 30) {
  say("")
  say("   the Kincaid cell (FPOE/g between +5 and +8 over 2 games), n = ", nrow(k), ":")
  say(sprintf("     mean ROS FPOE/g        %+.2f", mean(k$fpoe_ros)))
  say(sprintf("     median                 %+.2f", stats::median(k$fpoe_ros)))
  say(sprintf("     still positive         %.0f%%", 100 * mean(k$fpoe_ros > 0)))
  say(sprintf("     retained share of +6.5 %.0f%%", 100 * mean(k$fpoe_ros) / 6.5))
}

## ---- by position ----------------------------------------------------------------
say("")
say("== by position  (slope of ROS FPOE on window FPOE)")
for (po in c("QB", "RB", "WR", "TE")) {
  x <- d[pos == po]
  if (nrow(x) < 100) next
  say(sprintf("   %-3s  n %6d   slope %+.4f   r %+.3f", po, nrow(x),
              stats::coef(stats::lm(fpoe_ros ~ fpoe_pre, x))[[2]],
              stats::cor(x$fpoe_pre, x$fpoe_ros)))
}

writeLines(say_lines, file.path(out_dir, paste0("fpoe_persistence_", config$tag, ".txt")))
message("wrote ", file.path(out_dir, paste0("fpoe_persistence_", config$tag, ".txt")), " @ ", Sys.time())
