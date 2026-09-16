# draft_downstream.R -- what the QB plan implies for the rest of the draft
#
# The sweep answers "when should the quarterbacks come". It does not, on its
# own, answer the questions that follow: given that plan and a best-available
# opening, what do the middle picks have to look like, how much running back
# capital is enough, and whether late volume substitutes for early capital.
#
# Those are all readable from my_drafts.csv, which carries every individual
# pick in every world, joined to that world's outcome. Within a single arm the
# QB rounds are fixed and everything else is best-available, so the variation
# across boards is variation in what the ROOM left behind - which is as close
# to a natural experiment as this design gets.
#
#   FFS_DRAFT_OUT=<dir> Rscript dev/suite/draft_downstream.R

suppressPackageStartupMessages(library(data.table))

pick_value <- function(overall) 10000 * exp(-0.023 * overall)

load_downstream <- function(outdir) {
  d <- fread(file.path(outdir, "my_drafts.csv"))
  o <- fread(file.path(outdir, "search_by_board.csv"))
  key <- c("arm", "branch", "board")
  o <- o[, c(key, "champion_pct", "playoff_pct", "bye_pct", "qb_before_p1"), with = FALSE]
  d[, v := pick_value(overall)]
  list(picks = d, out = o, key = key)
}

# ---- 1. the shape of the middle rounds, given an opening --------------------

middle_shape <- function(z, arm_name, open = c("WR", "WR"), rounds = 3:8) {
  p <- z$picks[arm == arm_name]
  if (!nrow(p)) stop("no picks for arm ", arm_name)
  w <- dcast(p[round <= max(rounds)], board ~ round, value.var = "pos")
  rn <- setdiff(names(w), "board")
  setnames(w, rn, paste0("r", rn))
  sel <- w[get(paste0("r", 1)) == open[1] & get(paste0("r", 2)) == open[2]]
  cat(sprintf("\narm %s | opened %s: %d of %d boards (%.0f%%)\n",
              arm_name, paste(open, collapse = "-"), nrow(sel), nrow(w),
              100 * nrow(sel) / nrow(w)))
  if (!nrow(sel)) return(invisible(NULL))
  for (r in rounds) {
    cc <- paste0("r", r)
    if (!cc %in% names(sel)) next
    t <- sel[, .N, by = c(cc)][order(-N)]
    setnames(t, cc, "pos")
    cat(sprintf("  round %-2d: %s\n", r,
                paste(sprintf("%s %.0f%%", t$pos, 100 * t$N / nrow(sel)), collapse = "   ")))
  }
  invisible(sel)
}

# ---- 2. how much RB capital is enough? -------------------------------------

# Dose-response inside one arm: bin boards by the share of pick value spent on
# a position through a given round, and read the championship rate off each bin.
capital_response <- function(z, arm_name, position = "RB", through = 8, nbin = 6) {
  p <- z$picks[arm == arm_name & round <= through]
  cap <- p[, list(pos_v = sum(v[pos == position]), tot_v = sum(v)), by = board]
  cap[, share := 100 * pos_v / tot_v]
  m <- merge(cap, z$out[arm == arm_name], by = "board")
  m[, bin := cut(share, breaks = unique(stats::quantile(share, seq(0, 1, length.out = nbin + 1))),
                 include.lowest = TRUE)]
  r <- m[, list(boards = .N,
                share_lo = round(min(share), 1), share_hi = round(max(share), 1),
                champ = round(100 * mean(champion_pct), 2),
                playoff = round(100 * mean(playoff_pct), 1),
                se = round(100 * stats::sd(champion_pct) / sqrt(.N), 2)), by = bin]
  setorder(r, share_lo)
  cat(sprintf("\n%s capital through round %d, arm %s (champion %% by bin)\n",
              position, through, arm_name))
  print(r[])
  fit <- stats::lm(champion_pct ~ share, data = m)
  cat(sprintf("  slope: %+.4f pp champion per 1pp of %s capital (t = %.2f)\n",
              100 * coef(fit)[["share"]], position,
              summary(fit)$coefficients["share", "t value"]))
  invisible(m)
}

# ---- 3. does late volume substitute for early capital? ---------------------

late_substitution <- function(z, arm_name, position = "RB",
                              early_through = 8, late_from = 9) {
  p <- z$picks[arm == arm_name]
  e <- p[round <= early_through, list(early_v = sum(v[pos == position]),
                                      tot_v = sum(v)), by = board]
  e[, early_share := 100 * early_v / tot_v]
  l <- p[round >= late_from, list(late_n = sum(pos == position)), by = board]
  m <- merge(merge(e, l, by = "board"), z$out[arm == arm_name], by = "board")
  m[, early_grp := ifelse(early_share <= stats::median(early_share), "low early", "high early")]
  m[, late_grp := ifelse(late_n <= stats::median(late_n), "few late", "many late")]
  r <- m[, list(boards = .N, champ = round(100 * mean(champion_pct), 2),
                playoff = round(100 * mean(playoff_pct), 1),
                se = round(100 * stats::sd(champion_pct) / sqrt(.N), 2)),
         by = list(early_grp, late_grp)]
  cat(sprintf("\ncan late %s volume replace early %s capital? (arm %s)\n",
              position, position, arm_name))
  print(r[order(early_grp, late_grp)][])
  cat("  read: if 'low early + many late' matches 'high early', volume substitutes.\n")
  invisible(m)
}

run_downstream <- function(outdir = cfg$outdir, arm_name = NULL) {
  z <- load_downstream(outdir)
  if (is.null(arm_name)) {
    b <- fread(file.path(outdir, "builds.csv"))
    arm_name <- b[order(-d_champ)][1]$arm
    cat("best arm by champion delta:", arm_name, "\n")
  }
  middle_shape(z, arm_name)
  for (pos in c("RB", "WR")) capital_response(z, arm_name, pos, through = 8)
  late_substitution(z, arm_name, "RB")
  invisible(z)
}

if (!exists("cfg")) {
  outdir <- Sys.getenv("FFS_DRAFT_OUT", "")
  if (!nzchar(outdir)) stop("set FFS_DRAFT_OUT")
  run_downstream(outdir)
} else if ("downstream" %in% cfg$stages) {
  run_downstream()
}
