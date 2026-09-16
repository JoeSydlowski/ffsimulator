# draft_contingency.R -- the draft-day cheat sheet
#
# With a two-minute clock there is no time to simulate at the pick, so the plan
# has to be set in advance and keyed on something observable. The one thing that
# is both observable at pick 11 and genuinely uncertain beforehand is how many
# QBs the room has already taken (see flea-draft-tendencies: three seasons of
# history cannot pin it down).
#
# So: run the sweep with the opponent QB appetite drawn per board, then read the
# result BACK by the realised opening. Every world is an observation of "the
# room opened like this, and this build scored that" - post-stratification turns
# them into a table you can carry to the draft.
#
#   FFS_DRAFT_OUT=<dir> Rscript dev/suite/draft_contingency.R

suppressPackageStartupMessages(library(data.table))

# Strata over QBs gone before pick 11. The board says 3 and Joe's five-year
# memory says 5-6; the bins have to resolve that range without slicing so thin
# that no stratum has boards in it.
QB_BINS <- c(-1, 2, 3, 4, 5, 99)
QB_LABS <- c("<=2", "3", "4", "5", "6+")

load_boards <- function(outdir, which = c("confirm", "search")) {
  which <- match.arg(which)
  f <- file.path(outdir, paste0(which, "_by_board.csv"))
  if (!file.exists(f)) stop("missing ", f)
  d <- fread(f)
  need <- c("arm", "branch", "board", "champion_pct", "playoff_pct", "qb_before_p1")
  miss <- setdiff(need, names(d))
  if (length(miss)) {
    stop("this run predates board-state capture (missing ", paste(miss, collapse = ", "),
         ") - a contingency table needs a sweep run after that change")
  }
  d[, qb_stratum := cut(qb_before_p1, breaks = QB_BINS, labels = QB_LABS)]
  d[]
}

# Paired against the control WITHIN a stratum: same board seed, so the same
# room, the same draws and the same schedule - only the build differs.
contingency <- function(d, control = "bpa", metric = "champion_pct") {
  d <- copy(d)
  setnames(d, metric, "y")
  ctl <- d[arm == control, list(branch, board, y_ctl = y)]
  x <- merge(d, ctl, by = c("branch", "board"))
  x[, dy := y - y_ctl]
  se <- function(z) if (length(z) < 2L) NA_real_ else stats::sd(z) / sqrt(length(z))
  out <- x[arm != control, list(
    n_boards = .N, y = mean(y), d = mean(dy), se = se(dy)
  ), by = list(qb_stratum, arm)]
  out[, t := d / se]
  setorder(out, qb_stratum, -d)
  out[]
}

# The cheat sheet: for each opening, is it better or worse than BPA given how
# many QBs went before your pick?
cheat_sheet <- function(ct, arms_keep = NULL, t_cut = 2) {
  x <- if (is.null(arms_keep)) ct else ct[arm %in% arms_keep]
  w <- dcast(x, arm ~ qb_stratum, value.var = "d")
  n <- dcast(x, arm ~ qb_stratum, value.var = "n_boards")
  list(delta = w[], n = n[],
       best = x[, .SD[which.max(d)], by = qb_stratum][
         , list(qb_stratum, arm, d, se, t, n_boards)][order(qb_stratum)],
       separated = x[!is.na(t) & abs(t) >= t_cut][order(qb_stratum, -d)])
}

report_contingency <- function(outdir = cfg$outdir, which = "confirm") {
  d <- load_boards(outdir, which)
  msg("boards by QB stratum:")
  print(d[arm == "bpa", list(boards = .N), by = qb_stratum][order(qb_stratum)][])

  for (m in c("champion_pct", "playoff_pct")) {
    ct <- contingency(d, metric = m)
    fwrite(ct, file.path(outdir, sprintf("contingency_%s.csv", m)))
    cs <- cheat_sheet(ct)
    msg("== ", m, ": best opening per stratum ==")
    print(cs$best[])
    msg("arms separated at |t|>=2:", nrow(cs$separated))
    if (nrow(cs$separated)) print(head(cs$separated, 15)[])
  }

  # does the bye explain playoff-to-title conversion, and does it vary by build?
  if ("bye_pct" %in% names(d)) {
    agg <- d[, list(playoff = mean(playoff_pct), bye = mean(bye_pct),
                    champ = mean(champion_pct)), by = arm]
    agg[, conv := champ / playoff]
    fit <- stats::lm(champ ~ playoff + bye, data = agg)
    msg("champion ~ playoff + bye across arms:")
    print(round(summary(fit)$coefficients, 4))
    msg(sprintf("R2 with bye %.3f | without %.3f | cor(playoff,champ) %.3f",
                summary(fit)$r.squared,
                summary(stats::lm(champ ~ playoff, data = agg))$r.squared,
                stats::cor(agg$playoff, agg$champ)))
    msg(sprintf("conversion across arms: %.3f to %.3f (sd %.4f)",
                min(agg$conv), max(agg$conv), stats::sd(agg$conv)))
    fwrite(agg[order(-champ)], file.path(outdir, "conversion_by_arm.csv"))
  }
  invisible(d)
}

if (!exists("cfg")) {
  cfg <- list(outdir = Sys.getenv("FFS_DRAFT_OUT", ""))
  msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")
  if (!nzchar(cfg$outdir)) stop("set FFS_DRAFT_OUT")
  report_contingency(cfg$outdir)
} else if ("contingency" %in% cfg$stages) {
  report_contingency()
}
