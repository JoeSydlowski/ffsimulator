# draft_report.R -- turn the sweep output into the readable answer
#
# Sourced by draft_strategy.R after draft_runner.R, or run standalone against a
# finished output folder:
#
#   FFS_DRAFT_OUT=dev/league_sims/draft_<id>_<date> Rscript dev/suite/draft_report.R
#
# The one rule here: a build is only called better than the control when its
# PAIRED delta clears its own standard error. Ranking on the raw championship
# rate would be ranking noise - at 12 teams the base rate is 8.3% and the
# board-to-board spread swamps most of the differences between arms.

suppressPackageStartupMessages(library(data.table))

T_CUT <- 2  # paired t on the champion delta to call an arm separated

fmt_pct <- function(x) sprintf("%+.2f", 100 * x)

report_builds <- function(outdir = cfg$outdir) {
  f <- file.path(outdir, "builds.csv")
  if (!file.exists(f)) stop("no builds.csv in ", outdir, " - run the sweep first")
  b <- fread(f)

  lines <- c("# Draft strategy - slot 11", "",
             sprintf("League %s, %d-team superflex PPR, %d-round snake. Picks %s.",
                     cfg$league_id, length(D$draft_slots), cfg$n_rounds,
                     paste(head(MY_PICKS, 6), collapse = ", ")),
             sprintf("Board: DraftSharks superflex/PPR/Sleeper/12, %d players.", nrow(BOARD)),
             "")

  for (bn in unique(b$branch)) {
    x <- b[branch == bn][order(-d_champ)]
    lines <- c(lines, sprintf("## Branch: %s", bn), "",
      sprintf("Control is `%s` at %.1f%% championship / %.1f%% playoff.",
              CONTROL, 100 * x[arm == CONTROL]$champ, 100 * x[arm == CONTROL]$playoff),
      "",
      "| build | champ % | vs bpa (pp) | SE | t | playoff % | vs bpa (pp) |",
      "|---|---|---|---|---|---|---|")
    for (i in seq_len(min(12L, nrow(x)))) {
      r <- x[i]
      lines <- c(lines, sprintf("| %s | %.1f | %s | %.2f | %.1f | %.1f | %s |",
        r$arm, 100 * r$champ, fmt_pct(r$d_champ), 100 * r$se_champ,
        r$t_champ, 100 * r$playoff, fmt_pct(r$d_play)))
    }
    sep <- x[arm != CONTROL & !is.na(t_champ) & abs(t_champ) >= T_CUT]
    lines <- c(lines, "",
      if (nrow(sep)) sprintf("Separated from the control at |t| >= %g: %s.", T_CUT,
                             paste(sep$arm, collapse = ", "))
      else sprintf("**No arm separates from the control at |t| >= %g.** The confirm budget cannot rank these builds; treat them as equivalent on championship odds.", T_CUT),
      "")
  }

  # does the winner change between branches?
  best <- b[, .SD[which.max(d_champ)], by = branch]
  lines <- c(lines, "## Does the QB assumption change the answer?", "",
    paste(sprintf("- %s: best arm `%s` (%s pp, t=%.1f)", best$branch, best$arm,
                  fmt_pct(best$d_champ), best$t_champ), collapse = "\n"), "")
  lines <- c(lines,
    if (uniqueN(best$arm) == 1L)
      "The same build wins under both readings of the room, so the round-one QB uncertainty does not change what to do."
    else
      "The best build DIFFERS between branches - the round-one QB count is a live decision input. Count the QBs gone by pick 9 to tell which branch you are in before picking at 11.",
    "")

  writeLines(lines, file.path(outdir, "draft_plan.md"))
  msg("wrote", file.path(outdir, "draft_plan.md"))
  invisible(b)
}

report_qb_curve <- function(outdir = cfg$outdir) {
  f <- file.path(outdir, "qb_wait_curve.csv")
  if (!file.exists(f)) return(invisible(NULL))
  q <- fread(f)
  if (!nrow(q)) return(invisible(NULL))
  msg("QB wait curve (champion delta vs bpa, pp):")
  print(dcast(q, k + j ~ branch, value.var = "d_champ")[])
  invisible(q)
}

if ("report" %in% cfg$stages) {
  report_builds()
  report_qb_curve()
}
