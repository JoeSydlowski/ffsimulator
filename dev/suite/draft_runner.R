# draft_runner.R -- SEARCH / CONFIRM / CAPITAL over the build arms
#
# Sourced by draft_strategy.R after draft_sweep.R. Two budgets, the pattern
# keeper_intel.R uses: a cheap pass over every arm to rank them, then a real
# pass over the shortlist. Both opponent branches are run end to end, because
# the whole question is whether the recommendation survives the QB assumption.
#
# One world costs about 0.18s per simulated season (measured: n=100 -> 18.6s,
# n=300 -> 53.6s, n=800 -> 145.7s), so the sweep only fits in an evening in
# parallel. Workers are set up once and rebuild the board themselves from the
# cache; the score draws move as a file rather than 14 copies over a socket.

suppressPackageStartupMessages(library(data.table))

SW <- list(
  n_search   = as.integer(Sys.getenv("FFS_DRAFT_NSEARCH", "150")),
  b_search   = as.integer(Sys.getenv("FFS_DRAFT_BSEARCH", "10")),
  n_confirm  = as.integer(Sys.getenv("FFS_DRAFT_NCONFIRM", "500")),
  b_confirm  = as.integer(Sys.getenv("FFS_DRAFT_BCONFIRM", "40")),
  n_random   = as.integer(Sys.getenv("FFS_DRAFT_NRANDOM", "60")),
  n_top      = as.integer(Sys.getenv("FFS_DRAFT_NTOP", "10")),
  skip_confirm = nzchar(Sys.getenv("FFS_DRAFT_SKIP_CONFIRM", "")),
  workers    = as.integer(Sys.getenv("FFS_DRAFT_WORKERS", "0")),
  # board seeds start here + 1. Batches that each take a distinct slice can be
  # pooled afterwards as one larger sample; without it every batch re-runs the
  # same boards and adds no information.
  b_offset   = as.integer(Sys.getenv("FFS_DRAFT_BOFFSET", "0"))
)
if (SW$workers <= 0L) {
  SW$workers <- max(1L, parallel::detectCores(logical = FALSE) - 2L)
}

CONTROL <- "bpa"
ROOT <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))

# Draft-pick value. Same exponential decay keeper_intel.R prices picks with,
# evaluated at the OVERALL PICK - so this measures what a build spent, not what
# it happened to get.
pick_value <- function(overall) 10000 * exp(-0.023 * overall)

capital_shares <- function(drafted_me) {
  d <- copy(drafted_me)
  d[, v := pick_value(overall)]
  sh <- d[, list(v = sum(v)), by = pos]
  out <- setNames(numeric(length(cfg$pos_filter)), cfg$pos_filter)
  out[sh$pos] <- sh$v / sum(d$v)
  as.list(out)
}

# ------------------------------------------------------------- workers ------

sweep_cluster <- function(nw) {
  msg("starting", nw, "workers")
  cl <- parallel::makePSOCKcluster(nw)
  ok <- try(parallel::clusterCall(cl, function(root) {
    Sys.setenv(FFS_DRAFT_STAGES = "none")
    setwd(root)
    suppressMessages(source(file.path(root, "dev", "suite", "draft_strategy.R")))
    suppressMessages(source(file.path(root, "dev", "suite", "draft_sweep.R")))
    TRUE
  }, ROOT), silent = TRUE)
  if (inherits(ok, "try-error")) {
    parallel::stopCluster(cl)
    stop("worker setup failed: ", attr(ok, "condition")$message)
  }
  cl
}

push_draws <- function(cl, draws) {
  f <- file.path(tempdir(), "ffs_draft_draws.rds")
  saveRDS(draws, f)
  parallel::clusterCall(cl, function(path) {
    assign(".DRAWS", readRDS(path), envir = globalenv()); TRUE
  }, f)
  invisible(f)
}

# runs on a worker: one arm over one CHUNK of boards.
#
# Parallelising over arms alone wastes the machine whenever the arm count is
# not a multiple of the worker count - 24 arms on 14 workers runs a wave of 14
# then a wave of 10, leaving ten cores idle for the whole second wave. Splitting
# each arm into board chunks makes the task list long enough for parLapplyLB to
# keep every worker busy to the end.
.worker_chunk <- function(task, branch_name, keep_drafted) {
  spec <- task$spec
  pol <- arm_policy(spec)
  br <- BRANCHES[[branch_name]]
  dr <- get(".DRAWS", envir = globalenv())
  r <- score_arm(pol, br, dr, task$seeds, keep_drafted = keep_drafted)
  list(arm = spec$arm, kind = spec$kind, branch = branch_name,
       by_board = r$by_board, drafted = r$drafted)
}

score_all <- function(cl, arms, branch_name, draws, board_seeds, keep_drafted = FALSE,
                      chunks_per_arm = NULL) {
  res <- if (is.null(cl)) {
    lapply(arms, function(s) {
      r <- score_arm(arm_policy(s), BRANCHES[[branch_name]], draws, board_seeds,
                     keep_drafted = keep_drafted)
      list(arm = s$arm, kind = s$kind, branch = branch_name,
           by_board = r$by_board, drafted = r$drafted)
    })
  } else {
    # aim for several tasks per worker so the tail is short; chunks are board
    # slices, and every arm still sees the SAME board seeds so pairing holds
    nw <- length(cl)
    if (is.null(chunks_per_arm)) {
      chunks_per_arm <- max(1L, min(length(board_seeds),
                                    ceiling(3 * nw / max(1L, length(arms)))))
    }
    grp <- split(board_seeds, cut(seq_along(board_seeds), chunks_per_arm, labels = FALSE))
    tasks <- unlist(lapply(arms, function(s)
      lapply(grp, function(sd) list(spec = s, seeds = sd))), recursive = FALSE)
    parallel::parLapplyLB(cl, tasks, .worker_chunk, branch_name, keep_drafted)
  }
  rows <- rbindlist(lapply(res, function(z)
    cbind(data.table(arm = z$arm, kind = z$kind, branch = z$branch), z$by_board)))
  setorder(rows, arm, board)
  caps <- if (keep_drafted) rbindlist(lapply(res, function(z)
    cbind(data.table(arm = z$arm, branch = z$branch),
          z$drafted[, capital_shares(.SD), by = board]))) else NULL
  if (!is.null(caps)) setorder(caps, arm, board)
  raw <- if (keep_drafted) rbindlist(lapply(res, function(z)
    cbind(data.table(arm = z$arm, branch = z$branch), z$drafted))) else NULL
  list(by_board = rows, capital = caps, drafted = raw)
}

# ---------------------------------------------------------------- pairing ---

# Paired against the control on the SAME board seed. The control shares the
# draws, the schedule and the opponent draft, so the difference isolates the
# build; the SE is the spread of those paired differences across boards, which
# is far tighter than the spread of the raw championship rate.
pair_vs_control <- function(by_board, control = CONTROL) {
  ctl <- by_board[arm == control, list(branch, board,
                                       c_champ = champion_pct, c_play = playoff_pct,
                                       c_bye = bye_pct,
                                       c_wins = h2h_wins, c_pf = points_for)]
  x <- merge(by_board, ctl, by = c("branch", "board"))
  x[, `:=`(d_champ = champion_pct - c_champ, d_play = playoff_pct - c_play,
           d_bye = bye_pct - c_bye,
           d_wins = h2h_wins - c_wins, d_pf = points_for - c_pf)]
  se <- function(z) if (length(z) < 2L) NA_real_ else stats::sd(z) / sqrt(length(z))
  out <- x[, list(
    n_boards = .N,
    champ = mean(champion_pct), playoff = mean(playoff_pct),
    bye = mean(bye_pct), conv = mean(champion_pct) / mean(playoff_pct),
    wins = mean(h2h_wins), pf = mean(points_for),
    d_champ = mean(d_champ), se_champ = se(d_champ),
    d_play = mean(d_play), se_play = se(d_play),
    d_bye = mean(d_bye), se_bye = se(d_bye),
    d_wins = mean(d_wins), se_wins = se(d_wins)
  ), by = list(branch, arm, kind)]
  out[, t_champ := d_champ / se_champ]
  out[, t_play := d_play / se_play]
  out[]
}

# ------------------------------------------------------------------ stage ---

run_sweep <- function() {
  msg("== SWEEP ==")
  arms <- make_arms(include_qb_grid = TRUE, n_random = SW$n_random)
  msg("arms:", length(arms), "| branches:", paste(BRANCH_USE, collapse = " "),
      "| qb_hungry shift:", BRANCHES$qb_hungry$pos_shift[["QB"]])
  stopifnot(CONTROL %in% names(arms))

  cl <- sweep_cluster(SW$workers)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)

  # ---- SEARCH ----
  msg(sprintf("SEARCH: n=%d x %d boards, %d arms x %d branches = %d worlds",
              SW$n_search, SW$b_search, length(arms), length(BRANCH_USE),
              length(arms) * SW$b_search * length(BRANCH_USE)))
  draws_s <- make_draws(SW$n_search, cfg$seed)
  push_draws(cl, draws_s)
  seeds_s <- seq_len(SW$b_search) + SW$b_offset
  t0 <- Sys.time()
  # capital is collected HERE, on every arm, not on the confirm shortlist.
  # The shortlist is chosen on championship rate, so regressing championship
  # rate on its capital would be regressing an outcome on a set selected by
  # that same outcome. The search set is unselected, which is what the
  # frontier needs.
  s_res <- lapply(BRANCH_USE, function(bn) {
    r <- score_all(cl, arms, bn, draws_s, seeds_s, keep_drafted = TRUE)
    msg("  branch", bn, "done", sprintf("(%.1f min)", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    r
  })
  search <- rbindlist(lapply(s_res, `[[`, "by_board"))
  search_cap <- rbindlist(lapply(s_res, `[[`, "capital"))
  search_raw <- rbindlist(lapply(s_res, `[[`, "drafted"))
  fwrite(search_raw, file.path(cfg$outdir, "my_drafts.csv"))
  msg("saved", nrow(search_raw), "individual picks to my_drafts.csv")
  fwrite(search, file.path(cfg$outdir, "search_by_board.csv"))
  s_sum <- pair_vs_control(search)
  setorder(s_sum, branch, -d_champ)
  fwrite(s_sum, file.path(cfg$outdir, "search_summary.csv"))
  msg("SEARCH top 8 by champion delta, each branch:")
  print(s_sum[, head(.SD, 8), by = branch][])
  rm(draws_s); gc(FALSE)

  # A deep single pass is its own confirm: when the search budget was already
  # spent at full depth on a chosen arm list there is nothing left to promote.
  if (SW$skip_confirm) {
    cap <- merge(search_cap, search[, list(arm, branch, board, champion_pct, playoff_pct)],
                 by = c("arm", "branch", "board"))
    fwrite(cap, file.path(cfg$outdir, "capital_frontier.csv"))
    fwrite(search, file.path(cfg$outdir, "confirm_by_board.csv"))
    fwrite(s_sum, file.path(cfg$outdir, "builds.csv"))
    msg("skip_confirm: search pass written as the confirmed result")
    msg("wrote:", cfg$outdir)
    return(invisible(list(search = s_sum, confirm = s_sum, capital = cap)))
  }

  # ---- CONFIRM ----
  # union of each branch's top arms, so an arm that only wins under one
  # assumption still gets a real measurement under the other
  top <- unique(c(CONTROL, unlist(lapply(BRANCH_USE, function(bn)
    head(s_sum[branch == bn][order(-d_champ)]$arm, SW$n_top)))))
  arms_c <- arms[top]
  msg(sprintf("CONFIRM: n=%d x %d boards, %d arms x %d branches = %d worlds",
              SW$n_confirm, SW$b_confirm, length(arms_c), length(BRANCH_USE),
              length(arms_c) * SW$b_confirm * length(BRANCH_USE)))
  draws_c <- make_draws(SW$n_confirm, cfg$seed + 1L)
  push_draws(cl, draws_c)
  seeds_c <- seq_len(SW$b_confirm)
  t0 <- Sys.time()
  res_c <- lapply(BRANCH_USE, function(bn) {
    r <- score_all(cl, arms_c, bn, draws_c, seeds_c, keep_drafted = TRUE)
    msg("  branch", bn, "done", sprintf("(%.1f min)", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    r
  })
  confirm <- rbindlist(lapply(res_c, `[[`, "by_board"))
  fwrite(confirm, file.path(cfg$outdir, "confirm_by_board.csv"))

  c_sum <- pair_vs_control(confirm)
  setorder(c_sum, branch, -d_champ)
  fwrite(c_sum, file.path(cfg$outdir, "builds.csv"))
  msg("CONFIRM:")
  print(c_sum[])

  # ---- QB wait curve ----
  qb <- c_sum[kind == "qb_grid"]
  if (nrow(qb)) {
    qb[, k := as.integer(sub("^qb_([0-9]+)_.*$", "\\1", arm))]
    qb[, j := as.integer(sub("^qb_[0-9]+_([0-9]+)$", "\\1", arm))]
    fwrite(qb[order(branch, k, j)], file.path(cfg$outdir, "qb_wait_curve.csv"))
  }

  # ---- capital (search set: unselected) ----
  cap <- merge(search_cap, search[, list(arm, branch, board, champion_pct, playoff_pct)],
               by = c("arm", "branch", "board"))
  fwrite(cap, file.path(cfg$outdir, "capital_frontier.csv"))
  for (bn in BRANCH_USE) {
    sub <- cap[branch == bn]
    if (nrow(sub) > 20L && uniqueN(sub$arm) > 3L) {
      f <- stats::lm(champion_pct ~ QB + RB + TE, data = sub)  # WR share is the baseline
      msg("capital regression |", bn, "| champion_pct on positional share, WR omitted:")
      print(round(summary(f)$coefficients, 4))
    }
  }
  msg("wrote:", cfg$outdir)
  invisible(list(search = s_sum, confirm = c_sum, capital = cap))
}

if ("sweep" %in% cfg$stages) SWEEP <- run_sweep()
