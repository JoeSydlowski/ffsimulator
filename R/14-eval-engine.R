#' Incremental trade-evaluation engine
#'
#' [ffs_trade_eval()] re-summarises the whole league twice per call - once for
#' the unchanged `before` state and once for `after` - and each summary copies
#' the full `optimal_scores` table (list-columns included), re-ranks every
#' season-week for all-play, and re-joins the schedule. On a 2000-season
#' simulation that is ~19 seconds a deal, which caps a confirmation run at a few
#' dozen trades.
#'
#' Almost all of that work is invariant. A trade changes exactly two franchises'
#' weekly scores; the schedule, the season-week grid and the other franchises'
#' scores never move. This engine pre-computes the invariant structure once and
#' represents the league as a `[season-week x franchise]` score matrix, so a deal
#' costs two column overwrites plus a handful of vectorised matrix reductions.
#' The `before` summary is computed once at build time and reused.
#'
#' The engine reproduces [.ffs_summarise_optimal()] exactly, including its
#' rounding quirks: `result` is decided on **unrounded** scores while
#' `points_for` sums scores **rounded to 2dp**, all-play wins use average tie
#' ranks, and the playoff seeding breaks ties by franchise_id order. See
#' `tests/testthat/test-trade-engine.R` for the equivalence suite.
#'
#' @param base_simulation an `ff_simulation` from `return = "all"`
#'
#' @return an `ffs_eval_engine` object, or `NULL` when the simulation is not
#'   eligible (non-`rank` lineup method, best-ball, or a ragged franchise-week
#'   panel) - callers fall back to the legacy path.
#'
#' @keywords internal
.ffs_eval_engine <- function(base_simulation) {
  season <- week <- franchise_id <- opponent_id <- actual_score <- fw_id <- col <-
    ocol <- NULL

  checkmate::assert_class(base_simulation, "ff_simulation")
  params <- base_simulation$simulation_params
  lm <- if ("lineup_method" %in% names(params)) params[["lineup_method"]] else NULL
  if (!identical(as.character(lm), "rank") || isTRUE(params$best_ball)) return(NULL)

  opt <- data.table::as.data.table(base_simulation$optimal_scores)
  sched <- data.table::as.data.table(base_simulation$schedules)
  if (!all(c("season", "week", "franchise_id", "actual_score") %in% names(opt))) return(NULL)

  # franchise column order MUST be ascending franchise_id: ffs_summarise_season
  # groups in first-appearance order over a table sorted by season/week/
  # franchise_id, and the playoff seeding's "first" tie-break rides on it.
  fids <- sort(unique(as.character(opt$franchise_id)), method = "radix")
  nT <- length(fids)

  fw <- unique(opt[, list(season, week)])
  data.table::setorder(fw, season, week)
  fw[, fw_id := seq_len(.N)]
  n_fw <- nrow(fw)
  # a ragged panel would break the matrix representation (and the all-play
  # denominator); bail out to the legacy path rather than approximate it
  if (nrow(opt) != n_fw * nT) return(NULL)

  ix <- merge(opt[, list(season, week, franchise_id = as.character(franchise_id),
                         actual_score)],
              fw, by = c("season", "week"))
  ix[, col := match(franchise_id, fids)]
  S <- matrix(NA_real_, n_fw, nT)
  S[cbind(ix$fw_id, ix$col)] <- ix$actual_score
  if (anyNA(S)) return(NULL)

  sx <- merge(sched[, list(season, week, franchise_id = as.character(franchise_id),
                           opponent_id = as.character(opponent_id))],
              fw, by = c("season", "week"))
  sx[, `:=`(col = match(franchise_id, fids), ocol = match(opponent_id, fids))]
  sx <- sx[!is.na(col) & !is.na(ocol)]
  OPPC <- matrix(NA_integer_, n_fw, nT)
  OPPC[cbind(sx$fw_id, sx$col)] <- sx$ocol
  mask <- !is.na(OPPC)
  if (!any(mask)) return(NULL)

  # linear indices for the "my opponent's score this week" gather. Both the
  # target cells and their sources are fixed by the schedule, so the whole
  # opponent join collapses to one vector subscript per evaluation.
  msk_idx <- which(mask)
  row_of <- ((msk_idx - 1L) %% n_fw) + 1L
  gather_idx <- (OPPC[msk_idx] - 1L) * n_fw + row_of

  n_present <- rowSums(!is.na(S))         # franchises scored in each season-week
  ap_games <- matrix(n_present - 1, n_fw, nT)

  seasons <- fw$season
  season_lv <- sort(unique(seasons))
  season_grp <- match(seasons, season_lv)
  n_season <- length(season_lv)

  zero <- matrix(0, n_fw, nT)
  games <- zero; games[msk_idx] <- 1
  GP_s <- rowsum(games, season_grp, reorder = TRUE)
  APG_s <- rowsum(replace(zero, msk_idx, ap_games[msk_idx]), season_grp, reorder = TRUE)

  summarise <- function(S2) {
    # --- all-play: frank(actual_score) - 1 within each season-week, average ties
    lt <- matrix(0, n_fw, nT)
    eq <- matrix(0, n_fw, nT)
    for (j in seq_len(nT)) {
      sj <- S2[, j]
      lt[, j] <- rowSums(S2 < sj)
      eq[, j] <- rowSums(S2 == sj)
    }
    ap_wins <- lt + (eq - 1) / 2

    # --- head-to-head: decided on UNROUNDED scores (see ffs_summarise_week)
    OS2 <- matrix(NA_real_, n_fw, nT)
    OS2[msk_idx] <- S2[gather_idx]
    wnum <- zero
    wnum[msk_idx] <- as.numeric(S2[msk_idx] > OS2[msk_idx])

    # --- points: summed from scores ROUNDED to 2dp
    Sr <- round(S2, 2)
    pf <- zero
    pf[msk_idx] <- Sr[msk_idx]

    HW_s <- rowsum(wnum, season_grp, reorder = TRUE)
    PF_s <- rowsum(pf, season_grp, reorder = TRUE)
    APW_s <- rowsum(replace(zero, msk_idx, ap_wins[msk_idx]), season_grp, reorder = TRUE)

    hwp_s <- round(HW_s / GP_s, 3)
    awp_s <- round(APW_s / APG_s, 3)

    # --- playoff seeding: wins, then points-for, ties broken by franchise order
    rk <- matrix(0L, n_season, nT)
    for (j in seq_len(nT)) {
      hj <- HW_s[, j]
      pj <- PF_s[, j]
      strictly <- rowSums((HW_s > hj) | (HW_s == hj & PF_s > pj))
      tie_lower <- if (j > 1L) {
        rowSums(HW_s[, seq_len(j - 1L), drop = FALSE] == hj &
                  PF_s[, seq_len(j - 1L), drop = FALSE] == pj)
      } else 0
      rk[, j] <- as.integer(strictly + tie_lower + 1)
    }

    agg <- data.table::data.table(
      franchise_id = fids,
      h2h_wins = colMeans(HW_s),
      h2h_winpct = colMeans(hwp_s),
      allplay_winpct = colMeans(awp_s),
      points_for = colMeans(PF_s),
      playoff_pct = colMeans(rk <= 6L)
    )

    # --- championship bracket over the same seeding
    mu <- numeric(nT)
    sdv <- numeric(nT)
    for (j in seq_len(nT)) {
      v <- Sr[mask[, j], j]
      mu[j] <- mean(v)
      sdv[j] <- stats::sd(v)
    }
    sdv[is.na(sdv) | sdv <= 0] <- 1e-6
    strength <- data.table::data.table(franchise_id = fids, mu = mu, sd = sdv)
    ssdt <- data.table::data.table(
      season = rep(season_lv, times = nT),
      franchise_id = rep(fids, each = n_season),
      lg_rank = as.vector(rk)
    )
    champ <- .ffs_champion_pct_core(strength, ssdt)
    agg <- merge(agg, champ, by = "franchise_id", all.x = TRUE)
    agg[is.na(agg$champion_pct), "champion_pct"] <- 0
    agg[is.na(agg$top_seed_pct), "top_seed_pct"] <- 0
    agg
  }

  before <- summarise(S)
  data.table::setkey(fw, season, week)

  # pre-converted tables so .ffs_counterfactual_rows does not re-copy the
  # (multi-million-row) roster_scores table on every deal
  cache <- list(
    opt = opt,
    rs = data.table::as.data.table(base_simulation$roster_scores),
    lc = .ffs_sim_lineup_constraints(base_simulation)
  )
  data.table::setindexv(cache$rs, "franchise_id")
  data.table::setindexv(cache$rs, "player_id")

  fw_of <- function(dt) fw[list(dt$season, dt$week), fw_id, on = c("season", "week")]

  eval_deal <- function(franchise_a, gives_a, franchise_b, gives_b) {
    ca <- match(as.character(franchise_a), fids)
    cb <- match(as.character(franchise_b), fids)
    if (is.na(ca) || is.na(cb)) stop("franchise not present in the simulation")
    ga <- gives_a[!grepl("^PICK_", gives_a)]
    gb <- gives_b[!grepl("^PICK_", gives_b)]

    ra <- .ffs_counterfactual_rows(base_simulation, franchise_a,
                                   remove_ids = ga, add_ids = gb, cache = cache)
    rb <- .ffs_counterfactual_rows(base_simulation, franchise_b,
                                   remove_ids = gb, add_ids = ga, cache = cache)
    S2 <- S
    S2[fw_of(ra), ca] <- ra$actual_score
    S2[fw_of(rb), cb] <- rb$actual_score

    after <- summarise(S2)
    keep <- c(as.character(franchise_a), as.character(franchise_b))
    b <- before[before$franchise_id %in% keep][order(franchise_id)]
    a <- after[after$franchise_id %in% keep][order(franchise_id)]

    data.frame(
      franchise_id = b$franchise_id,
      h2h_wins_before = b$h2h_wins,
      h2h_wins_after = a$h2h_wins,
      h2h_wins_delta = a$h2h_wins - b$h2h_wins,
      allplay_delta = a$allplay_winpct - b$allplay_winpct,
      points_delta = a$points_for - b$points_for,
      playoff_pct_before = b$playoff_pct,
      playoff_pct_after = a$playoff_pct,
      playoff_pct_delta = a$playoff_pct - b$playoff_pct,
      champion_pct_before = b$champion_pct,
      champion_pct_after = a$champion_pct,
      champion_pct_delta = a$champion_pct - b$champion_pct,
      stringsAsFactors = FALSE
    )
  }

  # one-sided counterfactual: what one franchise's season looks like after
  # gaining or losing players. Used by ffs_player_value(), which a deep target
  # scan calls twice per candidate.
  eval_side <- function(franchise_id, remove_ids = character(0),
                        add_ids = character(0)) {
    # NB: hold the id in a differently-named local. data.table evaluates `i` in
    # the frame of the table first, so a bare `franchise_id` there would resolve
    # to the COLUMN and match every row.
    want <- as.character(franchise_id)
    cf <- match(want, fids)
    if (is.na(cf)) stop("franchise not present in the simulation")
    remove_ids <- remove_ids[!grepl("^PICK_", remove_ids)]
    add_ids <- add_ids[!grepl("^PICK_", add_ids)]
    r <- .ffs_counterfactual_rows(base_simulation, franchise_id,
                                  remove_ids = remove_ids, add_ids = add_ids,
                                  cache = cache)
    S2 <- S
    S2[fw_of(r), cf] <- r$actual_score
    after <- summarise(S2)
    after[after$franchise_id == want]
  }

  structure(
    list(eval = eval_deal, eval_side = eval_side, before = before,
         roster = cache$rs, franchise_ids = fids,
         n_seasons = n_season, n_franchise_weeks = n_fw),
    class = "ffs_eval_engine"
  )
}

#' Baseline franchise summary, from an engine when one is available
#'
#' The engine already holds the unmodified league summary; without one this falls
#' back to re-summarising the whole simulation.
#'
#' @param base_simulation an `ff_simulation`
#' @param fid franchise_id
#' @param engine an optional [ffs_trade_engine()]
#' @keywords internal
.ffs_franchise_summary_cached <- function(base_simulation, fid, engine = NULL) {
  if (!is.null(engine) && inherits(engine, "ffs_eval_engine")) {
    row <- engine$before[engine$before$franchise_id == as.character(fid)]
    if (nrow(row)) return(row)
  }
  .ffs_franchise_summary(base_simulation, fid)
}

#' Build a trade-evaluation engine for a simulation
#'
#' Thin wrapper over [.ffs_eval_engine()] that honours the `FFS_EVAL_ENGINE=0`
#' escape hatch (forcing the legacy per-call path) and returns `NULL` instead of
#' erroring when a simulation is ineligible.
#'
#' @param base_simulation an `ff_simulation` from `return = "all"`
#'
#' @return an `ffs_eval_engine`, or `NULL`
#'
#' @examples
#' \donttest{
#' # engine <- ffs_trade_engine(simulation)
#' # ffs_trade_eval(simulation, "A", "1234", "B", "5678", engine = engine)
#' }
#'
#' @export
ffs_trade_engine <- function(base_simulation) {
  if (identical(Sys.getenv("FFS_EVAL_ENGINE"), "0")) return(NULL)
  tryCatch(.ffs_eval_engine(base_simulation), error = function(e) {
    warning("trade engine unavailable, falling back to the legacy path: ",
            conditionMessage(e), call. = FALSE)
    NULL
  })
}

#' Strip a simulation down to what trade evaluation reads
#'
#' A 2000-season `ff_simulation` is ~3 GB, and the evaluation path touches only
#' five of its components. `projected_scores` alone is ~950 MB and is never read;
#' `roster_scores` is 8.5M rows of which 9 columns matter; `optimal_scores`
#' carries two list-columns (`optimal_player_id`, `optimal_player_score`) that
#' nothing on this path uses. Dropping the rest takes the object to ~0.6 GB,
#' which is what makes it affordable for every parallel worker to hold a copy.
#'
#' Only safe for the trade-evaluation path. In particular [ffs_pick_values()]
#' needs `rosters`, which this drops.
#'
#' @param base_simulation an `ff_simulation` from `return = "all"`
#'
#' @return a trimmed `ff_simulation`
#'
#' @keywords internal
.ffs_slim_simulation <- function(base_simulation) {
  keep_rs <- c("league_id", "franchise_id", "franchise_name", "player_id", "pos",
               "season", "week", "projected_score", "avg_week")
  keep_opt <- c("league_id", "franchise_id", "franchise_name", "season", "week",
                "actual_score", "starter_player_id")
  rs <- data.table::as.data.table(base_simulation$roster_scores)
  opt <- data.table::as.data.table(base_simulation$optimal_scores)
  out <- list(
    roster_scores = rs[, intersect(keep_rs, names(rs)), with = FALSE],
    optimal_scores = opt[, intersect(keep_opt, names(opt)), with = FALSE],
    schedules = base_simulation$schedules,
    lineup_constraints = base_simulation$lineup_constraints,
    simulation_params = base_simulation$simulation_params
  )
  class(out) <- class(base_simulation)
  out
}

#' Evaluate many trades against one simulation
#'
#' Batch companion to [ffs_trade_eval()]. Builds (or reuses) an
#' [ffs_trade_engine()] so the invariant league summary is computed once for the
#' whole batch, then evaluates each deal.
#'
#' Per-deal cost is dominated by re-solving both franchises' weekly lineups (one
#' small LP per franchise-week - ~56,000 of them a deal on a 2000-season
#' simulation), which is irreducible if results are to stay bit-identical. The
#' deals are independent, though, so `workers > 1` spreads them over a PSOCK
#' cluster. Each worker loads a slimmed copy of the simulation from a temporary
#' file rather than having it serialised down the socket.
#'
#' @param base_simulation an `ff_simulation` from `return = "all"`
#' @param deals a data.frame with columns `franchise_a`, `gives_a`, `franchise_b`
#'   and `gives_b`; the two `gives_` columns are list-columns of player_ids
#' @param engine an existing engine from [ffs_trade_engine()], or `NULL` to build one
#' @param progress optional function called as `progress(i, n)` after each deal
#'   (sequential runs only)
#' @param workers number of parallel workers. `1` (default) runs in-process;
#'   `NULL` picks `parallel::detectCores(logical = FALSE) - 2`. Parallel runs need
#'   roughly 0.8 GB per worker on a 2000-season simulation.
#'
#' @return a data.frame with one row per deal per franchise (two rows a deal),
#'   carrying a `deal_id` column plus the columns [ffs_trade_eval()] returns
#'
#' @export
ffs_trade_eval_many <- function(base_simulation, deals, engine = NULL,
                                progress = NULL, workers = 1L) {
  deals <- data.table::as.data.table(deals)
  checkmate::assert_names(names(deals),
    must.include = c("franchise_a", "gives_a", "franchise_b", "gives_b"))
  n <- nrow(deals)
  if (n == 0L) return(data.frame())

  if (is.null(workers)) workers <- max(1L, parallel::detectCores(logical = FALSE) - 2L)
  workers <- min(as.integer(workers), n)

  if (workers > 1L) {
    par <- tryCatch(.ffs_eval_parallel(base_simulation, deals, workers),
                    error = function(e) e)
    if (!inherits(par, "error")) return(par)
    # a dead worker (usually memory) must not lose the batch - finish in-process
    warning("parallel evaluation failed (", conditionMessage(par),
            "); falling back to sequential", call. = FALSE)
    workers <- 1L
  }

  if (is.null(engine)) engine <- ffs_trade_engine(base_simulation)
  out <- vector("list", n)
  for (i in seq_len(n)) {
    te <- ffs_trade_eval(base_simulation,
                         deals$franchise_a[[i]], deals$gives_a[[i]],
                         deals$franchise_b[[i]], deals$gives_b[[i]],
                         engine = engine)
    te$deal_id <- i
    out[[i]] <- te
    if (!is.null(progress)) progress(i, n)
  }
  data.table::setDF(data.table::rbindlist(out))
}

#' Spread a batch of trade evaluations over a PSOCK cluster
#'
#' @inheritParams ffs_trade_eval_many
#' @return the same data.frame [ffs_trade_eval_many()] returns
#' @keywords internal
.ffs_eval_parallel <- function(base_simulation, deals, workers) {
  n <- nrow(deals)
  simfile <- tempfile(fileext = ".rds")
  on.exit(unlink(simfile), add = TRUE)
  # compress = FALSE: the workers read this back immediately, so decompression
  # time costs far more than the disk space saves
  saveRDS(.ffs_slim_simulation(base_simulation), simfile, compress = FALSE)

  dev_pkg <- requireNamespace("pkgload", quietly = TRUE) &&
    isTRUE(tryCatch(pkgload::is_dev_package("ffsimulator"), error = function(e) FALSE))
  pkg_path <- if (dev_pkg) tryCatch(pkgload::pkg_path(), error = function(e) NULL) else NULL

  cl <- parallel::makePSOCKcluster(workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterExport(cl, c("simfile", "dev_pkg", "pkg_path"), envir = environment())
  parallel::clusterEvalQ(cl, {
    if (dev_pkg && !is.null(pkg_path)) pkgload::load_all(pkg_path, quiet = TRUE) else
      library(ffsimulator)
    .sim <- readRDS(simfile)
    .engine <- ffsimulator::ffs_trade_engine(.sim)
    NULL
  })

  # chunk so each worker's engine build (~1s) is amortised over many deals
  chunks <- split(seq_len(n), cut(seq_len(n), workers * 4L, labels = FALSE))
  chunks <- chunks[lengths(chunks) > 0L]
  payload <- lapply(chunks, function(ii) list(
    idx = ii,
    fa = as.character(deals$franchise_a[ii]),
    ga = deals$gives_a[ii],
    fb = as.character(deals$franchise_b[ii]),
    gb = deals$gives_b[ii]))

  res <- parallel::parLapplyLB(cl, payload, function(p) {
    out <- vector("list", length(p$idx))
    for (k in seq_along(p$idx)) {
      te <- ffsimulator::ffs_trade_eval(.sim, p$fa[[k]], p$ga[[k]], p$fb[[k]], p$gb[[k]],
                                        engine = .engine)
      te$deal_id <- p$idx[[k]]
      out[[k]] <- te
    }
    do.call(rbind, out)
  })

  out <- data.table::rbindlist(res)
  data.table::setorder(out, deal_id)
  data.table::setDF(out)
}
