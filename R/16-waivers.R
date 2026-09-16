#' Free agents in a league
#'
#' (EXPERIMENTAL) Every ranked player that no franchise in the league is
#' rostering. `latest_rankings` supplies the universe, so a player with no
#' FantasyPros ranking cannot appear here (and cannot be simulated either -
#' see [ffs_waiver_candidates()] for the usage-signal watchlist that catches
#' those).
#'
#' @param rosters a rosters dataframe (`ffs_rosters()`), or `base_simulation$rosters`
#' @param latest_rankings a rankings dataframe (`ffs_latest_rankings()`)
#' @param pos_filter a character vector of positions to keep
#'
#' @return the unrostered rows of `latest_rankings`, with a positional `ecr_rank`
#' @export
ffs_free_agents <- function(rosters,
                            latest_rankings,
                            pos_filter = c("QB", "RB", "WR", "TE")) {
  assert_df(rosters, "fantasypros_id")
  assert_df(latest_rankings, c("fantasypros_id", "pos", "ecr"))
  pos <- ecr <- ecr_rank <- fantasypros_id <- NULL

  rostered <- unique(stats::na.omit(as.character(rosters$fantasypros_id)))
  lr <- data.table::as.data.table(latest_rankings)[
    pos %in% pos_filter & !is.na(ecr) & !as.character(fantasypros_id) %in% rostered
  ]
  lr <- unique(lr, by = "fantasypros_id")
  data.table::setorder(lr, pos, ecr)
  lr[, ecr_rank := seq_len(.N), by = "pos"]
  lr[]
}

#' Recent usage signals from nflreadr
#'
#' (EXPERIMENTAL) Snap share (`nflreadr::load_snap_counts()`) and expected
#' fantasy points (`nflreadr::load_ff_opportunity()`) for the most recent
#' `lookback` weeks, against the weeks before them. Rankings move slowly; a
#' back-up who just took over a backfield shows up here a week or two before his
#' ECR catches up, which is exactly the player a waiver claim is for.
#'
#' Both sources are joined to `fantasypros_id` via `nflreadr::load_ff_playerids()`
#' (snaps on `pfr_id`, opportunity on `gsis_id`). Either source failing - which
#' happens in the first weeks of a season, before the data lands - degrades to
#' `NA` columns rather than an error.
#'
#' @param season the season to read, defaults to the current one
#' @param lookback how many recent weeks form the "recent" window (default 3)
#' @param through_week ignore weeks after this one (default: all available)
#' @param pos_filter positions to keep
#'
#' @return one row per `fantasypros_id` with recent/prior snap share, expected
#'   points per game, their trends, and points over expected
#' @export
ffs_usage_signals <- function(season = nflreadr::most_recent_season(),
                              lookback = 3L,
                              through_week = NULL,
                              pos_filter = c("QB", "RB", "WR", "TE")) {
  checkmate::assert_int(lookback, lower = 1)
  fantasypros_id <- gsis_id <- pfr_id <- position <- game_type <- week <- NULL
  offense_pct <- window <- player_id <- pfr_player_id <- NULL
  total_fantasy_points_exp <- total_fantasy_points <- NULL
  snap_pct_recent <- snap_pct_prior <- exp_pts_recent <- exp_pts_prior <- NULL
  fpts_recent <- games_recent <- snap_trend <- exp_pts_trend <- NULL
  fpoe_recent <- player <- pos <- NULL

  ids <- data.table::as.data.table(nflreadr::load_ff_playerids())
  ids <- ids[!is.na(fantasypros_id),
             list(fantasypros_id = as.character(fantasypros_id),
                  gsis_id = as.character(gsis_id),
                  pfr_id = as.character(pfr_id),
                  player = get("name"),
                  pos = position, team = get("team"))]
  ids <- unique(ids, by = "fantasypros_id")

  # window labeller shared by both sources: the last `lookback` played weeks are
  # "recent", everything before them is "prior"
  windowise <- function(dt) {
    if (!is.null(through_week)) dt <- dt[week <= through_week]
    if (!nrow(dt)) return(dt)
    cut <- max(dt$week, na.rm = TRUE) - lookback + 1L
    dt[, window := data.table::fifelse(week >= cut, "recent", "prior")][]
  }

  snaps <- tryCatch(
    data.table::as.data.table(nflreadr::load_snap_counts(season)),
    error = function(e) {
      cli::cli_alert_warning("snap counts unavailable for {season}: {conditionMessage(e)}")
      NULL
    })
  snap_agg <- NULL
  if (!is.null(snaps) && nrow(snaps)) {
    snaps <- windowise(snaps[game_type == "REG" & position %in% pos_filter])
    if (nrow(snaps)) {
      snap_agg <- snaps[, list(snap_pct = mean(offense_pct, na.rm = TRUE), games = .N),
                        by = c("pfr_player_id", "window")]
      snap_agg <- data.table::dcast(snap_agg, pfr_player_id ~ window,
                                    value.var = c("snap_pct", "games"), fill = NA)
    }
  }

  opp <- tryCatch(
    data.table::as.data.table(
      nflreadr::load_ff_opportunity(season, stat_type = "weekly")),
    error = function(e) {
      cli::cli_alert_warning("ff_opportunity unavailable for {season}: {conditionMessage(e)}")
      NULL
    })
  opp_agg <- NULL
  if (!is.null(opp) && nrow(opp)) {
    opp <- windowise(opp[position %in% pos_filter])
    if (nrow(opp)) {
      opp_agg <- opp[, list(exp_pts = mean(total_fantasy_points_exp, na.rm = TRUE),
                            fpts = mean(total_fantasy_points, na.rm = TRUE)),
                     by = c("player_id", "window")]
      opp_agg <- data.table::dcast(opp_agg, player_id ~ window,
                                   value.var = c("exp_pts", "fpts"), fill = NA)
    }
  }

  out <- data.table::copy(ids)
  if (!is.null(snap_agg)) {
    out <- merge(out, snap_agg, by.x = "pfr_id", by.y = "pfr_player_id", all.x = TRUE)
  }
  if (!is.null(opp_agg)) {
    out <- merge(out, opp_agg, by.x = "gsis_id", by.y = "player_id", all.x = TRUE)
  }
  # a whole window can be missing this early in a season - keep the shape stable
  for (nm in c("snap_pct_recent", "snap_pct_prior", "games_recent", "games_prior",
               "exp_pts_recent", "exp_pts_prior", "fpts_recent", "fpts_prior")) {
    if (!nm %in% names(out)) data.table::set(out, j = nm, value = NA_real_)
  }
  out[, `:=`(
    snap_trend = snap_pct_recent - snap_pct_prior,
    exp_pts_trend = exp_pts_recent - exp_pts_prior,
    fpoe_recent = fpts_recent - exp_pts_recent
  )]
  out <- out[!is.na(fantasypros_id),
             list(fantasypros_id, player, pos, team, games_recent,
                  snap_pct_recent, snap_pct_prior, snap_trend,
                  exp_pts_recent, exp_pts_prior, exp_pts_trend,
                  fpts_recent, fpoe_recent)]
  out[]
}

#' Assemble the waiver-wire candidate board
#'
#' (EXPERIMENTAL) Free agents in the league, annotated with everything available
#' about them: rest-of-season ECR from the simulation's own rankings, roster
#' percentage and rank movement from FantasyPros, market value from FantasyCalc,
#' and recent usage from [ffs_usage_signals()].
#'
#' The shortlist is a union of two screens - the best `max_per_pos` free agents
#' by ECR at each position, plus the biggest expected-points earners the
#' rankings have not repriced yet - so a breakout still gets priced.
#'
#' @param base_simulation an `ff_simulation` from `ff_simulate(..., return = "all")`
#' @param usage a dataframe from [ffs_usage_signals()], or `NULL` to skip
#' @param values a dataframe from [fc_dynasty_values()], or `NULL` to skip
#' @param pos_filter positions to consider (default: the simulation's own, minus K)
#' @param max_per_pos how many free agents per position to shortlist by ECR
#' @param max_risers how many usage risers per position to add on top
#' @param market attach FantasyPros roster% and rank movement (one extra download)
#'
#' @return a dataframe of free agents, with a `keep` flag marking the shortlist
#' @export
ffs_waiver_candidates <- function(base_simulation,
                                  usage = NULL,
                                  values = NULL,
                                  pos_filter = NULL,
                                  max_per_pos = 20L,
                                  max_risers = 10L,
                                  market = TRUE) {
  checkmate::assert_class(base_simulation, "ff_simulation")
  if (is.null(base_simulation$rosters) || is.null(base_simulation$latest_rankings)) {
    cli::cli_abort("{.arg base_simulation} must come from {.code ff_simulate(return = 'all')}")
  }
  pos <- ecr <- ecr_rank <- fantasypros_id <- exp_pts_recent <- NULL
  riser_rank <- id <- ecr_type <- page_type <- keep <- NULL

  if (is.null(pos_filter)) {
    pos_filter <- setdiff(base_simulation$simulation_params$pos_filter[[1]], "K")
  }

  fa <- ffs_free_agents(base_simulation$rosters, base_simulation$latest_rankings,
                        pos_filter = pos_filter)
  fa[, fantasypros_id := as.character(fantasypros_id)]

  if (isTRUE(market)) {
    mkt <- tryCatch({
      m <- data.table::as.data.table(nflreadr::load_ff_rankings())
      m <- m[ecr_type == "rp" & mapply(grepl, x = page_type,
                                       pattern = paste0(tolower(m$pos), "$"))]
      unique(m[, list(fantasypros_id = as.character(id),
                      roster_pct = get("player_owned_avg"),
                      rank_delta = get("rank_delta"))], by = "fantasypros_id")
    }, error = function(e) {
      cli::cli_alert_warning("FantasyPros market data unavailable: {conditionMessage(e)}")
      NULL
    })
    if (!is.null(mkt)) fa <- merge(fa, mkt, by = "fantasypros_id", all.x = TRUE)
  }

  if (!is.null(usage)) {
    u <- data.table::as.data.table(usage)
    u <- u[, setdiff(names(u), c("player", "pos", "team")), with = FALSE]
    u[, fantasypros_id := as.character(fantasypros_id)]
    fa <- merge(fa, unique(u, by = "fantasypros_id"), by = "fantasypros_id", all.x = TRUE)
  }

  if (!is.null(values)) {
    v <- data.table::as.data.table(values)
    v <- v[!is.na(fantasypros_id)][
      , list(fantasypros_id = as.character(fantasypros_id),
             fc_value = get("value"),
             fc_redraft_value = get("redraft_value"),
             fc_overall_rank = get("overall_rank"),
             fc_trend_30day = get("trend_30day"))]
    fa <- merge(fa, unique(v, by = "fantasypros_id"), by = "fantasypros_id", all.x = TRUE)
  }

  data.table::setorder(fa, pos, ecr)
  fa[, keep := ecr_rank <= max_per_pos]
  if ("exp_pts_recent" %in% names(fa) && any(!is.na(fa$exp_pts_recent))) {
    fa[!is.na(exp_pts_recent),
       riser_rank := data.table::frank(-exp_pts_recent, ties.method = "first"), by = "pos"]
    fa[!is.na(riser_rank) & riser_rank <= max_risers, keep := TRUE]
    fa[, riser_rank := NULL]
  }
  data.table::setorder(fa, pos, ecr)
  fa[]
}

#' Attach a free-agent pool to a simulation
#'
#' (EXPERIMENTAL) Generates simulated weekly scores for a set of free agents on
#' the *same* season/week grid as an existing simulation and appends them to its
#' `roster_scores` under a phantom franchise. Nothing else moves:
#' `optimal_scores`, the schedule and every real franchise's lineups are
#' untouched, so the league baseline stays exactly as it was and the
#' counterfactual machinery ([ffs_player_value()], [ffs_trade_engine()]) can
#' price a free agent onto a roster the same way it prices a trade.
#'
#' This is deliberately *not* [ffs_add_replacement_level()], which hands the best
#' free agents to **every** franchise at once and so makes a waiver claim worth
#' approximately nothing by construction. Run the base simulation with
#' `replacement_level = FALSE` if you want to price claims.
#'
#' @param base_simulation an `ff_simulation` from `ff_simulate(..., return = "all")`
#' @param candidates a dataframe of free agents ([ffs_waiver_candidates()]) with
#'   at least `fantasypros_id`, `player` and `pos`; a `keep` column, if present,
#'   filters it to the shortlist
#' @param fa_franchise_id the phantom franchise id to park the pool under
#'
#' @return the simulation with free-agent rows appended to `roster_scores` and a
#'   `waiver_pool` element holding the pool's roster rows
#' @export
ffs_attach_free_agents <- function(base_simulation, candidates,
                                   fa_franchise_id = "FA_POOL") {
  checkmate::assert_class(base_simulation, "ff_simulation")
  assert_df(candidates, c("fantasypros_id", "player", "pos"))
  fantasypros_id <- keep <- NULL

  cand <- data.table::as.data.table(candidates)
  if ("keep" %in% names(cand)) cand <- cand[keep %in% TRUE]
  cand <- unique(cand[!is.na(fantasypros_id)], by = "fantasypros_id")
  if (!nrow(cand)) cli::cli_abort("no free-agent candidates to attach")

  params <- base_simulation$simulation_params
  rs <- data.table::as.data.table(base_simulation$roster_scores)
  weeks <- sort(unique(rs$week))
  n_seasons <- max(rs$season)

  fa_rosters <- data.table::data.table(
    league_id = base_simulation$rosters$league_id[[1]],
    franchise_id = fa_franchise_id,
    franchise_name = "Free Agents",
    player_id = paste0("FA_", cand$fantasypros_id),
    player_name = cand$player,
    pos = cand$pos,
    team = if ("team" %in% names(cand)) as.character(cand$team) else NA_character_,
    age = NA_real_,
    fantasypros_id = as.character(cand$fantasypros_id)
  )

  adp_outcomes <- ffs_adp_outcomes(
    scoring_history = base_simulation$scoring_history,
    gp_model = params$gp_model %||% "simple",
    pos_filter = params$pos_filter[[1]],
    version = params$version %||% "v3"
  )

  projected <- ffs_generate_projections(
    adp_outcomes = adp_outcomes,
    latest_rankings = base_simulation$latest_rankings,
    n_seasons = n_seasons,
    weeks = weeks,
    rosters = fa_rosters,
    version = params$version %||% "v3"
  )

  fa_scores <- ffs_score_rosters(projected_scores = projected,
                                 rosters = data.table::copy(fa_rosters))
  missing <- setdiff(unique(fa_rosters$player_id), unique(fa_scores$player_id))
  if (length(missing)) {
    cli::cli_alert_info(
      "{length(missing)} candidate{?s} had no projectable ranking and were dropped")
  }

  out <- base_simulation
  out$roster_scores <- data.table::rbindlist(list(rs, fa_scores),
                                             use.names = TRUE, fill = TRUE)
  out$waiver_pool <- as.data.frame(
    fa_rosters[fa_rosters$player_id %in% unique(fa_scores$player_id)])
  out
}

#' Best add/drop pairings for one franchise
#'
#' (EXPERIMENTAL) Prices every candidate add and every droppable player on a
#' roster against the simulated season, then ranks the resulting swaps.
#'
#' Evaluating every add x drop pair exactly would cost `n_add * n_drop`
#' counterfactual re-optimisations. Instead each add is valued once (what he is
#' worth joining this roster) and each drop once (what losing him costs), which
#' is `n_add + n_drop` evaluations; their sum screens the pairs, and only the
#' best `top_pairs` are then evaluated **exactly** as a simultaneous swap. The
#' `exact` column marks which rows got the real evaluation - the approximation
#' is only wrong when the two players compete for the same lineup slot, which
#' the exact pass then corrects.
#'
#' @param base_simulation a simulation with a pool attached by [ffs_attach_free_agents()]
#' @param franchise_id the franchise making the move
#' @param add_ids player_ids to consider adding (default: the attached pool)
#' @param drop_ids player_ids to consider dropping (default: the franchise's
#'   whole roster at the simulated positions, minus replacement-level fillers)
#' @param screen_adds how many candidate adds survive the cheap points screen
#'   and get simulated (default 25); `Inf` simulates the whole pool
#' @param top_pairs how many screened pairs to evaluate exactly (default 40)
#' @param position_limits optional named vector of per-position roster maxima,
#'   e.g. `c(QB = 4)`; pairs that would breach a limit are dropped
#' @param workers number of parallel workers for the counterfactual batch. `1`
#'   (default) runs in-process; `NULL` picks `detectCores(logical = FALSE) - 2`.
#'   Each worker holds a slimmed copy of the simulation (~0.8 GB at 2000 seasons).
#' @param engine an optional pre-built [ffs_trade_engine()]
#'
#' @return a list of three dataframes: `pairs` (ranked add/drop swaps), `adds`
#'   (value of each add on its own) and `drops` (cost of each drop on its own)
#' @export
ffs_waiver_moves <- function(base_simulation,
                             franchise_id,
                             add_ids = NULL,
                             drop_ids = NULL,
                             screen_adds = 25L,
                             top_pairs = 40L,
                             position_limits = NULL,
                             workers = 1L,
                             engine = NULL) {
  checkmate::assert_class(base_simulation, "ff_simulation")
  player_id <- player_name <- pos <- owner <- add_id <- drop_id <- NULL
  add_player <- add_pos <- drop_player <- drop_pos <- NULL
  approx_wins <- approx_champ <- exact <- h2h_wins_delta <- NULL
  champion_pct_delta <- NULL

  fid <- as.character(franchise_id)
  if (is.null(engine)) engine <- ffs_trade_engine(base_simulation)
  if (is.null(engine)) {
    cli::cli_abort(c(
      "waiver pricing needs the fast counterfactual engine",
      i = "run the base simulation with {.code lineup_method = 'rank'} and {.code best_ball = FALSE}"
    ))
  }
  rs <- engine$roster
  if (!fid %in% engine$franchise_ids) {
    cli::cli_abort("{.val {fid}} is not a franchise in this simulation")
  }

  # a slimmed simulation (see .ffs_slim_simulation) carries no player_name
  players <- unique(rs[, list(player_id, pos,
                              player_name = if ("player_name" %in% names(rs))
                                get("player_name") else NA_character_,
                              owner = as.character(get("franchise_id")))],
                    by = "player_id")

  if (is.null(add_ids)) {
    if (is.null(base_simulation$waiver_pool)) {
      cli::cli_abort("no free-agent pool attached - see {.fn ffs_attach_free_agents}")
    }
    add_ids <- unique(as.character(base_simulation$waiver_pool$player_id))
  }
  add_ids <- intersect(add_ids, players$player_id)
  # replacement-level fillers ("WR_3", "FILL_K_1") are simulation scaffolding,
  # not players anyone can drop
  own_ids <- players[owner == fid & !grepl("^(QB|RB|WR|TE|K)_\\d+$|^FILL_", player_id)]$player_id
  if (is.null(drop_ids)) drop_ids <- own_ids
  drop_ids <- intersect(drop_ids, own_ids)
  if (!length(add_ids) || !length(drop_ids)) {
    cli::cli_abort("need at least one add candidate and one drop candidate")
  }

  # cheap screen before any re-optimisation: a candidate's mean weekly points
  # against what this roster already starts at his position. Simulating every
  # free agent costs one counterfactual apiece, and most of the pool is nowhere
  # near the lineup - see ffs_trade_targets(), which screens the same way.
  if (is.finite(screen_adds) && length(add_ids) > screen_adds) {
    mps <- rs[player_id %in% c(add_ids, own_ids),
              list(mps = mean(get("projected_score"), na.rm = TRUE),
                   pos = pos[[1]], owner = as.character(get("franchise_id"))[[1]]),
              by = "player_id"]
    lc <- data.table::as.data.table(base_simulation$lineup_constraints)
    baseline <- mps[owner == fid, list(baseline = {
      n_start <- lc[lc$pos == .BY$pos]$max
      if (!length(n_start)) n_start <- 1
      sorted <- sort(get("mps"), decreasing = TRUE)
      sorted[min(length(sorted), max(1, n_start))]
    }), by = "pos"]
    scr <- merge(mps[player_id %in% add_ids], baseline, by = "pos", all.x = TRUE)
    scr[is.na(get("baseline")), "baseline" := 0]
    scr[, "proxy" := get("mps") - get("baseline")]
    data.table::setorderv(scr, "proxy", -1L)
    add_ids <- scr$player_id[seq_len(min(as.integer(screen_adds), nrow(scr)))]
  }

  before <- data.table::as.data.table(engine$before)[
    as.character(get("franchise_id")) == fid]
  delta <- .ffs_waiver_delta(before)

  runner <- .ffs_waiver_runner(base_simulation, fid, workers = workers, engine = engine)
  on.exit(runner$stop(), add = TRUE)

  cli::cli_alert_info("valuing {length(add_ids)} add{?s} and {length(drop_ids)} drop{?s}")
  singles <- data.table::rbindlist(list(
    data.table::data.table(add_id = add_ids, drop_id = NA_character_),
    data.table::data.table(add_id = NA_character_, drop_id = drop_ids)))
  t0 <- Sys.time()
  single_out <- delta(runner$run(singles))
  cli::cli_alert_success("valued {nrow(singles)} move{?s} in {format(round(Sys.time() - t0, 1))}")

  adds <- cbind(data.table::data.table(add_id = add_ids),
                single_out[seq_along(add_ids)])
  adds <- merge(adds, players[, list(add_id = player_id, add_player = player_name,
                                     add_pos = pos)], by = "add_id")
  data.table::setorder(adds, -champion_pct_delta)

  drops <- cbind(data.table::data.table(drop_id = drop_ids),
                 single_out[length(add_ids) + seq_along(drop_ids)])
  drops <- merge(drops, players[, list(drop_id = player_id, drop_player = player_name,
                                       drop_pos = pos)], by = "drop_id")
  data.table::setorder(drops, -champion_pct_delta)

  # screen: an add's value plus a drop's (negative) value, ignoring the
  # interaction between the two - corrected by the exact pass below
  grid <- data.table::CJ(add_id = adds$add_id, drop_id = drops$drop_id, sorted = FALSE)
  grid <- merge(grid, adds[, list(add_id, add_player, add_pos,
                                  add_wins = h2h_wins_delta,
                                  add_champ = champion_pct_delta)], by = "add_id")
  grid <- merge(grid, drops[, list(drop_id, drop_player, drop_pos,
                                   drop_wins = h2h_wins_delta,
                                   drop_champ = champion_pct_delta)], by = "drop_id")
  grid[, `:=`(approx_wins = get("add_wins") + get("drop_wins"),
              approx_champ = get("add_champ") + get("drop_champ"))]

  if (!is.null(position_limits)) {
    have <- players[player_id %in% own_ids][, list(n = .N), by = "pos"]
    lim <- data.table::data.table(pos = names(position_limits),
                                  limit = as.integer(position_limits))
    have <- merge(have, lim, by = "pos")
    breaches <- function(ap, dp) {
      row <- have[have$pos == ap]
      if (!nrow(row)) return(FALSE)
      (row$n + 1L - as.integer(identical(ap, dp))) > row$limit
    }
    grid <- grid[!mapply(breaches, grid$add_pos, grid$drop_pos)]
    if (!nrow(grid)) cli::cli_abort("every candidate pairing breaches a position limit")
  }

  data.table::setorder(grid, -approx_wins)
  n_exact <- min(as.integer(top_pairs), nrow(grid))
  cli::cli_alert_info("exactly evaluating the top {n_exact} of {nrow(grid)} pairing{?s}")
  t1 <- Sys.time()
  exact_rows <- delta(runner$run(grid[seq_len(n_exact), list(add_id, drop_id)]))
  cli::cli_alert_success("evaluated {n_exact} pairing{?s} in {format(round(Sys.time() - t1, 1))}")
  pairs <- cbind(grid[seq_len(n_exact)], exact_rows)
  pairs[, exact := TRUE]
  if (n_exact < nrow(grid)) {
    rest <- data.table::copy(grid[seq.int(n_exact + 1L, nrow(grid))])
    rest[, `:=`(h2h_wins_delta = approx_wins,
                allplay_delta = NA_real_,
                points_delta = NA_real_,
                playoff_pct_delta = NA_real_,
                champion_pct_delta = approx_champ,
                exact = FALSE)]
    pairs <- data.table::rbindlist(list(pairs, rest), use.names = TRUE, fill = TRUE)
  }
  data.table::setorder(pairs, -exact, -champion_pct_delta, -h2h_wins_delta)

  list(pairs = as.data.frame(pairs),
       adds = as.data.frame(adds),
       drops = as.data.frame(drops))
}

#' Deltas of a counterfactual franchise summary against its baseline
#'
#' Vectorised over the rows of `after`, so a whole batch converts at once.
#'
#' @param before the franchise's one-row baseline summary
#' @return a function of an `after` summary returning the delta columns
#' @keywords internal
.ffs_waiver_delta <- function(before) {
  before <- data.table::as.data.table(before)
  nz <- function(x) {
    if (is.null(x) || !length(x)) return(0)
    data.table::fifelse(is.na(x), 0, as.numeric(x))
  }
  function(after) {
    after <- data.table::as.data.table(after)
    data.table::data.table(
      h2h_wins_delta = after$h2h_wins - before$h2h_wins,
      allplay_delta = after$allplay_winpct - before$allplay_winpct,
      points_delta = after$points_for - before$points_for,
      playoff_pct_delta = after$playoff_pct - before$playoff_pct,
      champion_pct_delta = nz(after$champion_pct) - nz(before$champion_pct)
    )
  }
}

#' Re-price named add/drop pairs on another simulation
#'
#' (EXPERIMENTAL) Evaluates a given set of swaps - and nothing else - so the
#' expensive, high-`n` simulation is spent only on the shortlist. The intended
#' pairing is [ffs_waiver_moves()] on a small, fast simulation to *rank* moves,
#' then this on the full standings simulation to put trustworthy numbers on the
#' handful you would actually make (the same search-then-confirm split
#' `dev/suite/trade_intel.R` uses for trades).
#'
#' Both simulations must carry the same free-agent pool, so attach the same
#' board to each with [ffs_attach_free_agents()] - the pool's player_ids are
#' derived from `fantasypros_id` and therefore match across simulations.
#'
#' @param base_simulation the simulation to evaluate on, with a pool attached
#' @param franchise_id the franchise making the move
#' @param pairs a dataframe with `add_id` and `drop_id` columns; `NA` in either
#'   column means "add only" or "drop only"
#' @param workers number of parallel workers (see [ffs_waiver_moves()])
#' @param engine an optional pre-built [ffs_trade_engine()]
#'
#' @return `pairs` with the delta columns appended
#' @export
ffs_waiver_confirm <- function(base_simulation, franchise_id, pairs,
                               workers = 1L, engine = NULL) {
  checkmate::assert_class(base_simulation, "ff_simulation")
  assert_df(pairs, c("add_id", "drop_id"))
  fid <- as.character(franchise_id)

  runner <- .ffs_waiver_runner(base_simulation, fid, workers = workers, engine = engine)
  on.exit(runner$stop(), add = TRUE)
  before <- data.table::as.data.table(runner$engine$before)[
    as.character(get("franchise_id")) == fid]
  if (!nrow(before)) cli::cli_abort("{.val {fid}} is not a franchise in this simulation")

  jobs <- data.table::as.data.table(pairs)[, list(add_id = as.character(get("add_id")),
                                                  drop_id = as.character(get("drop_id")))]
  known <- unique(runner$engine$roster$player_id)
  unknown <- setdiff(stats::na.omit(c(jobs$add_id, jobs$drop_id)), known)
  if (length(unknown)) {
    cli::cli_abort(c("{length(unknown)} player{?s} {?is/are} not in this simulation",
                     i = "attach the same free-agent board with {.fn ffs_attach_free_agents}"))
  }

  cli::cli_alert_info("confirming {nrow(jobs)} pairing{?s}")
  t0 <- Sys.time()
  out <- .ffs_waiver_delta(before)(runner$run(jobs))
  cli::cli_alert_success("confirmed in {format(round(Sys.time() - t0, 1))}")
  as.data.frame(cbind(data.table::as.data.table(pairs), out))
}

#' Run a batch of one-sided counterfactuals, optionally in parallel
#'
#' Each job is one `engine$eval_side()` call: this franchise's simulated season
#' after adding `add_id`, dropping `drop_id`, or both. The jobs are independent
#' and each re-solves the affected franchise-weeks, so a batch is the obvious
#' place to spend cores. Workers each load a slimmed copy of the simulation from
#' a temporary file (as [ffs_trade_eval_many()] does) and build their own engine,
#' which is why the cluster is created once and reused across batches.
#'
#' @param base_simulation an `ff_simulation` from `return = "all"`
#' @param fid the franchise id, as a character scalar
#' @param workers number of workers; 1 runs in-process, `NULL` auto-picks
#' @param engine an optional pre-built engine for the in-process path
#'
#' @return a list with `run(jobs)`, `stop()` and `engine`
#' @keywords internal
.ffs_waiver_runner <- function(base_simulation, fid, workers = 1L, engine = NULL) {
  if (is.null(engine)) engine <- ffs_trade_engine(base_simulation)
  if (is.null(engine)) cli::cli_abort("the fast counterfactual engine is unavailable")

  ids <- function(x) if (length(x) != 1 || is.na(x)) character(0) else as.character(x)
  if (is.null(workers)) workers <- max(1L, parallel::detectCores(logical = FALSE) - 2L)
  workers <- max(1L, as.integer(workers))

  sequential <- list(
    run = function(jobs) {
      jobs <- data.table::as.data.table(jobs)
      data.table::rbindlist(lapply(seq_len(nrow(jobs)), function(i) {
        engine$eval_side(fid, remove_ids = ids(jobs$drop_id[[i]]),
                         add_ids = ids(jobs$add_id[[i]]))
      }), fill = TRUE)
    },
    stop = function() invisible(NULL),
    engine = engine
  )
  if (workers <= 1L) return(sequential)

  simfile <- tempfile(fileext = ".rds")
  saveRDS(.ffs_slim_simulation(base_simulation), simfile, compress = FALSE)
  dev_pkg <- requireNamespace("pkgload", quietly = TRUE) &&
    isTRUE(tryCatch(pkgload::is_dev_package("ffsimulator"), error = function(e) FALSE))
  pkg_path <- if (dev_pkg) tryCatch(pkgload::pkg_path(), error = function(e) NULL) else NULL

  cl <- tryCatch({
    cl <- parallel::makePSOCKcluster(workers)
    parallel::clusterExport(cl, c("simfile", "dev_pkg", "pkg_path", "fid"),
                            envir = environment())
    parallel::clusterEvalQ(cl, {
      if (dev_pkg && !is.null(pkg_path)) pkgload::load_all(pkg_path, quiet = TRUE) else
        library(ffsimulator)
      .sim <- readRDS(simfile)
      .engine <- ffsimulator::ffs_trade_engine(.sim)
      .fid <- fid
      NULL
    })
    cl
  }, error = function(e) e)

  if (inherits(cl, "error")) {
    unlink(simfile)
    cli::cli_alert_warning(
      "could not start {workers} worker{?s} ({conditionMessage(cl)}) - running in-process")
    return(sequential)
  }
  cli::cli_alert_info("evaluating on {workers} worker{?s}")

  list(
    run = function(jobs) {
      .jobs <- data.table::as.data.table(jobs)
      n <- nrow(.jobs)
      if (!n) return(data.table::data.table())
      parallel::clusterExport(cl, ".jobs", envir = environment())
      chunks <- split(seq_len(n), cut(seq_len(n), min(n, workers * 4L), labels = FALSE))
      chunks <- chunks[lengths(chunks) > 0L]
      pieces <- parallel::clusterApplyLB(cl, chunks, function(idx) {
        rows <- lapply(idx, function(i) {
          a <- .jobs$add_id[[i]]; d <- .jobs$drop_id[[i]]
          .engine$eval_side(.fid,
            remove_ids = if (is.na(d)) character(0) else as.character(d),
            add_ids = if (is.na(a)) character(0) else as.character(a))
        })
        data.table::rbindlist(rows, fill = TRUE)
      })
      out <- data.table::rbindlist(pieces, fill = TRUE)
      out[order(unlist(chunks, use.names = FALSE))]
    },
    stop = function() {
      try(parallel::stopCluster(cl), silent = TRUE)
      unlink(simfile)
      invisible(NULL)
    },
    engine = engine
  )
}

#' Weekly waiver-wire report for one franchise
#'
#' (EXPERIMENTAL) One call from a finished simulation to a ranked list of
#' add/drop pairings: builds the candidate board ([ffs_waiver_candidates()]),
#' attaches the pool to the simulation ([ffs_attach_free_agents()]), prices every
#' move ([ffs_waiver_moves()]) and joins the market and usage columns back on.
#'
#' Redraft and dynasty differ only in how the rows are ordered. `dynasty_weight`
#' 0 ranks purely on simulated championship odds this season; 1 ranks purely on
#' the FantasyCalc value swing; in between the two are blended as percentile
#' ranks - scale-free, so `score` is an ordering heuristic and not a unit.
#'
#' @inheritParams ffs_waiver_moves
#' @param usage a dataframe from [ffs_usage_signals()], or `NULL`
#' @param values a dataframe from [fc_dynasty_values()], or `NULL`
#' @param dynasty_weight 0-1: how much future value counts against this season
#' @param max_per_pos,max_risers,market passed to [ffs_waiver_candidates()]
#' @param pos_filter positions to consider
#' @param confirm_simulation an optional second (larger) simulation on which the
#'   top `confirm_n` pairings are re-priced by [ffs_waiver_confirm()]. Use it to
#'   search on a small, fast simulation and report standings-grade numbers for
#'   the moves you would actually make; the `confirmed` column flags those rows
#' @param confirm_n how many of the best pairings to re-price (default 10)
#'
#' @return a list: `pairs`, `adds`, `drops`, `board` and the augmented `simulation`
#' @export
ffs_waiver_report <- function(base_simulation,
                              franchise_id,
                              usage = NULL,
                              values = NULL,
                              dynasty_weight = 0,
                              drop_ids = NULL,
                              screen_adds = 25L,
                              top_pairs = 40L,
                              position_limits = NULL,
                              workers = 1L,
                              max_per_pos = 20L,
                              max_risers = 10L,
                              market = TRUE,
                              pos_filter = NULL,
                              confirm_simulation = NULL,
                              confirm_n = 10L) {
  checkmate::assert_number(dynasty_weight, lower = 0, upper = 1)
  fantasypros_id <- add_id <- drop_id <- value_delta <- score <- keep <- NULL
  add_value <- drop_value <- mkt_value <- exact <- confirmed <- NULL

  board <- ffs_waiver_candidates(base_simulation, usage = usage, values = values,
                                 pos_filter = pos_filter, max_per_pos = max_per_pos,
                                 max_risers = max_risers, market = market)
  sim <- ffs_attach_free_agents(base_simulation, board)
  moves <- ffs_waiver_moves(sim, franchise_id, drop_ids = drop_ids,
                            screen_adds = screen_adds, top_pairs = top_pairs,
                            position_limits = position_limits, workers = workers)

  pairs <- data.table::as.data.table(moves$pairs)
  b <- data.table::as.data.table(board)
  b[, add_id := paste0("FA_", fantasypros_id)]
  carry <- intersect(c("add_id", "ecr", "ecr_rank", "roster_pct", "rank_delta",
                       "snap_pct_recent", "snap_trend", "exp_pts_recent",
                       "exp_pts_trend", "fc_value", "fc_redraft_value",
                       "fc_trend_30day"), names(b))
  pairs <- merge(pairs, b[, carry, with = FALSE], by = "add_id", all.x = TRUE)

  # market value on both sides of the swap, when values were supplied
  if (!is.null(values)) {
    rs <- data.table::as.data.table(sim$roster_scores)
    own <- unique(rs[, list(drop_id = get("player_id"),
                            fantasypros_id = as.character(get("fantasypros_id")))],
                  by = "drop_id")
    v <- data.table::as.data.table(values)[!is.na(fantasypros_id)]
    vcol <- if (dynasty_weight > 0) "value" else "redraft_value"
    v <- unique(v[, list(fantasypros_id = as.character(fantasypros_id),
                         mkt_value = get(vcol))], by = "fantasypros_id")
    own <- merge(own, v, by = "fantasypros_id", all.x = TRUE)
    pairs <- merge(pairs, own[, list(drop_id, drop_value = mkt_value)],
                   by = "drop_id", all.x = TRUE)
    add_v <- merge(b[, list(add_id, fantasypros_id = as.character(fantasypros_id))],
                   v, by = "fantasypros_id", all.x = TRUE)
    pairs <- merge(pairs, add_v[, list(add_id, add_value = mkt_value)],
                   by = "add_id", all.x = TRUE)
    pairs[, value_delta := data.table::fcoalesce(as.numeric(add_value), 0) -
            data.table::fcoalesce(as.numeric(drop_value), 0)]
  }

  pct <- function(x) {
    if (all(is.na(x))) return(rep(0, length(x)))
    r <- data.table::frank(x, na.last = "keep", ties.method = "average")
    r <- (r - 1) / max(1, sum(!is.na(x)) - 1)
    data.table::fifelse(is.na(r), 0, r)
  }
  rescore <- function(dt) {
    win_now <- pct(dt$champion_pct_delta)
    future <- if ("value_delta" %in% names(dt)) pct(dt$value_delta) else win_now
    dt[, score := (1 - dynasty_weight) * win_now + dynasty_weight * future]
    dt
  }
  pairs <- rescore(pairs)
  data.table::setorder(pairs, -exact, -score)

  # re-price the shortlist on the big simulation: the search sim ranks moves
  # cheaply, this puts standings-grade numbers on the ones worth making
  if (!is.null(confirm_simulation)) {
    top <- utils::head(pairs, confirm_n)
    keep_fp <- sub("^FA_", "", unique(top$add_id))
    csub <- data.table::copy(b[fantasypros_id %in% keep_fp])
    csub[, keep := TRUE]
    csim <- ffs_attach_free_agents(confirm_simulation, csub)
    conf <- data.table::as.data.table(ffs_waiver_confirm(
      csim, franchise_id, top[, list(add_id, drop_id)], workers = workers))
    dcols <- c("h2h_wins_delta", "allplay_delta", "points_delta",
               "playoff_pct_delta", "champion_pct_delta")
    data.table::setnames(conf, dcols, paste0("conf_", dcols))
    pairs <- merge(pairs, conf, by = c("add_id", "drop_id"), all.x = TRUE)
    pairs[, confirmed := !is.na(get("conf_h2h_wins_delta"))]
    for (cc in dcols) {
      pairs[get("confirmed"), (cc) := get(paste0("conf_", cc))]
    }
    pairs[, paste0("conf_", dcols) := NULL]
    pairs <- rescore(pairs)
    data.table::setorder(pairs, -confirmed, -exact, -score)
  }

  list(pairs = as.data.frame(pairs),
       adds = moves$adds,
       drops = moves$drops,
       board = as.data.frame(board),
       simulation = sim)
}
