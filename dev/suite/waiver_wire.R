# Weekly waiver wire: the best add/drop pairings for your team, priced on the
# simulated rest of season.
#
#   Rscript dev/suite/waiver_wire.R                        # every league below
#   FFS_LEAGUE_ID=1312145437842497536 Rscript dev/suite/waiver_wire.R
#   FFS_NSEASONS=2000 Rscript dev/suite/waiver_wire.R          # slower, tighter numbers
#
# League type (dynasty / keeper / redraft), superflex, PPR, team count and
# Sleeper's position limits are all read off the league - nothing to configure
# but the league ids and your username.
#
# Outputs land in dev/league_sims/<league_id>/<date>/waivers/:
#   pairs.csv     - ranked add/drop swaps
#   adds.csv      - what each free agent is worth to your roster on his own
#   drops.csv     - what dropping each of your players costs on its own
#   board.csv     - the whole free-agent board (ECR, roster%, snaps, xFP, value)
#   watchlist.csv - unrostered usage risers, including ones too unranked to price
#
# Every number in pairs.csv is a change in YOUR simulated season - h2h wins,
# playoff odds, championship odds - from swapping that free agent in for that
# player. `exact = TRUE` rows were evaluated as a real simultaneous swap; the
# rest are the additive screen (see ?ffs_waiver_moves).

library(data.table)
devtools::load_all(here::here(), quiet = TRUE)
# ffscrapr reads a dead nflverse asset, so ff_scoringhistory() returns ZERO rows
# for 2025+ and every simulation silently loses its most recent season.
source(here::here("dev", "suite", "scoring_history_shim.R")); install_shim()

## ---- CONFIG -----------------------------------------------------------------
config <- list(
  # every league sox05syd is in, from ffscrapr::sleeper_userleagues("sox05syd", 2026):
  # Goofball (10, SF dynasty) | JML (12, 1QB dynasty) | Dynasty Dimwits (12, SF
  # dynasty) | Galletti (8, SF keeper) | Ex-Athletes (12, SF REDRAFT) |
  # Don't be Chopped (5, 1QB redraft - ff_schedule() cannot parse it, see below)
  league_ids = strsplit(Sys.getenv("FFS_LEAGUE_ID", paste(
    "1312145437842497536", "1326464763936403456", "1359546500786434048",
    "1339817277222031360", "1389724668889399296", "1403227684191870976",
    sep = ",")), ",")[[1]],
  platform = Sys.getenv("FFS_PLATFORM", "sleeper"),
  season   = as.integer(Sys.getenv("FFS_SEASON", "2026")),
  # matched against Sleeper user_name first, then franchise_name
  my_team  = Sys.getenv("FFS_MY_TEAM", "sox05syd"),
  # standings sim: the trustworthy numbers. Reused from the suite when today's
  # simulation.rds already exists
  n_sims   = as.integer(Sys.getenv("FFS_NSEASONS", "400")),
  # search sim: every candidate add and drop is valued here, so this is where
  # the runtime goes. A counterfactual costs one lineup re-solve per simulated
  # franchise-week, so cost scales with n: ~7s per lineup-worthy add at n=400,
  # ~0.5s for a bench drop. Defaults to the standings sim (one set of numbers,
  # nothing to reconcile); set it lower on a big league and the top
  # FFS_CONFIRM_N pairings get re-priced on the standings sim instead
  search_sims = as.integer(Sys.getenv("FFS_SEARCH_NSEASONS",
                                      Sys.getenv("FFS_NSEASONS", "400"))),
  confirm_n = as.integer(Sys.getenv("FFS_CONFIRM_N", "10")),
  version  = "v3",
  lineup_method = "rank",
  # MUST stay FALSE: replacement_level = TRUE hands the whole free-agent pool to
  # every franchise at once, which prices every waiver claim at ~0 by construction
  replacement_level = FALSE,
  fill_missing_starters = TRUE,
  actual_schedule = TRUE,
  pos_filter = strsplit(Sys.getenv("FFS_POS_FILTER", "QB,RB,WR,TE,K"), ",")[[1]],
  # in a dynasty league, rank moves partly on market value rather than purely on
  # this season's championship odds (0 = pure win-now, 1 = pure market value)
  dynasty_weight = as.numeric(Sys.getenv("FFS_DYNASTY_WEIGHT", "0.35")),
  # free agents to price: top N per position by ROS ECR, plus usage risers
  max_per_pos = as.integer(Sys.getenv("FFS_MAX_PER_POS", "15")),
  max_risers  = as.integer(Sys.getenv("FFS_MAX_RISERS", "8")),
  # candidate adds that survive the cheap points screen and get simulated
  screen_adds = as.integer(Sys.getenv("FFS_SCREEN_ADDS", "25")),
  top_pairs   = as.integer(Sys.getenv("FFS_TOP_PAIRS", "30")),
  # 1 = in-process, and that is the right default: each worker holds its own
  # copy of the simulation (~2.4 GB at n=400) plus ~1 min of startup, and a
  # 6-worker run on this box paged so hard it was ~10x SLOWER than sequential.
  # Raise it only with real memory headroom (~2.5 GB free per worker)
  workers   = as.integer(Sys.getenv("FFS_WORKERS", "1")),
  lookback  = as.integer(Sys.getenv("FFS_LOOKBACK", "3")),
  # reuse today's simulation.rds (e.g. from run_league_suite.R) if it is there
  reuse_sim = as.logical(Sys.getenv("FFS_REUSE_SIM", "TRUE"))
)

day <- format(Sys.Date())
usage <- try(ffs_usage_signals(season = config$season, lookback = config$lookback),
             silent = TRUE)
if (inherits(usage, "try-error")) {
  message("usage signals unavailable: ", conditionMessage(attr(usage, "condition")))
  usage <- NULL
}

run_league <- function(league_id) {
  out_root <- here::here("dev", "league_sims", league_id, day)
  out <- file.path(out_root, "waivers")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)

  conn <- switch(config$platform,
    sleeper = ffscrapr::sleeper_connect(season = config$season, league_id = league_id),
    mfl = ffscrapr::mfl_connect(season = config$season, league_id = league_id)
  )

  ## ---- LEAGUE SETTINGS (all detected, none configured) ----------------------
  info <- as.data.table(ffscrapr::ff_league(conn))
  dynasty <- !identical(info$league_type[[1]], "redraft")
  superflex <- grepl("SF|2QB|superflex", info$qb_type[[1]], ignore.case = TRUE)
  ppr <- suppressWarnings(as.numeric(sub("_ppr.*$", "", info$scoring_flags[[1]])))
  if (is.na(ppr)) ppr <- 1
  cli::cli_rule("{info$league_name[[1]]} - {info$league_type[[1]]}{
    if (superflex) ' superflex' else ' 1QB'}, {ppr} ppr")

  position_limits <- NULL
  if (config$platform == "sleeper") {
    position_limits <- tryCatch({
      st <- ffscrapr::sleeper_getendpoint(paste0("league/", league_id))$content$settings
      enforce <- unlist(st[grepl("enforce", names(st), ignore.case = TRUE)])
      lims <- unlist(st[grepl("^position_limit_", names(st))])
      lims <- lims[!grepl("enforce", names(lims), ignore.case = TRUE)]
      if (length(lims) && !identical(as.integer(enforce), 0L)) {
        stats::setNames(as.integer(lims), toupper(sub("^position_limit_", "", names(lims))))
      } else NULL
    }, error = function(e) NULL)
  }
  if (!is.null(position_limits)) {
    message("position limits: ",
            paste(names(position_limits), position_limits, sep = "=", collapse = " "))
  }

  ## ---- SIMULATIONS ----------------------------------------------------------
  # ffscrapr's Sleeper schedule parser trips over a week where some franchise has
  # no opponent (odd-sized leagues). Fall back to random schedules over the weeks
  # that are left - add/drop values still hold, but the playoff and championship
  # columns no longer condition on the real slate or on games already played
  actual_schedule <- config$actual_schedule
  n_weeks <- 14L
  if (actual_schedule && inherits(try(ffs_schedule(conn), silent = TRUE), "try-error")) {
    playoff_start <- tryCatch(as.integer(
      ffscrapr::sleeper_getendpoint(paste0("league/", league_id)
        )$content$settings$playoff_week_start), error = function(e) 15L)
    if (is.na(playoff_start)) playoff_start <- 15L
    n_weeks <- max(1L, playoff_start - as.integer(nflreadr::get_current_week()))
    actual_schedule <- FALSE
    message("!! ff_schedule() failed for this league - simulating ", n_weeks,
            " remaining weeks on RANDOM schedules. Playoff/championship deltas ",
            "ignore the real slate and the games already played; the wins and ",
            "points deltas are still the rest-of-season answer.")
  }

  simulate_n <- function(n) {
    message("simulating ", n, " seasons @ ", Sys.time())
    s <- ff_simulate(
      conn,
      n_seasons = n,
      n_weeks = n_weeks,
      version = config$version,
      lineup_method = config$lineup_method,
      replacement_level = config$replacement_level,
      fill_missing_starters = config$fill_missing_starters,
      actual_schedule = actual_schedule,
      pos_filter = config$pos_filter,
      return = "all"
    )
    if (is.null(s$summary_season)) stop("no unplayed weeks left to simulate")
    s
  }

  sim_path <- file.path(out_root, "simulation.rds")
  sim <- NULL
  if (config$reuse_sim && file.exists(sim_path)) {
    cached <- readRDS(sim_path)
    # a cached run at a smaller n must not silently stand in for the one asked for
    if (isTRUE(cached$simulation_params$n_seasons >= config$n_sims)) {
      message("reusing ", sim_path, " (n=", cached$simulation_params$n_seasons, ")")
      sim <- cached
    } else {
      message("cached simulation is n=", cached$simulation_params$n_seasons,
              ", below the requested n=", config$n_sims, " - re-simulating")
    }
    rm(cached)
  }
  if (is.null(sim)) {
    sim <- simulate_n(config$n_sims)
    saveRDS(sim, sim_path)
  }
  if (!identical(sim$simulation_params$lineup_method, "rank")) {
    stop("this simulation used lineup_method = '", sim$simulation_params$lineup_method,
         "'; waiver pricing needs 'rank' (set FFS_REUSE_SIM=FALSE)")
  }
  # a simulation built with replacement_level = TRUE already hands every team the
  # top free agents, so every claim would price at ~0 - refuse to report that
  if (any(grepl("^(QB|RB|WR|TE|K)_\\d+$", unique(sim$roster_scores$player_id)))) {
    stop("this simulation was built with replacement_level = TRUE - every team ",
         "already has the free-agent pool, so claims price at ~0. Re-run with ",
         "FFS_REUSE_SIM=FALSE")
  }

  who <- tryCatch(as.data.table(ffscrapr::ff_franchises(conn)),
                  error = function(e) as.data.table(sim$franchises))
  keys <- intersect(c("user_name", "franchise_name"), names(who))
  hit <- who[Reduce(`|`, lapply(keys, function(k)
    grepl(config$my_team, who[[k]], ignore.case = TRUE)))]
  if (nrow(hit) != 1) {
    stop("set FFS_MY_TEAM to one of: ", paste(who$franchise_name, collapse = " | "))
  }
  my_id <- as.character(hit$franchise_id[[1]])
  message("franchise: ", hit$franchise_name[[1]], " (", my_id, ")")

  values <- try(fc_dynasty_values(num_qbs = if (superflex) 2 else 1,
                                  num_teams = info$franchise_count[[1]],
                                  ppr = ppr, is_dynasty = dynasty), silent = TRUE)
  if (inherits(values, "try-error")) {
    message("FantasyCalc unavailable: ", conditionMessage(attr(values, "condition")))
    values <- NULL
  }

  ## ---- BOARD ----------------------------------------------------------------
  # search on the cheap sim, confirm the shortlist on the standings sim
  search_sim <- if (config$search_sims < config$n_sims) simulate_n(config$search_sims) else sim
  report <- ffs_waiver_report(
    search_sim,
    franchise_id = my_id,
    confirm_simulation = if (identical(search_sim, sim)) NULL else sim,
    confirm_n = config$confirm_n,
    usage = usage,
    values = values,
    dynasty_weight = if (dynasty) config$dynasty_weight else 0,
    screen_adds = config$screen_adds,
    top_pairs = config$top_pairs,
    position_limits = position_limits,
    workers = config$workers,
    max_per_pos = config$max_per_pos,
    max_risers = config$max_risers
  )

  pairs <- as.data.table(report$pairs)
  show_cols <- intersect(c(
    "add_player", "add_pos", "drop_player", "drop_pos", "h2h_wins_delta",
    "playoff_pct_delta", "champion_pct_delta", "value_delta", "score",
    "confirmed", "exact", "ecr", "roster_pct", "snap_pct_recent",
    "exp_pts_recent", "exp_pts_trend"
  ), names(pairs))
  pairs_out <- pairs[, show_cols, with = FALSE]
  num <- names(pairs_out)[vapply(pairs_out, is.numeric, logical(1))]
  pairs_out[, (num) := lapply(.SD, round, 4), .SDcols = num]

  fwrite(pairs_out, file.path(out, "pairs.csv"))
  fwrite(report$adds, file.path(out, "adds.csv"))
  fwrite(report$drops, file.path(out, "drops.csv"))
  fwrite(report$board, file.path(out, "board.csv"))
  saveRDS(list(config = config, league = info, franchise = my_id),
          file.path(out, "config.rds"))

  # usage risers nobody rosters: the sim can only price a player FantasyPros
  # ranks, so unranked names here are a watchlist rather than a move
  board <- as.data.table(report$board)
  watch_col <- if ("exp_pts_trend" %in% names(board) &&
                   any(!is.na(board$exp_pts_trend))) {
    "exp_pts_trend"   # mid-season: who is gaining work
  } else if ("exp_pts_recent" %in% names(board)) {
    "exp_pts_recent"  # weeks 1-3: no prior window to trend against yet
  } else NULL
  if (!is.null(watch_col)) {
    watch <- board[!is.na(get(watch_col))]
    if (nrow(watch)) {
      setorderv(watch, watch_col, -1L)
      fwrite(head(watch, 25), file.path(out, "watchlist.csv"))
    }
  }

  cat("\n==== BEST ADD / DROP -", info$league_name[[1]], "-",
      hit$franchise_name[[1]], "-", day, "====\n")
  top_rows <- if ("confirmed" %in% names(pairs_out)) pairs_out[confirmed %in% TRUE] else
    pairs_out[exact %in% TRUE]
  print(head(top_rows, 20))
  cat("\n---- best adds on their own ----\n")
  print(head(as.data.table(report$adds), 12))
  cat("\n---- cheapest drops ----\n")
  print(head(as.data.table(report$drops), 12))
  cat("\nwrote ", out, "\n", sep = "")
  invisible(out)
}

for (lid in config$league_ids) {
  res <- try(run_league(lid), silent = FALSE)
  if (inherits(res, "try-error")) {
    message("league ", lid, " failed: ", conditionMessage(attr(res, "condition")))
  }
  # an n=2000 simulation is several GB - do not carry one into the next league
  gc()
}
