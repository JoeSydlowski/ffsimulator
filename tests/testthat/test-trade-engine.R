test_that("the trade engine reproduces ffs_trade_eval exactly", {
  skip_on_cran()
  sim <- .ffs_cache_example("trade_sim.rds")
  skip_if(is.null(sim$optimal_scores) || is.null(sim$roster_scores),
          "example simulation lacks return = 'all' components")

  engine <- ffs_trade_engine(sim)
  skip_if(is.null(engine), "example simulation is not engine-eligible")

  rs <- data.table::as.data.table(sim$roster_scores)
  pool <- unique(rs[, list(franchise_id, player_id)])
  # replacement-level filler ids are synthetic and not tradeable
  pool <- pool[!grepl("^(QB|RB|WR|TE|K)_[0-9]+$", player_id)]
  fids <- engine$franchise_ids
  skip_if(length(fids) < 2 || nrow(pool) < 20, "example simulation too small")

  set.seed(20260805)
  # span the shapes the board enumerates, including pick-carrying packages
  shapes <- list(c(1, 1), c(2, 1), c(1, 2), c(1, 3), c(2, 3), c(3, 3))
  deals <- lapply(seq_len(30), function(i) {
    sh <- shapes[[((i - 1L) %% length(shapes)) + 1L]]
    fa <- sample(fids, 1)
    fb <- sample(setdiff(fids, fa), 1)
    a <- pool[franchise_id == fa]$player_id
    b <- pool[franchise_id == fb]$player_id
    if (length(a) < sh[[1]] || length(b) < sh[[2]]) return(NULL)
    ga <- sample(a, sh[[1]])
    gb <- sample(b, sh[[2]])
    # every third deal also carries a draft pick, which the evaluator must drop
    if (i %% 3L == 0L) gb <- c(gb, "PICK_2027_1_5")
    list(fa = fa, ga = ga, fb = fb, gb = gb)
  })
  deals <- Filter(Negate(is.null), deals)
  expect_gt(length(deals), 20)

  for (d in deals) {
    legacy <- ffs_trade_eval(sim, d$fa, d$ga, d$fb, d$gb)
    fast <- ffs_trade_eval(sim, d$fa, d$ga, d$fb, d$gb, engine = engine)
    expect_identical(legacy$franchise_id, fast$franchise_id)
    for (cc in setdiff(names(legacy), "franchise_id")) {
      expect_equal(fast[[cc]], legacy[[cc]], tolerance = 1e-10,
                   info = paste("column", cc))
    }
  }
})

test_that("the engine's baseline summary matches the legacy summariser", {
  skip_on_cran()
  sim <- .ffs_cache_example("trade_sim.rds")
  engine <- ffs_trade_engine(sim)
  skip_if(is.null(engine), "example simulation is not engine-eligible")

  legacy <- data.table::as.data.table(.ffs_summarise_optimal(
    sim, data.table::as.data.table(sim$optimal_scores), engine$franchise_ids))
  data.table::setorder(legacy, franchise_id)
  fast <- engine$before[order(franchise_id)]

  for (cc in c("h2h_wins", "h2h_winpct", "allplay_winpct", "points_for",
               "playoff_pct", "champion_pct", "top_seed_pct")) {
    expect_equal(fast[[cc]], legacy[[cc]], tolerance = 1e-12, info = cc)
  }
})

test_that("ffs_trade_eval_many matches one-at-a-time evaluation", {
  skip_on_cran()
  sim <- .ffs_cache_example("trade_sim.rds")
  engine <- ffs_trade_engine(sim)
  skip_if(is.null(engine), "example simulation is not engine-eligible")

  rs <- data.table::as.data.table(sim$roster_scores)
  pool <- unique(rs[, list(franchise_id, player_id)])
  pool <- pool[!grepl("^(QB|RB|WR|TE|K)_[0-9]+$", player_id)]
  fids <- engine$franchise_ids

  set.seed(4)
  batch <- data.table::rbindlist(lapply(seq_len(5), function(i) {
    fa <- fids[[1]]; fb <- fids[[2]]
    data.table::data.table(
      franchise_a = fa, gives_a = list(sample(pool[franchise_id == fa]$player_id, 1)),
      franchise_b = fb, gives_b = list(sample(pool[franchise_id == fb]$player_id, 1)))
  }))

  many <- data.table::as.data.table(ffs_trade_eval_many(sim, batch, engine = engine))
  expect_equal(nrow(many), 2L * nrow(batch))
  for (i in seq_len(nrow(batch))) {
    one <- ffs_trade_eval(sim, batch$franchise_a[[i]], batch$gives_a[[i]],
                          batch$franchise_b[[i]], batch$gives_b[[i]], engine = engine)
    got <- many[deal_id == i]
    expect_equal(got$playoff_pct_delta, one$playoff_pct_delta)
    expect_equal(got$h2h_wins_delta, one$h2h_wins_delta)
  }
})

test_that("FFS_EVAL_ENGINE=0 forces the legacy path", {
  sim <- .ffs_cache_example("trade_sim.rds")
  old <- Sys.getenv("FFS_EVAL_ENGINE", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("FFS_EVAL_ENGINE") else
    Sys.setenv(FFS_EVAL_ENGINE = old), add = TRUE)
  Sys.setenv(FFS_EVAL_ENGINE = "0")
  expect_null(ffs_trade_engine(sim))
})

# ---------------------------------------------------------------------------
# Leagues with a non-offensive starting slot (K, DEF)
#
# The suite above asserts engine == ffs_trade_eval(), i.e. engine == the FAST
# partial-reoptimisation path. Both share .ffs_counterfactual_rows(), so a bug
# in it passes every one of those cases. This block asserts the assertion those
# structurally cannot make: fast == full re-optimisation, on a league where
# offense_starters != total_starters.
#
# The shipped fixture is QB/RB/WR/TE (offense_starters == total_starters == 10),
# where the two are equal and the bug is invisible. This derives a K/DEF variant
# from it: a kicker on every roster, a K slot and a DEF slot in the constraints,
# and base lineups recomputed so the object is internally consistent. DEF is
# never simulable - no DEF rows exist in scoring history - so it drops out of
# pos_filter and any re-optimisation must filter it out the same way, or its
# min = 1 forces a zero-point filler that also counts against offense_starters.
.trade_sim_with_kicker <- function(n_seasons = 2L) {
  sim <- .ffs_cache_example("trade_sim.rds")
  sub <- function(x) data.table::as.data.table(x)[season <= n_seasons]
  rs <- sub(sim$roster_scores)
  sched <- sub(sim$schedules)

  # one kicker per franchise-week, deterministic scores spread across franchises
  fweeks <- unique(rs[, list(league_id, franchise_id, franchise_name, season, week)])
  fid_lv <- sort(unique(fweeks$franchise_id))
  k <- data.table::copy(fweeks)
  k[, `:=`(
    player_id = paste0("KICKER_", franchise_id),
    player_name = paste0("Kicker ", franchise_id),
    pos = "K",
    avg_week = 8 + (match(franchise_id, fid_lv) %% 4),
    pos_rank = 1L
  )]
  k[, projected_score := avg_week + ((season * 7L + week * 3L) %% 5L) - 2]
  for (cc in setdiff(names(rs), names(k))) {
    data.table::set(k, j = cc, value = rs[[cc]][[1]][NA_integer_])
  }
  rs2 <- rbind(rs, k[, names(rs), with = FALSE])

  # K and DEF slots on top of the existing 10 offensive starters
  lc <- data.table::as.data.table(sim$lineup_constraints)
  extra <- data.table::data.table(pos = c("K", "DEF"), min = 1, max = 1)
  lc2 <- rbind(lc, extra, fill = TRUE)
  lc2[, `:=`(offense_starters = lc$offense_starters[[1]],
             defense_starters = 0,
             total_starters = lc$total_starters[[1]] + 2L)]

  # base lineups for the league as ff_simulate() would compute them
  params <- sim$simulation_params
  params$n_seasons <- n_seasons
  params$pos_filter <- list(c("QB", "RB", "WR", "TE", "K"))
  opt <- ffs_optimise_lineups(
    roster_scores = rs2, lineup_constraints = lc2,
    pos_filter = params$pos_filter[[1]], lineup_method = "rank",
    lineup_noise_sd = 0, best_ball = FALSE)

  out <- list(optimal_scores = opt, roster_scores = rs2, schedules = sched,
              lineup_constraints = lc2, simulation_params = params)
  class(out) <- class(sim)
  out
}

test_that("a kicker slot really does make offense_starters != total_starters", {
  skip_on_cran()
  sim <- .trade_sim_with_kicker()
  lc <- data.table::as.data.table(sim$lineup_constraints)
  expect_true(lc$offense_starters[[1]] != lc$total_starters[[1]])
  expect_true("DEF" %in% lc$pos)

  # the constraints the re-optimisation paths must use: DEF gone, K kept
  filtered <- .ffs_sim_lineup_constraints(sim)
  expect_false("DEF" %in% filtered$pos)
  expect_true("K" %in% filtered$pos)

  # 10 offensive starters + a kicker, not min(offense_starters, total_starters)
  expect_identical(.ffs_n_lineup_slots(filtered),
                   lc$offense_starters[[1]] + 1)

  # and the base lineups really do start that many real players
  starters <- lengths(lapply(sim$optimal_scores$starter_player_id, stats::na.omit))
  expect_identical(max(starters), 11L)
})

test_that("fast trade eval matches full re-optimisation with a K/DEF slot", {
  skip_on_cran()
  sim <- .trade_sim_with_kicker()
  rs <- data.table::as.data.table(sim$roster_scores)
  pool <- unique(rs[pos != "K", list(franchise_id, player_id)])
  fids <- sort(unique(rs$franchise_id))
  fa <- fids[[1]]; fb <- fids[[2]]
  a <- pool[franchise_id == fa]$player_id
  b <- pool[franchise_id == fb]$player_id

  set.seed(20260823)
  deals <- list(
    # the diagnostic case: one side receives a player for nothing. Receiving a
    # player cannot lower your optimal points, so a negative delta here is the bug.
    list(ga = sample(a, 1), gb = character(0)),
    list(ga = sample(a, 1), gb = sample(b, 1)),
    list(ga = sample(a, 2), gb = sample(b, 1)),
    list(ga = sample(a, 1), gb = c(sample(b, 3), "PICK_2027_1_5")),
    list(ga = sample(a, 3), gb = sample(b, 3))
  )

  engine <- ffs_trade_engine(sim)
  expect_false(is.null(engine))

  for (d in deals) {
    full <- ffs_trade_eval(sim, fa, d$ga, fb, d$gb, fast = FALSE)
    fast <- ffs_trade_eval(sim, fa, d$ga, fb, d$gb, fast = TRUE)
    eng <- ffs_trade_eval(sim, fa, d$ga, fb, d$gb, engine = engine)
    for (cc in setdiff(names(full), "franchise_id")) {
      expect_equal(fast[[cc]], full[[cc]], tolerance = 1e-10, info = paste("fast", cc))
      expect_equal(eng[[cc]], full[[cc]], tolerance = 1e-10, info = paste("engine", cc))
    }
  }

  # free player, no compensation: the receiver cannot be made worse off
  gift <- ffs_trade_eval(sim, fa, deals[[1]]$ga, fb, character(0), fast = TRUE)
  expect_gte(gift$points_delta[gift$franchise_id == fb], 0)
})

test_that("ffs_player_value fast path matches full with a K/DEF slot", {
  skip_on_cran()
  sim <- .trade_sim_with_kicker()
  rs <- data.table::as.data.table(sim$roster_scores)
  fids <- sort(unique(rs$franchise_id))
  set.seed(11)
  pids <- sample(unique(rs[franchise_id == fids[[1]] & pos != "K"]$player_id), 3)

  for (pid in pids) {
    for (fid in fids[1:2]) {
      fast <- ffs_player_value(sim, pid, fid, fast = TRUE)
      full <- ffs_player_value(sim, pid, fid, fast = FALSE)
      for (cc in intersect(names(full), names(fast))) {
        if (is.numeric(full[[cc]]))
          expect_equal(fast[[cc]], full[[cc]], tolerance = 1e-10,
                       info = paste(pid, fid, cc))
      }
    }
  }
})
