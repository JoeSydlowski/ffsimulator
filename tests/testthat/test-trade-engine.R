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
