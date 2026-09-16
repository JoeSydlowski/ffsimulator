test_that("ffs_free_agents keeps only unrostered ranked players", {
  rosters <- data.frame(fantasypros_id = c("1", "2", NA), stringsAsFactors = FALSE)
  rankings <- data.frame(
    fantasypros_id = as.character(1:6),
    player = paste0("p", 1:6),
    pos = c("RB", "RB", "RB", "WR", "WR", "K"),
    ecr = c(1, 2, 3, 1, 2, 1),
    stringsAsFactors = FALSE
  )

  fa <- ffs_free_agents(rosters, rankings, pos_filter = c("RB", "WR"))

  expect_false(any(fa$fantasypros_id %in% c("1", "2")))
  expect_false("K" %in% fa$pos)
  # ecr_rank is positional and re-ranked over the free agents alone
  expect_equal(fa[fa$pos == "RB", ]$ecr_rank, 1)
  expect_equal(fa[fa$pos == "WR", ]$ecr_rank, c(1, 2))
})

test_that("waiver add/drop values agree with ffs_player_value", {
  skip_on_cran()
  sim <- .ffs_cache_example("trade_sim.rds")
  skip_if(is.null(sim$optimal_scores) || is.null(sim$roster_scores),
          "example simulation lacks return = 'all' components")
  engine <- ffs_trade_engine(sim)
  skip_if(is.null(engine), "example simulation is not engine-eligible")

  rs <- data.table::as.data.table(sim$roster_scores)
  pool <- unique(rs[, list(franchise_id = as.character(franchise_id), player_id)])
  pool <- pool[!grepl("^(QB|RB|WR|TE|K)_[0-9]+$", pool$player_id)]
  fid <- engine$franchise_ids[[1]]
  mine <- pool[pool$franchise_id == fid]$player_id
  theirs <- pool[pool$franchise_id != fid]$player_id
  skip_if(length(mine) < 3 || length(theirs) < 3, "example simulation too small")

  set.seed(20260915)
  adds <- sample(theirs, 3)
  drops <- sample(mine, 3)

  moves <- ffs_waiver_moves(sim, fid, add_ids = adds, drop_ids = drops,
                            screen_adds = Inf, top_pairs = 3)

  for (a in adds) {
    pv <- ffs_player_value(sim, a, fid, engine = engine)
    row <- moves$adds[moves$adds$add_id == a, ]
    # adding a player is worth exactly what ffs_player_value says he is worth
    expect_equal(row$h2h_wins_delta, pv$h2h_wins, tolerance = 1e-10)
    expect_equal(row$playoff_pct_delta, pv$playoff_pct, tolerance = 1e-10)
  }
  for (d in drops) {
    pv <- ffs_player_value(sim, d, fid, engine = engine)
    row <- moves$drops[moves$drops$drop_id == d, ]
    # dropping him costs exactly what having him is worth
    expect_equal(row$h2h_wins_delta, -pv$h2h_wins, tolerance = 1e-10)
    expect_equal(row$playoff_pct_delta, -pv$playoff_pct, tolerance = 1e-10)
  }

  pairs <- moves$pairs
  expect_equal(nrow(pairs), length(adds) * length(drops))
  expect_true(all(pairs$exact[1:3]))
  # the exact pass must actually re-evaluate: its wins delta is the swap, not
  # the sum of the two one-sided values (they agree only when the two players
  # never compete for the same lineup slot)
  exact <- pairs[pairs$exact, ]
  expect_true(all(abs(exact$h2h_wins_delta - exact$approx_wins) < 1))
})

test_that("ffs_waiver_confirm re-prices the same pairs identically", {
  skip_on_cran()
  sim <- .ffs_cache_example("trade_sim.rds")
  engine <- ffs_trade_engine(sim)
  skip_if(is.null(engine), "example simulation is not engine-eligible")

  rs <- data.table::as.data.table(sim$roster_scores)
  fid <- engine$franchise_ids[[1]]
  mine <- unique(rs[as.character(rs$franchise_id) == fid]$player_id)
  theirs <- unique(rs[as.character(rs$franchise_id) != fid]$player_id)
  skip_if(length(mine) < 2 || length(theirs) < 2, "example simulation too small")

  moves <- ffs_waiver_moves(sim, fid, add_ids = theirs[1:2], drop_ids = mine[1:2],
                            screen_adds = Inf, top_pairs = 4)
  exact <- data.table::as.data.table(moves$pairs)[moves$pairs$exact]

  conf <- ffs_waiver_confirm(sim, fid, exact[, list(add_id, drop_id)])
  conf <- merge(data.table::as.data.table(conf), exact,
                by = c("add_id", "drop_id"), suffixes = c("", "_moves"))
  expect_equal(nrow(conf), nrow(exact))
  for (cc in c("h2h_wins_delta", "playoff_pct_delta", "champion_pct_delta")) {
    expect_equal(conf[[cc]], conf[[paste0(cc, "_moves")]], tolerance = 1e-12)
  }

  # a player the confirming simulation has never heard of is an error, not an NA
  expect_error(ffs_waiver_confirm(sim, fid,
    data.frame(add_id = "FA_999999", drop_id = mine[[1]])), "not in this simulation")
})

test_that("waiver moves respect position limits and skip replacement fillers", {
  skip_on_cran()
  sim <- .ffs_cache_example("trade_sim.rds")
  engine <- ffs_trade_engine(sim)
  skip_if(is.null(engine), "example simulation is not engine-eligible")

  rs <- data.table::as.data.table(sim$roster_scores)
  fid <- engine$franchise_ids[[1]]
  mine <- unique(rs[as.character(rs$franchise_id) == fid, list(player_id, pos)])

  # give the franchise a replacement-level filler and a starter-slot filler:
  # both are simulation scaffolding and must never be offered as drops
  seed_rows <- rs[rs$player_id == mine$player_id[[1]]]
  fillers <- c(paste0(seed_rows$pos[[1]], "_9"), paste0("FILL_", seed_rows$pos[[1]], "_1"))
  filler_rows <- data.table::rbindlist(lapply(fillers, function(f) {
    d <- data.table::copy(seed_rows)
    d[, "player_id" := f]
  }))
  sim2 <- sim
  sim2$roster_scores <- rbind(rs, filler_rows)

  add_id <- unique(rs[as.character(rs$franchise_id) != fid]$player_id)[[1]]
  moves <- ffs_waiver_moves(sim2, fid, add_ids = add_id,
                            screen_adds = Inf, top_pairs = 2)
  expect_false(any(moves$drops$drop_id %in% fillers))
  expect_true(all(mine$player_id %in% moves$drops$drop_id))

  # a position already at its roster cap can only be added by dropping that position
  add_pos <- unique(rs[rs$player_id == add_id]$pos)[[1]]
  have <- sum(mine$pos == add_pos)
  limited <- ffs_waiver_moves(sim, fid, add_ids = add_id,
                              screen_adds = Inf, top_pairs = 2,
                              position_limits = stats::setNames(have, add_pos))
  expect_true(all(limited$pairs$drop_pos == add_pos))
})
