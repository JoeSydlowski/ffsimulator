test_that(".ffs_market_edge prices the shape premium, not the raw gap", {
  # even swap: fair is a zero gap
  e <- .ffs_market_edge(1000, 1000, 1, 1, fair_premium = 0.05)
  expect_equal(e$fair_gap, 0)
  expect_equal(e$my_edge, 0)

  # 1-for-2: the package must overpay by 5%, so +5% is FAIR, not a win for me
  e <- .ffs_market_edge(1000, 1050, 1, 2, fair_premium = 0.05)
  expect_equal(e$fair_gap, 0.05)
  expect_equal(e$my_edge, 0, tolerance = 1e-8)

  # 2-for-1 consolidation: I send the package, so I must overpay by the same 5%.
  # fair_gap is the required PREMIUM magnitude; its direction is in my_edge.
  e <- .ffs_market_edge(1050, 1000, 2, 1, fair_premium = 0.05)
  expect_equal(e$fair_gap, 0.05)
  expect_equal(e$single_value, 1000)             # the single player I receive
  expect_equal(e$my_edge, 0, tolerance = 1e-8)
  # overpaying more than fair for the consolidation is a loss for me
  expect_lt(.ffs_market_edge(1150, 1000, 2, 1, fair_premium = 0.05)$my_edge, 0)

  # 1-for-3 doubles the premium; edges are always equal and opposite
  e <- .ffs_market_edge(1000, 1200, 1, 3, fair_premium = 0.05)
  expect_equal(e$fair_gap, 0.10)
  expect_equal(e$single_value, 1000)
  expect_equal(e$my_edge, 0.10, tolerance = 1e-8)   # 20% taken where 10% is fair
  expect_equal(e$my_edge, -e$opp_edge)
})

test_that("the market opponent model ignores playoff dings but vetoes a gutting", {
  dyn <- data.table::data.table(
    player_id = c("p1", "p2", "p3"), pos = c("WR", "RB", "QB"),
    cur_value = c(8000, 4200, 4000), next_value_mean = c(8100, 4100, 3900))
  # 8400 for an 8000 stud is exactly the shape-fair price of a 1-for-2 at 5%
  mk <- function(opp_pl) data.table::data.table(
    send_ids = list("p1"), recv_ids = list(c("p2", "p3")),
    send_value = 8000, recv_value = 8400, future_capital_delta = 0,
    my_playoff_delta = 0.04, opp_playoff_delta = opp_pl)

  # a routine deal at the fair price that costs them 4% of playoff odds: they
  # take it - this is the case the old mirror model wrongly refused.
  # fair_premium is pinned to the 5% this fixture was built for (8000 * 1.05 =
  # 8400), so the test measures the playoff veto and not the premium default.
  ok <- ffs_deal_scores(mk(-0.04), dyn, max_opp_drop = 0.15, fair_premium = 0.05)
  expect_true(ok$gettable)
  expect_equal(ok$my_edge, 0, tolerance = 1e-8)

  # same deal that guts them (-20%): vetoed
  bad <- ffs_deal_scores(mk(-0.20), dyn, max_opp_drop = 0.15, fair_premium = 0.05)
  expect_false(bad$gettable)

  # priced well over fair: refused on value, whatever it does to their odds
  rich <- data.table::data.table(
    send_ids = list("p1"), recv_ids = list(c("p2", "p3")),
    send_value = 6000, recv_value = 8200, future_capital_delta = 0,
    my_playoff_delta = 0.04, opp_playoff_delta = 0.02)
  expect_false(ffs_deal_scores(rich, dyn)$gettable)
})

test_that("every entry point shares one fair_premium default", {
  # 0.10 is the settled overpay the production boards are built at. It used to
  # default to 0.05 in the package while trade_chain_eval.R hardcoded 0.10 and
  # confirm_trade_board.R passed nothing at all, so a board and its own
  # confirmation graded `gettable` on different standards without saying so.
  expect_equal(eval(formals(ffs_deal_scores)$fair_premium), 0.10)
  expect_equal(eval(formals(ffs_build_trades)$fair_premium), 0.10)
  expect_equal(eval(formals(.ffs_market_edge)$fair_premium), 0.10)
  expect_equal(eval(formals(.ffs_band_ok)$fair_premium), 0.10)

  # and it is inert on even shapes, which is why it can be changed without
  # touching a 1-for-1 or 2-for-2 board
  expect_equal(.ffs_market_edge(1000, 1000, 2, 2)$fair_gap, 0)
  expect_equal(.ffs_market_edge(1000, 1000, 1, 1)$fair_gap, 0)
  expect_equal(.ffs_market_edge(1000, 1100, 2, 1)$fair_gap, 0.10)
})

test_that("opp_surplus is measured against the fair price, not the raw value gap", {
  dyn <- data.table::data.table(
    player_id = c("p1", "p2", "p3"), pos = c("WR", "RB", "WR"),
    cur_value = c(8000, 4200, 4200), next_value_mean = c(8000, 4200, 4200))
  # a fair 1-for-2: they hand over 8400 for an 8000 stud, which is the premium.
  # Raw value gain reads -400 (a loss); surplus vs fair reads ~0.
  d <- data.table::data.table(
    send_ids = list("p1"), recv_ids = list(c("p2", "p3")),
    send_value = 8000, recv_value = 8400, future_capital_delta = 0,
    my_playoff_delta = 0.01, opp_playoff_delta = -0.01)
  s <- ffs_deal_scores(d, dyn, fair_premium = 0.05)
  expect_equal(s$opp_value_gain, -400)
  expect_equal(s$opp_surplus, 0, tolerance = 1e-6)
  expect_equal(s$opp_score, 0, tolerance = 1e-6)
  expect_true(s$gettable)
})

test_that(".ffs_packages enumerates every combination with must_include honoured", {
  pool <- data.table::data.table(
    player_id = c("a", "b", "c", "PICK_2027_1_5"),
    player_name = c("A", "B", "C", "P"),
    cur_value = c(100, 200, 300, 50),
    next_value_mean = c(110, 190, 310, 60),
    value_to_me = c(1, 2, 3, 0))

  all2 <- .ffs_packages(pool, sizes = 2)
  expect_equal(nrow(all2), choose(4, 2))
  expect_true(all(lengths(all2$ids) == 2))
  expect_equal(sum(all2$has_pick), 3L)

  forced <- .ffs_packages(pool, sizes = c(1, 2), must_include = "b")
  expect_true(all(vapply(forced$ids, function(x) "b" %in% x, logical(1))))
  expect_equal(nrow(forced), 1L + 3L)   # b alone, plus b with each other asset

  # values, gains and the top-piece proxy are summed/maxed over the package
  bc <- all2[label == "B + C"]
  expect_equal(bc$value, 500)
  expect_equal(bc$next_value, 500)
  expect_equal(bc$gain, 5)
  expect_equal(bc$top, 3)
})

test_that(".ffs_band_ok reproduces the legacy bands when fair_band is NULL", {
  # even trades: symmetric band on the received value
  expect_true(.ffs_band_ok(1020, 1000, 1, 1, even_band = 0.03))
  expect_false(.ffs_band_ok(1050, 1000, 1, 1, even_band = 0.03))

  # uneven: the package side must overpay inside the premium band
  expect_true(.ffs_band_ok(1000, 1050, 1, 2, uneven_gap = c(0.02, 0.10)))
  expect_false(.ffs_band_ok(1000, 1005, 1, 2, uneven_gap = c(0.02, 0.10)))
  expect_false(.ffs_band_ok(1000, 1200, 1, 2, uneven_gap = c(0.02, 0.10)))
})

test_that("ffs_build_trades enumerates, scores and gates end to end", {
  skip_on_cran()
  sim <- .ffs_cache_example("trade_sim.rds")
  skip_if(is.null(ffs_trade_engine(sim)), "fixture is not engine-eligible")

  rs <- data.table::as.data.table(sim$roster_scores)
  fids <- sort(unique(rs$franchise_id))
  me <- fids[[1]]

  # a synthetic dynasty table: market value proxied by mean weekly projection, so
  # the test exercises the machinery without depending on the dynasty model
  dyn <- rs[!grepl("^(QB|RB|WR|TE|K)_[0-9]+$", player_id),
            list(cur_value = 400 * mean(projected_score, na.rm = TRUE)),
            by = list(player_id, pos)]
  dyn[, next_value_mean := cur_value * 0.98]

  targets <- data.table::as.data.table(ffs_trade_targets(sim, me, top_n = 25))

  b <- data.table::as.data.table(ffs_build_trades(
    sim, me, dynasty = dyn, targets = targets,
    shapes = list(c(1, 1), c(1, 2), c(2, 1)),
    fair_band = c(-0.05, 0.05), fair_premium = 0.05, opp_edge_tol = 0.05,
    require_positive_target = FALSE,
    screen_n = 12, screen_per_opp = 0, top_n = 12))

  skip_if(nrow(b) == 0, "fixture produced no banded deals")
  for (cc in c("my_edge", "opp_edge", "opp_value_gain", "opp_surplus",
               "opp_score", "gettable", "grade", "score"))
    expect_true(cc %in% names(b), info = cc)

  # the band held, and the two edges are always mirror images
  expect_true(all(b$my_edge >= -0.05 - 1e-9 & b$my_edge <= 0.05 + 1e-9))
  expect_equal(b$opp_edge, -b$my_edge)
  # the market gate pruned before the eval, so nothing below tolerance survives
  expect_true(all(b$opp_edge >= -0.05 - 1e-9))
  # opp_score is the surplus priced in playoff points
  expect_equal(b$opp_score, b$opp_surplus / 68, tolerance = 1e-8)
})

test_that("a draft pick is move-discounted like a player when it is priced", {
  # A pick's cur_value comes from the same market scrape as a player's, so only
  # its projected MOVE is speculative. The near pick appreciates none (disc(1)=1),
  # so sending it should cost its full market value - not 0.55 of it.
  players <- data.frame(
    player_id = c("p1", "p2"), pos = c("WR", "RB"),
    cur_value = c(4000, 4000), next_value_mean = c(4000, 4000))
  pick <- data.frame(
    player_id = "PICK_2027_1_x", pos = "PICK",
    cur_value = 3467, next_value_mean = 3467)
  withpick <- rbind(players, pick)

  # send the pick, receive nothing: future_capital_delta is -next(pick)
  f_new <- .ffs_adj_future_fn(withpick, format = "1qb")
  expect_equal(f_new("PICK_2027_1_x", character(0), -3467), -3467)

  # legacy path (pick absent from the table) level-discounts it, unchanged
  f_old <- .ffs_adj_future_fn(players, format = "1qb")
  expect_equal(f_old("PICK_2027_1_x", character(0), -3467), 0.55 * -3467)

  # a far pick still has its MOVE discounted, keeping its known level
  far <- data.frame(player_id = "PICK_2029_1_x", pos = "PICK",
                    cur_value = 2189, next_value_mean = 2487)
  f_far <- .ffs_adj_future_fn(rbind(players, far), format = "1qb")
  expect_equal(f_far(character(0), "PICK_2029_1_x", 2487),
               2189 + 0.55 * (2487 - 2189))

  # players are untouched by the change
  expect_equal(f_new(character(0), "p1", 4000), 4000)
})
