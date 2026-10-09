# Tests for the keeper-league draft machinery (R/14-keepers.R). All synthetic
# and offline - no league connection - so they run anywhere.

mini_pool <- function(n_each = 30) {
  data.table::data.table(
    fantasypros_id = paste0("p", seq_len(4 * n_each)),
    player = paste0("Player", seq_len(4 * n_each)),
    pos = rep(c("QB", "RB", "WR", "TE"), each = n_each),
    ecr = rep(seq_len(n_each), 4)
  )
}

test_that("keeper costs price drafted and undrafted players", {
  rosters <- data.frame(
    player_id = c("a", "b", "c"), franchise_id = c("1", "1", "1"),
    pos = c("RB", "WR", "TE"), fantasypros_id = c("fa", "fb", "fc"),
    stringsAsFactors = FALSE)
  prev <- data.frame(player_id = c("a", "b"), round = c(5L, 1L))
  adp <- data.frame(fantasypros_id = "fc", adp_rank = 100L)

  x <- ffs_keeper_costs(rosters, prev, adp, n_teams = 10L, max_round = 17L)
  expect_equal(x$base_round[x$player_id == "a"], 4L)   # round 5 - 1
  expect_equal(x$base_round[x$player_id == "b"], 1L)   # R1 keeper stays R1
  expect_equal(x$base_round[x$player_id == "c"], 10L)  # ceiling(100/10), ADP-priced
  expect_equal(x$cost_source[x$player_id == "c"], "adp")
  expect_true(x$top2_prev[x$player_id == "b"])
  expect_false(x$top2_prev[x$player_id == "a"])
})

test_that("ADP-priced keepers are capped at the last round", {
  rosters <- data.frame(player_id = "z", franchise_id = "1", pos = "WR",
                        fantasypros_id = "fz", stringsAsFactors = FALSE)
  x <- ffs_keeper_costs(rosters,
                        data.frame(player_id = character(), round = integer()),
                        data.frame(fantasypros_id = "fz", adp_rank = 400L),
                        n_teams = 8L, max_round = 17L)
  expect_equal(x$base_round, 17L)   # raw round 50 -> capped
})

test_that("next season's keeper costs follow the drafted round anywhere he goes", {
  # this season's draft: a kept at R3, b drafted R1, c R2, d R14, e R10 then
  # dropped (now a free agent), f drafted R6 and since traded
  picks <- data.frame(player_id = c("a", "b", "c", "d", "e", "f"),
                      round = c(3L, 1L, 2L, 14L, 10L, 6L),
                      is_keeper = c(TRUE, NA, NA, NA, NA, NA))
  players <- data.frame(
    player_id = c("a", "b", "c", "d", "e", "f", "g", "h"),
    fantasypros_id = c("fa", "fb", "fc", "fd", "fe", "ff", "fg", "fh"),
    franchise_id = c("7", "7", "7", "7", NA, "3", NA, "2"),
    stringsAsFactors = FALSE)
  # g and h were never drafted: priced off the proxy board, h past the last round
  adp <- data.frame(fantasypros_id = c("fg", "fh", "fe"), adp_rank = c(41L, 400L, 5L))

  x <- ffs_next_keeper_costs(players, picks, adp, n_teams = 8L, max_round = 17L)
  cost <- stats::setNames(x$keep_cost, x$player_id)
  expect_equal(nrow(x), 8L)
  expect_equal(x$player_id, players$player_id)     # input order kept
  expect_equal(unname(cost["a"]), 2L)    # kept at R3 -> R2
  expect_true(x$kept_now[x$player_id == "a"])
  expect_equal(unname(cost["b"]), 1L)    # R1 stays R1
  expect_equal(unname(cost["c"]), 1L)    # R2 -> R1 ...
  expect_true(all(x$top2[x$player_id %in% c("b", "c")]))  # ... and both top-two
  expect_equal(unname(cost["d"]), 13L)   # R14 -> R13
  # dropped draftee: his round sticks, the (much better) ADP is ignored
  expect_equal(unname(cost["e"]), 9L)
  expect_equal(x$cost_source[x$player_id == "e"], "draft")
  expect_equal(unname(cost["f"]), 5L)    # traded: still his drafted round
  expect_equal(unname(cost["g"]), 6L)    # undrafted: ceiling(41/8)
  expect_equal(x$cost_source[x$player_id == "g"], "adp")
  expect_equal(unname(cost["h"]), 17L)   # capped at the last round
  expect_true(is.na(x$draft_rd[x$player_id == "g"]))
})

test_that("round bar ignores keepers and reads a smooth fit at mid-round", {
  # 4-team draft whose non-keepers go exactly at rank = 2 * pick; a keeper taken
  # at pick 1 with rank 400 must not bend the fit
  pick_no <- c(1:8, 13:16)
  picks <- data.frame(round = ceiling(pick_no / 4), pick_no = pick_no,
                      adp_rank = 2 * pick_no,
                      is_keeper = c(TRUE, rep(NA, 11)))
  picks$adp_rank[1] <- 400
  b <- ffs_round_bar(picks, n_teams = 4L, n_rounds = 5L)
  expect_equal(b$round, 1:5)
  expect_equal(b$bar_rank, 2 * c(2.5, 6.5, 10.5, 14.5, 18.5), tolerance = 1e-8)
  expect_equal(b$n, c(3L, 4L, 0L, 4L, 0L))  # round 3 and 5 had no picks: fitted
  expect_true(is.na(b$med_rank[3]))
  expect_false(is.unsorted(b$bar_rank))

  # by slot: the snake turns, so slot 1 is best in odd rounds, worst in even
  s <- ffs_round_bar(picks, n_teams = 4L, n_rounds = 2L, slot = c(1L, 4L))
  expect_equal(s[slot == 1]$pick, c(1, 8))
  expect_equal(s[slot == 4]$pick, c(4, 5))
  expect_equal(s$bar_rank, 2 * s$pick, tolerance = 1e-8)
  expect_lt(s[slot == 1 & round == 1]$bar_rank, s[slot == 4 & round == 1]$bar_rank)
  expect_gt(s[slot == 1 & round == 2]$bar_rank, s[slot == 4 & round == 2]$bar_rank)
})

test_that("greedy keepers takes the best feasible set and skips infeasible pairs", {
  cand <- data.frame(player_id = c("r1a", "r1b", "late", "mid", "neg"),
                     base_round = c(1L, 1L, 13L, 6L, 9L),
                     top2_prev = c(TRUE, TRUE, FALSE, FALSE, FALSE),
                     surplus = c(900, 800, 700, 100, -50))
  k <- .ffs_greedy_keepers(cand, "surplus", max_keepers = 8, max_top2 = 1,
                           positive_only = TRUE)
  expect_equal(k, c("r1a", "late", "mid"))  # r1b collides at R1; neg is dropped
  expect_length(.ffs_greedy_keepers(cand, "surplus", max_keepers = 2), 2L)
})

test_that("round assignment bumps collisions up and forfeits the latest picks", {
  # three players all costing R15, plus R13 and R12 -> must occupy 11..15
  cst <- data.frame(player_id = c("a", "b", "c", "d", "e"),
                    base_round = c(15L, 15L, 15L, 13L, 12L), top2_prev = FALSE)
  a <- ffs_assign_keeper_rounds(cst, max_keepers = 8, max_top2 = 1)
  expect_true(a$feasible)
  expect_equal(a$rounds_used, 11:15)
  expect_true(all(a$assignment$keeper_round <= a$assignment$base_round))
  expect_equal(anyDuplicated(a$assignment$keeper_round), 0L)
})

test_that("two players costing round 1 is infeasible - cannot bump above R1", {
  cst <- data.frame(player_id = c("allen", "cmc"), base_round = c(1L, 1L),
                    top2_prev = c(TRUE, TRUE))
  # even with the house top-2 rule off, arithmetic forbids it
  expect_false(ffs_assign_keeper_rounds(cst, max_top2 = Inf)$feasible)
  r <- ffs_assign_keeper_rounds(cst, max_top2 = 1)
  expect_false(r$feasible)
  expect_match(r$reason, "top two")
})

test_that("assignment respects max_keepers and rejects unpriced players", {
  cst <- data.frame(player_id = c("a", "b"), base_round = c(5L, 6L), top2_prev = FALSE)
  expect_false(ffs_assign_keeper_rounds(cst, max_keepers = 1)$feasible)
  bad <- data.frame(player_id = "a", base_round = NA_integer_, top2_prev = FALSE)
  expect_false(ffs_assign_keeper_rounds(bad)$feasible)
})

test_that("snake pick map alternates and drops forfeited rounds", {
  slots <- stats::setNames(1:8, paste0("f", 1:8))
  pm <- ffs_draft_pick_map(slots, n_rounds = 17)
  expect_equal(nrow(pm), 8 * 17)
  expect_equal(anyDuplicated(pm$overall), 0L)
  # slot 8 picks last in odd rounds, first in even -> back-to-back at 8 and 9
  f8 <- pm[pm$franchise_id == "f8"][order(overall)]
  expect_equal(head(f8$overall, 3), c(8L, 9L, 24L))
  f1 <- pm[pm$franchise_id == "f1"][order(overall)]
  expect_equal(head(f1$overall, 3), c(1L, 16L, 17L))
  pm2 <- ffs_draft_pick_map(slots, 17, forfeited = list(f8 = c(1L, 2L)))
  expect_equal(nrow(pm2), 8 * 17 - 2)
  expect_false(any(pm2$franchise_id == "f8" & pm2$round %in% 1:2))
})

test_that("mock draft fills every pick exactly once with no duplicates", {
  slots <- stats::setNames(1:4, paste0("f", 1:4))
  pm <- ffs_draft_pick_map(slots, 8, forfeited = list(f1 = c(1L, 2L)))
  d <- ffs_mock_draft(mini_pool(), pm,
                      pos_need = c(QB = 1L, RB = 2L, WR = 3L, TE = 1L),
                      pos_cap = c(QB = 4, RB = Inf, WR = Inf, TE = Inf), seed = 1)
  expect_equal(nrow(d), nrow(pm))
  expect_equal(anyDuplicated(d$fantasypros_id), 0L)
  expect_equal(sum(d$franchise_id == "f1"), 6L)   # 8 rounds - 2 forfeited
})

test_that("mock draft never reaches outside the top window_k", {
  slots <- stats::setNames(1:4, paste0("f", 1:4))
  pm <- ffs_draft_pick_map(slots, 1)
  worst <- 0
  for (s in 1:100) {
    d <- ffs_mock_draft(mini_pool(), pm[1], pos_need = c(QB = 1L, RB = 2L, WR = 3L, TE = 1L),
                        pos_cap = c(QB = 4, RB = Inf, WR = Inf, TE = Inf),
                        window_k = 6L, seed = s)
    worst <- max(worst, d$ecr)
  }
  expect_lte(worst, 6)
})

test_that("pos_start_max stops stockpiling players who can never start", {
  # franchise already holds 3 QBs in a 2-QB-max lineup, with top-ranked QBs on
  # the board. Without the penalty it keeps taking them; with it, rarely.
  #
  # f1 needs more picks than unmet starter slots (10 picks vs 6 unmet), or the
  # hard backstop takes every pick deterministically from the unmet positions
  # and the pos_start_max weighting never runs at all.
  slots <- stats::setNames(1:2, c("f1", "f2"))
  pm <- ffs_draft_pick_map(slots, 10)
  kept <- data.table::data.table(franchise_id = "f1", pos = c("QB", "QB", "QB"))
  n_qb <- function(start_max) {
    mean(vapply(1:60, function(s) {
      d <- ffs_mock_draft(mini_pool(), pm, kept = kept,
                          pos_need = c(QB = 1L, RB = 2L, WR = 3L, TE = 1L),
                          pos_cap = c(QB = 6, RB = Inf, WR = Inf, TE = Inf),
                          pos_start_max = start_max, seed = s)
      sum(d$franchise_id == "f1" & d$pos == "QB")
    }, numeric(1)))
  }
  expect_gt(n_qb(NULL), n_qb(c(QB = 2L, RB = 5L, WR = 6L, TE = 4L)))
})

test_that("the hard starter floor is met when picks equal needs", {
  slots <- stats::setNames(1:4, paste0("f", 1:4))
  pm <- ffs_draft_pick_map(slots, 6)
  pm <- pm[pm$franchise_id == "f1"]              # exactly 6 picks
  kept <- data.table::data.table(franchise_id = "f1", pos = "QB")
  d <- ffs_mock_draft(mini_pool(), pm, kept = kept,
                      pos_need = c(QB = 1L, RB = 2L, WR = 3L, TE = 1L),
                      pos_cap = c(QB = 4, RB = Inf, WR = Inf, TE = Inf), seed = 1)
  tb <- table(d$pos)
  expect_gte(tb[["RB"]], 2)
  expect_gte(tb[["WR"]], 3)
  expect_gte(tb[["TE"]], 1)
})

test_that("bracket handles multi-week rounds and crowns exactly one champion", {
  set.seed(1)
  n_seas <- 40
  ss <- data.table::CJ(season = seq_len(n_seas), franchise_id = as.character(1:4))
  ss[, lg_rank := rep(1:4, n_seas)]
  opt <- data.table::CJ(season = seq_len(n_seas), week = 14:17,
                        franchise_id = as.character(1:4))
  opt[, actual_score := stats::rnorm(.N, 100, 15)]
  one <- ffsimulator:::.ffs_bracket_pct(opt, ss, 4L, 14:15, weeks_per_round = 1L)
  two <- ffsimulator:::.ffs_bracket_pct(opt, ss, 4L, 14:17, weeks_per_round = 2L)
  expect_equal(sum(one$champion_pct), 1, tolerance = 1e-8)
  expect_equal(sum(two$champion_pct), 1, tolerance = 1e-8)
  # too few weeks for the requested format -> warn, return empty
  expect_warning(z <- ffsimulator:::.ffs_bracket_pct(opt, ss, 4L, 14:15, weeks_per_round = 2L))
  expect_equal(nrow(z), 0L)
  # non-power-of-two field refused rather than silently mis-bracketed
  expect_warning(ffsimulator:::.ffs_bracket_pct(opt, ss, 6L, 14:17, weeks_per_round = 1L))
})

test_that("two-week rounds favour the stronger team more than one-week rounds", {
  # seed 1 is strictly better; aggregating two weeks should advance it more often
  set.seed(42)
  n_seas <- 400
  ss <- data.table::CJ(season = seq_len(n_seas), franchise_id = as.character(1:4))
  ss[, lg_rank := rep(1:4, n_seas)]
  opt <- data.table::CJ(season = seq_len(n_seas), week = 14:17,
                        franchise_id = as.character(1:4))
  mu <- c("1" = 115, "2" = 100, "3" = 100, "4" = 100)
  opt[, actual_score := stats::rnorm(.N, mu[franchise_id], 25)]
  one <- ffsimulator:::.ffs_bracket_pct(opt, ss, 4L, 14:15, weeks_per_round = 1L)
  two <- ffsimulator:::.ffs_bracket_pct(opt, ss, 4L, 14:17, weeks_per_round = 2L)
  expect_gt(two$champion_pct[two$franchise_id == "1"],
            one$champion_pct[one$franchise_id == "1"])
})
