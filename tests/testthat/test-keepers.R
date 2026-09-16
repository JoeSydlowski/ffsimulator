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
})

test_that("a non-power-of-two field plays byes for the top seeds", {
  # 12 teams, 6 berths: seeds 1-2 sit out round one, 3v6 and 4v5 play. Every
  # team is equally good, so the byes are the only thing separating them.
  set.seed(7)
  n_seas <- 3000
  ss <- data.table::CJ(season = seq_len(n_seas), franchise_id = as.character(1:12))
  data.table::setorder(ss, season, franchise_id)
  ss[, lg_rank := as.integer(sub("^", "", franchise_id))]
  opt <- data.table::CJ(season = seq_len(n_seas), week = 15:17,
                        franchise_id = as.character(1:12))
  opt[, actual_score := stats::rnorm(.N, 100, 20)]

  ch <- ffsimulator:::.ffs_bracket_pct(opt, ss, 6L, 15:17, weeks_per_round = 1L)
  expect_equal(nrow(ch), 6L)                                  # only real seeds
  expect_equal(sum(ch$champion_pct), 1, tolerance = 1e-8)
  expect_false(any(grepl("bye", ch$franchise_id)))
  expect_setequal(ch$franchise_id, as.character(1:6))

  pct <- stats::setNames(ch$champion_pct, ch$franchise_id)
  # a bye is worth one fewer game to win: ~1/4 each for seeds 1-2, ~1/8 for 3-6
  expect_equal(unname(pct["1"]), 0.25, tolerance = 0.03)
  expect_equal(unname(pct["5"]), 0.125, tolerance = 0.03)
  expect_gt(min(pct[c("1", "2")]), max(pct[c("3", "4", "5", "6")]))
})

test_that("padding leaves a power-of-two field exactly as it was", {
  set.seed(11)
  n_seas <- 200
  ss <- data.table::CJ(season = seq_len(n_seas), franchise_id = as.character(1:8))
  data.table::setorder(ss, season, franchise_id)
  ss[, lg_rank := as.integer(franchise_id)]
  opt <- data.table::CJ(season = seq_len(n_seas), week = 15:17,
                        franchise_id = as.character(1:8))
  opt[, actual_score := stats::rnorm(.N, 100, 20)]
  ch <- ffsimulator:::.ffs_bracket_pct(opt, ss, 8L, 15:17, weeks_per_round = 1L)
  expect_equal(nrow(ch), 8L)
  expect_equal(sum(ch$champion_pct), 1, tolerance = 1e-8)
  expect_setequal(ch$franchise_id, as.character(1:8))
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

# --- scripted builds and board shifts ---------------------------------------

# a pool with no cross-position ecr ties, so window_k = 1 makes the draft
# deterministic and the board order is the only thing under test
shift_pool <- function() {
  data.table::data.table(
    fantasypros_id = paste0("p", 1:80),
    player = paste0("Player", 1:80),
    pos = rep(c("RB", "QB", "WR", "TE"), each = 20),
    ecr = 1:80
  )
}

test_that("pos_shift reorders the board without touching ecr", {
  slots <- stats::setNames(1:4, paste0("f", 1:4))
  pm <- ffs_draft_pick_map(slots, 8)
  args <- list(pool = shift_pool(), pick_map = pm,
               pos_need = c(QB = 1L, RB = 1L, WR = 1L, TE = 1L),
               pos_cap = c(QB = Inf, RB = Inf, WR = Inf, TE = Inf),
               window_k = 1L)

  plain <- do.call(ffs_mock_draft, args)
  expect_equal(plain$pos[[1]], "RB")            # RB1 is the best ecr

  bumped <- do.call(ffs_mock_draft, c(args, list(pos_shift = c(QB = 25))))
  expect_equal(bumped$pos[[1]], "QB")           # QBs jump 20 ranks
  expect_equal(bumped$ecr[[1]], 21)              # reported ecr is untouched

  # a shift for a position that is not in the pool is a no-op, not an error
  noop <- do.call(ffs_mock_draft, c(args, list(pos_shift = c(K = 50))))
  expect_equal(noop$fantasypros_id, plain$fantasypros_id)
})

test_that("policy scripts one seat's build and leaves the rest autodrafting", {
  slots <- stats::setNames(1:4, paste0("f", 1:4))
  pm <- ffs_draft_pick_map(slots, 8)
  need <- c(QB = 1L, RB = 2L, WR = 3L, TE = 1L)

  # f1 takes WR with its first three picks, TE with the fourth, then BPA
  pol <- function(franchise_id, round, have) {
    if (franchise_id != "f1") return(character(0))
    if (round <= 3L) "WR" else if (round == 4L) "TE" else character(0)
  }
  d <- ffs_mock_draft(shift_pool(), pm, pos_need = need,
                      pos_cap = c(QB = Inf, RB = Inf, WR = Inf, TE = Inf),
                      policy = pol, seed = 3)

  f1 <- d[d$franchise_id == "f1"][order(round)]
  expect_equal(f1$pos[1:3], rep("WR", 3))
  expect_equal(f1$pos[[4]], "TE")
  expect_equal(nrow(d), nrow(pm))
  expect_equal(anyDuplicated(d$fantasypros_id), 0L)

  # the guarantee still holds for everyone, f1 included
  for (fid in paste0("f", 1:4)) {
    tb <- table(factor(d$pos[d$franchise_id == fid], levels = names(need)))
    expect_true(all(tb >= need), info = fid)
  }
})

test_that("a policy cannot starve a roster or outrank the hard backstop", {
  slots <- stats::setNames(1:4, paste0("f", 1:4))
  pm <- ffs_draft_pick_map(slots, 7)
  need <- c(QB = 1L, RB = 2L, WR = 3L, TE = 1L)   # 7 needs, 7 picks - no slack

  # an adversarial policy that only ever wants QBs
  d <- ffs_mock_draft(shift_pool(), pm, pos_need = need,
                      pos_cap = c(QB = Inf, RB = Inf, WR = Inf, TE = Inf),
                      policy = function(...) "QB", seed = 5)
  expect_equal(nrow(d), nrow(pm))
  for (fid in paste0("f", 1:4)) {
    tb <- table(factor(d$pos[d$franchise_id == fid], levels = names(need)))
    expect_true(all(tb >= need), info = fid)
  }

  # asking for a position that is exhausted falls back rather than losing a pick
  small <- shift_pool()[shift_pool()$pos != "TE"]
  d2 <- ffs_mock_draft(small, ffs_draft_pick_map(slots, 5),
                       pos_need = c(QB = 1L, RB = 1L, WR = 1L),
                       pos_cap = c(QB = Inf, RB = Inf, WR = Inf),
                       policy = function(...) "TE", seed = 5)
  expect_equal(nrow(d2), 20L)
})


test_that("pos_shift_exempt lets one seat draft the true board", {
  slots <- stats::setNames(1:4, paste0("f", 1:4))
  pm <- ffs_draft_pick_map(slots, 6)
  args <- list(pool = shift_pool(), pick_map = pm,
               pos_need = c(QB = 1L, RB = 1L, WR = 1L, TE = 1L),
               pos_cap = c(QB = Inf, RB = Inf, WR = Inf, TE = Inf),
               window_k = 1L, pos_shift = c(QB = 25))

  # everyone reaches: the whole first round is QBs (QB block is ecr 21-40,
  # shifted to -4..15, so it outranks RB1 at ecr 1)
  all_reach <- do.call(ffs_mock_draft, args)
  expect_equal(all_reach$pos[1:4], rep("QB", 4))

  # f1 exempt: it takes the true board leader while the rest still reach
  one_out <- do.call(ffs_mock_draft, c(args, list(pos_shift_exempt = "f1")))
  r1 <- one_out[one_out$round == 1L][order(overall)]
  expect_equal(r1$pos[r1$franchise_id == "f1"], "RB")
  expect_true(all(r1$pos[r1$franchise_id != "f1"] == "QB"))

  # and exempting nobody is identical to not passing the argument at all
  same <- do.call(ffs_mock_draft, c(args, list(pos_shift_exempt = character(0))))
  expect_equal(same$fantasypros_id, all_reach$fantasypros_id)
})

