test_that("bracket order is the standard fixed layout", {
  expect_identical(.ffs_bracket_order(1), 1L)
  expect_identical(.ffs_bracket_order(2), c(1L, 2L))
  expect_identical(.ffs_bracket_order(4), c(1L, 4L, 2L, 3L))
  expect_identical(.ffs_bracket_order(8), c(1L, 8L, 4L, 5L, 2L, 7L, 3L, 6L))
})

toy_bracket <- function(n_teams, playoff_teams, n_seasons = 50, seed = 1) {
  set.seed(seed)
  strength <- data.table::data.table(franchise_id = as.character(seq_len(n_teams)),
                                     mu = stats::runif(n_teams, 90, 130),
                                     sd = stats::runif(n_teams, 15, 30))
  ss <- data.table::rbindlist(lapply(seq_len(n_seasons), function(s)
    data.table::data.table(season = s, franchise_id = as.character(sample(n_teams)),
                           lg_rank = seq_len(n_teams))))
  .ffs_champion_pct_core(strength, ss, playoff_teams)
}

test_that("title odds sum to one for any bracket size", {
  for (cfg in list(c(8, 4), c(10, 6), c(12, 8), c(12, 5), c(6, 1), c(12, 12))) {
    ch <- toy_bracket(cfg[[1]], cfg[[2]])
    expect_equal(sum(ch$champion_pct), 1, tolerance = 1e-9,
                 info = paste(cfg, collapse = "/"))
    expect_equal(sum(ch$top_seed_pct), 1, tolerance = 1e-9)
  }
})

test_that("a six-team bracket gives the top two seeds a bye", {
  strength <- data.table::data.table(franchise_id = letters[1:6], mu = 100, sd = 20)
  ss <- data.table::data.table(season = 1, franchise_id = letters[1:6], lg_rank = 1:6)
  ch <- .ffs_champion_pct_core(strength, ss, 6L)
  # equal teams: a bye seed needs 2 wins (1/4), everyone else 3 (1/8)
  expect_equal(ch[franchise_id == "a"]$champion_pct, 0.25)
  expect_equal(ch[franchise_id == "b"]$champion_pct, 0.25)
  expect_equal(ch[franchise_id == "f"]$champion_pct, 0.125)
})

test_that("guillotine odds: the team that always scores least goes first", {
  opt <- data.table::CJ(season = 1:200, week = 2:4, franchise_id = c("a", "b", "c", "d"))
  set.seed(3)
  opt[, actual_score := stats::rnorm(.N, 120, 15)]
  opt[franchise_id == "d", actual_score := 10]
  sim <- structure(list(optimal_scores = opt), class = "ff_simulation")
  g <- ffs_guillotine_odds(sim, c("a", "b", "c", "d"), weeks = 2:4)
  expect_equal(g[franchise_id == "d"]$survive_wk2, 0)
  expect_equal(g[franchise_id == "d"]$win_pct, 0)
  expect_equal(sum(g$win_pct), 1)
  # one elimination a week: survivors after week w = teams - w
  expect_equal(sum(g$survive_wk2), 3)
  expect_equal(sum(g$survive_wk4), 1)
})

test_that("playoff odds count banked wins and the league's berths", {
  sim <- .ffs_cache_example("trade_sim.rds")
  skip_if(is.null(sim$optimal_scores) || is.null(sim$schedules), "example simulation lacks optimal_scores")
  fids <- unique(as.character(sim$optimal_scores$franchise_id))
  sim$simulation_params$playoff_teams <- 4L
  base <- ffs_playoff_odds(sim)
  expect_equal(sum(base$playoff_pct), 4, tolerance = 1e-9)
  expect_equal(sum(base$champion_pct), 1, tolerance = 1e-9)
  expect_true(all(is.na(base$bye_pct)))

  # bank a big lead for the weakest team and it should climb
  weakest <- base[which.min(playoff_pct)]$franchise_id
  sim$standings <- data.table::data.table(franchise_id = fids,
                                          wins = ifelse(fids == weakest, 6, 0),
                                          losses = ifelse(fids == weakest, 0, 3),
                                          ties = 0, points_for = 0)
  boosted <- ffs_playoff_odds(sim)
  expect_gt(boosted[franchise_id == weakest]$playoff_pct,
            base[franchise_id == weakest]$playoff_pct)
  expect_equal(boosted[franchise_id == weakest]$wins_now, 6)
  expect_equal(sum(boosted$playoff_pct), 4, tolerance = 1e-9)
})

test_that("team needs cover every franchise and position", {
  sim <- .ffs_cache_example("trade_sim.rds")
  skip_if(is.null(sim$optimal_scores) || is.null(sim$roster_scores),
          "example simulation lacks return = 'all' components")
  nd <- ffs_team_needs(sim)
  n_f <- length(unique(as.character(sim$optimal_scores$franchise_id)))
  expect_equal(nrow(nd), n_f * length(unique(nd$pos)))
  expect_true(all(nd[, min(need_rank), by = franchise_id]$V1 == 1L))
})

test_that("posture labels follow the odds and roster age", {
  odds <- data.table::data.table(franchise_id = c("1", "2", "3", "4"),
                                 playoff_pct = c(0.9, 0.5, 0.1, 0.1),
                                 champion_pct = c(0.4, 0.2, 0.05, 0.05))
  players <- data.table::data.table(franchise_id = c("1", "2", "3", "4"),
                                    age = c(26, 26, 30, 23), value = 1000)
  p <- ffs_team_posture(odds, players, "dynasty")
  expect_identical(p$posture, c("Contender", "Bubble", "Seller (aging roster)", "Rebuilding"))
  r <- ffs_team_posture(odds, players, "redraft")
  expect_identical(r$posture[3:4], c("Long shot", "Long shot"))
})
