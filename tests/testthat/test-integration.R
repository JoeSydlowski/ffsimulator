test_that("MFL simulation works", {
  skip_on_cran()

  foureight <- mfl_connect(2021, 22627, user_agent = "asdf23409lkjsafd")
  foureight_sim <- ff_simulate(foureight, n_seasons = 2)
  week_sim <- ff_simulate_week(foureight, n = 10, verbose = FALSE, actual_schedule = FALSE)

  checkmate::expect_list(foureight_sim, len = 7)
  checkmate::expect_list(week_sim, len = 6)
  checkmate::expect_data_frame(week_sim$summary_week, nrows = 120, any.missing = FALSE)
  checkmate::expect_data_frame(foureight_sim$summary_simulation, nrows = 12, any.missing = FALSE)
  checkmate::expect_data_frame(foureight_sim$summary_season, nrows = 24, any.missing = FALSE)
  checkmate::expect_data_frame(foureight_sim$summary_week, nrows = 336, any.missing = FALSE)
})

test_that("Sleeper simulation works", {
  skip_on_cran()

  jml <- ff_connect(platform = "sleeper", league_id = "652718526494253056", season = 2021)
  jml_sim <- ff_simulate(jml, n_seasons = 2, verbose = FALSE, return = "all")
  jml_week_sim <- ff_simulate_week(jml,
                                   n = 10,
                                   verbose = FALSE,
                                   actual_schedule = FALSE,
                                   replacement_level = TRUE,
                                   return = "all")

  checkmate::expect_list(jml_sim, len = 14)
  checkmate::expect_list(jml_week_sim, len = 13)
  checkmate::expect_data_frame(jml_week_sim$summary_simulation, nrows = 12, any.missing = FALSE)
  checkmate::expect_data_frame(jml_sim$summary_simulation, nrows = 12, any.missing = FALSE)
  checkmate::expect_data_frame(jml_sim$summary_season, nrows = 24, any.missing = FALSE)
  checkmate::expect_data_frame(jml_sim$summary_week, nrows = 336, any.missing = FALSE)
})

test_that("Fleaflicker simulation works", {
  skip_on_cran()

  got <- fleaflicker_connect(2020, 206154)
  got_sim <- ff_simulate(got, n_seasons = 2, verbose = FALSE, replacement_level = TRUE)

  checkmate::expect_list(got_sim, len = 7)
  checkmate::expect_data_frame(got_sim$summary_simulation, nrows = 16, any.missing = FALSE)
  checkmate::expect_data_frame(got_sim$summary_season, nrows = 32, any.missing = FALSE)
  checkmate::expect_data_frame(got_sim$summary_week, nrows = 448, any.missing = FALSE)
})

test_that("ESPN simulation works", {
  skip_on_cran()

  tony <- espn_connect(season = 2020, league_id = 899513)
  tony_sim <- ff_simulate(tony, n_seasons = 2, verbose = FALSE, replacement_level = FALSE)

  checkmate::expect_list(tony_sim, len = 7)
  checkmate::expect_data_frame(tony_sim$summary_simulation, nrows = 10, any.missing = FALSE)
  checkmate::expect_data_frame(tony_sim$summary_season, nrows = 20, any.missing = FALSE)
  checkmate::expect_data_frame(tony_sim$summary_week, nrows = 280, any.missing = FALSE)
})

test_that("Actual Schedule - completed_season = no sim", {
  skip_on_cran()
  ssb <- mfl_connect(2020, 54040, user_agent = "asdfafd")
  testthat::expect_message(ssb_sim <- ff_simulate(ssb, n_seasons = 2, actual_schedule = TRUE),
                           regexp = "No unplayed weeks")

  checkmate::expect_list(ssb_sim, len = 3)
})

test_that("wins added works", {
  skip_on_cran()
  ssb <- mfl_connect(2021, 54040)
  ssb_wa <- ff_wins_added(ssb, n_seasons = 2)

  checkmate::expect_list(ssb_wa, len = 15)
  checkmate::expect_data_frame(ssb_wa$war, min.rows = 200)
})

test_that("trade functions work", {
  skip_on_cran()
  conn <- mfl_connect(2021, 22627)
  sim <- ff_simulate(conn, n_seasons = 4, version = "v3",
                     lineup_method = "rank", return = "all")
  rs <- data.table::as.data.table(sim$roster_scores)
  real <- rs[!grepl("^(QB|RB|WR|TE|K)_\\d+$", player_id)]
  f_a <- real$franchise_id[[1]]
  f_b <- setdiff(unique(real$franchise_id), f_a)[[1]]
  p_a <- unique(real[franchise_id == f_a]$player_id)[[1]]
  p_b <- unique(real[franchise_id == f_b]$player_id)[[1]]

  pv <- ffs_player_value(sim, p_a, f_a) # removal (owner)
  pv2 <- ffs_player_value(sim, p_a, f_b) # addition (other team)
  checkmate::expect_data_frame(pv, nrows = 1)
  checkmate::expect_data_frame(pv2, nrows = 1)
  checkmate::expect_subset(
    c("h2h_wins", "allplay_winpct", "playoff_pct", "owner_id"), names(pv)
  )

  te <- ffs_trade_eval(sim, f_a, p_a, f_b, p_b)
  checkmate::expect_data_frame(te, nrows = 2)
  # zero-sum-ish structure: both rows report their own before/after
  expect_true(all(te$playoff_pct_after >= 0 & te$playoff_pct_after <= 1))

  tt <- ffs_trade_targets(sim, f_a, top_n = 3)
  checkmate::expect_data_frame(tt, nrows = 3)
  checkmate::expect_subset(c("value_to_you", "value_to_owner", "surplus"), names(tt))
})
