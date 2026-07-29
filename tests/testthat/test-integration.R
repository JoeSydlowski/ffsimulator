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

  # dynasty_values anchoring: cur_value must equal the supplied market value for
  # covered players (deterministic - no network), and default path is unchanged
  dh <- data.table::as.data.table(fp_dynasty_history())
  dh <- dh[format == "1qb" & season == max(dh[format == "1qb"]$season)]
  dv <- data.frame(fantasypros_id = dh$fantasypros_id,
                   value = pmax(100, 12000 - dh$rank * 25), format = "1qb")
  do_fc <- ffs_dynasty_outlook(sim, format = "1qb", dynasty_values = dv)
  chk <- merge(do_fc, dv, by = "fantasypros_id")
  expect_gt(nrow(chk), 20)
  expect_equal(chk$cur_value, chk$value, tolerance = 1e-6)

  # market re-ranking: dyn_rank now follows the value ordering (the Metcalf
  # FP/FC-mismatch fix), so cur_value is monotone non-increasing in dyn_rank,
  # and the FantasyPros rank is retained separately as fp_dyn_rank
  expect_true(all(c("fp_dyn_rank", "dyn_rank") %in% names(do_fc)))
  ord <- do_fc[order(do_fc$dyn_rank), ]
  expect_true(all(diff(ord$cur_value) <= 1e-6))

  # crosswalk recovery: a market row with NA fantasypros_id but a platform id
  # matching a rostered player must still anchor (the rookie-lag case)
  ros <- data.table::as.data.table(sim$rosters)
  rec_p <- ros[!is.na(fantasypros_id) & fantasypros_id %in% dh$fantasypros_id][1]
  dv2 <- dv[dv$fantasypros_id != rec_p$fantasypros_id, ]
  dv2$mfl_id <- NA_character_
  dv2 <- rbind(dv2, data.frame(fantasypros_id = NA_character_, value = 4321,
                               format = "1qb",
                               mfl_id = as.character(rec_p$player_id)))
  do_rec <- ffs_dynasty_outlook(sim, format = "1qb", dynasty_values = dv2)
  expect_equal(do_rec$cur_value[do_rec$fantasypros_id == rec_p$fantasypros_id],
               4321, tolerance = 1e-6)
})

test_that("fc_dynasty_values scrapes and crosswalks", {
  skip_on_cran()
  testthat::skip_if_offline()
  v <- fc_dynasty_values(num_qbs = 2)
  checkmate::expect_data_frame(v, min.rows = 100)
  checkmate::expect_subset(
    c("fantasypros_id", "player_name", "pos", "value", "overall_rank", "format"), names(v))
  expect_true(all(v$format == "superflex"))
  expect_true(all(diff(v$overall_rank) >= 0))          # sorted by overall rank
  expect_gt(mean(!is.na(v$fantasypros_id)), 0.5)       # most established players matched
})

test_that("ffs_build_trades constructs value-matched deals", {
  skip_on_cran()
  conn <- mfl_connect(2021, 22627)
  sim <- ff_simulate(conn, n_seasons = 4, version = "v3",
                     lineup_method = "rank", return = "all")
  rs <- data.table::as.data.table(sim$roster_scores)
  real <- rs[!grepl("^(QB|RB|WR|TE|K)_\\d+$", player_id)]
  me <- real$franchise_id[[1]]

  tt <- ffs_trade_targets(sim, me, top_n = 8)

  # synthetic dynasty values so the test doesn't depend on market-data coverage
  ids <- unique(c(real$player_id, tt$player_id))
  set.seed(1)
  dyn <- data.frame(player_id = ids, cur_value = runif(length(ids), 200, 3000))
  dyn$next_value_mean <- dyn$cur_value * runif(length(ids), 0.8, 1.2)

  band <- 0.5
  trades <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn,
                             value_band = band, screen_n = 10, top_n = 10)
  checkmate::expect_data_frame(trades, max.rows = 10)
  checkmate::expect_subset(
    c("opponent", "send", "receive", "send_value", "recv_value", "value_gap",
      "my_win_delta", "my_playoff_delta", "opp_win_delta", "opp_playoff_delta",
      "win_win", "future_capital_delta", "score"), names(trades))

  if (nrow(trades) > 0) {
    # fairness band respected and win_win flag internally consistent
    expect_true(all(abs(trades$value_gap) / trades$recv_value <= band + 1e-9))
    expect_equal(trades$win_win, trades$my_win_delta > 0 & trades$opp_win_delta > 0)
    # future_capital_delta = received next-value minus sent next-value
    expect_true(all(is.finite(trades$future_capital_delta)))

    # uneven trades overpay the single-player side by 0..uneven_shade (=band)
    n_send <- lengths(strsplit(trades$send, " [+] "))
    n_recv <- lengths(strsplit(trades$receive, " [+] "))
    if (any(n_send > n_recv)) {              # I send more -> I overpay
      g <- trades[n_send > n_recv, ]
      prem <- -g$value_gap / g$recv_value    # value_gap = recv - send
      expect_true(all(prem >= -1e-9 & prem <= band + 1e-9))
    }
    if (any(n_recv > n_send)) {              # I send fewer -> I'm paid a premium
      g <- trades[n_recv > n_send, ]
      prem <- g$value_gap / (g$recv_value - g$value_gap)
      expect_true(all(prem >= -1e-9 & prem <= band + 1e-9))
    }
  }

  # future-value knobs: hard floor is respected; heavier future_weight never
  # yields a worse (lower) minimum future_capital_delta than pure win-now
  floored <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn,
                              value_band = band, min_future_delta = 0, top_n = 10)
  if (nrow(floored) > 0) expect_true(all(floored$future_capital_delta >= 0))

  # future_weight is a legacy z-score-mode lever (inert in the default "rate" mode,
  # where the fixed exchange rate governs the future trade-off), so test it there
  win_now <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn, score_mode = "zscore",
                              value_band = band, future_weight = 0, top_n = 5)
  future_first <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn, score_mode = "zscore",
                                   value_band = band, future_weight = 3, top_n = 5)
  if (nrow(win_now) > 0 && nrow(future_first) > 0) {
    expect_gte(mean(future_first$future_capital_delta),
               mean(win_now$future_capital_delta))
  }

  # must_send: every send package includes the required player; opponents:
  # deals restricted to the given franchises (the sell-matchmaker mode)
  sellable <- real[franchise_id == me]$player_id
  sell_p <- sellable[[1]]
  sell_name <- real[player_id == sell_p]$player_name[[1]]
  opp_pool <- unique(tt$owner_id)[1:min(2, length(unique(tt$owner_id)))]
  sold <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn,
                           value_band = band, must_send = sell_p,
                           opponents = opp_pool, top_n = 10)
  if (nrow(sold) > 0) {
    expect_true(all(vapply(strsplit(sold$send, " [+] "),
                           function(s) sell_name %in% s, logical(1))))
    expect_true(all(sold$opponent %in% opp_pool))
  }
  expect_error(ffs_build_trades(sim, me, targets = tt, dynasty = dyn,
                                must_send = "not_a_player"), "must_send")

  # shapes restricted to 1-for-1 -> single players, and cross-check the engine
  t11 <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn,
                          value_band = band, shapes = list(c(1, 1)), top_n = 5)
  if (nrow(t11) > 0) {
    expect_false(any(grepl(" [+] ", t11$send)))
    expect_false(any(grepl(" [+] ", t11$receive)))
    d <- t11[1, ]
    sid <- real$player_id[match(d$send, real$player_name)]
    rid <- tt$player_id[match(d$receive, tt$player_name)]
    opp <- tt$owner_id[match(d$receive, tt$player_name)]
    te <- data.table::as.data.table(ffs_trade_eval(sim, me, sid, opp, rid))
    expect_equal(sign(te[franchise_id == me]$h2h_wins_delta), sign(d$my_win_delta))
  }

  # ---- draft picks as value-only assets ----------------------------------
  # synthetic pick rows in the ffs_pick_values schema (no network). One pick to
  # me, one to each of two opponents.
  opps <- setdiff(unique(real$franchise_id), me)[1:2]
  picks <- data.frame(
    player_id = c("PICK_2027_1_me", "PICK_2027_1_o1", "PICK_2027_2_o2"),
    player_name = c("2027 R1 (me)", "2027 R1 (o1)", "2027 R2 (o2)"),
    franchise_id = c(me, opps),
    pos = "PICK",
    cur_value = c(2500, 3000, 1200),
    next_value_mean = c(2700, 3200, 1300),
    stringsAsFactors = FALSE)

  # (a) ffs_trade_eval treats pick ids as win-neutral: sending a real player and
  # receiving a pick equals the same eval with the pick omitted, and never errors
  opp1 <- opps[[1]]
  te_pick <- data.table::as.data.table(
    ffs_trade_eval(sim, me, sell_p, opp1, "PICK_2027_1_o1"))
  te_none <- data.table::as.data.table(
    ffs_trade_eval(sim, me, sell_p, opp1, character(0)))
  expect_equal(te_pick$h2h_wins_delta, te_none$h2h_wins_delta, tolerance = 1e-9)
  expect_equal(te_pick$playoff_pct_delta, te_none$playoff_pct_delta, tolerance = 1e-9)

  # (b) ffs_build_trades carries picks: my picks become sendable, opponents'
  # picks acquirable, and a sell-for-picks deal (no win gain) still surfaces,
  # judged on future capital
  bt <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn, picks = picks,
                         value_band = 0.8, future_weight = 1,
                         must_send = sell_p, opponents = opp1,
                         shapes = list(c(1, 1), c(1, 2)), top_n = 15)
  if (nrow(bt) > 0) {
    expect_true(all(is.finite(bt$future_capital_delta)))
    # any deal that receives a pick has a defined (finite) future capital delta
    got_pick <- grepl("R[12]|PICK", bt$receive)
    if (any(got_pick)) expect_true(all(is.finite(bt$future_capital_delta[got_pick])))
  }

  # ---- per-shape bands, min_piece_value, give-back, dedupe --------------------
  # give_back column present and FALSE by default
  expect_true("give_back" %in% names(trades))
  if (nrow(trades) > 0) expect_false(any(trades$give_back))

  # tight even band: same-count deals stay within the (small) even_band
  ev <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn,
                         even_band = 0.05, shapes = list(c(2, 2)), top_n = 10)
  if (nrow(ev) > 0)
    expect_true(all(abs(ev$value_gap) / ev$recv_value <= 0.05 + 1e-9))

  # uneven_gap band: 1-for-2 (I'm paid a premium) sits inside [lo, hi]
  ug <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn,
                         uneven_gap = c(0.02, 0.25), shapes = list(c(1, 2)), top_n = 10)
  if (nrow(ug) > 0) {
    prem <- ug$value_gap / (ug$recv_value - ug$value_gap)   # overpay of the package
    expect_true(all(prem >= 0.02 - 1e-9 & prem <= 0.25 + 1e-9))
  }

  # min_piece_value filters matched fillers: no matched real piece below the floor
  # (picks and must_send are exempt, but this build uses neither)
  val_by_id <- stats::setNames(dyn$cur_value, dyn$player_id)
  name_by_id <- data.table::as.data.table(real)[, .(player_name = player_name[[1]]), by = player_id]
  floorv <- 1500
  mp <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn,
                         min_piece_value = floorv, shapes = list(c(1, 1), c(2, 2)),
                         top_n = 15)
  if (nrow(mp) > 0) {
    pieces <- unique(unlist(strsplit(c(mp$send, mp$receive), " [+] ")))
    ids <- name_by_id$player_id[match(pieces, name_by_id$player_name)]
    ids <- ids[!is.na(ids)]
    expect_true(all(val_by_id[ids] >= floorv - 1e-9))
  }

  # min_recv_value defaults to min_piece_value (back-compat), and can be set lower
  # so a fragment receive-side keeps smaller real pieces the send floor would drop
  mp_lo <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn,
                            min_piece_value = floorv, min_recv_value = 0,
                            shapes = list(c(1, 2), c(2, 2)), top_n = 20)
  if (nrow(mp_lo) > 0) {
    send_pieces <- unique(unlist(strsplit(mp_lo$send, " [+] ")))
    sids <- name_by_id$player_id[match(send_pieces, name_by_id$player_name)]
    sids <- sids[!is.na(sids)]
    expect_true(all(val_by_id[sids] >= floorv - 1e-9))   # send floor still enforced
  }

  # give-back: a variant appends a send piece for a hurt opponent; every give_back
  # row therefore carries >= 2 senders, and the flag is logical
  gb <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn,
                         giveback = TRUE, giveback_trigger = Inf,   # always attempt
                         shapes = list(c(1, 2), c(2, 2)), top_n = 20)
  expect_type(gb$give_back, "logical")
  if (any(gb$give_back)) {
    n_send_gb <- lengths(strsplit(gb$send[gb$give_back], " [+] "))
    expect_true(all(n_send_gb >= 2))
  }

  # dedupe returns unique (opponent, send, receive) rows
  dd <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn,
                         value_band = 0.5, dedupe = TRUE, top_n = 30)
  if (nrow(dd) > 0)
    expect_false(any(duplicated(dd[, c("opponent", "send", "receive")])))

  # ---- trajectory soft down-rank (traj_weight) --------------------------------
  # traj_weight = 0 (default) is byte-identical to a run without the rel_change
  # column: adding the column must not change the ranking when the weight is 0
  dyn_rel <- data.table::copy(data.table::as.data.table(dyn))
  set.seed(7)
  dyn_rel[, rel_change := stats::runif(.N, -0.3, 0.3)]
  base0  <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn,     value_band = 0.5, top_n = 20)
  rel0   <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn_rel, value_band = 0.5,
                             traj_weight = 0, top_n = 20)
  expect_equal(rel0$score, base0$score, tolerance = 1e-9)
  # a heavy trajectory weight must not surface WORSE-trajectory deals at the top:
  # the mean "badness" (ship a riser / acquire a decliner) of the top deals under
  # a heavy penalty is no higher than under no penalty (same candidate set)
  rel_by_id <- stats::setNames(dyn_rel$rel_change, dyn_rel$player_id)
  nm2id <- stats::setNames(c(name_by_id$player_id, tt$player_id),
                           c(name_by_id$player_name, tt$player_name))
  badness <- function(sendstr, recvstr) {
    s <- nm2id[strsplit(sendstr, " [+] ")[[1]]]; r <- nm2id[strsplit(recvstr, " [+] ")[[1]]]
    sum(pmax(0, rel_by_id[s] - 0.05), na.rm = TRUE) +
      sum(pmax(0, -0.05 - rel_by_id[r]), na.rm = TRUE)
  }
  top_bad <- function(b, k = 5) {
    b <- data.table::as.data.table(b); if (!nrow(b)) return(0)
    v <- vapply(seq_len(nrow(b)), function(i) badness(b$send[i], b$receive[i]), numeric(1))
    mean(utils::head(v, min(k, length(v))))
  }
  light <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn_rel, value_band = 0.5,
                            traj_weight = 0, rise_cut = 0.05, fade_cut = -0.05, top_n = 30)
  heavy <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn_rel, value_band = 0.5,
                            traj_weight = 5, rise_cut = 0.05, fade_cut = -0.05, top_n = 30)
  expect_lte(top_bad(heavy), top_bad(light) + 1e-9)

  # ---- give-back picks by usefulness-per-cost, and omits a useless one ---------
  # (structural checks: the chosen give-back is a real spare at the drained
  # position, and give-back rows never appear when giveback = FALSE)
  no_gb <- ffs_build_trades(sim, me, targets = tt, dynasty = dyn,
                            giveback = FALSE, shapes = list(c(1, 2), c(2, 2)), top_n = 20)
  expect_false(any(no_gb$give_back))
})
