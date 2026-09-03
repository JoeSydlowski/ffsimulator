# Byes have to reach the LINEUP SETTER, not just the score.
#
# projected_score has always carried the bye (`projection * gp_model *
# (week != bye)`), but `avg_week` - the tier average joined from adp_outcomes,
# and the column rank-based lineup setting actually sorts on - did not. So the
# optimiser ranked a player on his bye by his full-season average, started him,
# and banked a certain zero. Measured on a live n=2000 Goofball simulation
# before the fix: 9.76% of ALL starter slots scored zero, and 21,576 of those
# 27,325 starts were players the optimiser believed were available - led by the
# best players on the roster, who are exactly the ones a season average ranks
# highest.
#
# The second bug in the same expression: `bye` is NA for anyone without a team,
# and `week != NA` is NA, not TRUE. Two rostered players in that same league
# (both team = "FA" in the scrape) carried NA for all 28,000 of their simulated
# player-weeks.

test_that(".ffs_plays_this_week is NULL-safe on a missing bye", {
  expect_identical(.ffs_plays_this_week(c(5, 7), c(7, 7)), c(1, 0))
  # the whole point: no team, no bye, still plays - `week != NA` would be NA
  expect_identical(.ffs_plays_this_week(c(5, 7), c(NA_real_, NA_real_)), c(1, 1))
  expect_false(anyNA(.ffs_plays_this_week(1:14, rep(NA_real_, 14))))
})

test_that("a bye zeroes the score AND makes the player unrankable", {
  skip_on_cran()
  adp <- .ffs_cache_example("adp_outcomes.rds")
  lr <- data.table::as.data.table(.ffs_cache_example("latest_rankings.rds"))
  skip_if(is.null(adp) || is.null(lr), "cached projection fixtures unavailable")

  # one player with a real bye, one with none (the FA case)
  lr <- lr[!is.na(ecr) & !is.na(sd)][order(ecr)]
  keep <- lr[, utils::head(.SD, 3), by = "pos"]
  keep[, bye := 7]
  keep[seq_len(2), bye := NA_real_]        # the "team = FA" shape
  ros <- keep[, list(fantasypros_id)]

  set.seed(1)
  for (v in c("v2", "v3")) {
    ps <- data.table::as.data.table(ffs_generate_projections(
      adp_outcomes = adp, latest_rankings = keep, n_seasons = 2L,
      weeks = 1:8, version = v, rosters = ros))
    skip_if(!nrow(ps), paste("fixture produced no", v, "projections"))

    # a missing bye must never poison the season
    expect_false(anyNA(ps$projected_score), info = v)

    on_bye <- ps[!is.na(bye) & week == bye]
    skip_if(!nrow(on_bye), paste("no bye weeks in the", v, "fixture"))
    expect_true(all(on_bye$projected_score == 0), info = v)
    # THE FIX: unrankable, so .ffs_optimise_started cannot start him
    expect_true(all(is.na(on_bye$avg_week)), info = v)

    # and a non-bye week is untouched - the mask must not blank the season
    off <- ps[is.na(bye) | week != bye]
    expect_true(any(!is.na(off$avg_week)), info = v)
  }
})

test_that("the optimiser stops starting players on their bye", {
  skip_on_cran()
  sim <- .ffs_cache_example("trade_sim.rds")
  rs <- data.table::as.data.table(sim$roster_scores)
  skip_if(!all(c("avg_week", "projected_score") %in% names(rs)), "fixture too old")
  lc <- .ffs_sim_lineup_constraints(sim)
  fid <- sort(unique(rs$franchise_id))[[1]]

  mine <- rs[franchise_id == fid]
  # force a bye on the single best player: score zero, and (post-fix) NA avg_week
  best <- mine[, list(m = mean(data.table::fcoalesce(as.numeric(avg_week), 0))),
               by = "player_id"][order(-m)]$player_id[[1]]
  wk <- mine[player_id == best]$week[[1]]

  bye_scored_only <- data.table::copy(mine)
  bye_scored_only[player_id == best & week == wk, projected_score := 0]
  bye_fully <- data.table::copy(bye_scored_only)
  bye_fully[player_id == best & week == wk, avg_week := NA_real_]

  started <- function(x) {
    o <- .ffs_optimise_started(x[week == wk], lc)
    unlist(o$starter_player_id)
  }
  # zeroing the score alone leaves him top-ranked, so he is started for nothing
  expect_true(best %in% started(bye_scored_only))
  # blanking avg_week too is what actually benches him
  expect_false(best %in% started(bye_fully))
})
