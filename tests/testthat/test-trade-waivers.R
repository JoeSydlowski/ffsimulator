# Waiver backfill and cut-to-size.
#
# A trade that is not one-for-one changes how many BODIES each side carries. The
# base evaluation prices that as free - the side that sent two for one simply
# carries a smaller roster forever - which is wrong in any league with a live
# waiver wire and a roster cap. These cases assert the two halves of the fix and,
# more importantly, that turning it OFF changes nothing.

# A pool of waiver proxies derived from the shipped fixture: two positions, two
# donor alternates each, so donor-avoidance has something to fall through to.
# `strong = TRUE` clones the BEST players at the position, which is not a
# realistic wire but is what makes the backfill's effect visible in a fixture
# small enough to run in a test.
.trade_sim_with_waivers <- function(sim = NULL, strong = TRUE, positions = c("RB", "WR")) {
  if (is.null(sim)) sim <- .ffs_cache_example("trade_sim.rds")
  rs <- data.table::as.data.table(sim$roster_scores)
  avg <- rs[, list(m = mean(data.table::fcoalesce(as.numeric(avg_week), 0))),
            by = c("player_id", "pos")]
  data.table::setorderv(avg, "m", if (strong) -1L else 1L)

  w <- data.table::rbindlist(lapply(positions, function(p) {
    ids <- avg[avg$pos == p]$player_id
    if (length(ids) < 2) return(NULL)
    data.table::rbindlist(lapply(1:2, function(k) {
      r <- data.table::copy(rs[rs$player_id == ids[k]])
      data.table::set(r, j = "player_id", value = paste0("WAIVER_", p, "_1_", k))
      data.table::set(r, j = "franchise_id", value = NA_character_)
      if ("franchise_name" %in% names(r))
        data.table::set(r, j = "franchise_name", value = NA_character_)
      if ("player_name" %in% names(r))
        data.table::set(r, j = "player_name", value = paste0("Waiver ", p, " ", k))
      data.table::set(r, j = "waiver_slot", value = paste0(p, "_1"))
      data.table::set(r, j = "donor_id", value = ids[k])
      data.table::set(r, j = "donor_rank", value = k)
      r
    }))
  }))
  sim$waiver_scores <- w
  sim
}

# a franchise pair and a 2-for-1 out of the fixture's tradeable players
.waiver_deal <- function(sim, seed = 1L) {
  rs <- data.table::as.data.table(sim$roster_scores)
  pool <- unique(rs[, list(franchise_id, player_id)])
  pool <- pool[!grepl("^(QB|RB|WR|TE|K)_[0-9]+$", pool$player_id)]
  fids <- sort(unique(stats::na.omit(rs$franchise_id)))
  set.seed(seed)
  fa <- fids[[1]]; fb <- fids[[2]]
  list(fa = fa, fb = fb,
       ga = sample(pool[pool$franchise_id == fa]$player_id, 2),
       gb = sample(pool[pool$franchise_id == fb]$player_id, 1))
}

test_that("ffs_waiver_proxies clones the nearest-ECR rostered player", {
  # a hand-built simulation: the shipped fixture is already slimmed, and
  # ffs_waiver_proxies needs the `rosters` and `latest_rankings` that only
  # return = "all" keeps
  rosters <- data.table::data.table(
    league_id = "L", franchise_id = rep(c("1", "2"), each = 2),
    franchise_name = rep(c("A", "B"), each = 2),
    player_id = c("p1", "p2", "p3", "p4"),
    player_name = paste0("Player ", 1:4),
    pos = c("RB", "WR", "RB", "WR"),
    fantasypros_id = c("f1", "f2", "f3", "f4"))
  latest_rankings <- data.table::data.table(
    player = paste0("Player ", 1:6), fantasypros_id = paste0("f", 1:6),
    pos = c("RB", "WR", "RB", "WR", "RB", "WR"),
    ecr = c(10, 20, 50, 60, 48, 200))
  roster_scores <- data.table::CJ(player_id = c("p1", "p2", "p3", "p4"),
                                  season = 1:2, week = 1:3)
  roster_scores[, `:=`(
    league_id = "L",
    franchise_id = rosters$franchise_id[match(player_id, rosters$player_id)],
    franchise_name = rosters$franchise_name[match(player_id, rosters$player_id)],
    player_name = rosters$player_name[match(player_id, rosters$player_id)],
    pos = rosters$pos[match(player_id, rosters$player_id)],
    projected_score = 10, avg_week = 10)]
  sim <- structure(list(rosters = rosters, latest_rankings = latest_rankings,
                        roster_scores = roster_scores),
                   class = "ff_simulation")

  w <- ffs_waiver_proxies(sim, n_per_pos = 1L, pos_filter = c("RB", "WR"),
                          n_donors = 2L)

  # f5 (RB, ecr 48) and f6 (WR, ecr 200) are the only unrostered players
  expect_setequal(unique(w$waiver_slot), c("RB_1", "WR_1"))
  expect_setequal(unique(w$player_name), c("Player 5", "Player 6"))
  # nearest ECR at the position wins: RB 48 is nearer p3 (50) than p1 (10)
  rb <- unique(w[w$waiver_slot == "RB_1", c("donor_id", "donor_rank", "ecr_gap")])
  data.table::setorderv(rb, "donor_rank")
  expect_identical(rb$donor_id, c("p3", "p1"))
  expect_identical(rb$ecr_gap, c(2, 38))
  # ids are globally unique - .ffs_counterfactual_rows looks players up across
  # the whole table, so a shared id would pull one row set per franchise
  expect_false(any(w$player_id %in% roster_scores$player_id))
  expect_identical(anyDuplicated(unique(w[, c("player_id", "season", "week")])), 0L)
  # and the clone really is the donor's score profile
  expect_identical(nrow(w[w$player_id == "WAIVER_RB_1_1"]), 6L)

  # nobody unrostered -> nothing to sign, and the table is still shaped right
  sim2 <- sim
  sim2$latest_rankings <- latest_rankings[latest_rankings$fantasypros_id %in%
                                            rosters$fantasypros_id]
  expect_identical(nrow(ffs_waiver_proxies(sim2)), 0L)
})

test_that("the waiver index ranks slots by their primary donor", {
  sim <- .trade_sim_with_waivers()
  idx <- .ffs_waiver_index(sim$waiver_scores)

  expect_identical(nrow(idx), 4L)
  # every alternate of a slot shares the slot's score, so the sort cannot
  # interleave two slots' alternates
  expect_identical(data.table::uniqueN(idx, by = c("waiver_slot", "slot_avg")), 2L)
  # primary donor first within a slot, best slot first overall
  expect_identical(idx$donor_rank[idx$waiver_slot == idx$waiver_slot[[1]]], 1:2)
  expect_true(idx$slot_avg[[1]] >= idx$slot_avg[[nrow(idx)]])

  expect_identical(nrow(.ffs_waiver_index(NULL)), 0L)
})

test_that("waiver picks avoid donors who are themselves in the deal", {
  sim <- .trade_sim_with_waivers()
  idx <- .ffs_waiver_index(sim$waiver_scores)
  best_slot <- idx$waiver_slot[[1]]
  primary <- idx[idx$waiver_slot == best_slot & idx$donor_rank == 1L]

  expect_identical(.ffs_waiver_pick(idx, 1L), primary$player_id)
  # one player per slot, never two alternates of the same free agent
  expect_identical(length(.ffs_waiver_pick(idx, 5L)), 2L)
  expect_identical(data.table::uniqueN(
    idx$waiver_slot[match(.ffs_waiver_pick(idx, 5L), idx$player_id)]), 2L)

  # cloning a player who is being traded in this same deal would make sending
  # him free, so the pick falls through to the next-nearest donor
  fell <- .ffs_waiver_pick(idx, 1L, exclude_ids = primary$donor_id)
  expect_identical(fell, idx[idx$waiver_slot == best_slot &
                               idx$donor_rank == 2L]$player_id)
  # both donors unavailable -> the slot is skipped, not filled with a duplicate
  gone <- .ffs_waiver_pick(idx, 2L,
                           exclude_ids = idx$donor_id[idx$waiver_slot == best_slot])
  expect_false(any(idx$waiver_slot[match(gone, idx$player_id)] == best_slot))
  # a slot already handed to the other side is not handed out twice
  expect_false(primary$player_id %in%
                 .ffs_waiver_pick(idx, 2L, taken = primary$player_id))

  expect_identical(.ffs_waiver_pick(idx, 0L), character(0))
  expect_identical(.ffs_waiver_pick(.ffs_waiver_index(NULL), 2L), character(0))
})

test_that("cut-to-size drops the weakest player, not one in the deal", {
  sim <- .ffs_cache_example("trade_sim.rds")
  rs <- data.table::as.data.table(sim$roster_scores)
  fid <- sort(unique(rs$franchise_id))[[1]]
  avg <- rs[rs$franchise_id == fid,
            list(m = mean(data.table::fcoalesce(as.numeric(avg_week), 0))),
            by = "player_id"]
  data.table::setorderv(avg, "m")

  expect_identical(.ffs_cut_pick(rs, fid, 2L), avg$player_id[1:2])
  # a player already leaving in the trade cannot also be released
  expect_identical(.ffs_cut_pick(rs, fid, 1L, exclude_ids = avg$player_id[[1]]),
                   avg$player_id[[2]])
  expect_identical(.ffs_cut_pick(rs, fid, 0L), character(0))
})

test_that("roster churn conserves bodies on both sides", {
  sim <- .trade_sim_with_waivers()
  idx <- .ffs_waiver_index(sim$waiver_scores)
  rs <- .ffs_rs_with_waivers(sim)
  d <- .waiver_deal(sim)
  churn <- function(ga, gb, ...) .ffs_roster_churn(idx, rs, d$fa, ga, d$fb, gb, ...)

  # 2-for-1: I open a spot and sign a free agent, they overfill and release one
  ch <- churn(d$ga, d$gb, waiver = "me", cut_to_size = TRUE)
  expect_identical(length(ch$add_a), 1L)
  expect_identical(length(ch$cut_b), 1L)
  expect_identical(ch$add_b, character(0))
  expect_identical(ch$cut_a, character(0))
  # the released player is theirs, and is not one they are receiving
  expect_false(ch$cut_b %in% c(d$ga, d$gb))

  # "me" leaves their spot unfilled; "both" fills it with a DIFFERENT free agent
  expect_identical(churn(d$gb, d$ga, waiver = "me")$add_b, character(0))
  both <- churn(d$gb, d$ga, waiver = "both")
  expect_identical(length(both$add_b), 1L)
  expect_length(intersect(both$add_a, both$add_b), 0L)

  # a one-for-one moves no bodies, so neither knob does anything
  ch11 <- churn(d$ga[1], d$gb, waiver = "both", cut_to_size = TRUE)
  expect_identical(lengths(ch11), c(add_a = 0L, add_b = 0L, cut_a = 0L, cut_b = 0L))

  # draft picks are value-only assets and occupy no roster spot
  expect_identical(churn(d$ga, c(d$gb, "PICK_2027_1_5"), waiver = "me",
                         cut_to_size = TRUE),
                   churn(d$ga, d$gb, waiver = "me", cut_to_size = TRUE))

  # off by default
  expect_identical(lengths(churn(d$ga, d$gb)),
                   c(add_a = 0L, add_b = 0L, cut_a = 0L, cut_b = 0L))
})

test_that("waiver = 'none' reproduces the untouched evaluation exactly", {
  skip_on_cran()
  sim <- .trade_sim_with_waivers()
  bare <- .ffs_cache_example("trade_sim.rds")
  d <- .waiver_deal(sim)

  # merely CARRYING a waiver table must not move a single number: the proxies
  # have franchise_id = NA, so no franchise filter can reach them
  expect_equal(ffs_trade_eval(sim, d$fa, d$ga, d$fb, d$gb),
               ffs_trade_eval(bare, d$fa, d$ga, d$fb, d$gb))
  expect_equal(ffs_trade_engine(sim)$before, ffs_trade_engine(bare)$before)

  # no wire at all -> asking for a backfill is a no-op, not an error
  expect_equal(ffs_trade_eval(bare, d$fa, d$ga, d$fb, d$gb, waiver = "both"),
               ffs_trade_eval(bare, d$fa, d$ga, d$fb, d$gb))
  # neither is an empty one
  empty <- bare
  empty$waiver_scores <- data.table::as.data.table(bare$roster_scores)[0]
  expect_equal(ffs_trade_eval(empty, d$fa, d$ga, d$fb, d$gb, waiver = "both",
                              cut_to_size = TRUE)$playoff_pct_delta,
               ffs_trade_eval(bare, d$fa, d$ga, d$fb, d$gb,
                              cut_to_size = TRUE)$playoff_pct_delta)
  # and a one-for-one is untouched even with both knobs on
  expect_equal(ffs_trade_eval(sim, d$fa, d$ga[1], d$fb, d$gb, waiver = "both",
                              cut_to_size = TRUE),
               ffs_trade_eval(sim, d$fa, d$ga[1], d$fb, d$gb))
})

test_that("fast, full and engine agree once the roster churns", {
  skip_on_cran()
  sim <- .trade_sim_with_waivers()
  engine <- ffs_trade_engine(sim)
  skip_if(is.null(engine), "example simulation is not engine-eligible")
  rs <- data.table::as.data.table(sim$roster_scores)
  pool <- unique(rs[, list(franchise_id, player_id)])
  pool <- pool[!grepl("^(QB|RB|WR|TE|K)_[0-9]+$", pool$player_id)]
  fids <- sort(unique(stats::na.omit(rs$franchise_id)))

  # the shapes that actually move bodies, in both directions
  shapes <- list(c(2, 1), c(3, 1), c(1, 2), c(3, 2))
  modes <- list(list("me", FALSE), list("me", TRUE), list("both", TRUE))
  set.seed(20260902)
  for (i in seq_len(12)) {
    sh <- shapes[[((i - 1L) %% length(shapes)) + 1L]]
    md <- modes[[((i - 1L) %% length(modes)) + 1L]]
    fa <- sample(fids, 1); fb <- sample(setdiff(fids, fa), 1)
    a <- pool[pool$franchise_id == fa]$player_id
    b <- pool[pool$franchise_id == fb]$player_id
    if (length(a) < sh[[1]] || length(b) < sh[[2]]) next
    ga <- sample(a, sh[[1]]); gb <- sample(b, sh[[2]])

    fast <- ffs_trade_eval(sim, fa, ga, fb, gb, fast = TRUE,
                           waiver = md[[1]], cut_to_size = md[[2]])
    full <- ffs_trade_eval(sim, fa, ga, fb, gb, fast = FALSE,
                           waiver = md[[1]], cut_to_size = md[[2]])
    eng <- ffs_trade_eval(sim, fa, ga, fb, gb, engine = engine,
                          waiver = md[[1]], cut_to_size = md[[2]])
    expect_equal(fast, full, tolerance = 1e-8,
                 info = sprintf("shape %d-for-%d, waiver=%s cut=%s",
                                sh[[1]], sh[[2]], md[[1]], md[[2]]))
    expect_equal(fast, eng, tolerance = 1e-8)
  }
})

test_that("the backfill is what stops a 2-for-1 reading as a pure loss", {
  skip_on_cran()
  sim <- .trade_sim_with_waivers()
  d <- .waiver_deal(sim)
  me <- function(x) x$h2h_wins_delta[x$franchise_id == d$fa]

  none <- ffs_trade_eval(sim, d$fa, d$ga, d$fb, d$gb)
  back <- ffs_trade_eval(sim, d$fa, d$ga, d$fb, d$gb, waiver = "me")
  # the fixture's proxies are cloned from the best players at the position, so
  # refilling the vacated spot must strictly help the side that vacated it
  expect_gt(me(back), me(none))

  # and a wire nobody would claim off changes nothing: cloning the WORST players
  # gives a proxy who never clears the started cut-line, so no week re-optimises
  weak <- .trade_sim_with_waivers(strong = FALSE)
  expect_equal(me(ffs_trade_eval(weak, d$fa, d$ga, d$fb, d$gb, waiver = "me")),
               me(none))
})

test_that("slimming keeps the waiver table so parallel workers agree", {
  sim <- .trade_sim_with_waivers()
  slim <- .ffs_slim_simulation(sim)

  expect_false(is.null(slim$waiver_scores))
  expect_identical(sort(unique(slim$waiver_scores$player_id)),
                   sort(unique(sim$waiver_scores$player_id)))
  # the churn needs donor_id to dodge a donor in the deal, and waiver_slot to
  # avoid handing out two alternates of the same free agent
  expect_true(all(c("waiver_slot", "donor_id", "donor_rank") %in%
                    names(slim$waiver_scores)))
  expect_identical(nrow(.ffs_waiver_index(slim$waiver_scores)),
                   nrow(.ffs_waiver_index(sim$waiver_scores)))
  expect_null(.ffs_slim_simulation(.ffs_cache_example("trade_sim.rds"))$waiver_scores)
})

# --------------------------------------------------------------------------
# The builder knobs that decide whether a consolidation ever gets simulated.

test_that("min_send_players and screen_per_shape surface consolidations", {
  skip_on_cran()
  sim <- .trade_sim_with_waivers()
  skip_if(is.null(ffs_trade_engine(sim)), "fixture is not engine-eligible")
  rs <- data.table::as.data.table(sim$roster_scores)
  me <- sort(unique(stats::na.omit(rs$franchise_id)))[[1]]

  # same synthetic dynasty table the market suite uses: value proxied by mean
  # weekly projection, so this exercises the machinery and not the value model
  dyn <- rs[!grepl("^(QB|RB|WR|TE|K)_[0-9]+$", player_id),
            list(cur_value = 400 * mean(projected_score, na.rm = TRUE)),
            by = list(player_id, pos)]
  dyn[, next_value_mean := cur_value * 0.98]
  targets <- data.table::as.data.table(ffs_trade_targets(sim, me, top_n = 25))
  # my picks, priced - these are what flood a 2-send package when nothing
  # forces the second piece to be a real player
  picks <- data.table::data.table(
    franchise_id = me, player_id = c("PICK_2027_1_1", "PICK_2027_2_1"),
    player_name = c("2027 R1", "2027 R2"),
    cur_value = c(3000, 1200), next_value_mean = c(3000, 1200))

  build <- function(...) data.table::as.data.table(ffs_build_trades(
    sim, me, dynasty = dyn, targets = targets, picks = picks,
    shapes = list(c(1, 1), c(2, 1)), fair_band = c(-0.05, 0.05),
    require_positive_target = FALSE, top_n = Inf, ...))

  n_real <- function(ids) vapply(ids, function(x) sum(!grepl("^PICK_", x)), integer(1))

  # dry_run so this costs enumeration only, no exact evaluation
  loose <- build(screen_n = Inf, dry_run = TRUE)
  strict <- build(screen_n = Inf, dry_run = TRUE, min_send_players = 2L)
  skip_if(nrow(loose) == 0, "fixture produced no banded deals")

  # every send package now carries two real players, and the pick-fuelled
  # packages that dominated before are gone
  expect_true(all(n_real(strict$send_ids) >= 2L))
  expect_true(nrow(strict) < nrow(loose))
  expect_true(any(n_real(loose$send_ids) < 2L))

  # on a budgeted screen the consolidations lose to player+pick packages on
  # add_gain and never reach the eval; a per-shape quota is what gets them there
  budget <- build(screen_n = 6L, dry_run = TRUE)
  quota <- build(screen_n = 6L, screen_per_shape = 6L, dry_run = TRUE)
  expect_gte(sum(lengths(quota$send_ids) == 2L), sum(lengths(budget$send_ids) == 2L))
  expect_true(nrow(quota) >= nrow(budget))
})

test_that("the board names the waiver claim and the release", {
  skip_on_cran()
  sim <- .trade_sim_with_waivers()
  skip_if(is.null(ffs_trade_engine(sim)), "fixture is not engine-eligible")
  rs <- data.table::as.data.table(sim$roster_scores)
  me <- sort(unique(stats::na.omit(rs$franchise_id)))[[1]]
  dyn <- rs[!grepl("^(QB|RB|WR|TE|K)_[0-9]+$", player_id),
            list(cur_value = 400 * mean(projected_score, na.rm = TRUE)),
            by = list(player_id, pos)]
  dyn[, next_value_mean := cur_value * 0.98]
  targets <- data.table::as.data.table(ffs_trade_targets(sim, me, top_n = 25))

  b <- data.table::as.data.table(ffs_build_trades(
    sim, me, dynasty = dyn, targets = targets,
    shapes = list(c(1, 1), c(2, 1)), fair_band = c(-0.05, 0.05),
    require_positive_target = FALSE, screen_n = 12, top_n = 12,
    min_send_players = 2L, waiver = "me", cut_to_size = TRUE))
  skip_if(nrow(b) == 0, "fixture produced no banded deals")

  expect_true(all(c("backfill", "opp_drop") %in% names(b)))
  # every row here is a 2-for-1, so every row signs someone and releases someone
  expect_false(any(is.na(b$backfill)))
  expect_false(any(is.na(b$opp_drop)))
  expect_true(all(grepl("^Waiver ", b$backfill)))

  # off by default, and then the columns are present but empty rather than absent
  b0 <- data.table::as.data.table(ffs_build_trades(
    sim, me, dynasty = dyn, targets = targets,
    shapes = list(c(1, 1), c(2, 1)), fair_band = c(-0.05, 0.05),
    require_positive_target = FALSE, screen_n = 12, top_n = 12,
    min_send_players = 2L))
  expect_true(all(is.na(b0$backfill)))
  expect_true(all(is.na(b0$opp_drop)))
})

test_that("two spots are never filled with two clones of one player", {
  # free agents of similar rank often share a nearest-ECR donor - measured on
  # Goofball, the top two available QBs both borrow the same rostered QB - and
  # filling two vacated spots with both would hand me perfectly correlated weeks
  idx <- data.table::data.table(
    player_id = c("WAIVER_QB_1_1", "WAIVER_QB_2_1", "WAIVER_RB_1_1"),
    waiver_slot = c("QB_1", "QB_2", "RB_1"),
    donor_id = c("d1", "d1", "d2"),
    donor_rank = 1L,
    mean_avg = c(9.8, 9.8, 5.6), slot_avg = c(9.8, 9.8, 5.6))

  expect_identical(.ffs_waiver_pick(idx, 2L), c("WAIVER_QB_1_1", "WAIVER_RB_1_1"))
  # and a donor already used by the other side is off limits too
  expect_identical(.ffs_waiver_pick(idx, 1L, taken = "WAIVER_QB_1_1"),
                   "WAIVER_RB_1_1")
})

test_that("the screen credits the backfill the eval will actually make", {
  skip_on_cran()
  sim <- .trade_sim_with_waivers()
  skip_if(is.null(ffs_trade_engine(sim)), "fixture is not engine-eligible")
  rs <- data.table::as.data.table(sim$roster_scores)
  me <- sort(unique(stats::na.omit(rs$franchise_id)))[[1]]
  dyn <- rs[!grepl("^(QB|RB|WR|TE|K)_[0-9]+$", player_id),
            list(cur_value = 400 * mean(projected_score, na.rm = TRUE)),
            by = list(player_id, pos)]
  dyn[, next_value_mean := cur_value * 0.98]
  targets <- data.table::as.data.table(ffs_trade_targets(sim, me, top_n = 25))

  build <- function(...) data.table::as.data.table(ffs_build_trades(
    sim, me, dynasty = dyn, targets = targets, shapes = list(c(1, 1), c(2, 1)),
    fair_band = c(-0.05, 0.05), require_positive_target = FALSE,
    screen_n = Inf, dry_run = TRUE, ...))

  off <- build()
  on <- build(waiver = "me")
  skip_if(nrow(off) == 0, "fixture produced no banded deals")

  # the credit exists only where the deal actually vacates a spot, and only when
  # a backfill is asked for - otherwise the screen would keep pruning
  # consolidations before the eval that refills them ever runs
  expect_true(all(off$waiver_gain == 0))
  two <- lengths(on$send_ids) == 2L & lengths(on$recv_ids) == 1L
  expect_true(all(on$waiver_gain[!two] == 0))
  skip_if(!any(two), "fixture produced no 2-for-1s")
  expect_true(all(on$waiver_gain[two] > 0))
  # one open spot is credited exactly one free agent, no more
  expect_identical(data.table::uniqueN(on$waiver_gain[two]), 1L)
})

test_that("ffs_waiver_rank reorders the wire by value, not raw points", {
  skip_on_cran()
  sim <- .trade_sim_with_waivers()
  skip_if(is.null(ffs_trade_engine(sim)), "fixture is not engine-eligible")
  me <- sort(unique(stats::na.omit(
    data.table::as.data.table(sim$roster_scores)$franchise_id)))[[1]]

  # without a measurement the index falls back to mean weekly points
  by_pts <- .ffs_waiver_index(sim$waiver_scores)
  expect_identical(by_pts$slot_avg, by_pts$mean_avg[match(by_pts$waiver_slot, by_pts$waiver_slot)])

  sim$waiver_scores <- ffs_waiver_rank(sim, me, engine = ffs_trade_engine(sim))
  expect_true("value_w" %in% names(sim$waiver_scores))
  # one value per slot, shared by that slot's donor alternates
  expect_identical(
    data.table::uniqueN(sim$waiver_scores, by = c("waiver_slot", "value_w")),
    data.table::uniqueN(sim$waiver_scores$waiver_slot))

  by_val <- .ffs_waiver_index(sim$waiver_scores)
  # mean_avg is still reported, but rank_val - the measured value - is what the
  # order now rides on
  expect_true(all(c("mean_avg", "rank_val") %in% names(by_val)))
  src <- unique(sim$waiver_scores[, c("player_id", "value_w")])
  expect_equal(by_val$rank_val, src$value_w[match(by_val$player_id, src$player_id)])
  # best-first still holds, now on the measured value
  expect_true(by_val$slot_avg[[1]] >= by_val$slot_avg[[nrow(by_val)]])
  expect_identical(.ffs_waiver_pick(by_val, 1L), by_val$player_id[[1]])

  # no proxies at all is a no-op, not an error
  bare <- .ffs_cache_example("trade_sim.rds")
  expect_null(ffs_waiver_rank(bare, me))
})
