# draft_sweep.R -- score build policies from one draft slot
#
# Sourced by draft_strategy.R, which supplies cfg and D. Splits out the
# expensive half: the live board, the shared score draws, the build policies,
# and the two opponent branches.
#
# Everything is a PAIRED comparison. One set of player-score draws and one
# schedule are generated once and reused by every world, and every arm is run
# on the same list of mock-draft board seeds. So an arm minus the BPA control
# on the same seed differs only by the build, and the standard error comes from
# the spread of those paired differences, not from the raw champion rate.

suppressPackageStartupMessages(library(data.table))
source(here::here("dev", "suite", "adp_draftsharks.R"))

# ------------------------------------------------------- board and pool -----

board_live <- function() {
  b <- ds_board_cached(D$latest_rankings, quiet = TRUE)
  b <- b[!is.na(fantasypros_id) & pos %in% cfg$pos_filter]
  b <- b[!duplicated(fantasypros_id)]
  setorder(b, adp_rank)
  b[, adp_rank := seq_len(.N)]
  b[]
}

BOARD <- board_live()
n_picks <- length(D$draft_slots) * cfg$n_rounds
msg("board:", nrow(BOARD), "players |", n_picks, "picks |",
    nrow(BOARD) - n_picks, "left over for replacement level")
stopifnot(nrow(BOARD) > n_picks + 20L)

# The SAME id restriction has to apply to the draft pool and to the rankings
# used for projections and replacement level. Otherwise ffs_score_rosters does
# an inner join on fantasypros_id, silently drops a drafted player, and that
# franchise quietly plays a man short all season.
PROJ_IDS <- head(BOARD$fantasypros_id, cfg$proj_pool_n)
RANKINGS_SIM <- D$latest_rankings[as.character(fantasypros_id) %in% PROJ_IDS]
# Byes must not land inside the simulated playoffs. In reality NFL byes finish
# by week 14 and the fantasy playoffs run 15-17, so no player is ever on bye in
# a playoff game. Compressing the season to 13 regular weeks (see reg_weeks)
# would otherwise drop 77 week-14 players out of a playoff round they would
# always have been available for. Cap the bye at the last regular week.
RANKINGS_SIM <- data.table::copy(RANKINGS_SIM)
RANKINGS_SIM[!is.na(bye) & bye > cfg$reg_weeks, bye := cfg$reg_weeks]
POOL <- BOARD[fantasypros_id %in% RANKINGS_SIM$fantasypros_id,
              list(fantasypros_id, player, pos, ecr = adp_rank)]
msg("projection pool:", nrow(RANKINGS_SIM), "| draft pool:", nrow(POOL))

# Roster targets the autodrafter plans against. These deliberately differ from
# the raw lineup constraints, because the raw numbers describe what is LEGAL and
# the drafter needs what is SENSIBLE:
#
#   QB need 2, not 1. Sleeper reports SUPER_FLEX as a flex slot, so the lineup
#   minimum for QB is 1 - but every competitive roster in a 2QB/SF league starts
#   two, and the field here drafts 3.2 QBs a team. Left at 1 an exempt seat
#   drafts 1.4 QBs and cannot fill its own superflex.
#
#   TE cap 3, not the lineup max of 4-plus. TE+FLEX+FLEX+SF means four tight
#   ends could technically start, and because this league lets TEs fall ~20
#   ranks a value-following seat will happily take SEVEN of them - exploiting a
#   modelled market inefficiency in a way no real manager would. The cap is the
#   roster-construction judgement the fit heuristic is too soft to express.
# Every team in this league rosters exactly 3 QBs through round 17 - 62% finish
# on 3, 30% add a 4th, and 15 of those 18 fourth QBs came in rounds 17-18 as
# pure last-pick darts (Drew Lock three times). Modelling that tail adds noise
# to the only thing we care about, which is when the FIRST TWO come, so the
# drafter is pinned to exactly 3.
POS_NEED <- D$pos_need
POS_NEED[["QB"]] <- 3L
POS_CAP <- c(QB = 3, RB = 8, WR = 8, TE = 2)[names(D$pos_cap)]

# ...and the 3rd QB must not be taxed. pos_start_max is a start-eligibility
# ceiling (QB + SUPER_FLEX = 2), so at 2 the over_start_decay term treats a 3rd
# QB as near-worthless bench - which is why the room was leaving QBs on the
# board into rounds 11-12. Every team here wants three; only a 4th is surplus.
POS_START_MAX <- D$pos_start_max
POS_START_MAX[["QB"]] <- 3L
msg("drafter targets | need:",
    paste(sprintf("%s=%d", names(POS_NEED), POS_NEED), collapse = " "),
    "| cap:", paste(sprintf("%s=%s", names(POS_CAP), POS_CAP), collapse = " "))

# Round-dependent draft tightness. Real drafts are near-deterministic in round 1
# (86% of picks within 3 ranks of ADP, zero excess QBs) and fan out about tenfold
# by round 18; a single window_k and a full-strength positional shift in round 1
# reproduce neither. Fitted in dev/data/draft_tightness.rds - window_k widens
# 5 -> 39 across rounds and the positional bias phases in over rounds 1-4.
TIGHT <- local({
  fp <- here::here("dev", "data", "draft_tightness.rds")
  if (nzchar(Sys.getenv("FFS_DRAFT_TIGHTNESS")) && file.exists(fp)) readRDS(fp) else NULL
})
if (!is.null(TIGHT)) msg("tightness: window_k", TIGHT$window_k[1], "->",
    tail(TIGHT$window_k,1), "| shift ramp", paste(head(TIGHT$ramp,4), collapse=","))

PICK_MAP <- ffs_draft_pick_map(D$draft_slots, cfg$n_rounds)
MY_PICKS <- PICK_MAP[franchise_id == D$my_fid][order(overall)]$overall
msg("my picks:", paste(head(MY_PICKS, 8), collapse = " "), "...")

FR_INFO <- unique(D$franchises[, list(franchise_id = as.character(franchise_id),
                                      franchise_name, league_id)])

# ---------------------------------------------------------- shared draws ----

PLAYOFF_ROUNDS <- as.integer(ceiling(log2(cfg$playoff_slots)))
N_WEEKS <- cfg$reg_weeks + PLAYOFF_ROUNDS * cfg$weeks_per_round
PLAYOFF_WEEKS <- seq(cfg$reg_weeks + 1L, N_WEEKS)

make_draws <- function(n_seasons, seed) {
  set.seed(seed)
  ps <- as.data.table(ffs_generate_projections(
    adp_outcomes = D$adp_outcomes, latest_rankings = RANKINGS_SIM,
    n_seasons = n_seasons, weeks = seq_len(N_WEEKS), version = "v3"))
  ps[is.na(projected_score), projected_score := 0]
  sched <- ffs_build_schedules(franchises = D$franchises, n_seasons = n_seasons,
                               n_weeks = cfg$reg_weeks, seed = seed)
  list(projected_scores = ps, schedules = sched, n_seasons = n_seasons)
}

# ------------------------------------------------------ opponent branches ---

# How the room will treat QBs is not identified - three seasons of integer QB
# counts cannot discriminate a zero shift from a large one (see
# reach_summary.csv). Rather than pick one, condition on it.
#
#   board     - the room drafts the board, roughly 3 QBs in round one
#   qb_hungry - the room takes QBs at its own five-year rate, roughly 5-6
#   mixed     - the shift is DRAWN PER BOARD across the whole plausible range
#
# `mixed` is the one that supports a contingency table. Fixed branches populate
# only two values of "QBs gone before my pick" and leave the middle empty; a
# per-board draw spans the range, so every world contributes to the question
# actually being asked, which is what to do GIVEN what the room has done. The
# draw is deterministic in the board seed, so an arm and its control still see
# an identical room - the comparison stays paired.
# Drawing the shift uniformly over its own range does NOT spread the thing we
# stratify on: U(0,22) piles up at 4-5 QBs gone and leaves the board-faithful
# case (2-3, which is what the ADP actually predicts) with a quarter as many
# boards as the middle. These bands were read off the calibration sweep and are
# assigned round-robin by board seed, so each stratum gets a comparable number
# of boards and every cell of the contingency table is estimated to similar
# precision. This is an efficiency choice only - the paired comparison
# conditions on the REALISED opening and the control sees the same board, so
# reweighting the design cannot bias a within-stratum contrast.
# The room reaches on every position, not just QB. These shifts were fitted so
# the simulated field reproduces the reach measured on the 2023-25 drafts
# (QB +8.1, RB +4.8, WR -2.1, TE -18.6 centred): see calibrate step. Without the
# TE term the sim has tight ends going slightly EARLY, when in reality this
# league lets them fall nearly twenty ranks - the largest effect in the data.
POS_SHIFT_BASE <- local({
  f <- here::here("dev", "data", "draft_pos_shift_calibrated.rds")
  if (file.exists(f)) readRDS(f) else c(QB = 12.2, RB = 6.9, WR = 0.7, TE = -19.7)
})

# Only the QB term is genuinely uncertain, so only it is varied per board. The
# bands are deltas around the calibrated QB shift and span roughly 3.3 to 6.6
# QBs gone by pick 12, covering both the board reading and Joe five-year prior.
QB_SHIFT_BANDS <- as.numeric(strsplit(
  Sys.getenv("FFS_DRAFT_QBSHIFT_BANDS", "-6,-2,2,6,10,14"), ",")[[1]])
QB_SHIFT_JITTER <- as.numeric(Sys.getenv("FFS_DRAFT_QBSHIFT_JITTER", "1.5"))

board_shift <- function(board_seed) {
  band <- QB_SHIFT_BANDS[[(board_seed - 1L) %% length(QB_SHIFT_BANDS) + 1L]]
  # double arithmetic + modulus so a large board offset cannot overflow integer
  set.seed(as.integer((as.numeric(board_seed) * 7919) %% 2147483647))
  s <- POS_SHIFT_BASE
  s[["QB"]] <- s[["QB"]] + band + stats::runif(1L, -QB_SHIFT_JITTER, QB_SHIFT_JITTER)
  s
}

BRANCHES <- list(
  mixed     = list(shift_fn = board_shift),
  board     = list(pos_shift = NULL),
  qb_hungry = list(pos_shift = c(QB = as.numeric(Sys.getenv("FFS_DRAFT_QBSHIFT", "18"))))
)

# which branches this run scores
BRANCH_USE <- strsplit(Sys.getenv("FFS_DRAFT_BRANCHES", "board,qb_hungry"), ",")[[1]]
BRANCH_USE <- intersect(BRANCH_USE, names(BRANCHES))
stopifnot(length(BRANCH_USE) > 0L)

# ------------------------------------------------------------- policies -----

# A policy narrows only MY seat; everyone else autodrafts. Returning
# character(0) means "no restriction", which is how BPA rounds are expressed.
build_policy <- function(script) {
  force(script)
  function(franchise_id, round, have) {
    if (franchise_id != D$my_fid) return(character(0))
    if (round > length(script)) return(character(0))
    p <- script[[round]]
    if (is.null(p) || identical(p, "ANY")) character(0) else p
  }
}

# first QB at round k, second at round j, no QB before either, BPA otherwise
qb_timing_policy <- function(k, j) {
  force(k); force(j)
  skill <- c("RB", "WR", "TE")
  function(franchise_id, round, have) {
    if (franchise_id != D$my_fid) return(character(0))
    if (round == k || round == j) return("QB")
    if (round < j) return(skill)
    character(0)
  }
}

# "how many of this position should I draft" - timing free, but stop at n.
# The TE version matters because the calibrated board says this league lets
# tight ends fall ~20 ranks, so a value-following seat wants as many as the cap
# allows; whether that is actually GOOD is a question for the evaluator, not
# for the drafter heuristic, so it has to be an arm rather than an assumption.
cap_policy <- function(position, n) {
  force(position); force(n)
  others <- setdiff(c("QB", "RB", "WR", "TE"), position)
  function(franchise_id, round, have) {
    if (franchise_id != D$my_fid) return(character(0))
    if (isTRUE(have[[position]] >= n)) return(others)
    character(0)
  }
}
qb_cap_policy <- function(n) cap_policy("QB", n)

# --- the Winks hypotheses, as arms ------------------------------------------
#
# Hayden Winks (Underdog pick-by-pick, 2020-25) reports three roster-shape rules
# that this league can test directly: 3-4 RBs through round 8, 2-3 WRs through
# round 5, and no TE before round 10. His data is HALF-PPR BEST BALL 1QB and
# this league is full-PPR managed superflex, so none of it transfers on trust -
# which is the point of running them as arms rather than adopting them.
#
# His fourth rule, "avoid QB2 before round 9", is 1QB advice and is expected to
# INVERT here: he says outright that a second QB is not even required in his
# format, whereas a superflex seat starts two every week. The qb_* arms measure
# that inversion instead of assuming it.

# exactly n of a position by the end of round `through`, timing free within that
count_by_policy <- function(position, n, through) {
  force(position); force(n); force(through)
  others <- setdiff(c("QB", "RB", "WR", "TE"), position)
  function(franchise_id, round, have) {
    if (franchise_id != D$my_fid) return(character(0))
    if (round > through) return(character(0))
    hv <- have[[position]]
    if (isTRUE(hv >= n)) return(others)          # quota filled, stop taking them
    if (through - round + 1L <= n - hv) return(position)  # must take one now
    character(0)                                  # free to choose
  }
}

# nothing at this position before round `from`
no_before_policy <- function(position, from) {
  force(position); force(from)
  others <- setdiff(c("QB", "RB", "WR", "TE"), position)
  function(franchise_id, round, have) {
    if (franchise_id != D$my_fid) return(character(0))
    if (round < from) return(others)
    character(0)
  }
}

# "take a QB by round r at the latest, then free" - isolates the FIRST QB
qb_by_policy <- function(r) {
  force(r)
  function(franchise_id, round, have) {
    if (franchise_id != D$my_fid) return(character(0))
    if (isTRUE(have[["QB"]] >= 1L)) return(character(0))
    if (round >= r) return("QB")
    c("RB", "WR", "TE")
  }
}

ARCHETYPES <- list(
  bpa        = c("ANY", "ANY"),
  rb_rb      = c("RB", "RB"),
  wr_wr      = c("WR", "WR"),
  rb_wr      = c("RB", "WR"),
  wr_rb      = c("WR", "RB"),
  qb_wr      = c("QB", "WR"),
  qb_rb      = c("QB", "RB"),
  wr_qb      = c("WR", "QB"),
  rb_qb      = c("RB", "QB"),
  qb_qb      = c("QB", "QB"),
  te_early   = c("TE", "ANY"),
  hero_rb    = c("RB", "WR", "WR"),
  zero_rb    = c("WR", "WR", "WR", "RB"),
  robust_rb  = c("RB", "RB", "RB"),
  wr_wr_qb   = c("WR", "WR", "QB"),
  rb_rb_qb   = c("RB", "RB", "QB")
)

# Arms are plain SPECS, not closures. A closure would drag its whole enclosing
# environment across to a parallel worker; a spec is a few characters, and the
# worker rebuilds the policy locally with arm_policy().
make_arms <- function(include_qb_grid = TRUE, n_random = 0L, seed = 1L) {
  arms <- lapply(names(ARCHETYPES), function(nm) {
    list(arm = nm, kind = "archetype", script = ARCHETYPES[[nm]])
  })
  if (include_qb_grid) {
    # wide enough to answer three separate questions: how early is too early,
    # how long can the SECOND QB wait behind an early first one, and does
    # waiting past round 6 for the first QB start to cost anything
    grid <- CJ(k = 1:10, j = 1:14)[j > k][j <= k + 12]
    arms <- c(arms, lapply(seq_len(nrow(grid)), function(i) {
      list(arm = sprintf("qb_%d_%d", grid$k[i], grid$j[i]), kind = "qb_grid",
           k = grid$k[i], j = grid$j[i])
    }))
    arms <- c(arms, lapply(2:6, function(n)
      list(arm = sprintf("qbmax_%d", n), kind = "qb_cap", n = n)))
    arms <- c(arms, lapply(0:3, function(n)
      list(arm = sprintf("temax_%d", n), kind = "te_cap", n = n)))
    # Winks counts rounds in a 1QB world. A superflex seat spends ~2.2 picks
    # on QB by round 8 against well under 1 for him, so his "through round 8"
    # is really "through ~7 non-QB picks" - which this seat does not reach until
    # round 10. The windows are shifted right accordingly; the raw R8/R5 forms
    # would be testing a budget this format never has.
    for (thr in c(6L, 8L, 10L)) {
      arms <- c(arms, lapply(0:5, function(n)
        list(arm = sprintf("rb%d_by%d", n, thr), kind = "count_by",
             position = "RB", n = n, through = thr)))
      arms <- c(arms, lapply(0:5, function(n)
        list(arm = sprintf("wr%d_by%d", n, thr), kind = "count_by",
             position = "WR", n = n, through = thr)))
      arms <- c(arms, lapply(0:2, function(n)
        list(arm = sprintf("te%d_by%d", n, thr), kind = "count_by",
             position = "TE", n = n, through = thr)))
    }
    arms <- c(arms, lapply(c(8L, 10L), function(r)
      list(arm = sprintf("te_after_%d", r), kind = "no_before",
           position = "TE", from = r)))
    arms <- c(arms, lapply(1:12, function(r)
      list(arm = sprintf("qbby_%d", r), kind = "qb_by", r = r)))
  }
  if (n_random > 0L) {
    set.seed(seed)
    arms <- c(arms, lapply(seq_len(n_random), function(i) {
      list(arm = sprintf("rand_%03d", i), kind = "random",
           script = sample(c("QB", "RB", "WR", "TE"), 8, replace = TRUE,
                           prob = c(0.25, 0.3, 0.35, 0.10)))
    }))
  }
  arms <- setNames(arms, vapply(arms, `[[`, character(1), "arm"))
  # an explicit allow-list lets a deep run spend its whole budget on the arms a
  # cheap pass already showed are worth measuring
  keep <- Sys.getenv("FFS_DRAFT_ARMS", "")
  if (nzchar(keep)) {
    want <- trimws(strsplit(keep, ",")[[1]])
    missing <- setdiff(want, names(arms))
    if (length(missing)) stop("unknown arm(s): ", paste(missing, collapse = ", "))
    arms <- arms[want]
  }
  arms
}

arm_policy <- function(spec) {
  switch(spec$kind,
         qb_grid = qb_timing_policy(spec$k, spec$j),
         qb_cap  = cap_policy("QB", spec$n),
         te_cap  = cap_policy("TE", spec$n),
         count_by = count_by_policy(spec$position, spec$n, spec$through),
         no_before = no_before_policy(spec$position, spec$from),
         qb_by   = qb_by_policy(spec$r),
         build_policy(spec$script))
}

# ------------------------------------------------------------- one world ----

run_world <- function(policy, branch, draws, board_seed) {
  # a branch either fixes the shift or draws it from the board seed
  sh <- if (!is.null(branch$shift_fn)) branch$shift_fn(board_seed) else branch$pos_shift
  drafted <- ffs_mock_draft(
    POOL, PICK_MAP,
    pos_need = POS_NEED, pos_cap = POS_CAP, pos_start_max = POS_START_MAX,
    # a 3rd QB in superflex is bye and injury cover for two mandatory slots, so
    # bodies past the start max are discounted, not near-worthless. At the 0.15
    # default the room stopped at ~2.3 QBs by pick 131 against a real 30.4.
    over_start_decay = 0.4,
    soft_bench = cfg$soft_bench,
    window_k = if (is.null(TIGHT)) cfg$window_k else TIGHT$window_k,
    pos_shift_ramp = if (is.null(TIGHT)) NULL else TIGHT$ramp,
    rank_decay = cfg$rank_decay, need_boost = cfg$need_boost,
    depth_decay = cfg$depth_decay,
    # No exemption. Giving my seat the "true" board while the room reaches does
    # NOT make it a sharp value drafter - measured, it ends up with the three
    # worst quarterbacks at the table (mean board rank 119 vs the field 84) and
    # loses 4.6pp of playoff odds. In a two-QB-slot format the room reaching on
    # QB is correct, not exploitable, so my seat drafts the same board it does.
    # Verified fair: slot 11 under identical rules scores 8.24% champion and
    # 49.2% playoff against the 8.33/50.0 baseline.
    pos_shift = sh, policy = policy, seed = board_seed)

  rosters <- merge(drafted[, list(franchise_id, fantasypros_id, pos)],
                   FR_INFO, by = "franchise_id")
  rosters <- rosters[, list(league_id, franchise_id, franchise_name,
                            player_id = fantasypros_id, fantasypros_id, pos)]

  summary <- ffs_keeper_world(
    rosters = rosters, projected_scores = draws$projected_scores,
    franchises = D$franchises, lineup_constraints = D$lineup_constraints,
    latest_rankings = RANKINGS_SIM, schedules = draws$schedules,
    playoff_slots = cfg$playoff_slots, playoff_weeks = PLAYOFF_WEEKS,
    weeks_per_round = cfg$weeks_per_round, pos_filter = cfg$pos_filter,
    lineup_method = "rank",
    # the hindsight-optimal LP feeds only lineup_efficiency, which this study
    # never reads - skipping it is 1.42x faster and bit-identical on every
    # standings, playoff, bye and championship number
    compute_optimal = FALSE)

  list(drafted = drafted, summary = as.data.table(summary))
}

# The board state a real drafter would see at each of the first two picks.
# With a 2-minute clock there is no time to simulate live, so the plan has to be
# a contingency table set in advance: these columns let every arm be scored
# WITHIN a stratum of how the room actually opened, by post-stratification,
# rather than only on average over all openings.
draft_state <- function(d, picks = MY_PICKS) {
  at <- function(p, col) {
    v <- d[[col]][d$overall == p]
    if (length(v)) as.character(v[[1]]) else NA_character_
  }
  p1 <- picks[[1]]; p2 <- picks[[2]]
  data.table(
    qb_before_p1 = sum(d$pos == "QB" & d$overall < p1),
    rb_before_p1 = sum(d$pos == "RB" & d$overall < p1),
    wr_before_p1 = sum(d$pos == "WR" & d$overall < p1),
    qb_before_p2 = sum(d$pos == "QB" & d$overall < p2),
    pick1_pos = at(p1, "pos"), pick1_player = at(p1, "player"),
    pick2_pos = at(p2, "pos"), pick2_player = at(p2, "player"))
}

# per-board rows for one arm, so the caller can pair against the control
score_arm <- function(policy, branch, draws, board_seeds, keep_drafted = FALSE) {
  drafts <- vector("list", length(board_seeds))
  rows <- rbindlist(lapply(seq_along(board_seeds), function(i) {
    w <- run_world(policy, branch, draws, board_seeds[[i]])
    if (keep_drafted) drafts[[i]] <<- w$drafted[franchise_id == D$my_fid][, board := board_seeds[[i]]]
    s <- w$summary[franchise_id == D$my_fid]
    cbind(data.table(board = board_seeds[[i]]),
          s[, list(champion_pct, playoff_pct, bye_pct, h2h_wins, points_for)],
          draft_state(w$drafted))
  }))
  list(by_board = rows, drafted = if (keep_drafted) rbindlist(drafts) else NULL)
}
