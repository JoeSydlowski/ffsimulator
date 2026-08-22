#### Keeper-league draft simulation ####
#
# ffsimulator can only simulate a league whose rosters already exist. A keeper
# league before its draft has no rosters - each franchise has a handful of
# keepers and a set of draft picks. These functions build the missing piece:
# given a keeper selection for every franchise, they resolve what each keeper
# costs, mock-draft the remaining pool into the remaining picks, and score the
# resulting league. That makes "which keepers should I set?" answerable on
# simulated wins rather than on market value.

# --- keeper costs -----------------------------------------------------------

#' Keeper cost for every rostered player
#'
#' (EXPERIMENTAL) Resolves the draft round each rostered player would cost to
#' keep. The common keeper convention is "the round you drafted him, minus one,
#' floored at round 1"; players acquired off waivers have no prior draft round
#' and are instead priced at their current ADP round.
#'
#' @param rosters a rosters dataframe (`ffs_rosters()`) - needs `player_id`,
#'   `franchise_id`, `franchise_name`, `pos`, `fantasypros_id`
#' @param prev_picks the previous season's draft board - needs `player_id` and
#'   `round`, matching `rosters$player_id` (i.e. platform ids)
#' @param adp_ranks optional dataframe of `fantasypros_id` + `adp_rank` (an
#'   overall 1..N ordering) used to price players with no prior draft round
#' @param n_teams number of franchises, used to convert an ADP rank to a round
#' @param escalator how many rounds a keeper's cost escalates per year (1 = the
#'   usual "round drafted minus one")
#' @param floor_round the earliest round a keeper can cost (1 = a first-round
#'   keeper stays a first-round keeper)
#' @param max_round the last round of the draft; ADP-priced players beyond it
#'   get `base_round = max_round`. `NULL` leaves them uncapped.
#'
#' @return the rosters dataframe with `prev_round`, `base_round`,
#'   `top2_prev` (drafted in the previous draft's first two rounds - the input
#'   to a "keep only one of your top two rounds" rule) and `cost_source`
#'   (`"draft"`, `"adp"` or `NA` when the player cannot be priced)
#'
#' @export
ffs_keeper_costs <- function(rosters, prev_picks, adp_ranks = NULL,
                             n_teams = 12L, escalator = 1L, floor_round = 1L,
                             max_round = NULL) {
  assert_df(rosters, c("player_id", "franchise_id", "pos", "fantasypros_id"))
  assert_df(prev_picks, c("player_id", "round"))
  checkmate::assert_int(n_teams, lower = 2)
  checkmate::assert_int(escalator, lower = 0)
  checkmate::assert_int(floor_round, lower = 1)

  prev_round <- base_round <- adp_rank <- cost_source <- top2_prev <- NULL
  player_id <- round <- fantasypros_id <- NULL

  r <- data.table::as.data.table(data.table::copy(rosters))
  r[, player_id := as.character(player_id)]

  pp <- data.table::as.data.table(prev_picks)[, list(
    player_id = as.character(player_id),
    prev_round = as.integer(round)
  )]
  # a player can only have been drafted once; guard against duplicated boards
  pp <- pp[!duplicated(pp$player_id)]

  r <- merge(r, pp, by = "player_id", all.x = TRUE, sort = FALSE)

  r[, cost_source := data.table::fifelse(!is.na(prev_round), "draft", NA_character_)]
  r[, base_round := pmax(floor_round, prev_round - escalator)]
  r[, top2_prev := !is.na(prev_round) & prev_round <= 2L]

  if (!is.null(adp_ranks)) {
    assert_df(adp_ranks, c("fantasypros_id", "adp_rank"))
    ar <- data.table::as.data.table(adp_ranks)[, list(
      fantasypros_id = as.character(fantasypros_id),
      adp_rank = as.integer(adp_rank)
    )]
    ar <- ar[!duplicated(ar$fantasypros_id)]
    r[, fantasypros_id := as.character(fantasypros_id)]
    r <- merge(r, ar, by = "fantasypros_id", all.x = TRUE, sort = FALSE)
    # waiver adds are priced at the round their current ADP implies
    r[is.na(base_round) & !is.na(adp_rank),
      `:=`(base_round = pmax(floor_round, as.integer(ceiling(adp_rank / n_teams))),
           cost_source = "adp")]
  }

  if (!is.null(max_round)) {
    r[cost_source == "adp" & base_round > max_round, base_round := as.integer(max_round)]
  }

  r[]
}

# --- round assignment -------------------------------------------------------

#' Assign keepers to draft rounds
#'
#' (EXPERIMENTAL) A keeper costs the pick in its `base_round`, but a franchise
#' only owns one pick per round. When two keepers land on the same round the
#' usual house rule is that one of them *bumps up* to the next earlier round
#' (two round-6 keepers cost a 6th and a 5th). Each player is therefore
#' assignable to any round in `1..base_round`, each round is usable once, and
#' the franchise wants the assignment that forfeits the *latest* (cheapest)
#' picks.
#'
#' Processing players from the latest `base_round` backwards and giving each the
#' largest free round at or below its base is optimal here: a player with a
#' later base has strictly more options, so consuming his own round first never
#' blocks an earlier-base player who could not have used it anyway.
#'
#' @param costs a dataframe of candidates - needs `player_id`, `base_round` and
#'   (if `max_top2` is finite) `top2_prev`, as produced by [ffs_keeper_costs()]
#' @param chosen_ids optional character vector of `player_id`s to assign;
#'   defaults to every row of `costs`
#' @param max_keepers maximum number of keepers allowed
#' @param max_top2 maximum number of keepers that may come from the previous
#'   draft's first two rounds
#'
#' @return a list: `feasible` (flag), `reason` (character, `NA` when feasible),
#'   `assignment` (a data.table of `player_id`, `base_round`, `keeper_round`)
#'   and `rounds_used` (the forfeited rounds)
#'
#' @export
ffs_assign_keeper_rounds <- function(costs, chosen_ids = NULL,
                                     max_keepers = Inf, max_top2 = Inf) {
  assert_df(costs, c("player_id", "base_round"))
  player_id <- base_round <- keeper_round <- top2_prev <- NULL

  cst <- data.table::as.data.table(costs)
  cst[, player_id := as.character(player_id)]
  if (!is.null(chosen_ids)) cst <- cst[player_id %in% as.character(chosen_ids)]
  cst <- cst[!duplicated(cst$player_id)]

  fail <- function(reason) {
    list(feasible = FALSE, reason = reason,
         assignment = data.table::data.table(
           player_id = character(), base_round = integer(), keeper_round = integer()),
         rounds_used = integer())
  }

  if (nrow(cst) == 0L) {
    return(list(feasible = TRUE, reason = NA_character_,
                assignment = data.table::data.table(
                  player_id = character(), base_round = integer(), keeper_round = integer()),
                rounds_used = integer()))
  }
  if (nrow(cst) > max_keepers) return(fail("too many keepers"))
  if (anyNA(cst$base_round)) return(fail("unpriced keeper"))
  if (is.finite(max_top2)) {
    if (!"top2_prev" %in% names(cst)) return(fail("top2_prev column required"))
    if (sum(cst$top2_prev, na.rm = TRUE) > max_top2) {
      return(fail("too many keepers from the previous top two rounds"))
    }
  }

  cst <- cst[order(-base_round)]
  taken <- integer(0)
  assigned <- integer(nrow(cst))
  for (i in seq_len(nrow(cst))) {
    b <- cst$base_round[[i]]
    free <- setdiff(seq_len(b), taken)
    if (length(free) == 0L) return(fail("no free round at or before base_round"))
    rd <- max(free)
    assigned[[i]] <- rd
    taken <- c(taken, rd)
  }
  cst[, keeper_round := assigned]

  list(feasible = TRUE, reason = NA_character_,
       assignment = cst[order(keeper_round), list(player_id, base_round, keeper_round)],
       rounds_used = sort(taken))
}

# --- draft board ------------------------------------------------------------

#' Map snake-draft picks to franchises
#'
#' (EXPERIMENTAL) Builds the list of picks each franchise owns, given its draft
#' slot, and removes the rounds it has forfeited to keepers.
#'
#' @param draft_slots a named vector mapping `franchise_id` to draft slot
#'   (`1..n_teams`); names are the franchise ids
#' @param n_rounds number of draft rounds
#' @param forfeited a named list mapping `franchise_id` to the integer rounds
#'   that franchise spent on keepers
#'
#' @return a data.table of `franchise_id`, `round`, `slot`, `pick_in_round`,
#'   `overall`, ordered by `overall`
#'
#' @export
ffs_draft_pick_map <- function(draft_slots, n_rounds, forfeited = NULL) {
  checkmate::assert_int(n_rounds, lower = 1)
  n_teams <- length(draft_slots)
  overall <- franchise_id <- round <- slot <- pick_in_round <- NULL

  fid_by_slot <- character(n_teams)
  fid_by_slot[as.integer(draft_slots)] <- names(draft_slots)

  # pick_in_round is the position on the clock; snake means even rounds run back
  # down the board, so the slot picking there is mirrored
  grid <- data.table::CJ(round = seq_len(n_rounds), pick_in_round = seq_len(n_teams))
  grid[, slot := data.table::fifelse(
    round %% 2L == 0L, as.integer(n_teams - pick_in_round + 1L), as.integer(pick_in_round))]
  grid[, franchise_id := fid_by_slot[slot]]
  grid[, overall := (round - 1L) * n_teams + pick_in_round]

  if (!is.null(forfeited)) {
    drop <- data.table::rbindlist(lapply(names(forfeited), function(f) {
      rds <- forfeited[[f]]
      if (length(rds) == 0L) return(NULL)
      data.table::data.table(franchise_id = f, round = as.integer(rds))
    }))
    if (nrow(drop)) grid <- grid[!drop, on = c("franchise_id", "round")]
  }

  grid[order(overall)]
}

#' Mock-draft the remaining pool
#'
#' (EXPERIMENTAL) Fills each franchise's remaining picks from the undrafted
#' pool. Each pick is drawn from a bounded window of the best remaining players
#' by true rank (not the full pool - a fringe player's rank uncertainty can
#' never launch him to the top of the board), weighted toward the top of that
#' window but tilted by roster construction: a franchise below its starter
#' floor at a position is boosted, one comfortably past it is discounted. A
#' hard backstop still forces a franchise to fill unmet starter needs once it
#' has only that many picks left, with no randomness - the guarantee that
#' every team finishes with a legal lineup should never depend on a coin flip.
#'
#' @param pool available players - needs `fantasypros_id`, `player`, `pos`,
#'   `ecr` (a TRUE rank - board order is deterministic; the window +
#'   roster-fit weighting below is what makes the pick itself stochastic)
#' @param pick_map picks to fill, as produced by [ffs_draft_pick_map()]
#' @param kept optional dataframe of players already on rosters (keepers) -
#'   needs `franchise_id` and `pos` - used to seed each franchise's positional
#'   counts
#' @param pos_need named integer vector of positions a franchise must end up
#'   with (the hard starter floor - e.g. lineup minimums, not roster targets)
#' @param pos_start_max named vector of how many at each position can ever
#'   START (lineup_constraints$max). Bodies beyond this are pure bench: a 4th QB
#'   in a 2-QB-max lineup cannot play, so ranking him on board value alone makes
#'   the drafter take players it can never use. `NULL` disables the penalty.
#' @param over_start_decay weight multiplier per body past `pos_start_max`
#' @param rookie_boost ranks to move rookies UP the board. Redraft ranking
#'   services systematically distrust rookies, but drafters do not: in Joe's
#'   league rookies went ~11.7 picks (~26 ranks) earlier than their preseason
#'   rank implied, controlling for rank (p=0.03, 2024-25). Requires a logical
#'   `rookie` column on `pool`; 0 disables.
#' @param pos_cap named vector of positional roster maximums (`Inf` = no cap;
#'   this is a hard ceiling like a platform's per-position roster limit, not a
#'   preference - use `soft_bench`/`depth_decay` to softly discourage excess)
#' @param soft_bench extra copies beyond `pos_need` a franchise wants before a
#'   position starts getting discounted (bench/flex depth)
#' @param window_k how many of the best remaining eligible players (by true
#'   rank) are in play for a given pick
#' @param rank_decay within the window, how much the pick still favors the top:
#'   weight `exp(-rank_decay * (rank_in_window - 1))` before the fit multiplier
#' @param need_boost weight multiplier for a position below `pos_need`
#' @param depth_decay weight multiplier per pick beyond `pos_need + soft_bench`
#'   at a position (`depth_decay^k` for the k-th excess copy)
#' @param seed optional integer seed
#'
#' @return a data.table of drafted players: `franchise_id`, `round`, `overall`,
#'   `fantasypros_id`, `player`, `pos`, `ecr`
#'
#' @export
ffs_mock_draft <- function(pool, pick_map, kept = NULL,
                           pos_need = c(QB = 1L, RB = 2L, WR = 3L, TE = 1L),
                           pos_cap = c(QB = Inf, RB = Inf, WR = Inf, TE = Inf),
                           pos_start_max = NULL, over_start_decay = 0.15,
                           rookie_boost = 0,
                           soft_bench = 1L, window_k = 6L, rank_decay = 0.7,
                           need_boost = 1.6, depth_decay = 0.4,
                           seed = NULL) {
  assert_df(pool, c("fantasypros_id", "player", "pos", "ecr"))
  assert_df(pick_map, c("franchise_id", "round", "overall"))
  ecr <- pos <- fantasypros_id <- ecr_adj <- rookie <- NULL

  if (!is.null(seed)) set.seed(seed)

  p <- data.table::as.data.table(data.table::copy(pool))
  p <- p[!is.na(ecr) & pos %in% names(pos_cap)]
  # board order: true rank, optionally shifting rookies earlier to match how
  # this league actually drafts them (ecr itself is left untouched for reporting)
  if (rookie_boost != 0 && "rookie" %in% names(p)) {
    p[, ecr_adj := ecr - rookie_boost * as.integer(!is.na(rookie) & rookie)]
  } else {
    p[, ecr_adj := ecr]
  }
  data.table::setorder(p, ecr_adj)

  picks <- data.table::as.data.table(pick_map)[order(overall)]
  fids <- unique(picks$franchise_id)
  positions <- names(pos_cap)

  # positional counts each franchise already has from its keepers
  have <- matrix(0L, nrow = length(fids), ncol = length(positions),
                 dimnames = list(fids, positions))
  if (!is.null(kept) && nrow(kept)) {
    k <- data.table::as.data.table(kept)
    kt <- table(factor(as.character(k$franchise_id), levels = fids),
                factor(k$pos, levels = positions))
    have[] <- as.integer(kt)
  }

  picks_left <- table(factor(picks$franchise_id, levels = fids))

  avail <- rep(TRUE, nrow(p))
  pos_vec <- p$pos
  out_idx <- integer(nrow(picks))

  for (i in seq_len(nrow(picks))) {
    f <- picks$franchise_id[[i]]
    hv <- have[f, ]
    unmet <- pmax(0L, pos_need[positions] - hv)
    open_pos <- positions[hv < pos_cap[positions]]

    # picks_left counts this pick; once it equals the unmet starter needs every
    # remaining pick is committed to a position the roster still requires - and
    # taken deterministically, with no randomness, so the guarantee never fails
    hard_backstop <- picks_left[[f]] <= sum(unmet)
    allowed <- if (hard_backstop) {
      intersect(positions[unmet > 0L], open_pos)
    } else {
      open_pos
    }
    if (length(allowed) == 0L) allowed <- open_pos
    if (length(allowed) == 0L) allowed <- positions

    picks_left[[f]] <- picks_left[[f]] - 1L

    hit <- which(avail & pos_vec %in% allowed) # p is sorted by true ecr
    if (length(hit) == 0L) next

    if (hard_backstop) {
      sel <- hit[[1L]]
    } else {
      window <- hit[seq_len(min(window_k, length(hit)))]
      if (length(window) == 1L) {
        sel <- window[[1L]] # sample(x, 1) on a length-1 x samples from 1:x, not x
      } else {
        w_pos <- pos_vec[window]
        hv_w <- hv[w_pos]
        need_w <- pos_need[w_pos]
        soft_target <- need_w + soft_bench
        fit <- ifelse(hv_w < need_w, need_boost,
                      ifelse(hv_w < soft_target, 1,
                             depth_decay^(hv_w - soft_target + 1)))
        if (!is.null(pos_start_max)) {
          # past what can start, extra bodies are near-worthless regardless of rank
          over <- pmax(0L, hv_w - as.integer(pos_start_max[w_pos]) + 1L)
          fit <- fit * over_start_decay^over
        }
        rank_w <- exp(-rank_decay * (seq_along(window) - 1))
        sel <- sample(window, size = 1L, prob = fit * rank_w)
      }
    }

    out_idx[[i]] <- sel
    avail[[sel]] <- FALSE
    have[f, pos_vec[[sel]]] <- have[f, pos_vec[[sel]]] + 1L
  }

  keep <- out_idx > 0L
  data.table::data.table(
    franchise_id = picks$franchise_id[keep],
    round = picks$round[keep],
    overall = picks$overall[keep],
    fantasypros_id = p$fantasypros_id[out_idx[keep]],
    player = p$player[out_idx[keep]],
    pos = p$pos[out_idx[keep]],
    ecr = p$ecr[out_idx[keep]]
  )
}

# --- scoring one keeper world ----------------------------------------------

#' Score one keeper/draft world
#'
#' (EXPERIMENTAL) Runs a set of hypothetical rosters through the back half of
#' the simulation pipeline and returns each franchise's standings summary.
#' `projected_scores` and `schedules` are deliberately inputs rather than being
#' regenerated: comparing two keeper sets is a paired comparison, so every world
#' must share the same player-score draws and the same schedule, and differ only
#' in who is on which roster.
#'
#' Playoff seeding is wins, then points-for, resolved deterministically - the
#' same convention as [`.ffs_summarise_optimal`]. Random tie-breaks re-roll
#' between worlds and leak into the delta.
#'
#' @param rosters hypothetical rosters - needs `league_id`, `franchise_id`,
#'   `franchise_name`, `player_id`, `fantasypros_id`, `pos`
#' @param projected_scores projections from [ffs_generate_projections()]
#' @param franchises a franchises dataframe ([ffs_franchises()])
#' @param lineup_constraints from [ffs_starter_positions()]
#' @param latest_rankings from [ffs_latest_rankings()], used for replacement level
#' @param schedules from [ffs_build_schedules()] - its weeks define the regular season
#' @param playoff_slots number of playoff berths
#' @param playoff_weeks optional weeks (present in `projected_scores` but not in
#' @param weeks_per_round games per bracket round (2 for a league whose
#'   playoff rounds run two weeks and advance on combined score)
#'   `schedules`) used to play out a single-elimination bracket for
#'   `champion_pct`; `NULL` skips it
#' @param pos_filter positions to simulate
#' @param replacement_level add waiver-wire replacement players to every roster
#' @param lineup_method passed to [ffs_optimise_lineups()]
#'
#' @return a data.table with one row per franchise: `h2h_wins`, `points_for`,
#'   `playoff_pct`, and `champion_pct` when `playoff_weeks` is supplied
#'
#' @export
ffs_keeper_world <- function(rosters, projected_scores, franchises,
                             lineup_constraints, latest_rankings, schedules,
                             playoff_slots = 6L, playoff_weeks = NULL, weeks_per_round = 1L,
                             pos_filter = c("QB", "RB", "WR", "TE"),
                             replacement_level = TRUE,
                             lineup_method = "rank") {
  assert_df(rosters, c("league_id", "franchise_id", "franchise_name",
                       "fantasypros_id", "pos"))
  season <- week <- h2h_wins <- points_for <- lg_rank <- franchise_id <- NULL
  franchise_name <- playoff_pct <- NULL

  r <- data.table::as.data.table(data.table::copy(rosters))
  if (isTRUE(replacement_level)) {
    r <- ffs_add_replacement_level(
      rosters = r, latest_rankings = latest_rankings,
      franchises = franchises, lineup_constraints = lineup_constraints,
      pos_filter = pos_filter)
  }

  roster_scores <- ffs_score_rosters(
    projected_scores = projected_scores,
    rosters = r[, c("league_id", "franchise_id", "franchise_name",
                    "player_id", "fantasypros_id", "pos"), with = FALSE])

  optimal <- data.table::as.data.table(ffs_optimise_lineups(
    roster_scores = roster_scores,
    lineup_constraints = lineup_constraints,
    lineup_method = lineup_method,
    pos_filter = pos_filter))

  sw <- data.table::as.data.table(
    ffs_summarise_week(optimal_scores = optimal, schedules = schedules))
  ss <- data.table::as.data.table(ffs_summarise_season(summary_week = sw))
  ss[, lg_rank := data.table::frank(list(-h2h_wins, -points_for), ties.method = "first"),
     by = season]

  agg <- ss[, list(
    seasons = .N,
    h2h_wins = mean(h2h_wins),
    points_for = mean(points_for),
    playoff_pct = mean(lg_rank <= playoff_slots)
  ), by = list(franchise_id, franchise_name)]

  if (!is.null(playoff_weeks)) {
    champ <- .ffs_bracket_pct(optimal, ss, playoff_slots, playoff_weeks, weeks_per_round)
    agg <- merge(agg, champ, by = "franchise_id", all.x = TRUE)
    agg[is.na(agg$champion_pct), "champion_pct"] <- 0
  }

  agg[order(-playoff_pct)]
}

#' Championship odds from a played-out bracket
#'
#' Single-elimination over the top `playoff_slots` seeds using simulated scores
#' from `playoff_weeks` - no normal approximation, the bracket is played with
#' the same weekly draws as the regular season. Seed `i` meets seed
#' `playoff_slots + 1 - i` in the first round.
#'
#' @param optimal optimal_scores for all simulated weeks
#' @param ss summary_season carrying `lg_rank`
#' @param playoff_slots number of berths (a power of two)
#' @param playoff_weeks weeks available to the bracket, in order
#' @param weeks_per_round games per bracket round. Many leagues (Sleeper
#'   `playoff_round_type = 2`) play TWO weeks per round and advance on the
#'   combined score, which meaningfully damps upsets versus a single game.
#'
#' @return a data.table of `franchise_id`, `champion_pct`
#' @keywords internal
.ffs_bracket_pct <- function(optimal, ss, playoff_slots, playoff_weeks, weeks_per_round = 1L) {
  season <- week <- lg_rank <- franchise_id <- actual_score <- NULL

  n_rounds <- log2(playoff_slots)
  if (n_rounds != as.integer(n_rounds)) {
    cli::cli_warn("{.arg playoff_slots} = {playoff_slots} is not a power of two - skipping the bracket")
    return(data.table::data.table(franchise_id = character(), champion_pct = numeric()))
  }
  n_rounds <- as.integer(n_rounds)
  need_wk <- n_rounds * weeks_per_round
  if (length(playoff_weeks) < need_wk) {
    cli::cli_warn("need {need_wk} playoff week{?s}, got {length(playoff_weeks)} - skipping the bracket")
    return(data.table::data.table(franchise_id = character(), champion_pct = numeric()))
  }

  sc <- data.table::as.data.table(optimal)[
    week %in% playoff_weeks, list(season, week, franchise_id, actual_score)]
  data.table::setkeyv(sc, c("season", "week", "franchise_id"))

  # seeds: one row per (season, seed)
  field <- ss[lg_rank <= playoff_slots, list(season, franchise_id, seed = lg_rank)]
  alive <- data.table::copy(field)

  for (rd in seq_len(n_rounds)) {
    wk <- playoff_weeks[seq((rd - 1L) * weeks_per_round + 1L, rd * weeks_per_round)]
    # combined score across the round's weeks - a 2-week round aggregates two
    # games, which is why it favours the stronger team more than a single week
    rs <- sc[week %in% wk, list(actual_score = sum(actual_score)), by = list(season, franchise_id)]
    a <- merge(alive, rs,
               by = c("season", "franchise_id"), all.x = TRUE)
    # pair best remaining seed against worst
    data.table::setorder(a, season, seed)
    a[, "slot" := seq_len(.N), by = season]
    n_alive <- a[, .N, by = season]$N[[1]]
    a[, "match" := pmin(slot, n_alive - slot + 1L)]
    a[is.na(a$actual_score), "actual_score"] <- -Inf
    # higher score advances; seed breaks a tie
    data.table::setorder(a, season, match, -actual_score, seed)
    alive <- a[, .SD[1L], by = c("season", "match")][, list(season, franchise_id, seed)]
  }

  n_seasons <- length(unique(ss$season))
  champs <- alive[, list(champion_pct = .N / n_seasons), by = franchise_id]
  champs[]
}
