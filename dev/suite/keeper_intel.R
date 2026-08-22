#!/usr/bin/env Rscript
# keeper_intel.R -- which keepers should I set?
#
# A keeper costs a draft pick, so "is he worth keeping" is really "is he worth
# more than the player I'd have drafted with that pick, given everyone else's
# keepers thinned the pool too". This answers that on simulated wins:
#
#   1. resolve what every rostered player costs to keep (prior round - 1, ADP
#      for waiver adds), including the bump-up rule for collisions
#   2. take opponents' declared keepers as fact; predict the undeclared ones
#   3. draw player scores ONCE, then for each candidate keeper set: forfeit the
#      picks, mock-draft the thinned pool into what's left, and score the league
#   4. SEARCH cheap for the best set, CONFIRM the shortlist at a real budget
#
# Every world shares the same score draws and schedule -- these are paired
# comparisons, so anything but the keeper set must be held fixed.
#
# Usage:  Rscript dev/suite/keeper_intel.R          (env-configured, see below)
# or:     source() it after setting `keeper_config`
#
# `source()`-ing this file always builds costs/opponent predictions (cheap) but
# only RUNS the SEARCH/CONFIRM stages (expensive - full builds) when
# `keeper_config$run_search` is TRUE (the default; set FALSE for interactive
# testing of the setup half, e.g. from dev-scratch scripts).

suppressPackageStartupMessages({
  library(data.table)
  library(ffscrapr)
  library(jsonlite)
})
if (!"ffsimulator" %in% loadedNamespaces()) {
  suppressMessages(devtools::load_all(rprojroot::find_root(rprojroot::has_file("DESCRIPTION")), quiet = TRUE))
}
# ffscrapr reads a dead nflverse asset, so ff_scoringhistory() returns ZERO rows
# for 2025+ and every simulation silently loses its most recent season.
source(here::here("dev", "suite", "scoring_history_shim.R")); install_shim()

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------------------------------------------------------------- config ----

if (!exists("keeper_config")) keeper_config <- list()
cfg <- utils::modifyList(list(
  league_id      = Sys.getenv("FFS_KEEP_LEAGUE", "1339817277222031360"),
  prev_league_id = Sys.getenv("FFS_KEEP_PREV_LEAGUE", "1248319500349087744"),
  season         = as.integer(Sys.getenv("FFS_KEEP_SEASON", "2026")),
  prev_season    = as.integer(Sys.getenv("FFS_KEEP_PREV_SEASON", "2025")),
  my_franchise   = Sys.getenv("FFS_KEEP_ME", "7"),
  max_keepers    = as.integer(Sys.getenv("FFS_KEEP_MAX", "8")),
  max_top2       = as.integer(Sys.getenv("FFS_KEEP_MAX_TOP2", "1")),
  n_rounds       = as.integer(Sys.getenv("FFS_KEEP_ROUNDS", "17")),
  playoff_slots  = as.integer(Sys.getenv("FFS_KEEP_PLAYOFF_SLOTS", "4")),
  reg_weeks      = as.integer(Sys.getenv("FFS_KEEP_WEEKS", "13")),
  # Sleeper playoff_round_type=2 means TWO weeks per playoff round, advancing
  # on combined score. That damps upsets versus a single game, so it matters
  # for champion_pct even though it never touches playoff_pct.
  weeks_per_round = as.integer(Sys.getenv("FFS_KEEP_WPR", "2")),
  base_seasons   = 2012:2025,
  pos_filter     = c("QB", "RB", "WR", "TE"),
  proj_pool_n    = as.integer(Sys.getenv("FFS_KEEP_POOL", "300")),
  # ADP source. Empty = FantasyPros superflex overall (page_type "redraft-op").
  # A csv path = the league's own board (e.g. Sleeper's official ADP export),
  # which is the better model of who actually goes when, AND is what the house
  # rule prices undrafted keepers off. adp_col names the column to use.
  adp_csv        = Sys.getenv("FFS_KEEP_ADP_CSV", ""),
  adp_col        = Sys.getenv("FFS_KEEP_ADP_COL", "Redraft SF ADP"),
  # search stage: rank-only, cheap
  n_search       = as.integer(Sys.getenv("FFS_KEEP_NSEARCH", "20")),
  k_search       = as.integer(Sys.getenv("FFS_KEEP_KSEARCH", "1")),
  n_cand_search  = as.integer(Sys.getenv("FFS_KEEP_NCAND", "14")),
  max_swap_passes = as.integer(Sys.getenv("FFS_KEEP_SWAPPASSES", "3")),
  # confirm stage: K boards x n seasons per candidate set
  n_confirm      = as.integer(Sys.getenv("FFS_KEEP_NCONFIRM", "150")),
  k_boards       = as.integer(Sys.getenv("FFS_KEEP_KBOARDS", "3")),
  n_sets_confirm = as.integer(Sys.getenv("FFS_KEEP_NSETS", "4")),
  # autodraft: pick drawn from the top window_k remaining (by true rank),
  # weighted toward the top of that window (rank_decay) but tilted by roster
  # fit (need_boost/soft_bench/depth_decay) -- see ffs_mock_draft()
  window_k       = as.integer(Sys.getenv("FFS_KEEP_WINDOWK", "6")),
  rank_decay     = as.numeric(Sys.getenv("FFS_KEEP_RANKDECAY", "0.7")),
  need_boost     = as.numeric(Sys.getenv("FFS_KEEP_NEEDBOOST", "1.6")),
  soft_bench     = as.integer(Sys.getenv("FFS_KEEP_SOFTBENCH", "1")),
  depth_decay    = as.numeric(Sys.getenv("FFS_KEEP_DEPTHDECAY", "0.4")),
  seed           = as.integer(Sys.getenv("FFS_KEEP_SEED", "20260818")),
  cache          = Sys.getenv("FFS_KEEP_CACHE", "dev/data/keeper_cache.rds"),
  outdir         = Sys.getenv("FFS_KEEP_OUT", ""),
  run_search     = TRUE
), keeper_config)

if (!nzchar(cfg$outdir)) {
  cfg$outdir <- file.path("dev", "league_sims", paste0("keepers_", cfg$league_id, "_", Sys.Date()))
}
dir.create(cfg$outdir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(cfg$cache), recursive = TRUE, showWarnings = FALSE)

msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")

# ------------------------------------------------------------- 1. data ------

sleeper_get <- function(path) jsonlite::fromJSON(paste0("https://api.sleeper.app/v1/", path))

load_league_data <- function(cfg) {
  msg("pulling league data")
  conn  <- ffscrapr::ff_connect("sleeper", league_id = cfg$league_id, season = cfg$season)
  conn0 <- ffscrapr::ff_connect("sleeper", league_id = cfg$prev_league_id, season = cfg$prev_season)

  latest_rankings <- as.data.table(ffs_latest_rankings("draft"))
  rosters <- as.data.table(ffs_rosters(conn))
  rosters <- as.data.table(ffs_backfill_fp_ids(rosters, latest_rankings))
  franchises <- as.data.table(ffs_franchises(conn))
  lineup_constraints <- as.data.table(ffs_starter_positions(conn))

  # superflex overall board order -- latest_rankings$ecr is POSITIONAL, useless
  # for ordering a draft. redraft-op ("rsf") is FantasyPros' superflex overall.
  fp <- as.data.table(nflreadr::load_ff_rankings())
  op <- fp[fp$page_type == "redraft-op"][order(ecr)]
  op <- op[, list(fantasypros_id = as.character(id), op_ecr = ecr, op_sd = sd)]
  op <- op[!duplicated(op$fantasypros_id)][, adp_rank := seq_len(.N)]

  if (nzchar(cfg$adp_csv)) {
    # league's own ADP board, crosswalked sleeper_id -> fantasypros_id. Falls
    # back to cleaned name+pos for players dp_playerids has not caught up on
    # (the perennial rookie id-lag), then keeps the FantasyPros row for anyone
    # still unmatched so the pool never silently shrinks.
    s <- data.table::fread(cfg$adp_csv)
    data.table::setnames(s, cfg$adp_col, "sf_adp")
    s[, sleeper_id := as.character(`Player Id`)]
    s[, nm := nflreadr::clean_player_names(paste(`Player First Name`, `Player Last Name`))]
    s[, pos := `Fantasy Player Position`]
    s <- s[!is.na(sf_adp)]
    xw <- as.data.table(ffscrapr::dp_playerids())[
      , list(sleeper_id = as.character(sleeper_id), fantasypros_id = as.character(fantasypros_id))]
    s <- merge(s, xw[!is.na(sleeper_id)], by = "sleeper_id", all.x = TRUE)
    nm_lookup <- unique(latest_rankings[
      , list(fantasypros_id = as.character(fantasypros_id),
             nm = nflreadr::clean_player_names(player), pos)])
    nm_lookup <- nm_lookup[!duplicated(paste(nm, pos))]
    s <- merge(s, nm_lookup[, list(nm, pos, fp_by_name = fantasypros_id)],
               by = c("nm", "pos"), all.x = TRUE)
    s[is.na(fantasypros_id), fantasypros_id := fp_by_name]
    matched <- sum(!is.na(s$fantasypros_id))
    cli::cli_alert_info("ADP csv: {matched}/{nrow(s)} rows crosswalked to fantasypros_id")
    s <- s[!is.na(fantasypros_id)][order(sf_adp)]
    op <- s[!duplicated(s$fantasypros_id), list(fantasypros_id, op_ecr = sf_adp, op_sd = NA_real_)]
    op[, adp_rank := seq_len(.N)]
  }

  # league mechanics that ffscrapr does not surface
  raw_rosters <- sleeper_get(paste0("league/", cfg$league_id, "/rosters"))
  declared <- rbindlist(lapply(seq_len(nrow(raw_rosters)), function(i) {
    k <- raw_rosters$keepers[[i]]
    if (is.null(k) || !length(k)) return(NULL)
    data.table(franchise_id = as.character(raw_rosters$roster_id[i]), player_id = as.character(k))
  })) %||% data.table(franchise_id = character(), player_id = character())

  lg <- sleeper_get(paste0("league/", cfg$league_id))
  dr <- sleeper_get(paste0("draft/", lg$draft_id))
  slot_by_user <- unlist(dr$draft_order)
  draft_slots <- setNames(
    as.integer(slot_by_user[match(franchises$user_id, names(slot_by_user))]),
    as.character(franchises$franchise_id))
  stopifnot(!anyNA(draft_slots))

  prev_picks <- as.data.table(sleeper_get(paste0("draft/", dr$previous_draft_id %||%
    sleeper_get(paste0("league/", cfg$prev_league_id))$draft_id, "/picks")))
  prev_picks <- prev_picks[, list(player_id = as.character(player_id), round = as.integer(round))]

  # the autodraft's hard floor/ceiling, from the league's own rules -- NOT
  # hand-tuned guesses. pos_need = the true starter minimum (lineup_constraints
  # min already accounts for FLEX/SUPERFLEX eligibility only via max, not min,
  # so min IS the "cannot start a legal lineup without this many" floor).
  # pos_cap = lineup_constraints max is a STARTER-eligibility ceiling (how many
  # could ever start), not a roster cap - using it as a roster cap would forbid
  # realistic bench/trade-bait depth (e.g. a 3rd superflex QB). The only real
  # roster-wide ceiling is a platform position_limit_*, and only when the
  # platform is actually enforcing it.
  lc <- lineup_constraints[pos %in% cfg$pos_filter]
  pos_need <- setNames(as.integer(lc$min), lc$pos)[cfg$pos_filter]
  pos_cap <- setNames(rep(Inf, length(cfg$pos_filter)), cfg$pos_filter)
  if (isTRUE(as.logical(dr$settings$enforce_position_limits))) {
    lim_keys <- grep("^position_limit_", names(dr$settings), value = TRUE)
    lim <- setNames(as.numeric(unlist(dr$settings[lim_keys])),
                    toupper(sub("^position_limit_", "", lim_keys)))
    common <- intersect(names(pos_cap), names(lim))
    pos_cap[common] <- lim[common]
  }
  msg("keeper draft pos_need:", paste(sprintf("%s=%d", names(pos_need), pos_need), collapse = " "),
      "| pos_cap:", paste(sprintf("%s=%s", names(pos_cap), pos_cap), collapse = " "))

  msg("scoring history + adp outcomes")
  scoring_history <- ffscrapr::ff_scoringhistory(conn, cfg$base_seasons)
  adp_outcomes <- ffs_adp_outcomes(scoring_history = scoring_history, gp_model = "simple",
                                   pos_filter = cfg$pos_filter, version = "v3")

  list(conn = conn, latest_rankings = latest_rankings, rosters = rosters,
       franchises = franchises, lineup_constraints = lineup_constraints,
       op = op, declared = declared, draft_slots = draft_slots,
       prev_picks = prev_picks, adp_outcomes = adp_outcomes,
       pos_need = pos_need, pos_cap = pos_cap)
}

if (nzchar(cfg$cache) && file.exists(cfg$cache) && !identical(Sys.getenv("FFS_KEEP_REFRESH"), "1")) {
  msg("loading cached league data:", cfg$cache)
  D <- readRDS(cfg$cache)
} else {
  D <- load_league_data(cfg)
  if (nzchar(cfg$cache)) saveRDS(D, cfg$cache)
}

n_teams <- nrow(D$franchises)

# ------------------------------------------------- 2. keeper cost table -----

costs <- ffs_keeper_costs(
  rosters = D$rosters,
  prev_picks = D$prev_picks,
  adp_ranks = D$op[, list(fantasypros_id, adp_rank)],
  n_teams = n_teams,
  escalator = 1L, floor_round = 1L, max_round = cfg$n_rounds)
costs <- merge(costs, D$op[, list(fantasypros_id, op_ecr, adp_rank)],
               by = "fantasypros_id", all.x = TRUE, suffixes = c("", ".op"))
costs[, franchise_id := as.character(franchise_id)]
setorder(costs, franchise_id, base_round, op_ecr, na.last = TRUE)

fwrite(costs[, list(franchise_id, franchise_name, player_name, pos, prev_round,
                    base_round, top2_prev, cost_source, op_ecr, adp_rank)],
       file.path(cfg$outdir, "keeper_costs.csv"))

# ---------------------------------------------- 3. opponents' keepers -------

# rank-space value curve: exponential decay in overall superflex rank, the same
# shape ffsimulator's synthetic dynasty curve uses. Purely for the opponent
# heuristic and as a cross-check column -- my own decision is made on simulated
# wins, not on this.
rank_value <- function(rank) 10000 * exp(-0.023 * rank)
costs[, value := rank_value(adp_rank)]
costs[is.na(value), value := 0]

board_all <- D$op[, list(fantasypros_id, adp_rank)][, value := rank_value(adp_rank)][order(adp_rank)]

#' What each franchise's picks are actually worth
#'
#' Naively, round r returns the (r-1)*n_teams..r*n_teams slice of the board.
#' That is badly wrong in a keeper league: ~60 of the top players never reach
#' the draft, so every pick returns a much worse player than its round implies
#' and keepers look far cheaper than the naive number says. This instead walks
#' the real remaining picks in draft order against the pool that survives the
#' keepers -- the i-th surviving pick gets the i-th best surviving player.
#'
#' Each franchise is priced holding its OWN forfeits out: the question a keeper
#' has to answer is "what would this round have returned had I kept nobody",
#' given everyone else's keepers. Pricing a franchise against its own forfeits
#' is circular - the round it keeps at has no pick left to value.
pick_values <- function(kept_ids, forfeited) {
  pool <- board_all[!fantasypros_id %in% kept_ids]
  rbindlist(lapply(names(D$draft_slots), function(f) {
    pm <- ffs_draft_pick_map(D$draft_slots, cfg$n_rounds,
                             forfeited[setdiff(names(forfeited), f)])[order(overall)]
    pm[, exp_value := c(pool$value, rep(0, max(0L, .N - nrow(pool))))[seq_len(.N)]]
    pm[franchise_id == f, list(franchise_id, round, exp_value)]
  }))
}

#' Greedy keeper set by value-over-pick, respecting the assignment rules
greedy_keepers <- function(cand, max_keepers, max_top2) {
  cand <- cand[!is.na(base_round)][order(-value_over_pick)]
  chosen <- character(0)
  for (pid in cand$player_id) {
    trial <- c(chosen, pid)
    a <- ffs_assign_keeper_rounds(cand, trial, max_keepers = max_keepers, max_top2 = max_top2)
    if (a$feasible) chosen <- trial
    if (length(chosen) >= max_keepers) break
  }
  chosen
}

declared_fids <- unique(D$declared$franchise_id)
undeclared_fids <- setdiff(as.character(D$franchises$franchise_id),
                           c(declared_fids, cfg$my_franchise))
msg("declared keepers:", paste(declared_fids, collapse = ","),
    "| predicting:", paste(undeclared_fids, collapse = ","))

# Pick values depend on who is kept, and who is kept depends on pick values.
# Iterate: start from the declared keepers only, and let the predictions and the
# pick prices settle together. It converges in 2-3 passes.
#' Forfeited rounds implied by a keeper table, per franchise
forfeited_rounds <- function(kept_tbl) {
  fids <- unique(kept_tbl$franchise_id)
  setNames(lapply(fids, function(f) {
    a <- ffs_assign_keeper_rounds(costs[franchise_id == f],
                                  kept_tbl[franchise_id == f]$player_id,
                                  max_keepers = cfg$max_keepers, max_top2 = Inf)
    if (a$feasible) a$rounds_used else integer(0)
  }), fids)
}

#' Re-price every candidate against what its round's pick would actually return
reprice <- function(kept_tbl) {
  pv <- pick_values(costs[player_id %in% kept_tbl$player_id]$fantasypros_id,
                    forfeited_rounds(kept_tbl))
  pv <- unique(pv[, list(franchise_id, base_round = round, pick_val = exp_value)])
  if ("pick_val" %in% names(costs)) costs[, pick_val := NULL]
  if ("value_over_pick" %in% names(costs)) costs[, value_over_pick := NULL]
  out <- merge(costs, pv, by = c("franchise_id", "base_round"), all.x = TRUE, sort = FALSE)
  out[, value_over_pick := value - pick_val]
  out
}

#' Trim a declared keeper set down to its highest-value legal subset
#'
#' Sleeper does not validate keeper rules - the `keepers` field is just a list
#' the commissioner reads - so a declared set can break them. manshedHans
#' declared Hurts (2025 R1) and Nacua (2025 R2), which both cost a first and
#' both come out of last year's top two rounds. Rather than simulate an illegal
#' roster, drop the least valuable offenders until the set is legal.
legalize_keepers <- function(f, ids) {
  cand <- costs[franchise_id == f & player_id %in% ids]
  a <- ffs_assign_keeper_rounds(cand, cand$player_id, cfg$max_keepers, cfg$max_top2)
  if (a$feasible) return(cand$player_id)
  greedy_keepers(cand, cfg$max_keepers, cfg$max_top2)
}

legalize_declared <- function() {
  rbindlist(lapply(declared_fids, function(f) {
    data.table(franchise_id = f,
               player_id = legalize_keepers(f, D$declared[franchise_id == f]$player_id))
  }))
}

predicted <- data.table(franchise_id = character(), player_id = character())
declared <- D$declared
for (iter in seq_len(5L)) {
  costs <- reprice(rbind(declared, predicted))
  declared <- legalize_declared()
  new_pred <- rbindlist(lapply(undeclared_fids, function(f) {
    data.table(franchise_id = f,
               player_id = greedy_keepers(costs[franchise_id == f], cfg$max_keepers, cfg$max_top2))
  }))
  converged <- identical(sort(paste(new_pred$franchise_id, new_pred$player_id)),
                         sort(paste(predicted$franchise_id, predicted$player_id)))
  predicted <- new_pred
  if (converged) { msg("opponent prediction converged after", iter, "pass(es)"); break }
}
# final prices reflect the full league-wide keeper set, mine included later
costs <- reprice(rbind(declared, predicted))

dropped <- D$declared[!declared, on = c("franchise_id", "player_id")]
if (nrow(dropped)) {
  dd <- merge(dropped, costs[, list(franchise_id, player_id, franchise_name, player_name, base_round)],
              by = c("franchise_id", "player_id"))
  msg("ILLEGAL declared keepers trimmed:",
      paste(sprintf("%s - %s (R%s)", dd$franchise_name, dd$player_name, dd$base_round), collapse = "; "))
  fwrite(dd, file.path(cfg$outdir, "illegal_declared.csv"))
}

opp_keepers <- rbind(declared[franchise_id != cfg$my_franchise], predicted)

opp_out <- merge(opp_keepers, costs[, list(player_id, franchise_id, player_name, pos,
                                           base_round, value_over_pick)],
                 by = c("franchise_id", "player_id"), all.x = TRUE)
opp_out[, source := fifelse(franchise_id %in% declared_fids, "declared", "predicted")]
fwrite(opp_out[order(franchise_id, base_round)], file.path(cfg$outdir, "opponent_keepers.csv"))

# --------------------------------------------- 4. shared random draws -------

my_cand <- costs[franchise_id == cfg$my_franchise & !is.na(base_round) & pos %in% cfg$pos_filter]
msg("my candidates:", nrow(my_cand))

# Projection cost scales with the pool, and FantasyPros ranks ~1200 players when
# at most ~200 can ever matter here: 8 x 17 = 136 roster spots plus the
# replacement-level free agents. Restrict to everyone currently rostered (all of
# them are keeper candidates) plus the top of the superflex board. The SAME
# restriction has to apply to the rankings used for replacement level and for
# the draft pool, or a drafted player with no projection is silently dropped by
# the inner join in ffs_score_rosters and rosters come up short.
proj_ids <- unique(c(as.character(D$rosters$fantasypros_id),
                     head(D$op[order(adp_rank)]$fantasypros_id, cfg$proj_pool_n)))
proj_ids <- proj_ids[!is.na(proj_ids)]
rankings_sim <- D$latest_rankings[as.character(fantasypros_id) %in% proj_ids]
msg("projection pool:", nrow(rankings_sim), "of", nrow(D$latest_rankings), "ranked players")

make_draws <- function(n_seasons, seed) {
  set.seed(seed)
  weeks <- seq_len(cfg$reg_weeks + log2(cfg$playoff_slots) * cfg$weeks_per_round)
  ps <- ffs_generate_projections(
    adp_outcomes = D$adp_outcomes, latest_rankings = rankings_sim,
    n_seasons = n_seasons, weeks = weeks, version = "v3")
  ps <- as.data.table(ps)
  ps[is.na(projected_score), projected_score := 0]
  sched <- ffs_build_schedules(franchises = D$franchises, n_seasons = n_seasons,
                               n_weeks = cfg$reg_weeks, seed = seed)
  list(projected_scores = ps, schedules = sched,
       playoff_weeks = seq(cfg$reg_weeks + 1L, max(weeks)))
}

# ------------------------------------------------- 5. one keeper world ------

#' Assemble rosters for a keeper set and score them
#'
#' @param my_ids my keepers; @param draws from make_draws(); @param board_seed
#' controls the mock-draft jitter (shared across candidate sets so the
#' comparison stays paired)
run_world <- function(my_ids, draws, board_seed) {
  all_k <- rbind(opp_keepers, data.table(franchise_id = cfg$my_franchise, player_id = my_ids))

  assign_one <- function(f) {
    ids <- all_k[franchise_id == f]$player_id
    a <- ffs_assign_keeper_rounds(costs[franchise_id == f], ids,
                                  max_keepers = cfg$max_keepers, max_top2 = cfg$max_top2)
    if (!a$feasible) stop("infeasible keeper set for franchise ", f, ": ", a$reason)
    a$assignment[, franchise_id := f][]
  }
  assigned <- rbindlist(lapply(unique(all_k$franchise_id), assign_one))

  kept <- merge(assigned, costs[, list(player_id, franchise_id, franchise_name, league_id,
                                       player_name, pos, fantasypros_id)],
                by = c("player_id", "franchise_id"))

  forfeited <- split(assigned$keeper_round, assigned$franchise_id)
  pm <- ffs_draft_pick_map(D$draft_slots, cfg$n_rounds, forfeited)

  pool <- merge(rankings_sim[pos %in% cfg$pos_filter], D$op,
                by = "fantasypros_id", all.x = TRUE)
  pool <- pool[!fantasypros_id %in% kept$fantasypros_id & !is.na(op_ecr)]
  pool[, ecr := op_ecr] # board order is superflex overall, not positional ecr

  drafted <- ffs_mock_draft(pool, pm, kept = kept,
                            pos_need = D$pos_need, pos_cap = D$pos_cap,
                            soft_bench = cfg$soft_bench, window_k = cfg$window_k,
                            rank_decay = cfg$rank_decay, need_boost = cfg$need_boost,
                            depth_decay = cfg$depth_decay, seed = board_seed)

  rosters <- rbind(
    kept[, list(league_id, franchise_id, franchise_name, player_id, fantasypros_id, pos)],
    merge(drafted[, list(franchise_id, fantasypros_id, pos)],
          unique(D$franchises[, list(franchise_id = as.character(franchise_id),
                                     franchise_name, league_id)]),
          by = "franchise_id")[, list(league_id, franchise_id, franchise_name,
                                      player_id = fantasypros_id, fantasypros_id, pos)])

  list(rosters = rosters, drafted = drafted, kept = kept,
       summary = ffs_keeper_world(
         rosters = rosters, projected_scores = draws$projected_scores,
         franchises = D$franchises, lineup_constraints = D$lineup_constraints,
         latest_rankings = rankings_sim, schedules = draws$schedules,
         playoff_slots = cfg$playoff_slots, playoff_weeks = draws$playoff_weeks,
         weeks_per_round = cfg$weeks_per_round,
         pos_filter = cfg$pos_filter))
}

#' Playoff odds for my franchise, averaged over K jittered boards
score_set <- function(my_ids, draws, board_seeds) {
  res <- rbindlist(lapply(board_seeds, function(bs) {
    s <- run_world(my_ids, draws, bs)$summary
    s[franchise_id == cfg$my_franchise]
  }))
  list(playoff_pct = mean(res$playoff_pct), champion_pct = mean(res$champion_pct),
       h2h_wins = mean(res$h2h_wins), n_boards = length(board_seeds),
       by_board = res$playoff_pct)
}

# memoise: the greedy path revisits sets, and a world is expensive
.score_cache <- new.env(parent = emptyenv())
score_cached <- function(my_ids, draws, board_seeds, tag) {
  key <- paste(tag, paste(sort(my_ids), collapse = "|"))
  hit <- .score_cache[[key]]
  if (!is.null(hit)) return(hit)
  v <- score_set(my_ids, draws, board_seeds)
  assign(key, v, envir = .score_cache)
  v
}

my_costs <- costs[franchise_id == cfg$my_franchise]
is_feasible <- function(ids) {
  ffs_assign_keeper_rounds(my_costs, ids, cfg$max_keepers, cfg$max_top2)$feasible
}
pname <- function(ids) my_costs[match(ids, my_costs$player_id)]$player_name

if (isTRUE(cfg$run_search)) {

# --------------------------------------------------------- 6. SEARCH -------
# Cheap and rank-only. Nothing from this stage is quoted - it exists to pick the
# shortlist the confirm stage then prices properly.

msg("SEARCH: n =", cfg$n_search, "seasons")
draws_s <- make_draws(cfg$n_search, cfg$seed)
seeds_s <- cfg$seed + seq_len(cfg$k_search)

# rank candidates cheaply first, then greedy-select over the plausible ones
pool_ids <- my_cand[order(-value_over_pick)]$player_id
pool_ids <- head(pool_ids, cfg$n_cand_search)

msg("  marginal lift of each candidate on its own (", length(pool_ids), "worlds )")
base_s <- score_cached(character(0), draws_s, seeds_s, "S")
solo <- rbindlist(lapply(pool_ids, function(p) {
  s <- score_cached(p, draws_s, seeds_s, "S")
  data.table(player_id = p, player_name = pname(p),
             solo_lift = s$playoff_pct - base_s$playoff_pct)
}))
setorder(solo, -solo_lift)
msg("  best solo keepers:", paste(head(solo$player_name, 5), collapse = ", "))

msg("  greedy forward selection to", cfg$max_keepers, "keepers")
chosen <- character(0)
greedy_trace <- list()
for (step in seq_len(cfg$max_keepers)) {
  rest <- setdiff(pool_ids, chosen)
  rest <- rest[vapply(rest, function(p) is_feasible(c(chosen, p)), logical(1))]
  if (!length(rest)) break
  vals <- vapply(rest, function(p) {
    score_cached(c(chosen, p), draws_s, seeds_s, "S")$playoff_pct
  }, numeric(1))
  best <- rest[[which.max(vals)]]
  prev <- if (length(chosen)) score_cached(chosen, draws_s, seeds_s, "S")$playoff_pct else base_s$playoff_pct
  chosen <- c(chosen, best)
  greedy_trace[[step]] <- data.table(step = step, added = pname(best),
                                     playoff_pct = max(vals), gain = max(vals) - prev)
  msg(sprintf("   %d. +%-20s playoff %.3f (%+.3f)", step, pname(best), max(vals), max(vals) - prev))
}
greedy_trace <- rbindlist(greedy_trace)

# one swap pass: the greedy path can lock in an early pick that a later
# combination would have beaten
msg("  swap-improvement pass")
cur_val <- score_cached(chosen, draws_s, seeds_s, "S")$playoff_pct
for (pass in seq_len(cfg$max_swap_passes)) {
  improved <- FALSE
  for (out in chosen) {
    for (inn in setdiff(pool_ids, chosen)) {
      trial <- c(setdiff(chosen, out), inn)
      if (!is_feasible(trial)) next
      v <- score_cached(trial, draws_s, seeds_s, "S")$playoff_pct
      if (v > cur_val + 1e-9) {
        msg(sprintf("   swap %s -> %s (%.3f -> %.3f)", pname(out), pname(inn), cur_val, v))
        chosen <- trial; cur_val <- v; improved <- TRUE; break
      }
    }
    if (improved) break
  }
  if (!improved) break
}

# shortlist for confirmation: the greedy winner plus the best near-misses.
# When the candidate pool is small enough that greedy consumes all of it (as
# happens if n_cand_search <= max_keepers, or every candidate happens to fit),
# setdiff(pool_ids, chosen) is empty and there is nothing to swap in - the
# winner is the only set worth confirming.
shortlist <- list(winner = chosen)
outside <- setdiff(pool_ids, chosen)
if (length(outside)) {
  alts <- rbindlist(lapply(outside, function(inn) {
    rbindlist(lapply(chosen, function(out) {
      trial <- c(setdiff(chosen, out), inn)
      if (!is_feasible(trial)) return(NULL)
      data.table(out = out, inn = inn,
                 val = score_cached(trial, draws_s, seeds_s, "S")$playoff_pct)
    }))
  }), fill = TRUE)
  if (nrow(alts)) {
    setorder(alts, -val)
    for (i in seq_len(min(cfg$n_sets_confirm - 1L, nrow(alts)))) {
      shortlist[[paste0("alt", i)]] <- c(setdiff(chosen, alts$out[i]), alts$inn[i])
    }
  }
}
msg("SEARCH done -", length(shortlist), "sets shortlisted")

# -------------------------------------------------------- 7. CONFIRM -------
# Everything quoted comes from here: a real season budget, averaged over K
# jittered draft boards, with the same boards and the same score draws used for
# every candidate set so the comparisons stay paired.

msg("CONFIRM: n =", cfg$n_confirm, "seasons x", cfg$k_boards, "boards")
draws_c <- make_draws(cfg$n_confirm, cfg$seed + 1000L)
seeds_c <- cfg$seed + 5000L + seq_len(cfg$k_boards)

confirm_one <- function(ids, label) {
  s <- score_cached(ids, draws_c, seeds_c, "C")
  data.table(set = label, playoff_pct = s$playoff_pct, champion_pct = s$champion_pct,
             h2h_wins = s$h2h_wins,
             board_sd = stats::sd(s$by_board),
             keepers = paste(sort(pname(ids)), collapse = ", "))
}

sets_out <- rbindlist(lapply(names(shortlist), function(nm) confirm_one(shortlist[[nm]], nm)))
setorder(sets_out, -playoff_pct)
fwrite(sets_out, file.path(cfg$outdir, "keeper_sets.csv"))
print(sets_out)

winner <- shortlist[[sets_out$set[[1]]]]
win_val <- sets_out$playoff_pct[[1]]

# noise check on the winner: a second, fully independent seed (different score
# draws AND different boards). Per Joe's rule, nothing gets quoted unconverged
# -- if this doesn't roughly agree with the first estimate, the budget above
# needs to go up before trusting the recommendation.
msg("stability check: independent second seed on the winning set")
draws_c2 <- make_draws(cfg$n_confirm, cfg$seed + 9000L)
seeds_c2 <- cfg$seed + 15000L + seq_len(cfg$k_boards)
win_val2 <- score_cached(winner, draws_c2, seeds_c2, "C2")$playoff_pct
stability <- data.table(seed_set = c("primary", "independent"),
                        playoff_pct = c(win_val, win_val2))
stability_delta <- abs(win_val - win_val2)
msg(sprintf("  playoff_pct primary=%.3f independent=%.3f delta=%.3f (budget: n=%d x %d boards)",
            win_val, win_val2, stability_delta, cfg$n_confirm, cfg$k_boards))
if (stability_delta > 0.01) {
  msg("  ** NOISE WARNING: delta > 1pt - raise FFS_KEEP_NCONFIRM/FFS_KEEP_KBOARDS before trusting this ranking **")
}
fwrite(stability, file.path(cfg$outdir, "stability_check.csv"))

# leave-one-out: what each keeper in the winning set is actually worth, given
# that dropping him hands the pick back and puts him into the pool
msg("leave-one-out on the winning set")
loo <- rbindlist(lapply(winner, function(p) {
  s <- score_cached(setdiff(winner, p), draws_c, seeds_c, "C")
  data.table(player_id = p, player_name = pname(p),
             keep_round = NA_integer_,
             playoff_without = s$playoff_pct,
             lift = win_val - s$playoff_pct)
}))
asg <- ffs_assign_keeper_rounds(my_costs, winner, cfg$max_keepers, cfg$max_top2)$assignment
loo[, keep_round := asg$keeper_round[match(player_id, asg$player_id)]]
loo <- merge(loo, my_costs[, list(player_id, pos, base_round, adp_rank, value_over_pick)],
             by = "player_id", all.x = TRUE)
setorder(loo, -lift)
fwrite(loo, file.path(cfg$outdir, "keeper_candidates.csv"))
print(loo[, list(player_name, pos, base_round, keep_round, adp_rank,
                 lift = round(lift, 4), vop = round(value_over_pick))])

# the draft that the winning set actually buys
wboard <- run_world(winner, draws_c, seeds_c[[1]])
fwrite(wboard$drafted[franchise_id == cfg$my_franchise][order(overall)],
       file.path(cfg$outdir, "draft_board.csv"))
fwrite(wboard$summary, file.path(cfg$outdir, "standings.csv"))

msg("done ->", cfg$outdir)

} # cfg$run_search
