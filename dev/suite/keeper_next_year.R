# Next season's keeper price, for a keeper league in the middle of this one.
#
# source()d by dev/suite/waiver_watchlist.R (the Galletti tab's keeper columns)
# and dev/suite/keeper_outlook.R (the league-wide 2027 keeper study). Defines
# functions only; nothing runs on source.
#
# House rules this encodes (Galletti, confirmed by Joe 2026-09-29): next year a
# player costs the round he went in THIS year's draft minus one, floored at R1;
# that round follows him through drops, waivers and trades; only players nobody
# drafted are priced by ADP, at ceiling(ADP / teams) capped at the last round.
# At most one keeper from this draft's rounds 1-2, max 8, collisions bump up.

library(data.table)

.sleeper_json <- function(path) jsonlite::fromJSON(paste0("https://api.sleeper.app/v1/", path),
                                                   flatten = TRUE)

#' This season's draft, one row per pick, crosswalked to fantasypros ids
keeper_draft_picks <- function(league_id) {
  lg <- .sleeper_json(paste0("league/", league_id))
  dr <- .sleeper_json(paste0("draft/", lg$draft_id))
  p <- as.data.table(.sleeper_json(paste0("draft/", lg$draft_id, "/picks")))
  p <- p[, list(player_id = as.character(player_id), round = as.integer(round),
                pick_no = as.integer(pick_no), is_keeper = as.logical(is_keeper),
                picked_by_roster = as.character(roster_id),
                player = paste(metadata.first_name, metadata.last_name),
                pos = metadata.position)]
  xw <- as.data.table(ffscrapr::dp_playerids())[
    !is.na(sleeper_id) & !is.na(fantasypros_id),
    list(player_id = as.character(sleeper_id), fantasypros_id = as.character(fantasypros_id))]
  p <- merge(p, unique(xw, by = "player_id"), by = "player_id", all.x = TRUE)
  p[, pkey := ffsimulator:::.ffs_pkey(player, pos)]
  attr(p, "n_rounds") <- as.integer(dr$settings$rounds)
  attr(p, "n_teams") <- as.integer(dr$settings$teams)
  p[order(pick_no)]
}

#' What each round's pick returns in this league, cached per league + season
#'
#' Needs the PRESEASON board the draft was actually made against: board_rds is
#' a keeper_intel.R cache whose `op` is that board (for Galletti 2026, the
#' Sleeper SF ADP export as of 2026-08-22, the last pull before the draft).
#' `slot = NULL` gives the mid-round bar (one row per round); a vector of slots
#' gives each slot's own snake-pick bar (see ffs_round_bar()). What is cached is
#' the draft itself matched to that board, so any slot can be read off it.
keeper_round_bar <- function(league_id, season,
                             board_rds = Sys.getenv("FFS_KEEPER_BOARD",
                                                    here::here("dev", "data", "keeper_cache_0822.rds")),
                             picks = NULL, slot = NULL, refresh = FALSE) {
  path <- here::here("dev", "data", sprintf("draft_board_%s_%s.csv", league_id, season))
  if (file.exists(path) && !refresh) {
    pk <- fread(path)
  } else {
    if (is.null(picks)) picks <- keeper_draft_picks(league_id)
    D <- readRDS(board_rds)
    board <- as.data.table(D$op)[, list(fantasypros_id = as.character(fantasypros_id), adp_rank)]
    nm <- unique(as.data.table(D$latest_rankings)[, list(
      fp2 = as.character(fantasypros_id), pkey = ffsimulator:::.ffs_pkey(player, pos))], by = "pkey")
    pk <- merge(copy(picks), nm, by = "pkey", all.x = TRUE)
    pk[is.na(fantasypros_id), fantasypros_id := fp2]
    pk <- merge(pk, board, by = "fantasypros_id", all.x = TRUE)
    message("round bar: ", sum(!is.na(pk$adp_rank)), "/", nrow(pk), " picks found on the board")
    pk <- pk[order(pick_no), list(round, pick_no, is_keeper, player, pos, fantasypros_id, adp_rank,
                                  n_teams = attr(picks, "n_teams"), n_rounds = attr(picks, "n_rounds"))]
    fwrite(pk, path)
  }
  ffs_round_bar(pk, n_teams = pk$n_teams[[1]], n_rounds = pk$n_rounds[[1]], slot = slot)
}

#' Next year's draft slot for every franchise in every simulated season
#'
#' Galletti's order (read off 2025 -> 2026): the playoff teams pick last, in
#' reverse order of finish (champion 8th); the non-playoff teams pick 1-4 by
#' the TOILET BOWL - a bracket among them seeded by record, 5v8 and 6v7, whose
#' winner gets the 1.01, runner-up 2nd, consolation winner 3rd, loser 4th.
#'
#' The saved simulation stops at the regular season, so each bracket game is
#' two weeks (Sleeper playoff_round_type 2) of scores resampled from that
#' franchise's own simulated weeks in the same season. Final records add the
#' banked real record to the simulated weeks; ties go to points for.
keeper_next_slots <- function(sim, playoff_teams = 4L, weeks_per_round = 2L, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  ss <- as.data.table(sim$summary_season)[, list(season, franchise_id = as.character(franchise_id),
                                                 wins = h2h_wins, pf = points_for)]
  st <- as.data.table(sim$standings)[, list(franchise_id = as.character(franchise_id),
                                            w0 = wins, pf0 = points_for)]
  ss <- merge(ss, st, by = "franchise_id", all.x = TRUE)
  ss[, `:=`(wins = wins + fcoalesce(as.numeric(w0), 0), pf = pf + fcoalesce(as.numeric(pf0), 0))]
  wk <- as.data.table(sim$summary_week)[, list(season, franchise_id = as.character(franchise_id),
                                               score = team_score)]
  wk <- split(wk$score, paste(wk$season, wk$franchise_id))
  n_teams <- uniqueN(ss$franchise_id)

  game <- function(s, a, b) {
    sa <- sum(sample(wk[[paste(s, a)]], weeks_per_round, replace = TRUE))
    sb <- sum(sample(wk[[paste(s, b)]], weeks_per_round, replace = TRUE))
    if (sa >= sb) c(a, b) else c(b, a)
  }
  # a four-team bracket: 1v4, 2v3, then final and third-place game.
  # returns the teams in finishing order
  bracket4 <- function(s, seeds) {
    s1 <- game(s, seeds[1], seeds[4]); s2 <- game(s, seeds[2], seeds[3])
    fin <- game(s, s1[1], s2[1]); third <- game(s, s1[2], s2[2])
    c(fin, third)
  }
  setorder(ss, season, -wins, -pf)
  ss[, seed := seq_len(.N), by = season]
  stopifnot(playoff_teams == 4L, n_teams == 8L)  # the bracket shapes above
  out <- ss[, {
    po <- bracket4(season, franchise_id[1:4])        # finish 1..4 -> slots 8..5
    tb <- bracket4(season, franchise_id[5:8])        # finish 1..4 -> slots 1..4
    list(franchise_id = c(po, tb), slot = c(8:5, 1:4), playoffs = rep(c(TRUE, FALSE), each = 4))
  }, by = season]
  out[, list(sim_season = season, franchise_id, slot = as.integer(slot), playoffs)]
}

#' Next season's keeper cost + a value read for every row of a player table
#'
#' @param rows a table with `fantasypros_id`, `player`, `pos` (e.g. watchlist rows)
#' @param picks from keeper_draft_picks()
#' @param bar from keeper_round_bar()
#' @param fc FantasyCalc dynasty values for this format (fc_dynasty_values()):
#'   `overall_rank` is the dynasty rank that stands in for next year's ADP
#' @return rows with draft_rd, kept_now, keep_cost, top2, cost_source,
#'   dyn_rank, keep_bar and keep_margin (= keep_bar - dyn_rank: positive means he
#'   should beat what his round's pick would return)
keeper_next_year <- function(rows, picks, bar, fc) {
  r <- as.data.table(copy(rows))
  r[, .rid := .I]
  r[, fantasypros_id := as.character(fantasypros_id)]
  r[, pkey := ffsimulator:::.ffs_pkey(player, pos)]

  # the pick's sleeper id, by fantasypros id first, then name + position
  by_id <- picks[!is.na(fantasypros_id), list(fantasypros_id, sid = player_id)]
  by_id <- unique(by_id, by = "fantasypros_id")
  by_nm <- unique(picks[, list(pkey, sid_nm = player_id)], by = "pkey")
  r <- merge(r, by_id, by = "fantasypros_id", all.x = TRUE, sort = FALSE)
  r <- merge(r, by_nm, by = "pkey", all.x = TRUE, sort = FALSE)
  r[is.na(sid), sid := sid_nm]
  r[, player_id := fifelse(is.na(sid), paste0("fp:", fantasypros_id), sid)]

  f <- as.data.table(fc)[!is.na(overall_rank)]
  f <- f[, list(fantasypros_id = as.character(fantasypros_id), sleeper_id = as.character(sleeper_id),
                dyn_rank = as.integer(overall_rank))]
  r <- merge(r, unique(f[!is.na(fantasypros_id), list(fantasypros_id, dyn_rank)], by = "fantasypros_id"),
             by = "fantasypros_id", all.x = TRUE, sort = FALSE)
  r <- merge(r, unique(f[!is.na(sleeper_id), list(sid = sleeper_id, dyn_rank_s = dyn_rank)], by = "sid"),
             by = "sid", all.x = TRUE, sort = FALSE)
  r[is.na(dyn_rank), dyn_rank := dyn_rank_s]

  n_teams <- attr(picks, "n_teams"); n_rounds <- attr(picks, "n_rounds")
  adp <- r[!is.na(dyn_rank) & !is.na(fantasypros_id), list(fantasypros_id, adp_rank = dyn_rank)]
  priced <- ffs_next_keeper_costs(
    r[, list(.rid, player_id, fantasypros_id)],
    picks[, list(player_id, round, is_keeper)],
    adp_ranks = unique(adp, by = "fantasypros_id"),
    n_teams = n_teams, max_round = n_rounds)
  r <- merge(r, priced[, list(.rid, draft_rd, kept_now, keep_cost, top2, cost_source)],
             by = ".rid", sort = FALSE)
  r[, keep_bar := round(bar$bar_rank[match(keep_cost, bar$round)])]
  r[, keep_margin := keep_bar - dyn_rank]
  r[, c("sid", "sid_nm", "dyn_rank_s", "pkey", "player_id") := NULL]
  setorder(r, .rid)
  r[, .rid := NULL]
  r[]
}
