#!/usr/bin/env Rscript
# draft_strategy.R -- what should I do with the 11th pick?
#
# A redraft league before its draft has no rosters at all, so ff_simulate()
# cannot be pointed at it (ffs_rosters() literally errors on an empty league).
# Everything here is built on the keeper-league machinery instead, which was
# written for exactly this shape: mock-draft a board into a pick map, assemble
# the resulting rosters, and score them against ONE set of pre-drawn player
# scores so that every build is a paired comparison.
#
#   1. REACH   - measure how this league drafts relative to contemporaneous
#                Sleeper ADP, using past drafts paired with the snapshot taken
#                just before each one
#   2. CALIB   - fit a per-position board shift so the simulated draft consumes
#                positions the way this league actually does
#   3. SEARCH  - score many build policies from slot 11 at a cheap n
#   4. CONFIRM - re-score the shortlist at a real n
#   5. CAPITAL - regress results on positional draft capital
#
# Usage:  Rscript dev/suite/draft_strategy.R
#         FFS_DRAFT_STAGES=reach,calib Rscript dev/suite/draft_strategy.R

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
source(here::here("dev", "suite", "adp_board.R"))

`%||%` <- function(x, y) if (is.null(x)) y else x
msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")

# ---------------------------------------------------------------- config ----

if (!exists("draft_config")) draft_config <- list()
cfg <- utils::modifyList(list(
  league_id     = Sys.getenv("FFS_DRAFT_LEAGUE", "1389724668889399296"),
  season        = as.integer(Sys.getenv("FFS_DRAFT_SEASON", "2026")),
  my_user       = Sys.getenv("FFS_DRAFT_ME", "sox05syd"),
  n_rounds      = as.integer(Sys.getenv("FFS_DRAFT_ROUNDS", "18")),
  playoff_slots = as.integer(Sys.getenv("FFS_DRAFT_PLAYOFF_SLOTS", "6")),
  # 13, not the league true 14. The v3 projection samples historical weekly
  # rank trajectories out of fp_rankings_history_week.rds, which holds weeks
  # 1-16 ONLY, so any week past 16 indexes off the end and returns NA -> a zero
  # score for every player. At 14 regular weeks the 3-round bracket runs to week
  # 17 and the CHAMPIONSHIP GAME is scored 0-0 for everyone, decided purely by
  # the seed tiebreak. Dropping to 13 puts the final in week 16 where data
  # exists. Costs one regular-season game of seeding precision; buys a
  # championship round that is actually simulated.
  reg_weeks     = as.integer(Sys.getenv("FFS_DRAFT_WEEKS", "13")),
  weeks_per_round = as.integer(Sys.getenv("FFS_DRAFT_WPR", "1")),
  base_seasons  = 2012:2025,
  pos_filter    = c("QB", "RB", "WR", "TE"),
  adp_col       = Sys.getenv("FFS_DRAFT_ADP_COL", "Redraft SF ADP"),
  # how deep a pool to project. 12 x 18 = 216 picks plus replacement-level
  # headroom; the same restriction MUST apply to the rankings used for
  # replacement level and to the draft pool, or ffs_score_rosters' inner join
  # silently drops drafted players and rosters come up short.
  proj_pool_n   = as.integer(Sys.getenv("FFS_DRAFT_POOL", "300")),
  # autodraft knobs (ffs_mock_draft defaults, overridden by CALIB)
  window_k      = as.integer(Sys.getenv("FFS_DRAFT_WINDOWK", "6")),
  rank_decay    = as.numeric(Sys.getenv("FFS_DRAFT_RANKDECAY", "0.7")),
  need_boost    = as.numeric(Sys.getenv("FFS_DRAFT_NEEDBOOST", "1.6")),
  soft_bench    = as.integer(Sys.getenv("FFS_DRAFT_SOFTBENCH", "1")),
  depth_decay   = as.numeric(Sys.getenv("FFS_DRAFT_DEPTHDECAY", "0.4")),
  seed          = as.integer(Sys.getenv("FFS_DRAFT_SEED", "20260903")),
  stages        = strsplit(Sys.getenv("FFS_DRAFT_STAGES", "reach,calib"), ",")[[1]],
  cache         = Sys.getenv("FFS_DRAFT_CACHE", "dev/data/draft_cache.rds"),
  outdir        = Sys.getenv("FFS_DRAFT_OUT", "")
), draft_config)

if (!nzchar(cfg$outdir)) {
  cfg$outdir <- file.path("dev", "league_sims",
                          paste0("draft_", cfg$league_id, "_", Sys.Date()))
}
dir.create(cfg$outdir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(cfg$cache), recursive = TRUE, showWarnings = FALSE)
msg("output ->", cfg$outdir)

sleeper_get <- function(p) jsonlite::fromJSON(paste0("https://api.sleeper.app/v1/", p), flatten = TRUE)

# ------------------------------------------------------------- 1. data ------

load_draft_data <- function(cfg, refresh = FALSE) {
  if (!refresh && file.exists(cfg$cache)) {
    D <- readRDS(cfg$cache)
    if (identical(D$league_id, cfg$league_id) && identical(D$season, cfg$season)) {
      msg("cache hit:", cfg$cache); return(D)
    }
  }
  msg("pulling league data")
  conn <- ff_connect("sleeper", league_id = cfg$league_id, season = cfg$season)

  # NB: no ffs_rosters() - the league is pre-draft and has none.
  franchises <- as.data.table(ffs_franchises(conn))
  lineup_constraints <- as.data.table(ffs_starter_positions(conn))
  latest_rankings <- as.data.table(ffs_latest_rankings("draft"))

  lg <- sleeper_get(paste0("league/", cfg$league_id))
  dr <- sleeper_get(paste0("draft/", lg$draft_id))
  slot_by_user <- unlist(dr$draft_order)
  draft_slots <- setNames(
    as.integer(slot_by_user[match(franchises$user_id, names(slot_by_user))]),
    as.character(franchises$franchise_id))
  stopifnot(!anyNA(draft_slots))

  my_fid <- as.character(franchises$franchise_id[franchises$user_name == cfg$my_user])
  stopifnot(length(my_fid) == 1L)
  msg("me:", cfg$my_user, "= franchise", my_fid, "= draft slot", draft_slots[[my_fid]])

  # walk previous_league_id back for the draft history
  hist <- list(); id <- cfg$league_id
  repeat {
    l <- sleeper_get(paste0("league/", id))
    d <- sleeper_get(paste0("draft/", l$draft_id))
    hist[[as.character(l$season)]] <- list(
      league_id = l$league_id, draft_id = l$draft_id, season = as.integer(l$season),
      date = if (!is.null(d$start_time)) as.Date(as.POSIXct(d$start_time / 1000, origin = "1970-01-01")) else NA,
      status = d$status)
    pl <- l$previous_league_id
    if (is.null(pl) || is.na(pl) || pl %in% c("0", "")) break
    id <- pl
  }
  msg("draft history:", paste(rev(names(hist)), collapse = " "))

  past_picks <- rbindlist(lapply(hist, function(h) {
    if (identical(h$status, "pre_draft")) return(NULL)
    p <- as.data.table(sleeper_get(paste0("draft/", h$draft_id, "/picks")))
    p[, list(season = h$season, draft_date = h$date,
             sleeper_id = as.character(player_id), pick_no, round,
             pos = metadata.position,
             player = paste(metadata.first_name, metadata.last_name))]
  }))

  # lineup minimums are the hard starter floor; maxima are start-eligibility
  # ceilings, not roster caps (a 3rd QB is legal, just benched)
  lc <- lineup_constraints[pos %in% cfg$pos_filter]
  pos_need <- setNames(as.integer(lc$min), lc$pos)[cfg$pos_filter]
  pos_start_max <- setNames(as.integer(lc$max), lc$pos)[cfg$pos_filter]
  pos_cap <- setNames(rep(Inf, length(cfg$pos_filter)), cfg$pos_filter)
  if (isTRUE(as.logical(dr$settings$enforce_position_limits))) {
    lim_keys <- grep("^position_limit_", names(dr$settings), value = TRUE)
    if (length(lim_keys)) {
      lim <- setNames(as.numeric(unlist(dr$settings[lim_keys])),
                      toupper(sub("^position_limit_", "", lim_keys)))
      common <- intersect(names(pos_cap), names(lim))
      pos_cap[common] <- lim[common]
    }
  }
  msg("pos_need:", paste(sprintf("%s=%d", names(pos_need), pos_need), collapse = " "),
      "| start_max:", paste(sprintf("%s=%d", names(pos_start_max), pos_start_max), collapse = " "),
      "| cap:", paste(sprintf("%s=%s", names(pos_cap), pos_cap), collapse = " "))

  msg("scoring history + adp outcomes")
  scoring_history <- ff_scoringhistory(conn, cfg$base_seasons)
  adp_outcomes <- ffs_adp_outcomes(scoring_history = scoring_history,
                                   gp_model = "simple", pos_filter = cfg$pos_filter,
                                   version = "v3")

  D <- list(league_id = cfg$league_id, season = cfg$season, franchises = franchises,
            lineup_constraints = lineup_constraints, latest_rankings = latest_rankings,
            draft_slots = draft_slots, my_fid = my_fid, hist = hist,
            past_picks = past_picks, adp_outcomes = adp_outcomes,
            pos_need = pos_need, pos_cap = pos_cap, pos_start_max = pos_start_max,
            draft_date = hist[[as.character(cfg$season)]]$date)
  saveRDS(D, cfg$cache)
  D
}

D <- load_draft_data(cfg, refresh = identical(Sys.getenv("FFS_DRAFT_REFRESH"), "1"))

# ------------------------------------------------- 2. reach vs live ADP -----

tab_idx <- adp_tab_index()

# The board a drafter in this league would have been looking at on draft day.
season_boards <- function(seasons) {
  setNames(lapply(seasons, function(s) {
    d <- D$hist[[as.character(s)]]$date
    tab <- adp_tab_before(d, tab_idx)
    b <- adp_board(tab$gid, cfg$adp_col, D$latest_rankings,
                   scrape_date = tab$scrape_date, pos_keep = cfg$pos_filter,
                   quiet = TRUE)
    b[, season := s]
    b[, lag_days := as.integer(as.Date(d) - as.Date(tab$scrape_date))]
    b[]
  }), as.character(seasons))
}

has_snapshot <- function(s) {
  d <- D$hist[[as.character(s)]]$date
  !inherits(try(adp_tab_before(d, tab_idx), silent = TRUE), "try-error")
}

run_reach <- function() {
  msg("== REACH ==")
  seasons <- sort(unique(D$past_picks$season))
  seasons <- seasons[vapply(seasons, has_snapshot, logical(1))]
  msg("seasons with a usable ADP snapshot:", paste(seasons, collapse = " "))
  boards <- season_boards(seasons)

  picks <- rbindlist(lapply(seasons, function(s) {
    b <- boards[[as.character(s)]]
    merge(D$past_picks[season == s],
          b[, list(sleeper_id, adp, adp_rank, snapshot = scrape_date, lag_days)],
          by = "sleeper_id", all.x = TRUE)
  }))
  # reach = how many board slots earlier than his ADP rank a player went
  picks[, reach := adp_rank - pick_no]
  # centered on the season's own mean: a league that dips deep into the board
  # shows a positive offset on EVERY position, which is not a positional
  # preference. Only the deviation from that offset is.
  picks[!is.na(reach), reach_c := reach - mean(reach), by = season]
  fwrite(picks[order(season, pick_no)], file.path(cfg$outdir, "adp_reach.csv"))

  cov <- picks[, list(picks = .N, matched = sum(!is.na(adp_rank)),
                      pct = round(100 * mean(!is.na(adp_rank)), 1),
                      snapshot = snapshot[!is.na(snapshot)][1],
                      lag_days = lag_days[!is.na(lag_days)][1]), by = season]
  msg("coverage:"); print(cov[order(season)][])

  r <- picks[!is.na(reach)]
  scopes <- list(all = r, early = r[round <= 6])
  summ <- rbindlist(lapply(names(scopes), function(nm) {
    scopes[[nm]][, {
      tt <- tryCatch(stats::t.test(reach_c), error = function(e) NULL)
      list(scope = nm, n = .N, mean_reach_c = round(mean(reach_c), 1),
           se = round(stats::sd(reach_c) / sqrt(.N), 1),
           p = if (is.null(tt)) NA_real_ else signif(tt$p.value, 2))
    }, by = pos]
  }))
  setorder(summ, scope, -mean_reach_c)
  fwrite(summ, file.path(cfg$outdir, "reach_summary.csv"))
  msg("centered positional reach (+ve = this league takes them EARLY):")
  print(summ[])

  # The curve that actually matters: how many of each position are gone by pick
  # N, league versus board. That is what changes what is on the table at 11.
  ks <- c(12, 24, 36, 48, 60, 84, 120, 168, 216)
  curves <- rbindlist(lapply(seasons, function(s) {
    b <- boards[[as.character(s)]]; x <- picks[season == s]
    rbindlist(lapply(cfg$pos_filter, function(pp) data.table(
      season = s, pos = pp, pick = ks,
      league = vapply(ks, function(k) sum(x$pos == pp & x$pick_no <= k), integer(1)),
      board  = vapply(ks, function(k) sum(b$pos == pp & b$adp_rank <= k), integer(1)))))
  }))
  curves[, excess := league - board]
  fwrite(curves, file.path(cfg$outdir, "run_curves.csv"))
  msg("mean excess taken vs board, by pick:")
  print(dcast(curves[, list(excess = mean(excess)), by = list(pos, pick)],
              pick ~ pos, value.var = "excess")[])

  invisible(list(picks = picks, curves = curves, summary = summ,
                 boards = boards, seasons = seasons))
}

if ("reach" %in% cfg$stages) REACH <- run_reach()

# ------------------------------------------------------------- 3. sweep ----

if ("sweep" %in% cfg$stages) {
  source(here::here("dev", "suite", "draft_sweep.R"))
  source(here::here("dev", "suite", "draft_runner.R"))
}
if ("report" %in% cfg$stages) source(here::here("dev", "suite", "draft_report.R"))
if ("contingency" %in% cfg$stages) source(here::here("dev", "suite", "draft_contingency.R"))
if ("downstream" %in% cfg$stages) source(here::here("dev", "suite", "draft_downstream.R"))
