#' Rest-of-season playoff odds for every franchise
#'
#' (EXPERIMENTAL) Reads a simulation run on the league's real remaining
#' schedule (`ff_simulate(actual_schedule = TRUE)`), adds the record each
#' franchise has already banked (`sim$standings`, see [ffs_current_record()]),
#' and seeds every simulated season on wins and then points-for - the way
#' Sleeper breaks ties. Berths come from `sim$simulation_params$playoff_teams`;
#' the title odds play a fixed bracket over those seeds with byes to the top
#' seeds (see `.ffs_champion_pct_core`).
#'
#' @param sim an `ff_simulation`
#'
#' @return a data.table, one row per franchise: the record so far (`wins_now`,
#'   `losses_now`, `ties_now`, `pf_now`), projected final `proj_wins` (with a
#'   10th-90th percentile band) and `proj_pf`, then `playoff_pct`, `bye_pct`
#'   (`NA` when the bracket has no byes), `top_seed_pct`, `last_pct`,
#'   `champion_pct`, `mean_seed` and `seed_dist` (a list column: the chance of
#'   finishing in each seed, 1 to n)
#' @export
ffs_playoff_odds <- function(sim) {
  checkmate::assert_class(sim, "ff_simulation")
  h2h_wins <- points_for <- lg_rank <- franchise_id <- franchise_name <- NULL
  if (is.null(sim$summary_week) && !is.null(sim$optimal_scores) && !is.null(sim$schedules)) {
    sim$summary_week <- ffs_summarise_week(sim$optimal_scores, sim$schedules)
  }
  if (is.null(sim$summary_season) && !is.null(sim$summary_week)) {
    sim$summary_season <- ffs_summarise_season(sim$summary_week)
  }
  if (is.null(sim$summary_season) || is.null(sim$summary_week)) {
    cli::cli_abort("{.arg sim} has no simulated weeks to summarise")
  }

  n <- .ffs_sim_playoff_teams(sim)
  ss <- .ffs_seed(sim$summary_season, sim$standings)
  sw <- data.table::as.data.table(sim$summary_week)
  n_teams <- length(unique(ss$franchise_id))
  n <- min(n, n_teams)
  byes <- 2L^as.integer(ceiling(log2(n))) - n

  odds <- ss[, list(
    proj_wins = mean(h2h_wins),
    proj_wins_p10 = stats::quantile(h2h_wins, 0.1, names = FALSE),
    proj_wins_p90 = stats::quantile(h2h_wins, 0.9, names = FALSE),
    proj_pf = mean(points_for),
    playoff_pct = mean(lg_rank <= n),
    bye_pct = if (byes > 0) mean(lg_rank <= byes) else NA_real_,
    top_seed_pct = mean(lg_rank == 1L),
    last_pct = mean(lg_rank == n_teams),
    mean_seed = mean(lg_rank),
    seed_dist = list(tabulate(lg_rank, n_teams) / .N)
  ), by = list(franchise_id = as.character(franchise_id), franchise_name)]

  champ <- .ffs_champion_pct(sw, ss, n)
  odds <- merge(odds, champ[, list(franchise_id = as.character(franchise_id),
                                   champion_pct = get("champion_pct"))],
                by = "franchise_id", all.x = TRUE)
  odds[is.na(get("champion_pct")), "champion_pct" := 0]

  st <- sim$standings
  if (is.null(st) || !NROW(st)) {
    st <- data.table::data.table(franchise_id = odds$franchise_id, wins = 0,
                                 losses = 0, ties = 0, points_for = 0)
  }
  st <- data.table::as.data.table(st)[, list(
    franchise_id = as.character(franchise_id), wins_now = get("wins"),
    losses_now = get("losses"), ties_now = get("ties"), pf_now = get("points_for"))]
  odds <- merge(odds, st, by = "franchise_id", all.x = TRUE)
  odds[, "playoff_teams" := n]
  data.table::setcolorder(odds, c("franchise_id", "franchise_name", "wins_now",
                                  "losses_now", "ties_now", "pf_now"))
  odds[order(-get("playoff_pct"), -get("champion_pct"))]
}

#' Survival odds in a guillotine ("chopped") league
#'
#' (EXPERIMENTAL) In a guillotine league there is no schedule: every week the
#' lowest score among the teams still standing is eliminated, and the last one
#' left wins. This plays that out on each simulated season's weekly scores.
#'
#' @param sim an `ff_simulation` built with `return = "all"`, covering at least
#'   `weeks`
#' @param alive_ids franchise ids still in the league
#' @param weeks the weeks still to play, in order - one elimination each
#' @param points_so_far optional named numeric of season points so far, which
#'   breaks a tie for last in a given week (lower total goes)
#'
#' @return a data.table, one row per surviving franchise: `survive_wk<w>` for
#'   each week (the chance of still standing after it), `win_pct`, and
#'   `exp_weeks` (expected eliminations survived)
#' @export
ffs_guillotine_odds <- function(sim, alive_ids, weeks, points_so_far = NULL) {
  checkmate::assert_class(sim, "ff_simulation")
  if (is.null(sim$optimal_scores)) cli::cli_abort("{.arg sim} needs {.code return = \"all\"}")
  season <- week <- franchise_id <- NULL

  alive_ids <- as.character(alive_ids)
  opt <- data.table::as.data.table(sim$optimal_scores)[
    as.character(franchise_id) %in% alive_ids & week %in% weeks,
    list(season, week, franchise_id = as.character(franchise_id),
         score = get("actual_score"))]
  seasons <- sort(unique(opt$season))
  nT <- length(alive_ids)
  nW <- length(weeks)
  base_pts <- if (is.null(points_so_far)) stats::setNames(rep(0, nT), alive_ids) else
    points_so_far[alive_ids]
  base_pts[is.na(base_pts)] <- 0

  # [season, week, team] score cube; a missing franchise-week scores zero
  cube <- array(0, c(length(seasons), nW, nT))
  cube[cbind(match(opt$season, seasons), match(opt$week, weeks),
             match(opt$franchise_id, alive_ids))] <- opt$score
  cube[is.na(cube)] <- 0

  alive_after <- array(FALSE, c(length(seasons), nW, nT))
  winner <- integer(length(seasons))
  for (s in seq_along(seasons)) {
    alive <- rep(TRUE, nT)
    cum <- base_pts
    for (w in seq_len(nW)) {
      sc <- cube[s, w, ]
      cum <- cum + sc
      if (sum(alive) > 1L) {
        cand <- which(alive)
        # lowest score goes; a tie goes to the lower season total
        out <- cand[order(sc[cand], cum[cand])][1L]
        alive[out] <- FALSE
      }
      alive_after[s, w, ] <- alive
    }
    # if eliminations stop short of one survivor, the top season total wins
    left <- which(alive)
    winner[s] <- if (length(left) == 1L) left else left[which.max(cum[left])]
  }

  res <- data.table::data.table(franchise_id = alive_ids)
  for (w in seq_len(nW)) {
    data.table::set(res, j = paste0("survive_wk", weeks[[w]]),
                    value = apply(alive_after[, w, , drop = FALSE], 3, mean))
  }
  res[, "win_pct" := tabulate(winner, nT) / length(seasons)]
  res[, "exp_weeks" := colSums(apply(alive_after, c(2, 3), mean))]
  res[order(-get("win_pct"))]
}

#' Where each franchise's lineup is thin
#'
#' (EXPERIMENTAL) Mean weekly points each franchise's simulated *starters*
#' score at each position, against the league median at that position. The
#' most negative position is where a contender would pay to upgrade - and
#' where your veterans are worth most to them.
#'
#' @param sim an `ff_simulation` built with `return = "all"`
#'
#' @return a data.table: `franchise_id`, `pos`, `pts` (starter points per week
#'   at the position), `vs_median`, and `need_rank` (1 = the franchise's
#'   weakest position against the league)
#' @export
ffs_team_needs <- function(sim) {
  checkmate::assert_class(sim, "ff_simulation")
  if (is.null(sim$optimal_scores) || is.null(sim$roster_scores)) {
    cli::cli_abort("{.arg sim} needs {.code return = \"all\"}")
  }
  franchise_id <- player_id <- pos <- pts <- vs_median <- season <- week <- NULL

  opt <- data.table::as.data.table(sim$optimal_scores)
  long <- opt[, list(player_id = as.character(unlist(get("optimal_player_id"))),
                     score = as.numeric(unlist(get("optimal_player_score")))),
              by = list(season, week, franchise_id = as.character(franchise_id))]
  pos_of <- unique(data.table::as.data.table(sim$roster_scores)[
    , list(player_id = as.character(player_id), pos)], by = "player_id")
  long <- merge(long, pos_of, by = "player_id")
  n_fw <- opt[, list(n = .N), by = list(franchise_id = as.character(franchise_id))]

  pp <- long[, list(total = sum(get("score"), na.rm = TRUE)), by = list(franchise_id, pos)]
  pp <- merge(pp, n_fw, by = "franchise_id")
  pp[, pts := get("total") / get("n")]
  # every franchise gets every position, even one it never starts anyone at
  grid <- data.table::CJ(franchise_id = unique(n_fw$franchise_id), pos = unique(pp$pos))
  pp <- merge(grid, pp[, list(franchise_id, pos, pts)], by = c("franchise_id", "pos"),
              all.x = TRUE)
  pp[is.na(pts), pts := 0]
  pp[, vs_median := pts - stats::median(pts), by = pos]
  pp[, "need_rank" := data.table::frank(vs_median, ties.method = "first"), by = franchise_id]
  pp[order(franchise_id, get("need_rank"))]
}

#' Which way each franchise is facing
#'
#' (EXPERIMENTAL) A label per franchise from its playoff and title odds, and -
#' in a dynasty or keeper league, where a lost season has a use - the age of
#' its roster:
#'
#' - **Contender**: playoff odds at or above `contend`, or title odds at least
#'   `champ_mult` times a fair share
#' - **Bubble**: playoff odds between `longshot` and `contend`
#' - **Seller (aging roster)**: a long shot whose value-weighted roster age is
#'   above the league median - the team with veterans it should be moving
#' - **Rebuilding**: a long shot with a young roster
#' - **Long shot**: out of it, in a redraft league
#'
#' Early in a season the odds mostly restate roster strength; they only start
#' to mean "standings" once a few weeks are banked.
#'
#' @param odds a table from [ffs_playoff_odds()]
#' @param players one row per rostered player: `franchise_id`, `age` and a
#'   `value` to weight by (dynasty value in a dynasty league)
#' @param league_type `"dynasty"`, `"keeper"` or `"redraft"`
#' @param contend,longshot playoff-odds lines
#' @param champ_mult title odds, as a multiple of a fair share, that also make
#'   a contender
#'
#' @return `odds` with `roster_age` and `posture` added
#' @export
ffs_team_posture <- function(odds, players = NULL, league_type = "dynasty",
                             contend = 0.65, longshot = 0.30, champ_mult = 1.5) {
  franchise_id <- age <- value <- roster_age <- i.roster_age <- NULL
  o <- data.table::copy(data.table::as.data.table(odds))
  o[, franchise_id := as.character(franchise_id)]
  o[, roster_age := NA_real_]
  if (!is.null(players) && NROW(players)) {
    p <- data.table::as.data.table(players)[!is.na(age) & !is.na(value) & value > 0]
    p[, franchise_id := as.character(franchise_id)]
    ra <- p[, list(roster_age = stats::weighted.mean(age, value)), by = franchise_id]
    o[ra, roster_age := i.roster_age, on = "franchise_id"]
  }
  fair <- 1 / nrow(o)
  med_age <- stats::median(o$roster_age, na.rm = TRUE)
  long_term <- rep(league_type %in% c("dynasty", "keeper"), nrow(o))
  o[, "posture" := data.table::fcase(
    get("playoff_pct") >= contend | get("champion_pct") >= champ_mult * fair, "Contender",
    get("playoff_pct") >= longshot, "Bubble",
    long_term & !is.na(roster_age) & roster_age > med_age, "Seller (aging roster)",
    long_term, "Rebuilding",
    default = "Long shot")]
  o[]
}
