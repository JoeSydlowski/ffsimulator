# Rest-of-season playoff odds for every league, and who is buying and selling.
#
#   Rscript dev/suite/playoff_odds.R
#   FFS_NSEASONS=2000 Rscript dev/suite/playoff_odds.R        # tighter numbers
#   FFS_LEAGUE_ID=1326464763936403456 Rscript dev/suite/playoff_odds.R
#
# Per league it simulates the games LEFT on the real schedule from live
# FantasyPros rest-of-season rankings, adds the record already banked, and
# seeds the playoffs the way Sleeper does (wins, then points-for), for however
# many berths the league has. A guillotine league gets survival odds instead.
#
# On top of the odds, each franchise gets
#   posture   Contender / Bubble / Seller (aging roster) / Rebuilding / Long shot
#   need      the position its starters are weakest at, against the league
# and from those two, per league:
#   targets   win-now veterans on teams that are out of it - most useful to you
#             where they fill your need
#   pitches   your win-now veterans, and the contenders whose need they fill
#
# Outputs to dev/league_sims/playoff_odds/<date>/:
#   odds.csv, needs.csv, targets.csv, pitches.csv, playoff_odds.html
#   (+ playoff_odds_artifact.html, the same page shaped for publishing)
# and appends every run to dev/league_sims/playoff_odds/history.csv, which is
# where the page's "since last run" arrows come from.
#
# The simulation is saved where dev/suite/waiver_wire.R looks for it
# (dev/league_sims/<league_id>/<date>/simulation.rds), so the waiver report run
# afterwards on the same day prices claims on this exact base.

library(data.table)
devtools::load_all(here::here(), quiet = TRUE)
# ffscrapr reads a dead nflverse asset, so ff_scoringhistory() returns ZERO rows
# for 2025+ and every simulation silently loses its most recent season.
source(here::here("dev", "suite", "scoring_history_shim.R")); install_shim()

config <- list(
  league_ids = strsplit(Sys.getenv("FFS_LEAGUE_ID", paste(
    "1312145437842497536", "1326464763936403456", "1359546500786434048",
    "1339817277222031360", "1389724668889399296", "1413736327588896768",
    sep = ",")), ",")[[1]],
  season   = as.integer(Sys.getenv("FFS_SEASON", "2026")),
  my_team  = Sys.getenv("FFS_MY_TEAM", "sox05syd"),
  # playoff odds SE is ~sqrt(p(1-p)/n): ~1.5 points at n=1000
  n_sims   = as.integer(Sys.getenv("FFS_NSEASONS", "1000")),
  pos_filter = strsplit(Sys.getenv("FFS_POS_FILTER", "QB,RB,WR,TE,K"), ",")[[1]],
  reuse_sim = as.logical(Sys.getenv("FFS_REUSE_SIM", "TRUE")),
  # a veteran, for the target and pitch lists: past this age, or priced higher
  # for this season than for the long run
  vet_age = c(QB = 32, RB = 26, WR = 28, TE = 29, K = 99),
  winnow_ratio = 1.15,
  min_value = 500,
  # a "need" is a position at least this many points per week below the median;
  # anything closer is noise, and showed up as "QB -0.0"
  need_min = 1,
  top_targets = 25L,
  # leagues that are over: no longer simulated, shown on the page as a result
  finished = list(
    list(name = "Don't be Chopped", type = "guillotine", result = "Champion",
         detail = "last of 5 standing after week 4, 617 points")
  )
)

day <- format(Sys.Date())
# FFS_ODDS_ROOT sends a run elsewhere - e.g. a quick sanity check that should
# not land in history.csv and move the next real run's arrows
root <- Sys.getenv("FFS_ODDS_ROOT", here::here("dev", "league_sims", "playoff_odds"))
out <- file.path(root, day)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
generated <- format(Sys.time(), "%Y-%m-%d %H:%M")

get_json <- function(path) ffscrapr::sleeper_getendpoint(path)$content

ros_cache <- new.env(parent = emptyenv())
ros_for <- function(ppr) {
  key <- paste0("ppr", ppr)
  if (is.null(ros_cache[[key]])) {
    message("scraping FantasyPros rest-of-season rankings (", ppr, " ppr) ...")
    ros_cache[[key]] <- ffs_ros_rankings(season = config$season, ppr = ppr,
                                         pos_filter = config$pos_filter)
  }
  ros_cache[[key]]
}

# a same-day simulation is reused only if it is big enough, was run with the
# settings this report needs, and knows the banked record
usable_cached <- function(path, n) {
  if (!config$reuse_sim || !file.exists(path)) return(NULL)
  s <- readRDS(path)
  ok <- isTRUE(s$simulation_params$n_seasons >= n) &&
    identical(s$simulation_params$lineup_method, "rank") &&
    isTRUE(s$simulation_params$actual_schedule) &&
    !is.null(s$standings) && !is.null(s$optimal_scores) &&
    !any(grepl("^(QB|RB|WR|TE|K)_\\d+$", unique(s$roster_scores$player_id)))
  if (ok) {
    message("reusing ", path, " (n=", s$simulation_params$n_seasons, ")")
    return(s)
  }
  message("cached simulation at ", path, " does not fit - re-simulating")
  NULL
}

simulate <- function(conn, ppr, actual_schedule, n_weeks = 14L) {
  ff_simulate(
    conn,
    n_seasons = config$n_sims,
    n_weeks = n_weeks,
    version = "v3",
    lineup_method = "rank",
    # must stay FALSE: it would hand every franchise the whole free-agent pool,
    # and the waiver report reusing this simulation would price claims at ~0
    replacement_level = FALSE,
    fill_missing_starters = TRUE,
    actual_schedule = actual_schedule,
    pos_filter = config$pos_filter,
    latest_rankings = ros_for(ppr),
    return = "all"
  )
}

## ---- PER LEAGUE --------------------------------------------------------------
build_league <- function(league_id) {
  raw <- get_json(paste0("league/", league_id))
  st <- raw$settings
  lname <- raw$name
  teams <- as.integer(raw$total_rosters)
  slots <- unlist(raw$roster_positions)
  superflex <- any(slots %in% c("SUPER_FLEX", "SUPERFLEX")) || sum(slots == "QB") > 1
  ppr <- suppressWarnings(as.numeric(raw$scoring_settings$rec))
  if (!length(ppr) || is.na(ppr)) ppr <- 1
  type_code <- as.integer(st$type)
  guillotine <- identical(type_code, 3L)
  ltype <- c("redraft", "keeper", "dynasty", "guillotine")[type_code + 1L]
  if (!length(ltype) || is.na(ltype)) ltype <- "redraft"
  cli::cli_rule("{lname} - {ltype}{if (superflex) ' superflex' else ' 1QB'}, {ppr} ppr, {teams} teams")

  conn <- ffscrapr::sleeper_connect(season = config$season, league_id = league_id)
  fr <- as.data.table(ffscrapr::ff_franchises(conn))
  fr[, franchise_id := as.character(franchise_id)]
  keys <- intersect(c("user_name", "franchise_name"), names(fr))
  hit <- fr[Reduce(`|`, lapply(keys, function(k)
    grepl(config$my_team, fr[[k]], ignore.case = TRUE)))]
  my_id <- if (nrow(hit) == 1) hit$franchise_id[[1]] else NA_character_

  base <- list(id = league_id, name = lname, type = ltype,
               qb = if (superflex) "superflex" else "1QB", ppr = ppr, teams = teams,
               my_id = my_id, my_team = if (is.na(my_id)) NA_character_ else hit$franchise_name[[1]])

  if (guillotine) return(c(base, build_guillotine(conn, league_id, st, fr, ppr)))
  c(base, build_h2h(conn, league_id, st, fr, ppr, superflex, teams, ltype, my_id))
}

build_guillotine <- function(conn, league_id, st, fr, ppr) {
  rosters <- get_json(paste0("league/", league_id, "/rosters"))
  alive <- Filter(function(r) length(r$players) > 0 && is.null(r$settings$eliminated), rosters)
  alive_ids <- vapply(alive, function(r) as.character(r$roster_id), character(1))
  pts <- vapply(alive, function(r) as.numeric(r$settings$fpts %||% 0) +
                  as.numeric(r$settings$fpts_decimal %||% 0) / 100, numeric(1))
  names(pts) <- alive_ids
  # the first unscored week is the next elimination, one a week until one is left
  first <- as.integer(st$last_scored_leg %||% 0L) + 1L
  weeks <- seq(first, length.out = max(1L, length(alive_ids) - 1L))
  message(length(alive_ids), " teams alive; eliminations in weeks ",
          paste(weeks, collapse = ", "))
  sim <- simulate(conn, ppr, actual_schedule = FALSE, n_weeks = max(weeks))
  g <- ffs_guillotine_odds(sim, alive_ids, weeks, points_so_far = pts)
  g <- merge(g, fr[, list(franchise_id, franchise_name)], by = "franchise_id", all.x = TRUE)
  g[, pf_now := pts[franchise_id]]
  rm(sim); gc(verbose = FALSE)
  list(format = "guillotine", weeks = weeks, odds = g[order(-win_pct)],
       n_out = length(rosters) - length(alive_ids))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

build_h2h <- function(conn, league_id, st, fr, ppr, superflex, teams, ltype, my_id) {
  sim_dir <- here::here("dev", "league_sims", league_id, day)
  dir.create(sim_dir, recursive = TRUE, showWarnings = FALSE)
  sim_path <- file.path(sim_dir, "simulation.rds")
  sim <- usable_cached(sim_path, config$n_sims)
  if (is.null(sim)) {
    sim <- simulate(conn, ppr, actual_schedule = TRUE)
    if (is.null(sim$summary_season)) stop("no unplayed weeks left to simulate")
    saveRDS(sim, sim_path)
  }

  odds <- ffs_playoff_odds(sim)
  needs <- ffs_team_needs(sim)
  weeks_left <- length(unique(sim$optimal_scores$week))
  n_sims <- as.integer(sim$simulation_params$n_seasons)
  weeks_banked <- max(sim$standings$games, 0)
  rm(sim); gc(verbose = FALSE)

  # every rostered player with his age and both market values
  rosters <- as.data.table(ffs_rosters(conn))
  rosters[, `:=`(player_id = as.character(player_id), franchise_id = as.character(franchise_id))]
  vals <- tryCatch(as.data.table(fc_dynasty_values(
    num_qbs = if (superflex) 2 else 1, num_teams = teams, ppr = ppr, is_dynasty = TRUE)),
    error = function(e) {message("FantasyCalc unavailable: ", conditionMessage(e)); NULL})
  if (!is.null(vals)) {
    v <- unique(vals[!is.na(sleeper_id), list(player_id = as.character(sleeper_id),
                                               fc_age = age, dynasty_value = value,
                                               redraft_value)], by = "player_id")
    rosters <- merge(rosters, v, by = "player_id", all.x = TRUE)
    rosters[is.na(age), age := fc_age]
  } else {
    rosters[, `:=`(dynasty_value = NA_real_, redraft_value = NA_real_)]
  }
  long_term <- ltype %in% c("dynasty", "keeper")
  weight <- if (long_term) "dynasty_value" else "redraft_value"
  posture <- ffs_team_posture(
    odds, rosters[, list(franchise_id, age, value = get(weight))], ltype)
  odds <- posture

  # a franchise's need: its weakest position that is below the league median
  top_need <- needs[vs_median <= -config$need_min][order(franchise_id, need_rank)][
    , .SD[1L], by = franchise_id][, list(franchise_id, need_pos = pos, need_gap = vs_median)]
  odds <- merge(odds, top_need, by = "franchise_id", all.x = TRUE)

  # veterans: past the positional age line, or priced clearly higher for this
  # season than for the long run. A player in his prime sits near 1:1 (Puka
  # 1.04, Olave 1.10 - and only 5 of 89 under-the-line players reach 1.10), so
  # plain redraft >= dynasty flagged 24-year-olds as "veterans"
  vet_line <- config$vet_age[rosters$pos]
  rosters[, vet := (!is.na(age) & age >= vet_line) |
            (!is.na(redraft_value) & !is.na(dynasty_value) & dynasty_value > 0 &
               redraft_value >= config$winnow_ratio * dynasty_value)]
  rosters[is.na(vet), vet := FALSE]
  # a piece worth less than this moves nobody's needle - keep it off both lists
  rosters[is.na(redraft_value) | redraft_value < config$min_value, vet := FALSE]
  rosters <- merge(rosters, odds[, list(franchise_id, posture)], by = "franchise_id", all.x = TRUE)

  my_needs <- if (is.na(my_id)) character(0) else
    needs[franchise_id == my_id & vs_median <= -config$need_min & need_rank <= 2]$pos
  my_posture <- if (is.na(my_id)) NA_character_ else odds[franchise_id == my_id]$posture

  # A team that is out of it will move a veteran to anyone. At a position you
  # actually need, take the whole roster: the young back who would start for you
  # is the point of trading with a rebuilder, and he is no veteran (Bucky Irving
  # on 0-2 Prestige Worldwide is exactly the case).
  out_of_it <- c("Seller (aging roster)", "Long shot", "Rebuilding")
  targets <- rosters[posture %in% out_of_it & franchise_id != my_id &
                       !is.na(redraft_value) & redraft_value >= config$min_value &
                       (vet | pos %in% my_needs),
                     list(player = player_name, pos, age = round(age, 1), team,
                          owner = franchise_name, owner_posture = posture,
                          redraft_value, dynasty_value,
                          fits_need = pos %in% my_needs,
                          why = data.table::fcase(
                            vet & pos %in% my_needs, "vet, fills your need",
                            pos %in% my_needs, "fills your need",
                            default = "win-now vet"))]
  targets <- head(targets[order(-fits_need, -redraft_value)], config$top_targets)

  # my veterans, and the contenders whose need each one fills
  buyers <- needs[vs_median <= -config$need_min & need_rank <= 2]
  buyers <- merge(buyers, odds[posture %in% c("Contender", "Bubble"),
                               list(franchise_id, franchise_name, posture, playoff_pct)],
                  by = "franchise_id")
  mine <- rosters[franchise_id == my_id & vet & !is.na(redraft_value) & redraft_value > 0]
  pitches <- if (nrow(mine)) rbindlist(lapply(seq_len(nrow(mine)), function(i) {
    p <- mine[i]
    b <- buyers[pos == p$pos & franchise_id != my_id][order(vs_median)]
    if (!nrow(b)) return(NULL)
    data.table(player = p$player_name, pos = p$pos, age = round(p$age, 1),
               redraft_value = p$redraft_value, dynasty_value = p$dynasty_value,
               buyers = paste(sprintf("%s (%s, %+.1f/wk)", b$franchise_name, b$posture,
                                      b$vs_median), collapse = "; "),
               n_buyers = nrow(b))
  })) else data.table()
  if (nrow(pitches)) pitches <- pitches[order(-n_buyers, -redraft_value)]

  list(format = "h2h", playoff_teams = odds$playoff_teams[[1]], weeks_left = weeks_left,
       weeks_banked = weeks_banked, n_sims = n_sims, my_posture = my_posture, my_needs = my_needs,
       odds = odds, needs = needs, targets = targets, pitches = pitches)
}

leagues <- list()
for (lid in config$league_ids) {
  res <- try(build_league(lid), silent = FALSE)
  if (inherits(res, "try-error")) {
    message("league ", lid, " failed: ", conditionMessage(attr(res, "condition")))
  } else {
    leagues[[length(leagues) + 1L]] <- res
  }
  gc(verbose = FALSE)
}
if (!length(leagues)) stop("no leagues built")

## ---- HISTORY + CSVs ----------------------------------------------------------
hist_path <- file.path(root, "history.csv")
hist_prev <- if (file.exists(hist_path)) fread(hist_path, colClasses = c(
  franchise_id = "character", league_id = "character", generated = "character")) else NULL
this_run <- rbindlist(lapply(leagues, function(l) {
  o <- as.data.table(l$odds)
  data.table(generated = generated, league_id = l$id, league = l$name,
             franchise_id = o$franchise_id, franchise_name = o$franchise_name,
             playoff_pct = if (l$format == "h2h") o$playoff_pct else NA_real_,
             champion_pct = if (l$format == "h2h") o$champion_pct else o$win_pct)
}))
# the change since the most recent earlier run of each league
prev <- NULL
if (!is.null(hist_prev) && nrow(hist_prev)) {
  gen_now <- generated
  hp <- hist_prev[as.character(generated) < gen_now]
  if (nrow(hp)) {
    last <- hp[, list(generated = max(generated)), by = league_id]
    prev <- merge(hp, last, by = c("league_id", "generated"))[
      , list(league_id, franchise_id, prev_playoff = playoff_pct,
             prev_champion = champion_pct, prev_generated = generated)]
  }
}
fwrite(this_run, hist_path, append = file.exists(hist_path))

attach_prev <- function(o, lid) {
  o <- copy(as.data.table(o))
  if (is.null(prev)) {
    o[, `:=`(prev_playoff = NA_real_, prev_champion = NA_real_, prev_generated = NA_character_)]
    return(o)
  }
  p <- prev[league_id == lid, !"league_id"]
  merge(o, p, by = "franchise_id", all.x = TRUE)
}
for (i in seq_along(leagues)) leagues[[i]]$odds <- attach_prev(leagues[[i]]$odds, leagues[[i]]$id)

flat <- function(field) rbindlist(lapply(leagues, function(l) {
  x <- l[[field]]
  if (is.null(x) || !NROW(x)) return(NULL)
  x <- copy(as.data.table(x))
  lc <- names(x)[vapply(x, is.list, logical(1))]
  for (cc in lc) set(x, j = cc, value = vapply(x[[cc]], function(v)
    paste(round(v, 4), collapse = "|"), character(1)))
  cbind(league = l$name, x)
}), fill = TRUE)
fwrite(flat("odds"), file.path(out, "odds.csv"))
fwrite(flat("needs"), file.path(out, "needs.csv"))
fwrite(flat("targets"), file.path(out, "targets.csv"))
fwrite(flat("pitches"), file.path(out, "pitches.csv"))

## ---- PAGE --------------------------------------------------------------------
payload <- lapply(leagues, function(l) {
  keep <- l[setdiff(names(l), "needs")]
  keep$needs <- if (!is.null(l$needs)) l$needs[, list(franchise_id, pos, pts = round(pts, 1),
                                                       vs_median = round(vs_median, 1),
                                                       need_rank)] else NULL
  keep$my_needs <- I(l$my_needs %||% character(0))
  keep$weeks <- I(l$weeks %||% integer(0))
  keep
})
# a reused same-day simulation can be bigger than this run asked for
n_used <- unlist(lapply(leagues, function(l) l$n_sims %||% config$n_sims))
meta <- list(generated = generated, n_min = min(n_used), n_max = max(n_used), season = config$season,
             week = as.integer(nflreadr::get_current_week()),
             prev_generated = if (is.null(prev)) NA_character_ else max(prev$prev_generated),
             finished = config$finished)
json <- jsonlite::toJSON(list(meta = meta, leagues = payload), auto_unbox = TRUE,
                         na = "null", digits = 4, dataframe = "rows")

tpl <- readLines(here::here("dev", "suite", "playoff_odds_page.html"), warn = FALSE,
                 encoding = "UTF-8")
page <- paste(tpl, collapse = "\n")
# the payload goes in a JSON script block; "</" inside it would close the tag
page <- sub("__DATA__", gsub("</", "<\\\\/", json, fixed = TRUE), page, fixed = TRUE)
writeLines(page, file.path(out, "playoff_odds.html"), useBytes = TRUE)
# the same page shaped for publishing as an artifact: the host supplies the
# doctype/html/head/body skeleton and the charset + viewport meta itself
art <- gsub("(?is)<!doctype html>|</?html[^>]*>|</?head>|</?body>|<meta [^>]*>", "", page, perl = TRUE)
writeLines(trimws(art), file.path(out, "playoff_odds_artifact.html"), useBytes = TRUE)

cat("\n")
for (l in leagues) {
  o <- as.data.table(l$odds)
  cat("==", l$name, "(", l$type, ") ==\n")
  if (l$format == "h2h") {
    print(o[, list(team = franchise_name, rec = paste0(wins_now, "-", losses_now),
                   proj_w = round(proj_wins, 1), playoff = round(playoff_pct, 3),
                   champ = round(champion_pct, 3), posture, need = need_pos)])
  } else {
    print(o)
  }
}
cat("\nwrote ", out, "\n", sep = "")
