# Weekly waiver watchlist - every league, every signal, no simulation.
#
#   Rscript dev/suite/waiver_watchlist.R
#   FFS_LEAGUE_ID=1312145437842497536 Rscript dev/suite/waiver_watchlist.R
#
# Runs in a couple of minutes: it scrapes rather than simulates. Per league it
# lists every ranked player - free agent or rostered - with
#   rankings  ecr / pos_rank / ecr_delta   (LIVE FantasyPros rest-of-season scrape)
#   market    roster_pct, Sleeper adds+drops over the last 24h
#   next week Sleeper projection for the upcoming week, in league scoring
#   usage     snap share, expected fantasy points, actual points (nflreadr)
#   value     FantasyCalc redraft and dynasty values
#
# Outputs to dev/league_sims/waivers/<date>/:
#   watchlist.csv            every league in one table, every column
#   waiver_watchlist.html    all leagues, sortable, open it in a browser
#   waiver_watchlist_artifact.html  same page, shaped for publishing
#
# To price a specific add against a specific drop - in wins, playoff odds and
# championship odds - take the shortlist to dev/suite/waiver_wire.R.

library(data.table)
devtools::load_all(here::here(), quiet = TRUE)
options(ffpros.cache = "memory")

config <- list(
  # every league sox05syd is in (ffscrapr::sleeper_userleagues("sox05syd", 2026));
  # Season 2 (guillotine) replaced Don't be Chopped, which finished in week 4
  league_ids = strsplit(Sys.getenv("FFS_LEAGUE_ID", paste(
    "1312145437842497536", "1326464763936403456", "1359546500786434048",
    "1339817277222031360", "1389724668889399296", "1413736327588896768",
    sep = ",")), ",")[[1]],
  season   = as.integer(Sys.getenv("FFS_SEASON", "2026")),
  my_team  = Sys.getenv("FFS_MY_TEAM", "sox05syd"),
  # the upper bound; each league is narrowed to what it actually starts
  pos_filter = strsplit(Sys.getenv("FFS_POS_FILTER", "QB,RB,WR,TE,K,DST"), ",")[[1]],
  # snaps and expected points only exist for the skill positions
  usage_pos = c("QB", "RB", "WR", "TE"),
  lookback = as.integer(Sys.getenv("FFS_LOOKBACK", "3")),
  trend_hours = as.integer(Sys.getenv("FFS_TREND_HOURS", "24")),
  # keeper leagues that get next season's keeper price on their tab: Galletti
  keeper_leagues = strsplit(Sys.getenv("FFS_KEEPER_LEAGUES", "1339817277222031360"), ",")[[1]]
)
source(here::here("dev", "suite", "keeper_next_year.R"))

day <- format(Sys.Date())
out <- here::here("dev", "league_sims", "waivers", day)
dir.create(out, recursive = TRUE, showWarnings = FALSE)

## ---- SHARED SIGNALS (fetched once, reused by every league) -------------------
message("usage signals (snaps + expected points) ...")
usage <- tryCatch(ffs_usage_signals(season = config$season, lookback = config$lookback,
                                    pos_filter = config$usage_pos),
                  error = function(e) {message("  unavailable: ", conditionMessage(e)); NULL})


week <- as.integer(nflreadr::get_current_week())
message("sleeper projections for week ", week, " ...")
projections <- tryCatch(
  ffs_sleeper_projections(season = config$season, week = week,
                          pos_filter = config$pos_filter),
  error = function(e) {message("  unavailable: ", conditionMessage(e)); NULL})

# live weekly positional + flex ranks, one scrape per scoring and flex type
week_cache <- new.env(parent = emptyenv())
week_for <- function(ppr, flex) {
  key <- paste0("ppr", ppr, "_", flex)
  if (is.null(week_cache[[key]])) {
    message("scraping FantasyPros weekly rankings (", ppr, " ppr, ", flex, ") ...")
    week_cache[[key]] <- tryCatch(
      ffs_week_rankings(ppr = ppr, pos_filter = config$pos_filter, flex = flex),
      error = function(e) {message("  unavailable: ", conditionMessage(e)); NULL})
  }
  week_cache[[key]]
}

message("sleeper player file (injuries + NFL depth chart) ...")
sleeper_players <- tryCatch(ffs_sleeper_players(pos_filter = config$pos_filter),
                            error = function(e) {
                              message("  unavailable: ", conditionMessage(e)); NULL})

message("sleeper trending (", config$trend_hours, "h) ...")
trending <- list(
  add = tryCatch(ffs_sleeper_trending("add", config$trend_hours, 200L, players = sleeper_players),
                 error = function(e) NULL),
  drop = tryCatch(ffs_sleeper_trending("drop", config$trend_hours, 200L, players = sleeper_players),
                  error = function(e) NULL))

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

fc_cache <- new.env(parent = emptyenv())
fc_for <- function(num_qbs, num_teams, ppr) {
  key <- paste(num_qbs, num_teams, ppr, sep = "_")
  if (is.null(fc_cache[[key]])) {
    message("fantasycalc values (", num_qbs, "qb, ", num_teams, " teams, ", ppr, " ppr) ...")
    # is_dynasty = TRUE returns BOTH values: `value` dynasty, `redraft_value` redraft
    fc_cache[[key]] <- tryCatch(
      fc_dynasty_values(num_qbs = num_qbs, num_teams = num_teams, ppr = ppr,
                        is_dynasty = TRUE),
      error = function(e) {message("  unavailable: ", conditionMessage(e)); NULL})
  }
  fc_cache[[key]]
}

## ---- PER LEAGUE --------------------------------------------------------------
build_league <- function(league_id) {
  conn <- ffscrapr::sleeper_connect(season = config$season, league_id = league_id)

  # Read the settings off Sleeper's own league object rather than ff_league():
  # ffscrapr builds its columns from whatever the payload happens to carry, and
  # on one of these leagues it returns no league_type column at all, which took
  # the whole league out of the run.
  raw <- ffscrapr::sleeper_getendpoint(paste0("league/", league_id))$content
  lname <- raw$name
  teams <- as.integer(raw$total_rosters)
  slots <- unlist(raw$roster_positions)
  superflex <- any(slots %in% c("SUPER_FLEX", "SUPERFLEX")) || sum(slots == "QB") > 1
  ppr <- suppressWarnings(as.numeric(raw$scoring_settings$rec))
  if (!length(ppr) || is.na(ppr)) ppr <- 1
  ltype <- c("redraft", "keeper", "dynasty", "guillotine")[as.integer(raw$settings$type) + 1L]
  if (!length(ltype) || is.na(ltype)) ltype <- "redraft"

  # only carry the positions this league actually starts: Goofball is the one
  # with a K and a DEF slot, and a kicker on a JML board would be noise
  lg_pos <- unique(unlist(list(
    QB = "QB", RB = "RB", WR = "WR", TE = "TE", K = "K", DEF = "DST", DST = "DST",
    FLEX = c("RB", "WR", "TE"), REC_FLEX = c("WR", "TE"), WRRB_FLEX = c("WR", "RB"),
    SUPER_FLEX = c("QB", "RB", "WR", "TE"), SUPERFLEX = c("QB", "RB", "WR", "TE")
  )[intersect(c("QB", "RB", "WR", "TE", "K", "DEF", "DST", "FLEX", "REC_FLEX",
                "WRRB_FLEX", "SUPER_FLEX", "SUPERFLEX"), slots)]))
  lg_pos <- intersect(config$pos_filter, unname(lg_pos))
  if (!length(lg_pos)) lg_pos <- intersect(config$pos_filter, c("QB", "RB", "WR", "TE"))

  # the widest flex slot the league has decides the flex scale
  flex_type <- if (any(slots %in% c("SUPER_FLEX", "SUPERFLEX"))) "superflex" else "flex"
  flex_pos <- if (flex_type == "superflex") c("QB", "RB", "WR", "TE") else
    if ("FLEX" %in% slots) c("RB", "WR", "TE") else
      unique(c(if ("WRRB_FLEX" %in% slots) c("WR", "RB"),
               if ("REC_FLEX" %in% slots) c("WR", "TE")))
  if (!length(flex_pos)) flex_pos <- c("RB", "WR", "TE")
  cli::cli_rule("{lname} - {ltype}{
    if (superflex) ' superflex' else ' 1QB'}, {ppr} ppr, {teams} teams, {paste(lg_pos, collapse = '/')}")

  who <- as.data.table(ffscrapr::ff_franchises(conn))
  keys <- intersect(c("user_name", "franchise_name"), names(who))
  hit <- who[Reduce(`|`, lapply(keys, function(k)
    grepl(config$my_team, who[[k]], ignore.case = TRUE)))]
  my_id <- if (nrow(hit) == 1) as.character(hit$franchise_id[[1]]) else NULL
  my_name <- if (nrow(hit) == 1) hit$franchise_name[[1]] else NA_character_

  fc <- fc_for(if (superflex) 2 else 1, teams, ppr)
  w <- as.data.table(ffs_waiver_watchlist(
    conn,
    ros = ros_for(ppr),
    usage = usage,
    values = fc,
    trending = trending,
    projections = projections,
    weekly = week_for(ppr, flex_type),
    sleeper_players = sleeper_players,
    lineups = tryCatch(ffs_sleeper_lineups(league_id, week = week),
                       error = function(e) NULL),
    ppr = ppr,
    pos_filter = lg_pos,
    flex_pos = flex_pos,
    my_franchise = my_id))

  # a keeper league mid-season: what each player would cost to keep next year,
  # priced off this year's draft (see dev/suite/keeper_next_year.R)
  keeper <- league_id %in% config$keeper_leagues && !is.null(fc)
  if (keeper) {
    kk <- tryCatch({
      picks <- keeper_draft_picks(league_id)
      bar <- keeper_round_bar(league_id, config$season, picks = picks)
      keeper_next_year(w[, list(fantasypros_id, player, pos)], picks, bar, fc)
    }, error = function(e) {message("  keeper columns unavailable: ", conditionMessage(e)); NULL})
    if (is.null(kk)) keeper <- FALSE else {
      kcols <- c("draft_rd", "kept_now", "keep_cost", "top2", "cost_source",
                 "dyn_rank", "keep_bar", "keep_margin")
      w[, (kcols) := kk[, kcols, with = FALSE]]
    }
  }

  list(
    keeper = keeper,
    id = league_id,
    name = lname,
    slots = slots[!slots %in% c("BN", "IR", "TAXI")],
    type = ltype,
    qb = if (superflex) "superflex" else "1QB",
    ppr = ppr,
    teams = teams,
    my_team = my_name,
    flex_label = if (flex_type == "superflex") "SF" else "FLEX",
    flex_pos = flex_pos,
    # what a trade costs here: a keeper roster turns over slowly enough to price
    # like dynasty
    value_key = if (ltype %in% c("redraft", "guillotine")) "redraft_value" else "dynasty_value",
    rows = w
  )
}

leagues <- list()
for (lid in config$league_ids) {
  res <- try(build_league(lid), silent = FALSE)
  if (inherits(res, "try-error")) {
    message("league ", lid, " failed: ", conditionMessage(attr(res, "condition")))
  } else {
    leagues[[length(leagues) + 1L]] <- res
  }
}
if (!length(leagues)) stop("no leagues built")

# one table for every league, for your own slicing; the week-by-week vectors go
# in as "0.74|0.81" strings, which fwrite cannot do itself for a player with none
csv <- rbindlist(lapply(leagues, function(l)
  cbind(league = l$name, as.data.table(l$rows))), fill = TRUE)
for (cc in names(csv)[vapply(csv, is.list, logical(1))]) {
  set(csv, j = cc, value = vapply(csv[[cc]], function(x)
    if (is.null(x)) NA_character_ else paste(x, collapse = "|"), character(1)))
}
fwrite(csv, file.path(out, "watchlist.csv"))

## ---- PAGE --------------------------------------------------------------------
# columns the page shows, in order; everything else stays in the CSV
show <- c("player", "pos", "team", "bye", "status", "rostered_by", "mine", "ranked", "signal",
          "pos_rank", "ecr", "ecr_delta", "roster_pct", "trend_add", "trend_drop",
          "proj", "wk_rank", "flex_rank", "opp", "injury", "starting", "slot", "slot_no",
          "nfl_depth", "snap_pct", "xfp_g", "fp_g", "snap_wk", "xfp_wk", "fp_wk",
          "redraft_value", "dynasty_value")
keep_show <- c("draft_rd", "kept_now", "keep_cost", "top2", "cost_source",
               "dyn_rank", "keep_bar", "keep_margin")

payload <- lapply(leagues, function(l) {
  d <- as.data.table(l$rows)
  cols <- c(show, if (isTRUE(l$keeper)) keep_show)
  for (cc in setdiff(cols, names(d))) d[, (cc) := NA]
  d <- d[, cols, with = FALSE]
  # jsonlite writes NA as null, which the page treats as "no data"
  list(id = l$id, name = l$name, type = l$type, qb = l$qb, ppr = l$ppr,
       keeper = isTRUE(l$keeper),
       slots = l$slots,
       teams = l$teams, my_team = l$my_team,
       flex_label = l$flex_label, flex_pos = l$flex_pos, value_key = l$value_key,
       n_fa = sum(d$status == "free agent"),
       rows = d)
})

scrape <- unique(unlist(lapply(leagues, function(l) as.character(l$rows$source))))
ros_date <- unique(unlist(lapply(leagues, function(l) as.character(l$rows$scrape_date))))
meta <- list(
  generated = format(Sys.time(), "%Y-%m-%d %H:%M"),
  ros_source = paste(scrape, collapse = "/"),
  ros_date = paste(ros_date, collapse = "/"),
  week = as.integer(nflreadr::get_current_week()),
  season = config$season,
  trend_hours = config$trend_hours,
  lookback = config$lookback,
  # the axis every snap_wk / xfp_wk / fp_wk vector is aligned to
  weeks = I(as.integer(attr(usage, "weeks")))
)

json <- jsonlite::toJSON(list(meta = meta, leagues = payload),
                         auto_unbox = TRUE, na = "null", digits = 4)

head_bits <- paste0(
'<title>Waiver Watchlist</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Archivo:wght@600;700&family=IBM+Plex+Mono:wght@400;500&family=IBM+Plex+Sans:wght@400;500;600&display=swap">
<style>
:root{
  --ground:#f5f7f6; --surface:#ffffff; --surface-2:#eef2f0;
  --ink:#111a17; --ink-2:#4a5a54; --ink-3:#7a8a84;
  --line:#dfe6e2; --line-strong:#c6d1cc;
  --accent:#0f6b4f; --accent-soft:#dff0e8; --accent-ink:#0b5540;
  --up:#116b4c; --down:#a5402f; --mine:#9a6412; --mine-soft:#f7ecd8;
  --bar:#bfe3d2; --chip-on:#ffffff;
  --shadow:0 1px 2px rgba(17,26,23,.06), 0 8px 24px -16px rgba(17,26,23,.25);
}
@media (prefers-color-scheme: dark){
  :root:not([data-theme="light"]){
    --ground:#0e1412; --surface:#151d1a; --surface-2:#1b2522;
    --ink:#e7edea; --ink-2:#9db0a8; --ink-3:#74867f;
    --line:#233029; --line-strong:#31423a;
    --accent:#49bd8e; --accent-soft:#13312a; --accent-ink:#8fe0bf;
    --up:#49bd8e; --down:#e0806e; --mine:#d8a44a; --mine-soft:#2b2317;
    --bar:#1d4a3a; --chip-on:#08130f;
    --shadow:0 1px 2px rgba(0,0,0,.4), 0 8px 24px -16px rgba(0,0,0,.8);
  }
}
:root[data-theme="dark"]{
  --ground:#0e1412; --surface:#151d1a; --surface-2:#1b2522;
  --ink:#e7edea; --ink-2:#9db0a8; --ink-3:#74867f;
  --line:#233029; --line-strong:#31423a;
  --accent:#49bd8e; --accent-soft:#13312a; --accent-ink:#8fe0bf;
  --up:#49bd8e; --down:#e0806e; --mine:#d8a44a; --mine-soft:#2b2317;
  --bar:#1d4a3a; --chip-on:#08130f;
  --shadow:0 1px 2px rgba(0,0,0,.4), 0 8px 24px -16px rgba(0,0,0,.8);
}
*{box-sizing:border-box}
body{
  margin:0; background:var(--ground); color:var(--ink);
  font-family:"IBM Plex Sans",-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif;
  font-size:14px; line-height:1.45; -webkit-font-smoothing:antialiased;
}
.wrap{max-width:1280px; margin:0 auto; padding-block:0 48px; padding-inline:16px}

header.top{
  position:sticky; top:env(safe-area-inset-top,0px); z-index:20;
  background:var(--ground);
  background:color-mix(in srgb, var(--ground) 92%, transparent);
  backdrop-filter:blur(8px);
  border-bottom:1px solid var(--line);
  margin-inline:-16px; padding:14px 16px 0;
}
.masthead{display:flex; flex-wrap:wrap; align-items:baseline; gap:8px 16px}
h1{
  font-family:Archivo,"IBM Plex Sans",sans-serif; font-weight:700;
  font-size:22px; letter-spacing:-.01em; margin:0; text-wrap:balance;
}
.provenance{color:var(--ink-3); font-size:12px; font-family:"IBM Plex Mono",monospace}
.provenance b{color:var(--ink-2); font-weight:500}

.tabs{display:flex; gap:2px; overflow-x:auto; margin-top:12px; scrollbar-width:thin}
.tab{
  appearance:none; border:0; background:transparent; color:var(--ink-2);
  font:inherit; font-size:13px; padding:8px 12px 10px; cursor:pointer;
  border-bottom:2px solid transparent; white-space:nowrap; border-radius:6px 6px 0 0;
}
.tab:hover{color:var(--ink); background:var(--surface-2)}
.tab[aria-selected="true"]{color:var(--accent-ink); border-bottom-color:var(--accent); font-weight:600}
.tab .cnt{font-family:"IBM Plex Mono",monospace; font-size:11px; color:var(--ink-3); margin-left:6px}
.tab:focus-visible,.seg button:focus-visible,.chip:focus-visible,
th button:focus-visible,#q:focus-visible{outline:2px solid var(--accent); outline-offset:2px}

.leaguebar{
  display:flex; flex-wrap:wrap; align-items:center; gap:10px 18px;
  padding:16px 0 12px;
}
.leaguebar .facts{color:var(--ink-2); font-size:13px}
.leaguebar .facts b{color:var(--ink); font-weight:600}
.dot{color:var(--line-strong)}

.controls{display:flex; flex-wrap:wrap; gap:10px; align-items:center; margin-bottom:12px}
.seg{display:inline-flex; background:var(--surface-2); border-radius:8px; padding:2px; gap:2px}
.seg button{
  appearance:none; border:0; background:transparent; color:var(--ink-2);
  font:inherit; font-size:13px; padding:5px 11px; border-radius:6px; cursor:pointer;
}
.seg button[aria-pressed="true"]{background:var(--surface); color:var(--ink); font-weight:600; box-shadow:var(--shadow)}
.chips{display:flex; gap:6px; flex-wrap:wrap}
.chip{
  appearance:none; font:inherit; font-size:12px; padding:4px 10px; cursor:pointer;
  border:1px solid var(--line-strong); background:var(--surface); color:var(--ink-2);
  border-radius:999px;
}
.chip[aria-pressed="true"]{background:var(--accent); border-color:var(--accent); color:var(--chip-on)}
#q{
  flex:1 1 180px; min-width:0; max-width:280px; font:inherit; font-size:13px;
  padding:6px 10px; border-radius:8px; border:1px solid var(--line-strong);
  background:var(--surface); color:var(--ink);
}
#q::placeholder{color:var(--ink-3)}

.tablewrap{
  overflow-x:auto; background:var(--surface); border:1px solid var(--line);
  border-radius:10px; box-shadow:var(--shadow);
}
table{border-collapse:separate; border-spacing:0; width:100%; min-width:1120px}
thead th{
  position:sticky; top:0; z-index:5; background:var(--surface);
  border-bottom:1px solid var(--line-strong);
  padding:0; text-align:right; white-space:nowrap;
}
thead th:first-child{text-align:left}
th button{
  appearance:none; border:0; background:transparent; width:100%; cursor:pointer;
  font-family:Archivo,"IBM Plex Sans",sans-serif; font-weight:600; font-size:11px;
  letter-spacing:.06em; text-transform:uppercase; color:var(--ink-3);
  padding:11px 10px; text-align:inherit;
}
th button:hover{color:var(--ink)}
th[aria-sort] button{color:var(--accent-ink)}
th button .arw{opacity:.55; font-size:9px; margin-left:3px}
tbody td{
  padding:9px 10px; border-bottom:1px solid var(--line); text-align:right;
  font-family:"IBM Plex Mono",monospace; font-variant-numeric:tabular-nums; font-size:13px;
}
tbody tr:last-child td{border-bottom:0}
tbody tr:hover td{background:var(--surface-2)}
td.name{
  text-align:left; font-family:"IBM Plex Sans",sans-serif; font-size:13.5px;
  font-weight:500; white-space:nowrap;
}
td.name .sub{display:block; font-size:11px; color:var(--ink-3); font-weight:400}
td.name .owner{color:var(--ink-2)}
tr.is-mine td{background:var(--mine-soft)}
tr.is-mine td.name{box-shadow:inset 3px 0 0 var(--mine)}
tr.is-mine:hover td{background:var(--mine-soft); filter:brightness(.98)}
.tag-mine{
  display:inline-block; margin-left:7px; padding:1px 6px; border-radius:4px;
  font-family:"IBM Plex Mono",monospace; font-size:10px; letter-spacing:.04em;
  text-transform:uppercase; color:var(--mine); border:1px solid var(--mine);
  vertical-align:1px; font-weight:500;
}
.unranked{color:var(--mine); font-style:italic}
.pos{
  display:inline-block; min-width:30px; text-align:center; font-size:11px; font-weight:600;
  font-family:"IBM Plex Mono",monospace; padding:2px 5px; border-radius:4px;
  background:var(--surface-2); color:var(--ink-2); border:1px solid var(--line);
}
td.sig{text-align:left; width:104px}
.sigbar{display:flex; align-items:center; gap:7px}
.sigbar .track{flex:1; height:5px; border-radius:3px; background:var(--surface-2); overflow:hidden}
.sigbar .fill{display:block; height:100%; background:var(--bar); border-right:2px solid var(--accent)}
.sigbar .val{font-size:12px; color:var(--ink-2); width:30px; text-align:right}
.delta.up{color:var(--up)} .delta.down{color:var(--down)}
.dim{color:var(--ink-3)}
.empty{padding:28px 14px; color:var(--ink-3); text-align:center}
.visually-hidden{position:absolute; width:1px; height:1px; overflow:hidden; clip:rect(0 0 0 0); white-space:nowrap}

.legend{margin-top:22px; border-top:1px solid var(--line); padding-top:16px}
.legend h2{
  font-family:Archivo,sans-serif; font-size:11px; letter-spacing:.08em;
  text-transform:uppercase; color:var(--ink-3); margin:0 0 10px;
}
.legend dl{
  display:grid; grid-template-columns:repeat(auto-fit,minmax(240px,1fr));
  gap:8px 24px; margin:0;
}
.legend div{display:flex; gap:8px; align-items:baseline}
.legend dt{
  font-family:"IBM Plex Mono",monospace; font-size:11.5px; color:var(--accent-ink);
  white-space:nowrap; font-weight:500;
}
.legend dd{margin:0; font-size:12.5px; color:var(--ink-2)}
/* --- depth views ------------------------------------------------------- */
.views{margin-left:auto; align-self:center}
.teamsel{
  font:inherit; font-size:13px; padding:5px 10px; border-radius:8px;
  border:1px solid var(--line-strong); background:var(--surface); color:var(--ink);
}
.posblock{margin-bottom:20px}
.posblock h3{
  font-family:Archivo,sans-serif; font-size:11px; letter-spacing:.08em;
  text-transform:uppercase; color:var(--ink-3); margin:0 0 6px; padding-left:2px;
}
.dhead,.drow{
  display:grid; grid-template-columns:minmax(150px,1.6fr) 104px repeat(4,62px);
  gap:8px; align-items:center; padding:7px 10px;
}
.dhead{
  font-family:Archivo,sans-serif; font-size:10px; letter-spacing:.06em;
  text-transform:uppercase; color:var(--ink-3); padding-bottom:4px;
}
.dhead span:not(:first-child),.drow span:not(:first-child){text-align:right}
.drow{border-top:1px solid var(--line); font-size:13px}
.drow .num{font-family:"IBM Plex Mono",monospace; font-variant-numeric:tabular-nums}
.drow.is-fa{background:var(--accent-soft)}
.drow.is-mine{background:var(--mine-soft)}
.drow .who{display:flex; align-items:baseline; gap:7px; min-width:0}
.drow .who b{font-weight:500}
.drow .tag{
  font-family:"IBM Plex Mono",monospace; font-size:9.5px; letter-spacing:.04em;
  text-transform:uppercase; padding:1px 5px; border-radius:3px; white-space:nowrap;
}
.tag.fa{color:var(--accent-ink); border:1px solid var(--accent)}
.tag.own{color:var(--ink-3); border:1px solid var(--line-strong)}
.tag.mine{color:var(--mine); border:1px solid var(--mine)}
.share{display:flex; align-items:center; gap:6px}
.share .track{flex:1; height:5px; border-radius:3px; background:var(--surface-2); overflow:hidden}
.share .fill{display:block; height:100%; background:var(--bar); border-right:2px solid var(--accent)}

.gridwrap{overflow-x:auto}
.grid{display:grid; gap:6px; min-width:760px}
.gridrow{display:grid; gap:6px; align-items:stretch}
.gridhead{
  font-family:Archivo,sans-serif; font-size:10px; letter-spacing:.06em;
  text-transform:uppercase; color:var(--ink-3); padding:0 8px 2px; align-self:end;
}
.owncell{
  display:flex; align-items:center; font-size:12.5px; font-weight:600;
  padding:8px; border-radius:6px; background:var(--surface-2);
}
.owncell.mine{background:var(--mine-soft); color:var(--mine)}
.owncell.fa{background:var(--accent-soft); color:var(--accent-ink)}
.slot{
  background:var(--surface); border:1px solid var(--line); border-radius:6px;
  padding:6px 8px; min-height:46px;
}
.slot.empty-slot{background:transparent; border-style:dashed; opacity:.5}
.slot.fa{background:var(--accent-soft); border-color:var(--accent)}
.slot.mine{background:var(--mine-soft); border-color:var(--mine)}
.slot .nm{display:block; font-size:12.5px; font-weight:500; line-height:1.25}
.slot .mt{
  display:block; font-family:"IBM Plex Mono",monospace; font-size:10.5px;
  color:var(--ink-3); margin-top:2px;
}
.slot .nm.un{font-style:italic}
.slot .mt b{color:var(--ink); font-weight:600}
.slot .ownedby{display:block; font-size:10px; color:var(--ink-3); margin-top:2px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap}
.slot.own{background:var(--surface)}
.posmini{font-family:"IBM Plex Mono",monospace; font-size:10px; color:var(--ink-3); margin-left:6px}
.benchhead{font-family:Archivo,sans-serif; font-size:10px; letter-spacing:.06em; text-transform:uppercase; color:var(--ink-3); margin-bottom:2px}
.inj{
  margin-left:6px; font-family:"IBM Plex Mono",monospace; font-size:9.5px;
  letter-spacing:.04em; text-transform:uppercase; color:var(--down);
  border:1px solid var(--down); border-radius:3px; padding:0 4px; white-space:nowrap;
}

/* lineups */
.lineups{display:grid; grid-template-columns:repeat(auto-fill,minmax(320px,1fr)); gap:14px}
.lineup{
  background:var(--surface); border:1px solid var(--line); border-radius:10px;
  padding:12px 14px 10px; box-shadow:var(--shadow);
}
.lineup.mine{border-color:var(--mine)}
.lineup h3{
  display:flex; align-items:baseline; justify-content:space-between; gap:10px;
  font-family:Archivo,sans-serif; font-size:13px; margin:0 0 8px;
}
.lineup h3 .tot{
  font-family:"IBM Plex Mono",monospace; font-size:11px; color:var(--ink-3); font-weight:400;
}
.chiprow{display:flex; flex-wrap:wrap; gap:6px; margin-bottom:8px}
.chipflag{
  font-size:11px; padding:2px 7px; border-radius:999px; white-space:nowrap;
}
.chipflag.bad{background:var(--surface-2); color:var(--down); border:1px solid var(--down)}
.chipflag.good{background:var(--accent-soft); color:var(--accent-ink); border:1px solid var(--accent)}
.lrow{
  display:grid; grid-template-columns:46px 1fr 56px 50px; gap:8px; align-items:center;
  padding:5px 0; border-top:1px solid var(--line); font-size:12.5px;
}
.lrow:first-of-type{border-top:0}
.lrow.bad .who b{color:var(--down)}
.lrow.good{background:var(--accent-soft); border-radius:5px; padding-inline:6px}
.lrow .slotname{
  font-family:"IBM Plex Mono",monospace; font-size:10px; letter-spacing:.04em;
  color:var(--ink-3); text-transform:uppercase;
}
.lrow .num{
  font-family:"IBM Plex Mono",monospace; font-variant-numeric:tabular-nums; text-align:right;
}
.steals{margin-top:8px; border-top:1px dashed var(--line-strong); padding-top:6px}
.lrow .val{font-family:"IBM Plex Mono",monospace; font-size:11px; color:var(--ink-3); text-align:right}
.lrow .opp,.tfrow .opp{font-family:"IBM Plex Mono",monospace; font-size:10px; color:var(--ink-3); margin-left:6px}
.bpos{font-family:"IBM Plex Mono",monospace; font-size:10px; color:var(--ink-3); margin:6px 0 0}
.setdiff{font-size:11.5px; color:var(--ink-2); margin:0 0 8px}

/* trade finder: who would start where */
.tfblock{margin-bottom:22px}
.tfblock h3{
  font-family:Archivo,sans-serif; font-size:13px; margin:0 0 2px;
  display:flex; align-items:baseline; gap:8px;
}
.tfblock h3 .cnt{font-family:"IBM Plex Mono",monospace; font-size:11px; color:var(--ink-3); font-weight:400}
.tfnote{font-size:12.5px; color:var(--ink-2); margin:0 0 8px; max-width:760px}
.tfscroll{
  overflow-x:auto; background:var(--surface); border:1px solid var(--line);
  border-radius:10px; box-shadow:var(--shadow);
}
.tf{min-width:720px}
.tfrow{
  display:grid; grid-template-columns:minmax(190px,1.5fr) minmax(120px,1fr) 88px 44px minmax(160px,1.3fr) 56px;
  gap:10px; align-items:center; padding:7px 12px; border-top:1px solid var(--line); font-size:12.5px;
}
.tfrow.tfhead{
  border-top:0; font-family:Archivo,sans-serif; font-size:10px; letter-spacing:.06em;
  text-transform:uppercase; color:var(--ink-3); padding-block:9px 5px;
}
.tfrow.grpstart{border-top:1px solid var(--line-strong)}
.tfrow .who b,.tfrow .repl b{font-weight:500}
.tfrow .owner{color:var(--ink-2); overflow:hidden; text-overflow:ellipsis; white-space:nowrap}
.tfrow .num{font-family:"IBM Plex Mono",monospace; font-variant-numeric:tabular-nums; text-align:right}
.tfrow .val{color:var(--ink-3); font-size:11px}
.tfrow .slotname{font-family:"IBM Plex Mono",monospace; font-size:10.5px; color:var(--ink-3)}
.tfrow span:nth-child(3),.tfrow span:nth-child(6){text-align:right}
.tfrow .good,.repl .good{color:var(--accent-ink)}
.tfcards summary{
  cursor:pointer; font-family:Archivo,sans-serif; font-size:13px; font-weight:600;
  padding:8px 0 12px; color:var(--ink-2);
}
.tfcards summary:focus-visible{outline:2px solid var(--accent); outline-offset:2px}
/* on a phone each row stacks: player + rank, owner + value, slot + who he replaces */
@media (max-width:640px){
  .tf{min-width:0}
  .tfrow.tfhead{display:none}
  .tfrow{
    grid-template-columns:40px 1fr auto; row-gap:3px;
    grid-template-areas:"who who rank" "owner owner val" "slot repl repl";
  }
  .tfrow .who{grid-area:who} .tfrow .owner{grid-area:owner}
  .tfrow .num{grid-area:rank} .tfrow .num.val{grid-area:val}
  .tfrow .slotname{grid-area:slot} .tfrow .repl{grid-area:repl}
}

/* week-by-week strip: the window number on top, each week exact below it */
td.tr{vertical-align:top}
.trend{display:flex; flex-direction:column; align-items:flex-end; gap:3px}
.wks{display:inline-grid; grid-auto-flow:column; grid-auto-columns:25px; gap:2px}
.wk{
  font-size:10px; line-height:1.15; text-align:center; border-radius:3px;
  padding:2px 0 1px; background:var(--surface-2); color:var(--ink);
}
.wk i{display:block; font-style:normal; font-size:8.5px; color:var(--ink-3)}
.wk.miss{background:transparent; color:var(--ink-3); outline:1px dashed var(--line)}
.slot .wks{margin-top:4px; grid-auto-columns:23px}
@media (max-width:520px){
  h1{font-size:19px}
  .wrap{padding-inline:12px}
  header.top{margin-inline:-12px; padding-inline:12px}
}
@media (prefers-reduced-motion:reduce){*{transition:none!important; animation:none!important}}
</style>')

body_bits <- paste0(
'<div class="wrap">
  <header class="top">
    <div class="masthead">
      <h1>Waiver Watchlist</h1>
      <div class="provenance" id="prov"></div>
    </div>
    <nav class="tabs" id="tabs" role="tablist" aria-label="Leagues"></nav>
  </header>

  <div class="leaguebar">
    <div class="facts" id="facts"></div>
    <div class="seg views" id="views" role="group" aria-label="View">
      <button type="button" data-view="board" aria-pressed="true">Board</button>
      <button type="button" data-view="nfl" aria-pressed="false">NFL depth</button>
      <button type="button" data-view="league" aria-pressed="false">League depth</button>
      <button type="button" data-view="lineups" aria-pressed="false">Lineups</button>
    </div>
  </div>

  <div class="controls">
    <div class="seg" id="scope" role="group" aria-label="Which players">
      <button type="button" data-scope="compare" aria-pressed="true">My roster + free agents</button>
      <button type="button" data-scope="fa" aria-pressed="false">Free agents</button>
      <button type="button" data-scope="mine" aria-pressed="false">My roster</button>
      <button type="button" data-scope="all" aria-pressed="false">Everyone</button>
      <button type="button" data-scope="keep" aria-pressed="false" hidden>Keeper targets</button>
    </div>
    <div class="chips" id="poschips" role="group" aria-label="Position"></div>
    <input id="q" type="search" placeholder="Search player or team" autocomplete="off">
  </div>

  <div class="tablewrap" id="boardwrap">
    <table id="tbl">
      <thead><tr id="hrow"></tr></thead>
      <tbody id="tbody"></tbody>
    </table>
    <div class="empty" id="empty" hidden>Nothing matches those filters.</div>
  </div>

  <div id="nflwrap" hidden></div>
  <div id="leaguewrap" class="gridwrap" hidden></div>
  <div id="lineupwrap" hidden></div>

  <section class="legend">
    <h2>What the columns mean</h2>
    <dl>
      <div><dt>Signal</dt><dd>Average of the percentile ranks below, taken <em>within position</em> across everyone in the league, rostered and free alike &mdash; so a claim and the man you&rsquo;d cut sort in one list. An opening sort, not a projection.</dd></div>
      <div><dt>The views</dt><dd><b>Board</b> is the sortable table. <b>NFL depth</b> takes one real team and stacks its players by recent snap share, left to right, with each week&rsquo;s snaps underneath &mdash; so a backfield reads as a backfield, and a changing one shows it. <b>League depth</b> lays out every franchise&rsquo;s stack at one position in rest-of-season rank order, with the wire on the bottom row in green.</dd></div>
      <div><dt>mine / unranked</dt><dd><span class="tag-mine">mine</span> is on your roster. <span class="unranked">unranked</span> means FantasyPros lists no rest-of-season rank for him at all &mdash; he is here because his snaps or the Sleeper market say he is playing.</dd></div>
      <div><dt>Rank</dt><dd>FantasyPros rest-of-season consensus, with the week&rsquo;s movement.</dd></div>
      <div><dt>Own%</dt><dd>Share of all leagues that roster him. High and still free here is the classic miss.</dd></div>
      <div><dt>Adds / Drops</dt><dd>Sleeper claims across every league in the window. Moves hours after news, days before rankings. Read them next to Own% &mdash; a player most leagues already roster can only be added in the few where he is free.</dd></div>
      <div><dt>Proj</dt><dd>Sleeper&rsquo;s projection for the upcoming week, in this league&rsquo;s scoring. The only column about next Sunday rather than the rest of the season.</dd></div>
      <div><dt>Wk rank / Flex</dt><dd>FantasyPros consensus for the coming week, scraped live: by position, and on the flex scale (<b>SF</b> in superflex leagues) over just the positions this league&rsquo;s flex slot takes.</dd></div>
      <div><dt>Snap%</dt><dd>Share of his team&rsquo;s offensive snaps. The top number is the recent-window average and is what the column sorts on; under it, each week exactly, shaded against the best week anyone at his position has had. <i>bye</i> is his bye; a dash is a week he did not play. Hover for the whole season.</dd></div>
      <div><dt>xFP/g</dt><dd>Expected fantasy points per game from opportunity alone &mdash; volume without the finishing luck. Same week strip.</dd></div>
      <div><dt>FP/g</dt><dd>Points he actually scored (full PPR, whatever this league scores). Far above xFP/g is production you should not bank on.</dd></div>
      <div><dt>Redraft / Dynasty</dt><dd>FantasyCalc trade-market value at this league&rsquo;s size and format.</dd></div>
      <div><dt>Keeper columns</dt><dd>Keeper leagues only. <b>Drafted</b> is the round he went in this year&rsquo;s draft (<i>K</i> = kept there; <i>UDFA</i> = nobody drafted him). <b>Keep &rsquo;27</b> is what he costs to keep next year: that round minus one, floored at R1, and the round follows him through drops and trades. <i>ADP</i> marks a never-drafted player, priced off next year&rsquo;s ADP &mdash; stood in for here by his dynasty rank &mdash; so claiming him buys no discount. <i>top-2</i> marks rounds 1&ndash;2: only one of those can be kept. <b>Dyn rk</b> is the FantasyCalc superflex dynasty rank at this league&rsquo;s size. <b>Margin</b> is the board rank a pick in his keeper round actually returned in this year&rsquo;s draft, minus his dynasty rank: above zero, he should beat the pick he costs. <b>Keeper targets</b> lists your roster and the wire by margin.</dd></div>
      <div><dt>Lineups</dt><dd>Projected lineups are filled by the weekly rank, not the Sleeper projection &mdash; positional rank for a positional slot, flex rank for a flex one &mdash; skipping anyone out or on bye. &ldquo;Would start&rdquo; means exactly that: add the player to that roster, re-fill the lineup the same way, and see whether he makes it and who drops out, so a QB is only ever weighed against a QB or superflex starter. Ranks show by position (QB18); the flex-scale rank rides along only for a flex slot. The right-hand number is trade value: dynasty in dynasty and keeper leagues, redraft otherwise.</dd></div>
    </dl>
  </section>
</div>

<script id="data" type="application/json">', json, '</script>
<script>
(function(){
  var D = JSON.parse(document.getElementById("data").textContent);
  var meta = D.meta, leagues = D.leagues;
  var state = {league:0, view:"board", scope:"compare", pos:"ALL", team:null,
               q:"", sort:"signal", dir:-1};

  document.getElementById("prov").innerHTML =
    "week <b>" + meta.week + "</b> &middot; " + meta.season +
    " &middot; ROS ranks <b>" + meta.ros_source + " " + meta.ros_date + "</b>" +
    " &middot; usage last <b>" + meta.lookback + "</b>wk &middot; adds <b>" + meta.trend_hours + "</b>h" +
    " &middot; built " + meta.generated;

  var COLS = [
    {k:"player",        h:"Player",  type:"name"},
    {k:"signal",        h:"Signal",  type:"sig"},
    {k:"pos_rank",      h:"Rank",    type:"rank"},
    {k:"proj",          h:"Proj",    type:"num1"},
    {k:"wk_rank",       h:"Wk rank", type:"posrank"},
    {k:"flex_rank",     h:"Flex",    type:"flex"},
    {k:"roster_pct",    h:"Own%",    type:"pct1"},
    {k:"trend_add",     h:"Adds",    type:"count"},
    {k:"trend_drop",    h:"Drops",   type:"count"},
    {k:"snap_pct",      h:"Snap%",   type:"trend", wk:"snap_wk", fmt:"pct"},
    {k:"xfp_g",         h:"xFP/g",   type:"trend", wk:"xfp_wk",  fmt:"num"},
    {k:"fp_g",          h:"FP/g",    type:"trend", wk:"fp_wk",   fmt:"num"},
    {k:"redraft_value", h:"Redraft", type:"int"},
    {k:"dynasty_value", h:"Dynasty", type:"int"}
  ];
  // keeper leagues: the price to keep him next season, right after the name
  var KEEP_COLS = [
    {k:"draft_rd",    h:"Drafted",  type:"drafted"},
    {k:"keep_cost",   h:"Keep &rsquo;27", type:"keepcost"},
    {k:"dyn_rank",    h:"Dyn rk",   type:"int"},
    {k:"keep_margin", h:"Margin",   type:"margin"}
  ];
  function colsFor(L){ return L.keeper ? [COLS[0]].concat(KEEP_COLS, COLS.slice(1)) : COLS; }

  function fmtCount(v){
    if(v===null||v===undefined) return "";
    if(v>=1e6) return (v/1e6).toFixed(1)+"M";
    if(v>=1e3) return Math.round(v/1e3)+"k";
    return String(v);
  }
  function esc(s){ return String(s).replace(/[&<>"]/g, function(c){
    return ({"&":"&amp;","<":"&lt;",">":"&gt;","\\"":"&quot;"})[c]; }); }

  // ---- week-by-week strip ------------------------------------------------
  // the last few played weeks, each with its exact value, tinted against the
  // best week anyone at the position has had in this league
  var WEEKS = [].concat(meta.weeks || []);
  var SHOW_WEEKS = 6;
  var scaleCache = {};
  function wkVec(r, key){
    var v = r[key];
    if(v===null||v===undefined) return [];
    // a one-week season arrives unboxed, and jsonlite writes an NA inside a list
    // column as the string "NA" whatever na= says
    return (Array.isArray(v) ? v : [v]).map(function(x){ return typeof x==="number" ? x : null; });
  }
  function scaleMax(key, pos){
    var id = state.league+"|"+key+"|"+pos;
    if(scaleCache[id]!=null) return scaleCache[id];
    var m = 0;
    leagues[state.league].rows.forEach(function(r){
      if(r.pos!==pos) return;
      wkVec(r, key).forEach(function(x){ if(x!=null && x>m) m = x; });
    });
    return (scaleCache[id] = m || 1);
  }
  function fmtWk(x, fmt){ return fmt==="pct" ? String(Math.round(x*100)) : x.toFixed(1); }
  function weekStrip(r, key, fmt){
    var vec = wkVec(r, key);
    if(!WEEKS.length || !vec.some(function(x){ return x!=null; })) return "";
    var mx = scaleMax(key, r.pos);
    var full = WEEKS.map(function(w, i){
      var x = vec[i];
      return "W"+w+" "+(x==null ? (r.bye===w ? "bye" : "-") : fmtWk(x, fmt));
    }).join("  ");
    var from = Math.max(0, WEEKS.length - SHOW_WEEKS);
    var cells = WEEKS.slice(from).map(function(w, j){
      var x = vec[from + j];
      if(x==null) return "<span class=\\"wk miss\\"><i>"+w+"</i>"+(r.bye===w ? "bye" : "&ndash;")+"</span>";
      var a = Math.round(8 + 37 * Math.max(0, Math.min(1, x / mx)));
      return "<span class=\\"wk\\" style=\\"background:color-mix(in srgb, var(--accent) "+a+
             "%, var(--surface-2))\\"><i>"+w+"</i>"+fmtWk(x, fmt)+"</span>";
    }).join("");
    return "<span class=\\"wks\\" title=\\""+esc(full)+"\\">"+cells+"</span>";
  }

  function cell(row, col){
    var v = row[col.k];
    if(col.type==="trend"){
      var top = (v===null||v===undefined) ? "<span class=\\"dim\\">&mdash;</span>" :
        (col.fmt==="pct" ? String(Math.round(v*100)) : v.toFixed(1));
      return "<td class=\\"tr\\"><span class=\\"trend\\"><span>"+top+"</span>"+
             weekStrip(row, col.wk, col.fmt)+"</span></td>";
    }
    if(col.type==="name"){
      var sub = [row.team||"FA", row.opp||null, row.bye?("bye "+row.bye):null].filter(Boolean).join(" &middot; ");
      if(row.ranked===false) sub += " &middot; <span class=\\"unranked\\">unranked</span>";
      if(row.status!=="free agent" && !row.mine) sub += " &middot; <span class=\\"owner\\">"+esc(row.rostered_by)+"</span>";
      var tag = row.mine ? "<span class=\\"tag-mine\\">mine</span>" : "";
      return "<td class=\\"name\\"><span class=\\"pos\\">"+esc(row.pos)+"</span> "+esc(row.player)+tag+
             "<span class=\\"sub\\">"+sub+"</span></td>";
    }
    if(col.type==="sig"){
      if(v===null||v===undefined) return "<td class=\\"sig\\"><span class=\\"dim\\">&mdash;</span></td>";
      var pc = Math.max(2, Math.round(v*100));
      return "<td class=\\"sig\\"><span class=\\"sigbar\\"><span class=\\"track\\">"+
             "<span class=\\"fill\\" style=\\"width:"+pc+"%\\"></span></span>"+
             "<span class=\\"val\\">"+v.toFixed(2)+"</span></span></td>";
    }
    if(col.type==="drafted"){
      if(v===null||v===undefined) return "<td class=\\"dim\\">UDFA</td>";
      return "<td>R"+v+(row.kept_now ? " <span class=\\"dim\\" title=\\"kept this year\\">K</span>" : "")+"</td>";
    }
    if(v===null||v===undefined||v!==v) return "<td class=\\"dim\\">&mdash;</td>";
    if(col.type==="keepcost"){
      var tags = "";
      if(row.cost_source==="adp") tags += " <span class=\\"dim\\" title=\\"never drafted: priced off next year&rsquo;s ADP\\">ADP</span>";
      if(row.top2) tags += " <span class=\\"delta down\\" title=\\"a round 1-2 pick: only one can be kept\\">top-2</span>";
      return "<td>R"+v+tags+"</td>";
    }
    if(col.type==="margin"){
      var cls = v>0 ? "delta up" : (v<0 ? "delta down" : "dim");
      return "<td title=\\"a R"+row.keep_cost+" pick returned board rank "+row.keep_bar+"\\"><span class=\\""+cls+"\\">"+
        (v>0?"+":"")+v+"</span></td>";
    }
    if(col.type==="rank"){
      var d = row.ecr_delta, ds = "";
      if(d!==null&&d!==undefined&&d!==0){
        ds = " <span class=\\"delta "+(d>0?"up":"down")+"\\">"+(d>0?"&#9650;":"&#9660;")+Math.abs(d)+"</span>";
      }
      return "<td>"+esc(row.pos)+v+ds+"</td>";
    }
    if(col.type==="posrank") return "<td>"+esc(row.pos)+v+"</td>";
    if(col.type==="flex") return "<td>"+esc(leagues[state.league].flex_label||"FLEX")+v+"</td>";
    if(col.type==="pct1") return "<td>"+v.toFixed(1)+"</td>";
    if(col.type==="frac") return "<td>"+Math.round(v*100)+"</td>";
    if(col.type==="num1") return "<td>"+v.toFixed(1)+"</td>";
    if(col.type==="int")  return "<td>"+Math.round(v).toLocaleString()+"</td>";
    if(col.type==="count")return "<td>"+fmtCount(v)+"</td>";
    return "<td>"+esc(v)+"</td>";
  }

  function rows(){
    var L = leagues[state.league], out = L.rows.filter(function(r){
      // the default view: what I could claim, next to what I would cut for him
      if(state.scope==="compare" && r.status!=="free agent" && !r.mine) return false;
      if(state.scope==="fa" && r.status!=="free agent") return false;
      if(state.scope==="mine" && !r.mine) return false;
      if(state.scope==="keep" && ((r.status!=="free agent" && !r.mine) || r.keep_margin==null)) return false;
      if(state.pos!=="ALL" && r.pos!==state.pos) return false;
      if(state.q){
        var hay = (r.player+" "+(r.team||"")).toLowerCase();
        if(hay.indexOf(state.q) === -1) return false;
      }
      return true;
    });
    var k = state.sort, dir = state.dir;
    out.sort(function(a,b){
      var x = a[k], y = b[k];
      if(k==="player"){ return dir * String(x).localeCompare(String(y)); }
      var xn = (x===null||x===undefined||x!==x), yn = (y===null||y===undefined||y!==y);
      if(xn && yn) return 0;
      if(xn) return 1;          // blanks always sink
      if(yn) return -1;
      return dir * (x-y);
    });
    return out;
  }

  // shared cell: a player in a depth slot, shaded by what he projects -------
  // a player in a depth slot. Colour says one thing only - can I have him -
  // and every number carries its own label.
  function slotCell(r, strip, kind){
    if(!r) return "<div class=\\"slot empty-slot\\"></div>";
    var bits = [];
    if(r.wk_rank!=null) bits.push("<b>"+r.pos+r.wk_rank+"</b> wk");
    if(r.proj!=null) bits.push("<b>"+r.proj.toFixed(1)+"</b> proj");
    if(r.snap_pct!=null) bits.push("<b>"+Math.round(r.snap_pct*100)+"%</b> snap");
    var inj = r.injury ? "<span class=\\"inj\\">"+esc(r.injury)+"</span>" : "";
    var own = (kind==="" && r.rostered_by) ?
      "<span class=\\"ownedby\\">"+esc(r.rostered_by)+"</span>" : "";
    return "<div class=\\"slot "+kind+"\\"><span class=\\"nm"+(r.ranked===false?" un":"")+"\\">"+
      esc(r.player)+inj+"</span><span class=\\"mt\\">"+bits.join(" &middot; ")+"</span>"+
      (strip ? weekStrip(r, "snap_wk", "pct") : "")+own+"</div>";
  }

  function kindOf(r){ return !r ? "" : (r.mine ? "mine" : (r.status==="free agent" ? "fa" : "")); }

  // who actually plays, not where the team prints him: recent snap share first,
  // then the depth chart, then the weekly rank for men who have not played
  function depthSort(a,b){
    var as = a.snap_pct==null?-1:a.snap_pct, bs = b.snap_pct==null?-1:b.snap_pct;
    if(as!==bs) return bs-as;
    var ad = a.nfl_depth, bd = b.nfl_depth;
    if(ad!=null && bd!=null && ad!==bd) return ad-bd;
    if(ad!=null && bd==null) return -1;
    if(ad==null && bd!=null) return 1;
    var ar = a.wk_rank==null?9999:a.wk_rank, br = b.wk_rank==null?9999:b.wk_rank;
    return ar-br;
  }

  function gridHead(first, pos, cols, tmpl, label){
    return "<div class=\\"gridrow\\" style=\\""+tmpl+"\\"><span class=\\"gridhead\\">"+first+"</span>"+
      Array.apply(null,{length:cols}).map(function(_,i){
        return "<span class=\\"gridhead\\">"+(label ? label(i) : pos+(i+1))+"</span>"; }).join("")+"</div>";
  }

  // ---- NFL depth: every team at one position, in NFL depth-chart order -----
  function drawNfl(){
    var L = leagues[state.league];
    var pos = state.pos==="ALL" ? "RB" : state.pos;
    var byTeam = {};
    L.rows.forEach(function(r){
      if(r.pos!==pos || !r.team || r.team==="FA") return;
      if(state.q && (r.player+" "+r.team).toLowerCase().indexOf(state.q)===-1) return;
      (byTeam[r.team] = byTeam[r.team] || []).push(r);
    });
    var teams = Object.keys(byTeam).sort();
    var cols = Math.min(5, Math.max.apply(null, teams.map(function(t){
      return byTeam[t].length; }).concat([1])));
    var tmpl = "grid-template-columns:70px repeat("+cols+",minmax(160px,1fr))";
    var body = teams.map(function(t){
      var list = byTeam[t].slice().sort(depthSort);
      return "<div class=\\"gridrow\\" style=\\""+tmpl+"\\">"+
        "<div class=\\"owncell\\">"+esc(t)+"</div>"+
        Array.apply(null,{length:cols}).map(function(_,i){
          return slotCell(list[i], true, kindOf(list[i])); }).join("")+"</div>";
    }).join("");
    var snapLabel = function(i){ return pos+(i+1)+" by snaps"; };
    document.getElementById("nflwrap").innerHTML = teams.length ?
      ("<div class=\\"grid\\">"+gridHead("Team", pos, cols, tmpl, snapLabel)+body+"</div>") :
      "<div class=\\"empty\\">Nothing matches that search.</div>";
  }

  // ---- League depth: each franchise stack at one position ------------------
  function drawLeague(){
    var L = leagues[state.league];
    var pos = state.pos==="ALL" ? "RB" : state.pos;
    // stack order is the rest-of-season consensus, which separates starters far
    // better than the blended signal; unranked men fall to the back
    var byRank = function(a,b){
      var ar = a.pos_rank, br = b.pos_rank;
      if(ar!=null && br!=null && ar!==br) return ar-br;
      if(ar!=null && br==null) return -1;
      if(ar==null && br!=null) return 1;
      return (b.proj==null?-1:b.proj) - (a.proj==null?-1:a.proj);
    };
    var mine = null, teams = {};
    L.rows.forEach(function(r){
      if(r.pos!==pos || r.status!=="rostered") return;
      (teams[r.rostered_by] = teams[r.rostered_by] || []).push(r);
      if(r.mine) mine = r.rostered_by;
    });
    var names = Object.keys(teams).sort(function(a,b){
      if(a===mine) return -1;
      if(b===mine) return 1;
      return teams[b].length - teams[a].length;
    });
    var fas = L.rows.filter(function(r){ return r.pos===pos && r.status==="free agent"; }).sort(byRank);
    var cols = Math.min(8, Math.max.apply(null, names.map(function(n){ return teams[n].length; })
                 .concat([Math.min(fas.length,6), 1])));
    var tmpl = "grid-template-columns:150px repeat("+cols+",minmax(150px,1fr))";
    var body = names.map(function(n){
      var list = teams[n].slice().sort(byRank);
      var isMine = n===mine;
      return "<div class=\\"gridrow\\" style=\\""+tmpl+"\\">"+
        "<div class=\\"owncell"+(isMine?" mine":"")+"\\">"+esc(n)+"</div>"+
        Array.apply(null,{length:cols}).map(function(_,i){
          return slotCell(list[i], 0, isMine?"mine":"own"); }).join("")+"</div>";
    }).join("");
    var faRow = "<div class=\\"gridrow\\" style=\\""+tmpl+"\\">"+
      "<div class=\\"owncell fa\\">Free agents</div>"+
      Array.apply(null,{length:cols}).map(function(_,i){
        return slotCell(fas[i], 0, "fa"); }).join("")+"</div>";
    document.getElementById("leaguewrap").innerHTML =
      "<div class=\\"grid\\">"+gridHead("Franchise", pos, cols, tmpl)+body+faRow+"</div>";
  }

  // ---- Lineups: who would start where, i.e. the trades that fit ----
  var OUT_STATUS = ["Out","IR","Doubtful","Sus","PUP","NA","COV"];
  function isOut(r){ return !!r.injury && OUT_STATUS.indexOf(r.injury)!==-1; }
  function onBye(r){ return r.bye!=null && r.bye===meta.week; }
  function unavailable(r){ return isOut(r) || onBye(r); }
  function isFlexSlot(slot){
    return ["FLEX","SUPER_FLEX","SUPERFLEX","REC_FLEX","WRRB_FLEX","WRRB-FLEX"].indexOf(slot)!==-1;
  }
  function fits(slot, pos){
    if(slot===pos || (slot==="DEF" && pos==="DST")) return true;
    if(slot==="FLEX") return ["RB","WR","TE"].indexOf(pos)!==-1;
    if(slot==="SUPER_FLEX"||slot==="SUPERFLEX") return ["QB","RB","WR","TE"].indexOf(pos)!==-1;
    if(slot==="REC_FLEX") return ["WR","TE"].indexOf(pos)!==-1;
    if(slot==="WRRB_FLEX"||slot==="WRRB-FLEX") return ["WR","RB"].indexOf(pos)!==-1;
    return false;
  }
  // greedy best-available fill: the lineup a manager who was paying attention
  // would set, which is what you are really playing against
  // The FantasyPros weekly rank decides it, not the Sleeper projection: the
  // positional rank for a positional slot, the flex rank for a flex one. A man
  // with no rank only gets in behind every ranked one, on projection.
  function rankFor(r, slot){ return isFlexSlot(slot) ? r.flex_rank : r.wk_rank; }
  function better(slot){
    return function(a, b){
      var ka = rankFor(a, slot), kb = rankFor(b, slot);
      if(ka!=null && kb!=null) return ka-kb;
      if(ka!=null) return -1;
      if(kb!=null) return 1;
      return (b.proj==null?-1:b.proj) - (a.proj==null?-1:a.proj);
    };
  }
  function projectLineup(roster, slots){
    var pool = roster.filter(function(r){
      return !unavailable(r) && (r.wk_rank!=null || r.flex_rank!=null || r.proj!=null); });
    var taken = [], picks = slots.map(function(sl){ return {slot: sl, player: null}; });
    // positional slots first, so a flex never takes the man a slot needs
    var order = slots.map(function(sl, i){ return i; }).sort(function(i, j){
      return (isFlexSlot(slots[i])?1:0) - (isFlexSlot(slots[j])?1:0) || i-j; });
    order.forEach(function(i){
      var sl = slots[i];
      var cands = pool.filter(function(r){ return taken.indexOf(r)===-1 && fits(sl, r.pos); })
                      .sort(better(sl));
      if(cands.length){ picks[i].player = cands[0]; taken.push(cands[0]); }
    });
    return picks;
  }
  // The one test for "would he start there": put him on that roster, re-fill
  // the lineup the same way, and see whether he makes it and who drops out.
  // Slot rules, byes and injuries all come along for free - a quarterback can
  // only ever push out someone in a QB or superflex slot.
  function wouldStart(roster, cand, slots){
    if(unavailable(cand)) return null;
    var before = projectLineup(roster, slots);
    var after = projectLineup(roster.concat([cand]), slots);
    var hit = null, inAfter = [];
    after.forEach(function(p){
      if(!p.player) return;
      inAfter.push(p.player);
      if(p.player===cand) hit = p;
    });
    if(!hit) return null;
    var out = null;
    before.forEach(function(p){
      if(p.player && !out && inAfter.indexOf(p.player)===-1) out = p;
    });
    // out.slot is where the dropped man was starting; null means an empty slot
    return {slot: hit.slot, displaced: out ? out.player : null, from: out ? out.slot : null};
  }
  function slotShort(slot){
    return ({SUPER_FLEX:"SF", SUPERFLEX:"SF", REC_FLEX:"W/T", WRRB_FLEX:"W/R", "WRRB-FLEX":"W/R"})[slot] || slot;
  }
  // always the positional weekly rank first, so a QB is only ever compared as
  // a QB; the flex-scale rank rides along only when the slot is a flex one
  function rankText(r, slot){
    var L = leagues[state.league];
    var s = r.wk_rank!=null ? r.pos+r.wk_rank :
            (r.proj!=null ? r.proj.toFixed(1)+"p" : "&mdash;");
    if(slot && isFlexSlot(slot) && r.flex_rank!=null)
      s += " <span class=\\"dim\\">"+(L.flex_label||"FLEX")+r.flex_rank+"</span>";
    return s;
  }
  function whoText(r){
    var bits = "<b>"+esc(r.player)+"</b><span class=\\"posmini\\">"+esc(r.pos)+
      (r.team?" "+esc(r.team):"")+"</span>";
    if(r.opp) bits += "<span class=\\"opp\\">"+esc(r.opp)+"</span>";
    if(isOut(r)) bits += "<span class=\\"inj\\">"+esc(r.injury)+"</span>";
    else if(onBye(r)) bits += "<span class=\\"inj\\">BYE</span>";
    return bits;
  }
  function fmtVal(v){
    if(v==null) return "&mdash;";
    return v>=1000 ? (v/1000).toFixed(1)+"k" : String(Math.round(v));
  }
  function byWeekRank(a, b){
    var ka = a.wk_rank!=null ? a.wk_rank : 9999, kb = b.wk_rank!=null ? b.wk_rank : 9999;
    return ka-kb || (b.proj==null?-1:b.proj) - (a.proj==null?-1:a.proj);
  }
  var POS_ORDER = ["QB","RB","WR","TE","K","DST"];
  function posIdx(p){ var i = POS_ORDER.indexOf(p); return i===-1 ? 99 : i; }
  function replacesText(res){
    if(!res.displaced) return "<span class=\\"good\\">fills an empty "+esc(slotShort(res.slot))+"</span>";
    return "<b>"+esc(res.displaced.player)+"</b> <span class=\\"dim\\">"+
      rankText(res.displaced, res.from)+"</span>";
  }

  function drawLineups(){
    var L = leagues[state.league];
    var slots = L.slots || [];
    var vk = L.value_key || "redraft_value";
    var vname = vk==="dynasty_value" ? "dynasty" : "redraft";
    var teams = {};
    L.rows.forEach(function(r){
      if(r.status!=="rostered" || !r.rostered_by) return;
      (teams[r.rostered_by] = teams[r.rostered_by] || []).push(r);
    });
    var myName = null;
    Object.keys(teams).forEach(function(n){
      if(teams[n].some(function(r){return r.mine;})) myName = n;
    });
    var names = Object.keys(teams).sort(function(a,b){
      if(a===myName) return -1;
      if(b===myName) return 1;
      return a.localeCompare(b);
    });
    var posOk = function(r){ return state.pos==="ALL" || r.pos===state.pos; };

    // every team: its projected lineup, and the bench is everyone not in it
    var likely = {}, bench = {};
    names.forEach(function(n){
      likely[n] = projectLineup(teams[n], slots);
      var inIt = likely[n].map(function(p){ return p.player; });
      bench[n] = teams[n].filter(function(r){ return inIt.indexOf(r)===-1; });
    });

    // A: their bench men who would start for me
    var forMe = [];
    // B: my bench men who would start for them
    var forThem = [];
    if(myName){
      names.forEach(function(n){
        if(n===myName) return;
        bench[n].forEach(function(r){
          if(!posOk(r)) return;
          var res = wouldStart(teams[myName], r, slots);
          if(res) forMe.push({r: r, owner: n, res: res});
        });
      });
      bench[myName].forEach(function(r){
        if(!posOk(r)) return;
        names.forEach(function(n){
          if(n===myName) return;
          var res = wouldStart(teams[n], r, slots);
          if(res) forThem.push({r: r, owner: n, res: res});
        });
      });
    }
    var byPosRank = function(a, b){
      return posIdx(a.r.pos)-posIdx(b.r.pos) || byWeekRank(a.r, b.r) || a.owner.localeCompare(b.owner);
    };
    forMe.sort(byPosRank);
    forThem.sort(byPosRank);
    var starterFor = {};
    forMe.forEach(function(x){ starterFor[x.r.player+"|"+x.owner] = x.res; });

    var posName = state.pos==="ALL" ? "" : state.pos+" ";
    function tfHead(cols){
      return "<div class=\\"tfrow tfhead\\">"+cols.map(function(c){
        return "<span>"+c+"</span>"; }).join("")+"</div>";
    }
    var aHtml = "<section class=\\"tfblock\\"><h3>Their bench, would start for you"+
      "<span class=\\"cnt\\">"+forMe.length+"</span></h3>"+
      "<p class=\\"tfnote\\">Each "+posName+"player sitting outside another team&rsquo;s projected lineup "+
      "who would make <em>your</em> projected lineup this week, and the man of yours he would push out.</p>"+
      (forMe.length ? "<div class=\\"tfscroll\\"><div class=\\"tf\\">"+
        tfHead(["Player","Owner","Wk rank","Slot","Replaces your",vname])+
        forMe.map(function(x){
          return "<div class=\\"tfrow\\"><span class=\\"who\\">"+whoText(x.r)+"</span>"+
            "<span class=\\"owner\\">"+esc(x.owner)+"</span>"+
            "<span class=\\"num\\">"+rankText(x.r, x.res.slot)+"</span>"+
            "<span class=\\"slotname\\">"+esc(slotShort(x.res.slot))+"</span>"+
            "<span class=\\"repl\\">"+replacesText(x.res)+"</span>"+
            "<span class=\\"num val\\">"+fmtVal(x.r[vk])+"</span></div>";
        }).join("")+"</div></div>"
      : "<div class=\\"empty\\">"+(myName ? "No "+posName+"bench player elsewhere would start for you this week." :
          "Your team was not found in this league.")+"</div>")+
      "</section>";

    var bHtml = "<section class=\\"tfblock\\"><h3>Your bench, would start for them"+
      "<span class=\\"cnt\\">"+forThem.length+"</span></h3>"+
      "<p class=\\"tfnote\\">Each "+posName+"player on your bench who would make another team&rsquo;s projected lineup "+
      "this week, and who he would start over &mdash; a thin QB room in superflex, a bye, an injury.</p>"+
      (forThem.length ? "<div class=\\"tfscroll\\"><div class=\\"tf\\">"+
        tfHead(["Your player","Team","Wk rank","Slot","Starts over their",vname])+
        forThem.map(function(x, i){
          var first = i===0 || forThem[i-1].r!==x.r;
          return "<div class=\\"tfrow"+(first?" grpstart":"")+"\\">"+
            "<span class=\\"who\\">"+(first ? whoText(x.r) : "<span class=\\"dim\\">&#8627;</span>")+"</span>"+
            "<span class=\\"owner\\">"+esc(x.owner)+"</span>"+
            "<span class=\\"num\\">"+(first ? rankText(x.r, x.res.slot) : "")+"</span>"+
            "<span class=\\"slotname\\">"+esc(slotShort(x.res.slot))+"</span>"+
            "<span class=\\"repl\\">"+replacesText(x.res)+"</span>"+
            "<span class=\\"num val\\">"+(first ? fmtVal(x.r[vk]) : "")+"</span></div>";
        }).join("")+"</div></div>"
      : "<div class=\\"empty\\">"+(myName ? "None of your "+posName+"bench would start anywhere else this week." :
          "Your team was not found in this league.")+"</div>")+
      "</section>";

    // team cards: the projected lineup, then the whole bench by position
    var cards = names.map(function(n){
      var roster = teams[n];
      var isMine = n===myName;
      var set = roster.filter(function(r){ return r.starting; });
      var proj = likely[n];
      var projIds = proj.map(function(p){ return p.player; });
      var benchedSet = set.filter(function(r){ return projIds.indexOf(r)===-1; });
      var promoted = proj.filter(function(p){ return p.player && set.indexOf(p.player)===-1; })
                         .map(function(p){ return p.player; });
      var broken = set.filter(unavailable);
      var nFor = isMine ? 0 : forMe.filter(function(x){ return x.owner===n; }).length;

      var chips = "";
      if(!set.length) chips += "<span class=\\"chipflag bad\\">no lineup set</span>";
      if(broken.length) chips += "<span class=\\"chipflag bad\\">"+broken.length+
        " starter"+(broken.length>1?"s":"")+" out or on bye</span>";
      if(nFor) chips += "<span class=\\"chipflag good\\">"+nFor+" benched who would start for you</span>";

      var diff = (set.length && (benchedSet.length || promoted.length)) ?
        "<p class=\\"setdiff\\">Set lineup differs: starts "+
          (benchedSet.length ? benchedSet.map(function(r){ return "<b>"+esc(r.player)+"</b>"; }).join(", ") : "nobody extra")+
          "; the projection would start "+
          (promoted.length ? promoted.map(function(r){ return "<b>"+esc(r.player)+"</b>"; }).join(", ") : "nobody else")+
          " instead.</p>" : "";

      var lines = proj.map(function(p){
        if(!p.player) return "<div class=\\"lrow\\"><span class=\\"slotname\\">"+esc(slotShort(p.slot))+
          "</span><span class=\\"who dim\\">empty</span><span class=\\"num\\">&mdash;</span>"+
          "<span class=\\"val\\"></span></div>";
        var r = p.player;
        return "<div class=\\"lrow\\">"+
          "<span class=\\"slotname\\">"+esc(slotShort(p.slot))+"</span>"+
          "<span class=\\"who\\">"+whoText(r)+"</span>"+
          "<span class=\\"num\\">"+rankText(r, p.slot)+"</span>"+
          "<span class=\\"val\\">"+fmtVal(r[vk])+"</span></div>";
      }).join("");

      var bn = bench[n].slice().sort(function(a,b){
        return posIdx(a.pos)-posIdx(b.pos) || byWeekRank(a, b); });
      var lastPos = null;
      var benchLines = bn.length ? ("<div class=\\"steals\\"><div class=\\"benchhead\\">Bench</div>"+
        bn.map(function(r){
          var head = r.pos!==lastPos ? "<div class=\\"bpos\\">"+esc(r.pos)+"</div>" : "";
          lastPos = r.pos;
          var res = isMine ? null : starterFor[r.player+"|"+n];
          var tag = res ? "<span class=\\"tag fa\\">starts for you over "+
            (res.displaced ? esc(res.displaced.player) : "empty "+esc(slotShort(res.slot)))+"</span>" : "";
          return head+"<div class=\\"lrow"+(res?" good":"")+"\\">"+
            "<span class=\\"slotname\\">BN</span><span class=\\"who\\">"+whoText(r)+tag+"</span>"+
            "<span class=\\"num\\">"+rankText(r)+"</span>"+
            "<span class=\\"val\\">"+fmtVal(r[vk])+"</span></div>";
        }).join("")+"</div>") : "";

      // what the starters would cost to trade for, on this league&rsquo;s value scale
      var total = proj.reduce(function(s,p){ return s + (p.player&&p.player[vk]||0); }, 0);
      return "<div class=\\"lineup"+(isMine?" mine":"")+"\\">"+
        "<h3>"+esc(n)+"<span class=\\"tot\\">starters "+fmtVal(total)+" "+vname+"</span></h3>"+
        (chips?("<div class=\\"chiprow\\">"+chips+"</div>"):"")+
        diff+lines+benchLines+"</div>";
    }).join("");

    var wrap = document.getElementById("lineupwrap");
    wrap.innerHTML = names.length ?
      aHtml+bHtml+"<details class=\\"tfcards\\""+(state.cardsOpen?" open":"")+">"+
        "<summary>Every team&rsquo;s projected lineup and full bench</summary>"+
        "<div class=\\"lineups\\">"+cards+"</div></details>" :
      "<div class=\\"empty\\">No rosters for this league.</div>";
    var det = wrap.querySelector("details");
    if(det) det.addEventListener("toggle", function(){ state.cardsOpen = det.open; });
  }

  function draw(){
    var L = leagues[state.league];
    // the keeper scope only exists on the tab of a keeper league
    var kb = document.querySelector("#scope button[data-scope=keep]");
    kb.hidden = !L.keeper;
    if(!L.keeper && state.scope==="keep"){
      state.scope = "compare"; state.sort = "signal"; state.dir = -1;
      [].forEach.call(document.getElementById("scope").children, function(c){
        c.setAttribute("aria-pressed", c.dataset.scope==="compare"); });
    }
    if(!L.keeper && ["draft_rd","keep_cost","dyn_rank","keep_margin"].indexOf(state.sort)!==-1){
      state.sort = "signal"; state.dir = -1;
    }
    // controls that only make sense in one view
    document.getElementById("scope").hidden = state.view!=="board";
    document.getElementById("q").hidden = (state.view==="league" || state.view==="lineups");
    document.getElementById("boardwrap").hidden = state.view!=="board";
    document.getElementById("nflwrap").hidden = state.view!=="nfl";
    document.getElementById("leaguewrap").hidden = state.view!=="league";
    document.getElementById("lineupwrap").hidden = state.view!=="lineups";
    if(state.view==="nfl"){ drawNfl(); }
    if(state.view==="league"){ drawLeague(); }
    if(state.view==="lineups"){ drawLineups(); }
    if(state.view!=="board"){ drawFacts(); return; }
    drawBoard();
  }

  function drawFacts(){
    var L = leagues[state.league];
    document.getElementById("facts").innerHTML =
      "<b>"+esc(L.my_team||"&mdash;")+"</b> <span class=\\"dot\\">/</span> " +
      esc(L.type)+" <span class=\\"dot\\">&middot;</span> "+esc(L.qb)+
      " <span class=\\"dot\\">&middot;</span> "+L.ppr+" ppr"+
      " <span class=\\"dot\\">&middot;</span> "+L.teams+" teams"+
      " <span class=\\"dot\\">&middot;</span> <b>"+L.n_fa+"</b> free agents";
  }

  function drawBoard(){
    var L = leagues[state.league];
    document.getElementById("facts").innerHTML =
      "<b>"+esc(L.my_team||"&mdash;")+"</b> <span class=\\"dot\\">/</span> " +
      esc(L.type)+" <span class=\\"dot\\">&middot;</span> "+esc(L.qb)+
      " <span class=\\"dot\\">&middot;</span> "+L.ppr+" ppr"+
      " <span class=\\"dot\\">&middot;</span> "+L.teams+" teams"+
      " <span class=\\"dot\\">&middot;</span> <b>"+L.n_fa+"</b> ranked free agents";

    var CS = colsFor(L);
    document.getElementById("hrow").innerHTML = CS.map(function(c){
      var on = state.sort===c.k;
      var arw = on ? "<span class=\\"arw\\">"+(state.dir===1?"&#9650;":"&#9660;")+"</span>" : "";
      return "<th"+(on?" aria-sort=\\""+(state.dir===1?"ascending":"descending")+"\\"":"")+
             "><button type=\\"button\\" data-k=\\""+c.k+"\\">"+
             (c.k==="flex_rank" ? esc(L.flex_label||"Flex") : c.h)+arw+"</button></th>";
    }).join("");

    var rs = rows();
    document.getElementById("tbody").innerHTML = rs.map(function(r){
      return "<tr"+(r.mine?" class=\\"is-mine\\"":"")+">"+
        CS.map(function(c){return cell(r,c);}).join("")+"</tr>";
    }).join("");
    document.getElementById("empty").hidden = rs.length > 0;
  }

  var tabs = document.getElementById("tabs");
  tabs.innerHTML = leagues.map(function(l,i){
    return "<button type=\\"button\\" class=\\"tab\\" role=\\"tab\\" data-i=\\""+i+"\\" aria-selected=\\""+
      (i===0)+"\\">"+esc(l.name)+"<span class=\\"cnt\\">"+l.n_fa+"</span></button>";
  }).join("");
  tabs.addEventListener("click", function(e){
    var b = e.target.closest(".tab"); if(!b) return;
    state.league = +b.dataset.i;
    [].forEach.call(tabs.children, function(t){ t.setAttribute("aria-selected", t===b); });
    buildChips();
    draw();
  });

  // chips come from the league on screen, so a kicker chip only shows up in the
  // league that starts one
  var POS_ORDER = ["QB","RB","WR","TE","K","DST"];
  var chips = document.getElementById("poschips");
  function buildChips(){
    var have = [];
    leagues[state.league].rows.forEach(function(r){ if(have.indexOf(r.pos)===-1) have.push(r.pos); });
    have.sort(function(a,b){ return POS_ORDER.indexOf(a) - POS_ORDER.indexOf(b); });
    if(state.pos!=="ALL" && have.indexOf(state.pos)===-1) state.pos = "ALL";
    chips.innerHTML = ["ALL"].concat(have).map(function(p){
      return "<button type=\\"button\\" class=\\"chip\\" data-pos=\\""+p+"\\" aria-pressed=\\""+(p===state.pos)+"\\">"+p+"</button>";
    }).join("");
  }
  buildChips();
  chips.addEventListener("click", function(e){
    var b = e.target.closest(".chip"); if(!b) return;
    state.pos = b.dataset.pos;
    [].forEach.call(chips.children, function(c){ c.setAttribute("aria-pressed", c===b); });
    draw();
  });


  document.getElementById("views").addEventListener("click", function(e){
    var b = e.target.closest("button[data-view]"); if(!b) return;
    state.view = b.dataset.view;
    [].forEach.call(this.children, function(c){ c.setAttribute("aria-pressed", c===b); });
    draw();
  });

  document.getElementById("scope").addEventListener("click", function(e){
    var b = e.target.closest("button[data-scope]"); if(!b) return;
    state.scope = b.dataset.scope;
    [].forEach.call(this.children, function(c){ c.setAttribute("aria-pressed", c===b); });
    if(state.scope==="keep"){ state.sort = "keep_margin"; state.dir = -1; }
    draw();
  });

  document.getElementById("hrow").addEventListener("click", function(e){
    var b = e.target.closest("button[data-k]"); if(!b) return;
    var k = b.dataset.k;
    if(state.sort===k){ state.dir = -state.dir; }
    else {
      state.sort = k;
      // a name and a rank read best ascending; every other column is a bigger-is-better number
      state.dir = (k==="player" || k==="pos_rank" || k==="wk_rank" || k==="flex_rank" ||
                   k==="draft_rd" || k==="keep_cost" || k==="dyn_rank") ? 1 : -1;
    }
    draw();
  });

  var q = document.getElementById("q");
  q.addEventListener("input", function(){ state.q = q.value.trim().toLowerCase(); draw(); });

  draw();
})();
</script>')

writeLines(paste0(head_bits, "\n", body_bits),
           file.path(out, "waiver_watchlist_artifact.html"), useBytes = TRUE)
writeLines(paste0(
  '<!doctype html>\n<html lang="en">\n<head>\n<meta charset="utf-8">\n',
  '<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">\n',
  head_bits, "\n</head>\n<body>\n", body_bits, "\n</body>\n</html>"),
  file.path(out, "waiver_watchlist.html"), useBytes = TRUE)

cat("\nbuilt", length(leagues), "leagues\n")
for (l in leagues) {
  d <- as.data.table(l$rows)[status == "free agent"]
  data.table::setorderv(d, "signal", -1L)
  cat("\n==", l$name, "-", l$my_team, "==\n")
  print(head(d[, .(player, pos, signal, rank = paste0(pos, pos_rank),
                   rost = roster_pct, adds = trend_add, snap = snap_pct,
                   xfp = xfp_g, redraft = redraft_value)], 8))
}
cat("\nwrote ", out, "\n", sep = "")
