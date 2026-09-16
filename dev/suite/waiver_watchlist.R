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
  # every league sox05syd is in (ffscrapr::sleeper_userleagues("sox05syd", 2026))
  league_ids = strsplit(Sys.getenv("FFS_LEAGUE_ID", paste(
    "1312145437842497536", "1326464763936403456", "1359546500786434048",
    "1339817277222031360", "1389724668889399296", "1403227684191870976",
    sep = ",")), ",")[[1]],
  season   = as.integer(Sys.getenv("FFS_SEASON", "2026")),
  my_team  = Sys.getenv("FFS_MY_TEAM", "sox05syd"),
  pos_filter = strsplit(Sys.getenv("FFS_POS_FILTER", "QB,RB,WR,TE"), ",")[[1]],
  lookback = as.integer(Sys.getenv("FFS_LOOKBACK", "3")),
  trend_hours = as.integer(Sys.getenv("FFS_TREND_HOURS", "24"))
)

day <- format(Sys.Date())
out <- here::here("dev", "league_sims", "waivers", day)
dir.create(out, recursive = TRUE, showWarnings = FALSE)

## ---- SHARED SIGNALS (fetched once, reused by every league) -------------------
message("usage signals (snaps + expected points) ...")
usage <- tryCatch(ffs_usage_signals(season = config$season, lookback = config$lookback),
                  error = function(e) {message("  unavailable: ", conditionMessage(e)); NULL})

message("sleeper trending (", config$trend_hours, "h) ...")
trending <- list(
  add = tryCatch(ffs_sleeper_trending("add", config$trend_hours, 200L),
                 error = function(e) NULL),
  drop = tryCatch(ffs_sleeper_trending("drop", config$trend_hours, 200L),
                  error = function(e) NULL))

week <- as.integer(nflreadr::get_current_week())
message("sleeper projections for week ", week, " ...")
projections <- tryCatch(
  ffs_sleeper_projections(season = config$season, week = week,
                          pos_filter = config$pos_filter),
  error = function(e) {message("  unavailable: ", conditionMessage(e)); NULL})

message("fantasypros weekly rankings ...")
weekly <- tryCatch(ffs_latest_rankings(type = "week"),
                   error = function(e) {message("  unavailable: ", conditionMessage(e)); NULL})

message("sleeper player file (injuries + NFL depth chart) ...")
sleeper_players <- tryCatch(ffs_sleeper_players(pos_filter = config$pos_filter),
                            error = function(e) {
                              message("  unavailable: ", conditionMessage(e)); NULL})

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
  ltype <- c("redraft", "keeper", "dynasty")[as.integer(raw$settings$type) + 1L]
  if (!length(ltype) || is.na(ltype)) ltype <- "redraft"
  cli::cli_rule("{lname} - {ltype}{
    if (superflex) ' superflex' else ' 1QB'}, {ppr} ppr, {teams} teams")

  who <- as.data.table(ffscrapr::ff_franchises(conn))
  keys <- intersect(c("user_name", "franchise_name"), names(who))
  hit <- who[Reduce(`|`, lapply(keys, function(k)
    grepl(config$my_team, who[[k]], ignore.case = TRUE)))]
  my_id <- if (nrow(hit) == 1) as.character(hit$franchise_id[[1]]) else NULL
  my_name <- if (nrow(hit) == 1) hit$franchise_name[[1]] else NA_character_

  w <- as.data.table(ffs_waiver_watchlist(
    conn,
    ros = ros_for(ppr),
    usage = usage,
    values = fc_for(if (superflex) 2 else 1, teams, ppr),
    trending = trending,
    projections = projections,
    weekly = weekly,
    sleeper_players = sleeper_players,
    lineups = tryCatch(ffs_sleeper_lineups(league_id, week = week),
                       error = function(e) NULL),
    ppr = ppr,
    pos_filter = config$pos_filter,
    my_franchise = my_id))

  list(
    id = league_id,
    name = lname,
    slots = slots[!slots %in% c("BN", "IR", "TAXI")],
    type = ltype,
    qb = if (superflex) "superflex" else "1QB",
    ppr = ppr,
    teams = teams,
    my_team = my_name,
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

# one table for every league, for your own slicing
fwrite(rbindlist(lapply(leagues, function(l)
  cbind(league = l$name, as.data.table(l$rows))), fill = TRUE),
  file.path(out, "watchlist.csv"))

## ---- PAGE --------------------------------------------------------------------
# columns the page shows, in order; everything else stays in the CSV
show <- c("player", "pos", "team", "bye", "status", "rostered_by", "mine", "ranked", "signal",
          "pos_rank", "ecr", "ecr_delta", "roster_pct", "trend_add", "trend_drop",
          "proj", "wk_rank", "injury", "starting", "slot", "slot_no", "nfl_depth",
          "snap_pct", "xfp_g", "fp_g", "redraft_value", "dynasty_value")

payload <- lapply(leagues, function(l) {
  d <- as.data.table(l$rows)
  for (cc in setdiff(show, names(d))) d[, (cc) := NA]
  d <- d[, show, with = FALSE]
  # jsonlite writes NA as null, which the page treats as "no data"
  list(id = l$id, name = l$name, type = l$type, qb = l$qb, ppr = l$ppr,
       slots = l$slots,
       teams = l$teams, my_team = l$my_team,
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
  lookback = config$lookback
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
.likely{display:block; font-size:10.5px; color:var(--accent-ink); margin-top:1px}
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
  display:grid; grid-template-columns:46px 1fr 48px; gap:8px; align-items:center;
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
  <div id="lineupwrap" class="lineups" hidden></div>

  <section class="legend">
    <h2>What the columns mean</h2>
    <dl>
      <div><dt>Signal</dt><dd>Average of the percentile ranks below, taken <em>within position</em> across everyone in the league, rostered and free alike &mdash; so a claim and the man you&rsquo;d cut sort in one list. An opening sort, not a projection.</dd></div>
      <div><dt>The three views</dt><dd><b>Board</b> is the sortable table. <b>NFL depth</b> takes one real team and stacks its players by snap share, so a backfield reads as a backfield. <b>League depth</b> lays out every franchise&rsquo;s stack at one position in rest-of-season rank order, with the wire on the bottom row in green.</dd></div>
      <div><dt>mine / unranked</dt><dd><span class="tag-mine">mine</span> is on your roster. <span class="unranked">unranked</span> means FantasyPros lists no rest-of-season rank for him at all &mdash; he is here because his snaps or the Sleeper market say he is playing.</dd></div>
      <div><dt>Rank</dt><dd>FantasyPros rest-of-season consensus, with the week&rsquo;s movement.</dd></div>
      <div><dt>Own%</dt><dd>Share of all leagues that roster him. High and still free here is the classic miss.</dd></div>
      <div><dt>Adds / Drops</dt><dd>Sleeper claims across every league in the window. Moves hours after news, days before rankings. Read them next to Own% &mdash; a player most leagues already roster can only be added in the few where he is free.</dd></div>
      <div><dt>Proj</dt><dd>Sleeper&rsquo;s projection for the upcoming week, in this league&rsquo;s scoring. The only column about next Sunday rather than the rest of the season.</dd></div>
      <div><dt>Snap%</dt><dd>Share of his team&rsquo;s offensive snaps, recent window.</dd></div>
      <div><dt>xFP/g</dt><dd>Expected fantasy points per game from opportunity alone &mdash; volume without the finishing luck.</dd></div>
      <div><dt>FP/g</dt><dd>Points he actually scored. Far above xFP/g is production you should not bank on.</dd></div>
      <div><dt>Redraft / Dynasty</dt><dd>FantasyCalc trade-market value at this league&rsquo;s size and format.</dd></div>
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
    {k:"roster_pct",    h:"Own%",    type:"pct1"},
    {k:"trend_add",     h:"Adds",    type:"count"},
    {k:"trend_drop",    h:"Drops",   type:"count"},
    {k:"snap_pct",      h:"Snap%",   type:"frac"},
    {k:"xfp_g",         h:"xFP/g",   type:"num1"},
    {k:"fp_g",          h:"FP/g",    type:"num1"},
    {k:"redraft_value", h:"Redraft", type:"int"},
    {k:"dynasty_value", h:"Dynasty", type:"int"}
  ];

  function fmtCount(v){
    if(v===null||v===undefined) return "";
    if(v>=1e6) return (v/1e6).toFixed(1)+"M";
    if(v>=1e3) return Math.round(v/1e3)+"k";
    return String(v);
  }
  function esc(s){ return String(s).replace(/[&<>"]/g, function(c){
    return ({"&":"&amp;","<":"&lt;",">":"&gt;","\\"":"&quot;"})[c]; }); }

  function cell(row, col){
    var v = row[col.k];
    if(col.type==="name"){
      var sub = [row.team||"FA", row.bye?("bye "+row.bye):null].filter(Boolean).join(" &middot; ");
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
    if(v===null||v===undefined||v!==v) return "<td class=\\"dim\\">&mdash;</td>";
    if(col.type==="rank"){
      var d = row.ecr_delta, ds = "";
      if(d!==null&&d!==undefined&&d!==0){
        ds = " <span class=\\"delta "+(d>0?"up":"down")+"\\">"+(d>0?"&#9650;":"&#9660;")+Math.abs(d)+"</span>";
      }
      return "<td>"+esc(row.pos)+v+ds+"</td>";
    }
    if(col.type==="posrank") return "<td>"+esc(row.pos)+v+"</td>";
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
  function slotCell(r, _unused, kind){
    if(!r) return "<div class=\\"slot empty-slot\\"></div>";
    var bits = [];
    if(r.proj!=null) bits.push("<b>"+r.proj.toFixed(1)+"</b> proj");
    if(r.wk_rank!=null) bits.push("<b>"+r.pos+r.wk_rank+"</b> wk");
    if(r.snap_pct!=null) bits.push("<b>"+Math.round(r.snap_pct*100)+"%</b> snap");
    var inj = r.injury ? "<span class=\\"inj\\">"+esc(r.injury)+"</span>" : "";
    var own = (kind==="" && r.rostered_by) ?
      "<span class=\\"ownedby\\">"+esc(r.rostered_by)+"</span>" : "";
    return "<div class=\\"slot "+kind+"\\"><span class=\\"nm"+(r.ranked===false?" un":"")+"\\">"+
      esc(r.player)+inj+"</span><span class=\\"mt\\">"+bits.join(" &middot; ")+"</span>"+own+"</div>";
  }

  function kindOf(r){ return !r ? "" : (r.mine ? "mine" : (r.status==="free agent" ? "fa" : "")); }

  function depthSort(a,b){
    var ad = a.nfl_depth, bd = b.nfl_depth;
    if(ad!=null && bd!=null && ad!==bd) return ad-bd;
    if(ad!=null && bd==null) return -1;
    if(ad==null && bd!=null) return 1;
    var as = a.snap_pct==null?-1:a.snap_pct, bs = b.snap_pct==null?-1:b.snap_pct;
    if(as!==bs) return bs-as;
    return (b.proj==null?-1:b.proj) - (a.proj==null?-1:a.proj);
  }

  function gridHead(first, pos, cols, tmpl){
    return "<div class=\\"gridrow\\" style=\\""+tmpl+"\\"><span class=\\"gridhead\\">"+first+"</span>"+
      Array.apply(null,{length:cols}).map(function(_,i){
        return "<span class=\\"gridhead\\">"+pos+(i+1)+"</span>"; }).join("")+"</div>";
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
    var tmpl = "grid-template-columns:70px repeat("+cols+",minmax(150px,1fr))";
    var body = teams.map(function(t){
      var list = byTeam[t].slice().sort(depthSort);
      return "<div class=\\"gridrow\\" style=\\""+tmpl+"\\">"+
        "<div class=\\"owncell\\">"+esc(t)+"</div>"+
        Array.apply(null,{length:cols}).map(function(_,i){
          return slotCell(list[i], 0, kindOf(list[i])); }).join("")+"</div>";
    }).join("");
    document.getElementById("nflwrap").innerHTML = teams.length ?
      ("<div class=\\"grid\\">"+gridHead("Team", pos, cols, tmpl)+body+"</div>") :
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

  // ---- Lineups: what is set, what they will probably set, and the bench ----
  var OUT_STATUS = ["Out","IR","Doubtful","Sus","PUP","NA","COV"];
  function isOut(r){ return !!r.injury && OUT_STATUS.indexOf(r.injury)!==-1; }
  function onBye(r){ return r.bye!=null && r.bye===meta.week; }
  function unavailable(r){ return isOut(r) || onBye(r); }
  function fits(slot, pos){
    if(slot===pos) return true;
    if(slot==="FLEX") return ["RB","WR","TE"].indexOf(pos)!==-1;
    if(slot==="SUPER_FLEX"||slot==="SUPERFLEX") return ["QB","RB","WR","TE"].indexOf(pos)!==-1;
    if(slot==="REC_FLEX") return ["WR","TE"].indexOf(pos)!==-1;
    if(slot==="WRRB_FLEX"||slot==="WRRB-FLEX") return ["WR","RB"].indexOf(pos)!==-1;
    return false;
  }
  // greedy best-available fill: the lineup a manager who was paying attention
  // would set, which is what you are really playing against
  function projectLineup(roster, slots){
    var pool = roster.filter(function(r){ return !unavailable(r) && r.proj!=null; })
                     .sort(function(a,b){ return b.proj-a.proj; });
    var taken = {}, picks = [];
    slots.forEach(function(sl){
      var pick = null;
      for(var i=0;i<pool.length;i++){
        var r = pool[i];
        if(taken[r.player] || !fits(sl, r.pos)) continue;
        pick = r; taken[r.player] = true; break;
      }
      picks.push({slot: sl, player: pick});
    });
    return picks;
  }

  function drawLineups(){
    var L = leagues[state.league];
    var slots = L.slots || [];
    var teams = {};
    L.rows.forEach(function(r){
      if(r.status!=="rostered" || !r.rostered_by) return;
      (teams[r.rostered_by] = teams[r.rostered_by] || []).push(r);
    });
    var myName = null;
    Object.keys(teams).forEach(function(n){
      if(teams[n].some(function(r){return r.mine;})) myName = n;
    });
    // the weakest starter in MY likely lineup at each position: anything on
    // another bench that clears it is a player who would start for me
    var myFloor = {};
    if(myName){
      projectLineup(teams[myName], slots).forEach(function(p){
        if(!p.player || p.player.proj==null) return;
        var pos = p.player.pos;
        if(myFloor[pos]==null || p.player.proj < myFloor[pos]) myFloor[pos] = p.player.proj;
      });
    }
    var names = Object.keys(teams).sort(function(a,b){
      if(a===myName) return -1;
      if(b===myName) return 1;
      return a.localeCompare(b);
    });

    var html = names.map(function(n){
      var roster = teams[n];
      var isMine = n===myName;
      var set = roster.filter(function(r){ return r.starting; })
        .sort(function(a,b){ return (a.slot_no||99)-(b.slot_no||99); });
      var likely = projectLineup(roster, slots);
      var likelyIds = {};
      likely.forEach(function(p){ if(p.player) likelyIds[p.player.player] = p.slot; });
      var bench = roster.filter(function(r){ return !likelyIds[r.player]; })
        .sort(function(a,b){ return (b.proj==null?-1:b.proj)-(a.proj==null?-1:a.proj); });
      var steals = isMine ? [] : bench.filter(function(r){
        return r.proj!=null && !unavailable(r) && myFloor[r.pos]!=null && r.proj > myFloor[r.pos];
      }).slice(0,4);
      var broken = set.filter(unavailable);

      var chips = "";
      if(!set.length) chips += "<span class=\\"chipflag bad\\">no lineup set</span>";
      if(broken.length) chips += "<span class=\\"chipflag bad\\">"+broken.length+
        " starter"+(broken.length>1?"s":"")+" out or on bye</span>";
      if(steals.length) chips += "<span class=\\"chipflag good\\">"+steals.length+
        " benched who would start for you</span>";

      var setBy = {};
      set.forEach(function(r){ setBy[r.slot_no] = r; });
      var lines = likely.map(function(p, i){
        var cur = setBy[i+1];
        var pick = p.player;
        var swap = (cur && pick && cur.player!==pick.player) || (!cur && pick);
        var shown = cur || pick;
        if(!shown) return "<div class=\\"lrow\\"><span class=\\"slotname\\">"+esc(p.slot)+
          "</span><span class=\\"who dim\\">empty</span><span class=\\"num\\">&mdash;</span></div>";
        var why = isOut(shown) ? "<span class=\\"inj\\">"+esc(shown.injury)+"</span>" :
                  (onBye(shown) ? "<span class=\\"inj\\">BYE</span>" : "");
        var alt = (swap && pick) ? "<span class=\\"likely\\">likely "+esc(pick.player)+
          " "+(pick.proj!=null?pick.proj.toFixed(1):"")+"</span>" : "";
        return "<div class=\\"lrow"+(cur&&unavailable(cur)?" bad":"")+"\\">"+
          "<span class=\\"slotname\\">"+esc(p.slot)+"</span>"+
          "<span class=\\"who\\"><b>"+esc(shown.player)+"</b><span class=\\"posmini\\">"+
          esc(shown.pos)+"</span>"+why+alt+"</span>"+
          "<span class=\\"num\\">"+(shown.proj==null?"&mdash;":shown.proj.toFixed(1))+"</span></div>";
      }).join("");

      var benchTop = bench.slice(0, 6);
      var benchLines = benchTop.length ? ("<div class=\\"steals\\"><div class=\\"benchhead\\">Bench</div>"+
        benchTop.map(function(r){
          var isSteal = steals.indexOf(r)!==-1;
          var why = isOut(r) ? "<span class=\\"inj\\">"+esc(r.injury)+"</span>" :
                    (onBye(r) ? "<span class=\\"inj\\">BYE</span>" : "");
          var tag = isSteal ? "<span class=\\"tag fa\\">beats your "+
            myFloor[r.pos].toFixed(1)+"</span>" : "";
          return "<div class=\\"lrow"+(isSteal?" good":"")+"\\">"+
            "<span class=\\"slotname\\">BN</span><span class=\\"who\\"><b>"+esc(r.player)+
            "</b><span class=\\"posmini\\">"+esc(r.pos)+"</span>"+why+tag+"</span>"+
            "<span class=\\"num\\">"+(r.proj==null?"&mdash;":r.proj.toFixed(1))+"</span></div>";
        }).join("")+"</div>") : "";

      var total = likely.reduce(function(s,p){ return s + (p.player&&p.player.proj||0); }, 0);
      return "<div class=\\"lineup"+(isMine?" mine":"")+"\\">"+
        "<h3>"+esc(n)+"<span class=\\"tot\\">"+total.toFixed(1)+" likely proj</span></h3>"+
        (chips?("<div class=\\"chiprow\\">"+chips+"</div>"):"")+
        lines+benchLines+"</div>";
    }).join("");

    document.getElementById("lineupwrap").innerHTML = html ||
      "<div class=\\"empty\\">No rosters for this league.</div>";
  }

  function draw(){
    var L = leagues[state.league];
    // controls that only make sense in one view
    document.getElementById("scope").hidden = state.view!=="board";
    document.getElementById("q").hidden = (state.view==="league" || state.view==="lineups");
    document.getElementById("poschips").hidden = state.view==="lineups";
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

    document.getElementById("hrow").innerHTML = COLS.map(function(c){
      var on = state.sort===c.k;
      var arw = on ? "<span class=\\"arw\\">"+(state.dir===1?"&#9650;":"&#9660;")+"</span>" : "";
      return "<th"+(on?" aria-sort=\\""+(state.dir===1?"ascending":"descending")+"\\"":"")+
             "><button type=\\"button\\" data-k=\\""+c.k+"\\">"+c.h+arw+"</button></th>";
    }).join("");

    var rs = rows();
    document.getElementById("tbody").innerHTML = rs.map(function(r){
      return "<tr"+(r.mine?" class=\\"is-mine\\"":"")+">"+
        COLS.map(function(c){return cell(r,c);}).join("")+"</tr>";
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
    draw();
  });

  var POS = ["ALL"].concat(leagues.reduce(function(a,l){
    l.rows.forEach(function(r){ if(a.indexOf(r.pos)===-1) a.push(r.pos); }); return a; }, [])
    .sort(function(a,b){ return ["QB","RB","WR","TE"].indexOf(a) - ["QB","RB","WR","TE"].indexOf(b); }));
  var chips = document.getElementById("poschips");
  chips.innerHTML = POS.map(function(p){
    return "<button type=\\"button\\" class=\\"chip\\" data-pos=\\""+p+"\\" aria-pressed=\\""+(p==="ALL")+"\\">"+p+"</button>";
  }).join("");
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
    draw();
  });

  document.getElementById("hrow").addEventListener("click", function(e){
    var b = e.target.closest("button[data-k]"); if(!b) return;
    var k = b.dataset.k;
    if(state.sort===k){ state.dir = -state.dir; }
    else {
      state.sort = k;
      // a name and a rank read best ascending; every other column is a bigger-is-better number
      state.dir = (k==="player" || k==="pos_rank") ? 1 : -1;
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
