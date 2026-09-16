# Weekly waiver watchlist - every league, every signal, no simulation.
#
#   Rscript dev/suite/waiver_watchlist.R
#   FFS_LEAGUE_ID=1312145437842497536 Rscript dev/suite/waiver_watchlist.R
#
# Runs in a couple of minutes: it scrapes rather than simulates. Per league it
# lists every ranked player - free agent or rostered - with
#   rankings  ecr / pos_rank / ecr_delta   (LIVE FantasyPros rest-of-season scrape)
#   market    roster_pct, Sleeper adds+drops over the last 24h
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
    pos_filter = config$pos_filter,
    my_franchise = my_id))

  list(
    id = league_id,
    name = lname,
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
          "pos_rank", "ecr", "ecr_delta", "roster_pct", "trend_add", "adds_per_open",
          "trend_drop", "drops_per_owned",
          "snap_pct", "xfp_g", "fp_g", "redraft_value", "dynasty_value")

payload <- lapply(leagues, function(l) {
  d <- as.data.table(l$rows)
  for (cc in setdiff(show, names(d))) d[, (cc) := NA]
  d <- d[, show, with = FALSE]
  # jsonlite writes NA as null, which the page treats as "no data"
  list(id = l$id, name = l$name, type = l$type, qb = l$qb, ppr = l$ppr,
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

  <div class="tablewrap">
    <table id="tbl">
      <thead><tr id="hrow"></tr></thead>
      <tbody id="tbody"></tbody>
    </table>
    <div class="empty" id="empty" hidden>Nothing matches those filters.</div>
  </div>

  <section class="legend">
    <h2>What the columns mean</h2>
    <dl>
      <div><dt>Signal</dt><dd>Average of the percentile ranks below, taken <em>within position</em> against the free agents &mdash; your own players are scored on that same scale, so the default view sorts a claim and the man you&rsquo;d cut in one list. An opening sort, not a projection.</dd></div>
      <div><dt>mine / unranked</dt><dd><span class="tag-mine">mine</span> is on your roster. <span class="unranked">unranked</span> means FantasyPros lists no rest-of-season rank for him at all &mdash; he is here because his snaps or the Sleeper market say he is playing.</dd></div>
      <div><dt>Rank</dt><dd>FantasyPros rest-of-season consensus, with the week&rsquo;s movement.</dd></div>
      <div><dt>Own%</dt><dd>Share of all leagues that roster him. High and still free here is the classic miss.</dd></div>
      <div><dt>Adds / Drops</dt><dd>Sleeper claims across every league in the window. Moves hours after news, days before rankings.</dd></div>
      <div><dt>Adds/open</dt><dd>Adds divided by the leagues where he was actually free, so ownership stops hiding the signal &mdash; a player 88% of leagues already roster can only be added in the other 12%. Drops/own does the same the other way: of the leagues that have him, how hard they are cutting.</dd></div>
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
  var state = {league:0, scope:"compare", pos:"ALL", q:"", sort:"signal", dir:-1};

  document.getElementById("prov").innerHTML =
    "week <b>" + meta.week + "</b> &middot; " + meta.season +
    " &middot; ROS ranks <b>" + meta.ros_source + " " + meta.ros_date + "</b>" +
    " &middot; usage last <b>" + meta.lookback + "</b>wk &middot; adds <b>" + meta.trend_hours + "</b>h" +
    " &middot; built " + meta.generated;

  var COLS = [
    {k:"player",        h:"Player",  type:"name"},
    {k:"signal",        h:"Signal",  type:"sig"},
    {k:"pos_rank",      h:"Rank",    type:"rank"},
    {k:"roster_pct",    h:"Own%",       type:"pct1"},
    {k:"trend_add",     h:"Adds",       type:"count"},
    {k:"adds_per_open", h:"Adds/open",  type:"count"},
    {k:"trend_drop",    h:"Drops",      type:"count"},
    {k:"drops_per_owned", h:"Drops/own", type:"count"},
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
        ds = " <span class=\\"delta "+(d>0?"up":"down")+"\\">"+(d>0?"\\u25b2":"\\u25bc")+Math.abs(d)+"</span>";
      }
      return "<td>"+esc(row.pos)+v+ds+"</td>";
    }
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

  function draw(){
    var L = leagues[state.league];
    document.getElementById("facts").innerHTML =
      "<b>"+esc(L.my_team||"&mdash;")+"</b> <span class=\\"dot\\">/</span> " +
      esc(L.type)+" <span class=\\"dot\\">&middot;</span> "+esc(L.qb)+
      " <span class=\\"dot\\">&middot;</span> "+L.ppr+" ppr"+
      " <span class=\\"dot\\">&middot;</span> "+L.teams+" teams"+
      " <span class=\\"dot\\">&middot;</span> <b>"+L.n_fa+"</b> ranked free agents";

    document.getElementById("hrow").innerHTML = COLS.map(function(c){
      var on = state.sort===c.k;
      var arw = on ? "<span class=\\"arw\\">"+(state.dir===1?"\\u25b2":"\\u25bc")+"</span>" : "";
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
