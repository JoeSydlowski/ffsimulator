# Player map - rest-of-season rank, age, usage and dynasty value on two charts.
#
#   Rscript dev/suite/player_map.R
#
# League-free: FantasyPros rest-of-season rankings (full PPR), season-to-date
# usage from nflreadr (ffs_usage_signals) and FantasyCalc dynasty values in
# both superflex and 1QB (12-team, full PPR). For each position the page draws
#   1. age (x) against rest-of-season positional rank (y), coloured by dynasty value
#   2. dynasty value (x) against expected fantasy points per game (y)
# for the top N of the rest-of-season board, with position, top-N and value
# format controls, and a search box that rings a player on both charts.
#
# Outputs dev/league_sims/player_map/<date>/: player_map.csv, player_map.html
# (open in a browser) and player_map_artifact.html (shaped for publishing).

suppressPackageStartupMessages(library(data.table))
devtools::load_all(here::here(), quiet = TRUE)
options(ffpros.cache = "memory")

config <- list(
  season = as.integer(Sys.getenv("FFS_SEASON", "2026")),
  pos = c("QB", "RB", "WR", "TE"),
  # how deep the page carries each position; the page picks 24/36/48 from it
  depth = as.integer(Sys.getenv("FFS_MAP_DEPTH", "60")),
  teams = as.integer(Sys.getenv("FFS_MAP_TEAMS", "12"))
)
day <- format(Sys.Date())
out <- here::here("dev", "league_sims", "player_map", day)
dir.create(out, recursive = TRUE, showWarnings = FALSE)

message("FantasyPros rest-of-season rankings ...")
ros <- as.data.table(ffs_ros_rankings(season = config$season, ppr = 1, pos_filter = config$pos))
ros <- unique(ros[!is.na(pos_rank) & pos_rank <= config$depth], by = c("pos", "pos_rank"))
message("usage (snaps + expected points) ...")
usage <- as.data.table(ffs_usage_signals(season = config$season, lookback = 3, pos_filter = config$pos))
message("FantasyCalc dynasty values (superflex + 1QB) ...")
fc2 <- as.data.table(fc_dynasty_values(num_qbs = 2, num_teams = config$teams, ppr = 1))
fc1 <- as.data.table(fc_dynasty_values(num_qbs = 1, num_teams = config$teams, ppr = 1))

# every join is by fantasypros_id first, then cleaned name + position (ids get
# re-issued, and rookies lag in the crosswalk)
pkey <- function(player, pos) ffsimulator:::.ffs_pkey(player, pos)
join <- function(base, side, cols) {
  side <- copy(side)[, fantasypros_id := as.character(fantasypros_id)]
  by_id <- unique(side[!is.na(fantasypros_id), c("fantasypros_id", cols), with = FALSE], by = "fantasypros_id")
  base <- merge(base, by_id, by = "fantasypros_id", all.x = TRUE, sort = FALSE)
  by_nm <- unique(side[, c("pkey", cols), with = FALSE], by = "pkey")
  setnames(by_nm, cols, paste0(cols, ".nm"))
  base <- merge(base, by_nm, by = "pkey", all.x = TRUE, sort = FALSE)
  for (cc in cols) {
    base[is.na(get(cc)), (cc) := get(paste0(cc, ".nm"))]
    base[, (paste0(cc, ".nm")) := NULL]
  }
  base
}
m <- ros[, list(fantasypros_id = as.character(fantasypros_id), player, pos, team, rank = pos_rank,
                ecr_delta, roster_pct)]
m[, pkey := pkey(player, pos)]
usage[, pkey := pkey(player, pos)]
m <- join(m, usage, c("snap_pct_avg", "exp_pts_avg", "fp_avg", "games"))
fc2[, pkey := pkey(player_name, pos)]
fc1[, pkey := pkey(player_name, pos)]
m <- join(m, fc2[, list(fantasypros_id, pkey, age, dyn_sf = value, redraft = redraft_value)],
          c("age", "dyn_sf", "redraft"))
m <- join(m, fc1[, list(fantasypros_id, pkey, dyn_1qb = value)], "dyn_1qb")
m[, pkey := NULL]
setorder(m, pos, rank)
message(sprintf("%d players; missing age %d, usage %d, dynasty value %d", nrow(m),
                sum(is.na(m$age)), sum(is.na(m$exp_pts_avg)), sum(is.na(m$dyn_sf))))
fwrite(m, file.path(out, "player_map.csv"))

rows <- m[, list(player, pos, team, rank, age = round(age, 1),
                 xfp = round(exp_pts_avg, 1), fp = round(fp_avg, 1), snap = round(snap_pct_avg, 2),
                 games, dyn_sf = round(dyn_sf), dyn_1qb = round(dyn_1qb), redraft = round(redraft),
                 own = round(roster_pct), delta = ecr_delta)]
meta <- list(generated = format(Sys.time(), "%Y-%m-%d %H:%M"), season = config$season,
             week = as.integer(nflreadr::get_current_week()), teams = config$teams,
             weeks = paste(range(attr(usage, "weeks")), collapse = "-"))
json <- jsonlite::toJSON(list(meta = meta, rows = rows), auto_unbox = TRUE, na = "null", digits = 4)

head_bits <- r"---(<title>Player Value Map</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Archivo:wght@600;700&family=IBM+Plex+Mono:wght@400;500&family=IBM+Plex+Sans:wght@400;500;600&display=swap">
<style>
:root{
  --ground:#f5f7f6; --surface:#ffffff; --surface-2:#eef2f0;
  --ink:#111a17; --ink-2:#4a5a54; --ink-3:#6c7c76;
  --line:#dfe6e2; --line-strong:#c6d1cc;
  --accent:#0f6b4f; --accent-soft:#dff0e8; --accent-ink:#0b5540; --mark:#9a6412;
  --shadow:0 1px 2px rgba(17,26,23,.06), 0 8px 24px -16px rgba(17,26,23,.25);
}
@media (prefers-color-scheme: dark){
  :root:not([data-theme="light"]){
    color-scheme:dark;
    --ground:#0e1412; --surface:#151d1a; --surface-2:#1b2522;
    --ink:#e7edea; --ink-2:#9db0a8; --ink-3:#7f918a;
    --line:#233029; --line-strong:#31423a;
    --accent:#49bd8e; --accent-soft:#13312a; --accent-ink:#8fe0bf; --mark:#d8a44a;
    --shadow:0 1px 2px rgba(0,0,0,.4), 0 8px 24px -16px rgba(0,0,0,.8);
  }
}
:root[data-theme="dark"]{
  color-scheme:dark;
  --ground:#0e1412; --surface:#151d1a; --surface-2:#1b2522;
  --ink:#e7edea; --ink-2:#9db0a8; --ink-3:#7f918a;
  --line:#233029; --line-strong:#31423a;
  --accent:#49bd8e; --accent-soft:#13312a; --accent-ink:#8fe0bf; --mark:#d8a44a;
  --shadow:0 1px 2px rgba(0,0,0,.4), 0 8px 24px -16px rgba(0,0,0,.8);
}
*{box-sizing:border-box}
body{margin:0; background:var(--ground); color:var(--ink);
  font-family:"IBM Plex Sans",-apple-system,"Segoe UI",sans-serif; font-size:14px; line-height:1.5}
.wrap{max-width:1100px; margin:0 auto; padding-block:20px 56px; padding-inline:16px}
h1{font-family:Archivo,"IBM Plex Sans",sans-serif; font-weight:700; font-size:24px; margin:0; text-wrap:balance}
.prov{font-family:"IBM Plex Mono",monospace; font-size:12px; color:var(--ink-3); margin-top:4px}
.lede{max-width:72ch; color:var(--ink-2); margin:12px 0 16px}
.controls{display:flex; flex-wrap:wrap; align-items:center; gap:10px 16px; margin-bottom:12px}
.seg{display:inline-flex; background:var(--surface-2); border-radius:8px; padding:2px; gap:2px}
.seg button{appearance:none; border:0; background:transparent; color:var(--ink-2); font:inherit; font-size:13px;
  padding:5px 11px; border-radius:6px; cursor:pointer}
.seg button[aria-pressed="true"]{background:var(--surface); color:var(--ink); font-weight:600; box-shadow:var(--shadow)}
.ctl{display:inline-flex; align-items:center; gap:7px; font-size:12px; color:var(--ink-3);
  font-family:Archivo,sans-serif; letter-spacing:.06em; text-transform:uppercase}
.chip{appearance:none; font:inherit; font-size:12px; padding:4px 10px; cursor:pointer; border-radius:999px;
  border:1px solid var(--line-strong); background:var(--surface); color:var(--ink-2)}
.chip[aria-pressed="true"]{background:var(--accent); border-color:var(--accent); color:var(--surface)}
#q{flex:1 1 180px; min-width:0; max-width:260px; font:inherit; font-size:13px; padding:6px 10px; border-radius:8px;
  border:1px solid var(--line-strong); background:var(--surface); color:var(--ink)}
.seg button:focus-visible,.chip:focus-visible,#q:focus-visible{outline:2px solid var(--accent); outline-offset:2px}
.legend{display:flex; flex-wrap:wrap; align-items:center; gap:10px 18px; margin:0 0 12px}
.lg{display:inline-flex; align-items:center; gap:7px; font-size:12.5px; color:var(--ink-2)}
.ramp{display:inline-block; width:110px; height:10px; border-radius:5px; border:1px solid var(--line)}
.charts{position:relative; display:grid; gap:18px}
.chart{margin:0; background:var(--surface); border:1px solid var(--line); border-radius:10px; padding:12px 12px 6px; box-shadow:var(--shadow)}
.chart figcaption{display:flex; flex-direction:column; gap:2px; margin:0 4px 6px}
.chart figcaption b{font-family:Archivo,sans-serif; font-size:14px}
.chart figcaption span{font-size:12.5px; color:var(--ink-2)}
.plot{overflow-x:auto}
.plot svg{display:block; width:100%; min-width:520px; height:auto}
.ax{font:11px "IBM Plex Mono",monospace; fill:var(--ink-3); font-variant-numeric:tabular-nums}
.axt{font:600 10.5px Archivo,sans-serif; letter-spacing:.06em; text-transform:uppercase; fill:var(--ink-3)}
.lab{font:500 10.5px "IBM Plex Sans",sans-serif; fill:var(--ink-2); paint-order:stroke; stroke:var(--surface); stroke-width:3px}
.labm{fill:var(--ink); font-weight:600}
.hit{cursor:pointer}
.tip{position:absolute; z-index:5; width:250px; pointer-events:none; background:var(--surface); color:var(--ink);
  border:1px solid var(--line-strong); border-radius:8px; padding:8px 10px; font-size:12.5px; box-shadow:var(--shadow)}
.tip div{margin-top:2px}
.dim{color:var(--ink-3)}
details{margin-top:14px}
summary{cursor:pointer; font-family:Archivo,sans-serif; font-size:13px; color:var(--ink-2); padding:6px 0}
.tablewrap{overflow-x:auto; background:var(--surface); border:1px solid var(--line); border-radius:10px}
table{border-collapse:separate; border-spacing:0; width:100%}
th{text-align:right; font:600 10.5px Archivo,sans-serif; letter-spacing:.06em; text-transform:uppercase; color:var(--ink-3);
  padding:9px 10px; border-bottom:1px solid var(--line-strong); white-space:nowrap}
th:first-child,td:first-child{text-align:left}
td{padding:7px 10px; border-bottom:1px solid var(--line); text-align:right;
  font-family:"IBM Plex Mono",monospace; font-variant-numeric:tabular-nums; font-size:12.5px}
td:first-child{font-family:"IBM Plex Sans",sans-serif; font-weight:500; white-space:nowrap}
tr:last-child td{border-bottom:0}
@media (max-width:520px){ h1{font-size:20px} }
@media (prefers-reduced-motion:reduce){*{transition:none!important}}
</style>)---"

body_bits <- paste0(r"---(<div class="wrap">
  <h1>Player Value Map</h1>
  <div class="prov" id="prov"></div>
  <p class="lede">Where each position&rsquo;s top players sit on age, rest-of-season rank, opportunity and dynasty price. No league attached: rankings are the FantasyPros rest-of-season consensus (full PPR), usage is season to date, and values are FantasyCalc 12-team dynasty.</p>
  <div class="controls">
    <div class="seg" id="posseg" role="group" aria-label="Position"></div>
    <span class="ctl">Top <span class="seg" id="topseg" role="group" aria-label="How many"></span></span>
    <span class="ctl">Values <span class="seg" id="fmtseg" role="group" aria-label="Value format"></span></span>
    <input id="q" type="search" placeholder="Ring players, e.g. Hall, Henderson" autocomplete="off">
  </div>
  <div id="panel"></div>
</div>
<script id="data" type="application/json">)---", json, r"---(</script>
<script>
(function(){
  var D = JSON.parse(document.getElementById("data").textContent), M = D.meta;
  var st = {pos:"RB", top:36, fmt:"sf", q:[], labelAll:false};
  try { var h = (location.hash||"").slice(1).toUpperCase(); if(["QB","RB","WR","TE"].indexOf(h)!==-1) st.pos = h; } catch(e){}
  function esc(s){ return String(s).replace(/[&<>"]/g, function(c){
    return {"&":"&amp;","<":"&lt;",">":"&gt;","\"":"&quot;"}[c]; }); }
  function k(v){ return v==null ? "&mdash;" : (Math.abs(v)>=1000 ? (v/1000).toFixed(1)+"k" : String(Math.round(v))); }
  document.getElementById("prov").textContent = "built " + M.generated + " · " + M.season + " week " + M.week +
    " · usage weeks " + M.weeks + " · FantasyCalc " + M.teams + "-team";

  function seg(id, opts, key){
    var el = document.getElementById(id);
    el.innerHTML = opts.map(function(o){ return "<button type=\"button\" data-v=\""+o[0]+"\" aria-pressed=\""+(String(st[key])===String(o[0]))+"\">"+o[1]+"</button>"; }).join("");
    el.onclick = function(e){ var b = e.target.closest("button"); if(!b) return;
      st[key] = key==="top" ? +b.dataset.v : b.dataset.v; seg(id, opts, key); draw(); };
  }
  seg("posseg", [["QB","QB"],["RB","RB"],["WR","WR"],["TE","TE"]], "pos");
  seg("topseg", [[24,"24"],[36,"36"],[48,"48"]], "top");
  seg("fmtseg", [["sf","Superflex"],["1qb","1QB"]], "fmt");
  document.getElementById("q").addEventListener("input", function(){
    st.q = this.value.toLowerCase().split(",").map(function(s){ return s.trim(); }).filter(Boolean); draw(); });

  var RB = [], dynMin = 0, dynMax = 1;
  function dyn(r){ return st.fmt==="sf" ? r.dyn_sf : r.dyn_1qb; }
  function hit(r){ var n = r.player.toLowerCase(); return st.q.some(function(q){ return n.indexOf(q)!==-1; }); }
  // one hue, light to dark: share of the accent mixed into the surface
  function dynFill(v){
    if(v==null) return "var(--surface-2)";
    var t = dynMax>dynMin ? (v-dynMin)/(dynMax-dynMin) : 1;
    return "color-mix(in srgb, var(--accent) "+Math.round(30+70*t)+"%, var(--surface))";
  }
  function niceTicks(lo, hi, n){
    var step = Math.pow(10, Math.floor(Math.log10((hi-lo)/n))), err = (hi-lo)/n/step;
    step *= err>=5 ? 10 : err>=2 ? 5 : err>=1.5 ? 2 : 1;
    var out = []; for(var v=Math.ceil(lo/step)*step; v<=hi+1e-9; v+=step) out.push(+v.toFixed(6));
    return out;
  }
  function scatter(o){
    var W = 720, H = 440, m = {l:56, r:18, t:24, b:46}, pw = W-m.l-m.r, ph = H-m.t-m.b;
    var pts = RB.map(function(r, i){ return {r:r, i:i, x:o.x(r), y:o.y(r)}; })
                .filter(function(p){ return p.x!=null && p.y!=null; });
    var sx = function(v){ return m.l + (v-o.xd[0])/(o.xd[1]-o.xd[0])*pw; };
    var sy = function(v){ var t = (v-o.yd[0])/(o.yd[1]-o.yd[0]); return o.invert ? m.t + t*ph : m.t + (1-t)*ph; };
    var g = "";
    o.xt.forEach(function(v){ var x = sx(v);
      g += "<line x1=\""+x+"\" x2=\""+x+"\" y1=\""+m.t+"\" y2=\""+(m.t+ph)+"\" stroke=\"var(--line)\"/>"+
           "<text x=\""+x+"\" y=\""+(m.t+ph+16)+"\" text-anchor=\"middle\" class=\"ax\">"+o.fx(v)+"</text>"; });
    o.yt.forEach(function(v){ var y = sy(v);
      g += "<line x1=\""+m.l+"\" x2=\""+(m.l+pw)+"\" y1=\""+y+"\" y2=\""+y+"\" stroke=\"var(--line)\"/>"+
           "<text x=\""+(m.l-8)+"\" y=\""+(y+4)+"\" text-anchor=\"end\" class=\"ax\">"+o.fy(v)+"</text>"; });
    g += "<text x=\""+(m.l+pw/2)+"\" y=\""+(H-6)+"\" text-anchor=\"middle\" class=\"axt\">"+o.xl+"</text>"+
         "<text transform=\"translate(14,"+(m.t+ph/2)+") rotate(-90)\" text-anchor=\"middle\" class=\"axt\">"+o.yl+"</text>";
    var order = pts.slice().sort(function(a,b){ return (hit(a.r)?1:0)-(hit(b.r)?1:0); });
    var dots = order.map(function(p){
      var cx = sx(p.x), cy = sy(p.y), ring = hit(p.r) ? "stroke=\"var(--mark)\" stroke-width=\"2.5\"" :
        "stroke=\"var(--surface)\" stroke-width=\"2\"";
      return "<circle cx=\""+cx+"\" cy=\""+cy+"\" r=\"6.5\" fill=\""+(o.fill ? o.fill(p.r) : dynFill(dyn(p.r)))+"\" "+ring+"/>"+
             "<circle cx=\""+cx+"\" cy=\""+cy+"\" r=\"12\" fill=\"transparent\" class=\"hit\" data-i=\""+p.i+"\"><title>"+esc(p.r.player)+"</title></circle>";
    }).join("");
    // labels: greedy, searched players first, then by rank. A name that would
    // collide or run off the plot is left to the hover; a searched player only
    // has to clear other names, not dots
    var boxes = pts.map(function(p){ return {x0:sx(p.x)-7, x1:sx(p.x)+7, y0:sy(p.y)-7, y1:sy(p.y)+7}; });
    var labBoxes = [], labs = "";
    pts.filter(function(p){ return st.labelAll || hit(p.r) || p.r._top[o.key]; })
      .sort(function(a,b){ return (hit(b.r)?1:0)-(hit(a.r)?1:0) || a.r.rank-b.r.rank; })
      .forEach(function(p){
        var cx = sx(p.x), cy = sy(p.y), name = p.r.player.replace(/^(\S)\S*\s/, "$1. "), w = name.length*6.1+2, pri = hit(p.r);
        var tries = [[cx+10, cy+4, "start", cx+9, cx+10+w], [cx-10, cy+4, "end", cx-10-w, cx-9],
                     [cx-w/2, cy-11, "start", cx-w/2, cx+w/2], [cx-w/2, cy+19, "start", cx-w/2, cx+w/2]];
        for(var j=0; j<tries.length; j++){
          var t = tries[j], b = {x0:t[3], x1:t[4], y0:t[1]-10, y1:t[1]+3};
          if(b.x0<m.l || b.x1>W-4 || b.y0<4 || b.y1>m.t+ph) continue;
          if((pri ? labBoxes : boxes).some(function(q){ return b.x0<q.x1 && b.x1>q.x0 && b.y0<q.y1 && b.y1>q.y0; })) continue;
          boxes.push(b); labBoxes.push(b);
          labs += "<text x=\""+t[0]+"\" y=\""+t[1]+"\" text-anchor=\""+t[2]+"\" class=\"lab"+(pri?" labm":"")+"\">"+esc(name)+"</text>";
          break;
        }
      });
    var missing = RB.length - pts.length;
    return "<figure class=\"chart\"><figcaption><b>"+o.title+"</b><span>"+o.sub+
      (missing ? " "+missing+" without data "+(missing===1?"is":"are")+" left off." : "")+"</span></figcaption>"+
      "<div class=\"plot\"><svg viewBox=\"0 0 "+W+" "+H+"\" role=\"img\" aria-label=\""+esc(o.title)+"\">"+g+dots+labs+"</svg></div></figure>";
  }

  function draw(){
    var P = st.pos, fmtName = st.fmt==="sf" ? "superflex" : "1QB";
    RB = D.rows.filter(function(r){ return r.pos===P && r.rank<=st.top; }).sort(function(a,b){ return a.rank-b.rank; });
    var dv = RB.map(dyn).filter(function(v){ return v!=null; });
    dynMin = Math.min.apply(null, dv); dynMax = Math.max.apply(null, dv);
    var byDyn = RB.slice().sort(function(a,b){ return (dyn(b)||0)-(dyn(a)||0); }).slice(0,8);
    var byXfp = RB.slice().sort(function(a,b){ return (b.xfp||0)-(a.xfp||0); }).slice(0,8);
    RB.forEach(function(r){ r._top = {age: r.rank<=10 || byDyn.indexOf(r)!==-1 || (r.age!=null && r.age>=29 && r.rank<=st.top*2/3),
                                      val: byDyn.indexOf(r)!==-1 || byXfp.indexOf(r)!==-1}; });
    var ages = RB.map(function(r){ return r.age; }).filter(function(v){ return v!=null; });
    var aLo = Math.floor(Math.min.apply(null, ages)) - 0.5, aHi = Math.ceil(Math.max.apply(null, ages)) + 0.5;
    var xMax = Math.max.apply(null, RB.map(function(r){ return r.xfp||0; })) * 1.08;
    var yt = [1]; for(var v=6; v<=st.top; v+=6) yt.push(v);
    var c1 = scatter({key:"age", title:P+"s: age against rest-of-season rank",
      sub:"FantasyPros rest-of-season "+P+" rank, "+P+"1 at the top. Darker = more "+fmtName+" dynasty value.",
      x:function(r){ return r.age; }, y:function(r){ return r.rank; },
      xd:[aLo, aHi], yd:[0.5, st.top+0.5], invert:true, xt:niceTicks(aLo, aHi, 8), yt:yt,
      fx:function(v){ return v; }, fy:function(v){ return P+v; }, xl:"Age", yl:"Rest-of-season rank"});
    var c2 = scatter({key:"val", title:P+"s: dynasty value against expected points per game",
      sub:"Expected fantasy points per game from opportunity alone (season to date), against FantasyCalc "+fmtName+" dynasty value.",
      x:dyn, y:function(r){ return r.xfp; }, xd:[0, dynMax*1.05], yd:[0, xMax],
      xt:niceTicks(0, dynMax*1.05, 6), yt:niceTicks(0, xMax, 5),
      fx:function(v){ return k(v); }, fy:function(v){ return v; }, xl:"Dynasty value ("+fmtName+")", yl:"xFP per game",
      fill:function(){ return "color-mix(in srgb, var(--accent) 60%, var(--surface))"; }});
    var stops = [0,25,50,75,100].map(function(t){
      return "color-mix(in srgb, var(--accent) "+Math.round(30+70*t/100)+"%, var(--surface)) "+t+"%"; }).join(",");
    var legend = "<div class=\"legend\"><span class=\"lg\"><span class=\"ramp\" style=\"background:linear-gradient(90deg,"+stops+")\"></span>"+
      "Dynasty value "+k(dynMin)+" &rarr; "+k(dynMax)+"</span>"+
      (st.q.length ? "<span class=\"lg\"><svg width=\"18\" height=\"18\"><circle cx=\"9\" cy=\"9\" r=\"6\" fill=\"var(--surface-2)\" stroke=\"var(--mark)\" stroke-width=\"2.5\"/></svg>Searched</span>" : "")+
      "<button type=\"button\" class=\"chip\" id=\"labelall\" aria-pressed=\""+st.labelAll+"\">Label every player</button></div>";
    var rows = RB.map(function(r){
      return "<tr><td>"+esc(r.player)+"</td><td>"+esc(r.team||"")+"</td><td>"+P+r.rank+"</td><td>"+(r.age==null?"&mdash;":r.age)+"</td><td>"+
        (r.xfp==null?"&mdash;":r.xfp.toFixed(1))+"</td><td>"+(r.fp==null?"&mdash;":r.fp.toFixed(1))+"</td><td>"+
        (r.snap==null?"&mdash;":Math.round(r.snap*100))+"</td><td>"+k(r.dyn_sf)+"</td><td>"+k(r.dyn_1qb)+"</td><td>"+k(r.redraft)+"</td></tr>"; }).join("");
    document.getElementById("panel").innerHTML = legend+"<div class=\"charts\" id=\"charts\">"+c1+c2+
      "<div class=\"tip\" id=\"tip\" hidden></div></div><details><summary>Table view</summary><div class=\"tablewrap\"><table><thead><tr>"+
      ["Player","Team","ROS rank","Age","xFP/g","FP/g","Snap%","Dynasty SF","Dynasty 1QB","Redraft"].map(function(h){ return "<th>"+h+"</th>"; }).join("")+
      "</tr></thead><tbody>"+rows+"</tbody></table></div></details>";
  }
  var panel = document.getElementById("panel");
  panel.addEventListener("click", function(e){
    if(e.target.closest("#labelall")){ st.labelAll = !st.labelAll; draw(); } });
  panel.addEventListener("mouseover", function(e){
    var c = e.target.closest && e.target.closest("circle.hit"), tip = document.getElementById("tip");
    if(!c || !tip) return;
    var r = RB[+c.getAttribute("data-i")], wrap = document.getElementById("charts").getBoundingClientRect(),
        b = c.getBoundingClientRect();
    tip.innerHTML = "<b>"+esc(r.player)+"</b> <span class=\"dim\">"+esc(r.team||"")+"</span>"+
      "<div>"+r.pos+r.rank+" rest of season &middot; age "+(r.age==null?"?":r.age)+"</div>"+
      "<div>"+(r.xfp==null?"no usage yet":r.xfp.toFixed(1)+" xFP/g, "+(r.fp==null?"?":r.fp.toFixed(1))+" FP/g")+
        (r.snap==null?"":" &middot; "+Math.round(r.snap*100)+"% snaps")+"</div>"+
      "<div>dynasty "+k(r.dyn_sf)+" SF / "+k(r.dyn_1qb)+" 1QB &middot; redraft "+k(r.redraft)+"</div>";
    tip.hidden = false;
    var left = b.left - wrap.left + 16, top = b.top - wrap.top - 8;
    if(left + 260 > wrap.width) left = b.left - wrap.left - 266;
    tip.style.left = Math.max(0, left)+"px"; tip.style.top = Math.max(0, top)+"px";
  });
  panel.addEventListener("mouseout", function(e){
    if(e.target.closest && e.target.closest("circle.hit")){ var t = document.getElementById("tip"); if(t) t.hidden = true; } });
  draw();
})();
</script>)---")

writeLines(paste0(head_bits, "\n", body_bits), file.path(out, "player_map_artifact.html"), useBytes = TRUE)
writeLines(paste0('<!doctype html>\n<html lang="en">\n<head>\n<meta charset="utf-8">\n',
                  '<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">\n',
                  head_bits, "\n</head>\n<body>\n", body_bits, "\n</body>\n</html>"),
           file.path(out, "player_map.html"), useBytes = TRUE)
message("wrote ", out)
