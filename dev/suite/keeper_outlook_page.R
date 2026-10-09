# Page for dev/suite/keeper_outlook.R: reads its keeper_outlook.rds and writes
# keeper_outlook.html (open in a browser) + keeper_outlook_artifact.html (for
# publishing). source()d at the end of keeper_outlook.R; also runnable alone:
#
#   Rscript dev/suite/keeper_outlook_page.R [path/to/keeper_outlook.rds]

suppressPackageStartupMessages(library(data.table))

keeper_outlook_page <- function(rds) {
  K <- readRDS(rds)
  out <- dirname(rds)
  r0 <- function(x) round(as.numeric(x))
  r2 <- function(x) round(as.numeric(x), 2)

  mine <- K$mine[, list(player, pos, age = r0(age), drafted = draft_rd, kept = kept_now,
                        cost = keep_cost, top2, bar = r0(bar_exp), proj = r0(next_rank_med),
                        p_keep = r2(p_keep), surplus = r0(surplus), option = r0(option),
                        redraft = r0(redraft_value), verdict, why)]
  sell <- K$sell[, list(player, pos, age = r0(age), cost = keep_cost, p_keep = r2(p_keep),
                        option = r0(option), redraft = r0(redraft_value))]
  buy <- K$buy[, list(player, pos, age = r0(age), owner = franchise_name, posture,
                      drafted = draft_rd, cost = keep_cost, costs_me = marg_round, replaces, source = cost_source,
                      p_keep = r2(p_keep_me), option = r0(option_net), option_gross = r0(option_me), owner_option = r0(option),
                      redraft = r0(redraft_value), owner_keeps, contender = from_contender)]
  pairs <- if (nrow(K$pairs)) K$pairs[, list(owner, posture, give, give_pos, get, get_pos, get_cost, replaces = if ("replaces" %in% names(K$pairs)) replaces else NA_character_,
                                               their_2026 = r0(their_gain_2026),
                                               my_2027 = r0(my_gain_2027), ratio = r2(ratio),
                                               owner_keeps)] else data.table()
  league <- K$league[, list(rank = keeper_rank, franchise = franchise_name, posture,
                            playoff = r2(playoff_pct), champ = r2(champion_pct),
                            slot = round(exp_slot, 1),
                            keepers, surplus = r0(keeper_surplus), option = r0(keeper_option),
                            list = keepers_list)]
  bar <- K$bar[, list(round, bar = r0(bar_rank), raw = med_rank, n)]

  me <- K$league[franchise_id == K$mine$franchise_id[[1]]]
  meta <- list(
    generated = K$meta$generated, n_sims = K$meta$n_sims,
    sim = basename(dirname(K$meta$sim)), first_sim_week = K$meta$first_sim_week,
    my_team = me$franchise_name, my_rank = me$keeper_rank, n_teams = nrow(K$league),
    my_posture = me$posture, my_playoff = me$playoff_pct,
    n_keep = sum(K$mine$in_set, na.rm = TRUE),
    keep_list = me$keepers_list
  )
  fa <- if (!is.null(K$fa) && nrow(K$fa)) {
    f <- as.data.table(K$fa)
    for (cc in setdiff(c("team", "roster_pct", "trend_add", "snap_pct", "xfp_g", "injury",
                         "redraft_value", "dynasty_value", "age"), names(f))) f[, (cc) := NA]
    for (cc in setdiff(c("marg_round", "excess", "excess_net", "replaces", "p_keep_est"), names(f))) f[, (cc) := NA]
    f[, list(player, pos, team, age = r0(age), drafted = draft_rd, cost = keep_cost, costs_me = marg_round, replaces,
             source = cost_source, dyn = dyn_rank, bar = r0(my_bar), margin = my_margin,
             excess = r0(excess), excess_net = r0(excess_net), p_keep = r2(p_keep_est), own = round(roster_pct),
             adds = trend_add, snap = r2(snap_pct), xfp = round(as.numeric(xfp_g), 1),
             injury, redraft = r0(redraft_value), dynasty = r0(dynasty_value))]
  } else data.table()
  meta$my_rounds <- paste0("R", K$meta$my_rounds, collapse = ", ")
  meta$pk_r2 <- if (is.null(K$meta$pk_r2)) NA_real_ else round(K$meta$pk_r2, 2)
  meta$fa_source <-if (is.null(K$fa_source)) NA_character_ else basename(dirname(K$fa_source))
  by_slot <- K$my_by_slot[, c("player", "keep_cost", paste0("slot", seq_len(meta$n_teams))), with = FALSE]
  bar_slot <- dcast(K$bar_slot, round ~ paste0("slot", slot), value.var = "bar_rank")
  bar_slot <- bar_slot[, lapply(.SD, r0)]
  json <- jsonlite::toJSON(list(meta = meta, mine = mine, sell = sell, buy = buy, pairs = pairs,
                                league = league, bar = bar, by_slot = by_slot, fa = fa, bar_slot = bar_slot,
                                my_slots = K$my_slots[, list(slot, p = r2(p))]),
                           auto_unbox = TRUE, na = "null", digits = 4)

  head_bits <- r"---(<title>Galletti Keeper Outlook</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Archivo:wght@600;700&family=IBM+Plex+Mono:wght@400;500&family=IBM+Plex+Sans:wght@400;500;600&display=swap">
<style>
:root{
  --ground:#f5f7f6; --surface:#ffffff; --surface-2:#eef2f0;
  --ink:#111a17; --ink-2:#4a5a54; --ink-3:#6c7c76;
  --line:#dfe6e2; --line-strong:#c6d1cc;
  --accent:#0f6b4f; --accent-soft:#dff0e8; --accent-ink:#0b5540;
  --up:#116b4c; --down:#a5402f; --down-soft:#f6e3de; --mine:#9a6412; --mine-soft:#f7ecd8;
  --shadow:0 1px 2px rgba(17,26,23,.06), 0 8px 24px -16px rgba(17,26,23,.25);
}
@media (prefers-color-scheme: dark){
  :root:not([data-theme="light"]){
    color-scheme:dark;
    --ground:#0e1412; --surface:#151d1a; --surface-2:#1b2522;
    --ink:#e7edea; --ink-2:#9db0a8; --ink-3:#7f918a;
    --line:#233029; --line-strong:#31423a;
    --accent:#49bd8e; --accent-soft:#13312a; --accent-ink:#8fe0bf;
    --up:#49bd8e; --down:#e0806e; --down-soft:#3a211c; --mine:#d8a44a; --mine-soft:#2b2317;
    --shadow:0 1px 2px rgba(0,0,0,.4), 0 8px 24px -16px rgba(0,0,0,.8);
  }
}
:root[data-theme="dark"]{
  color-scheme:dark;
  --ground:#0e1412; --surface:#151d1a; --surface-2:#1b2522;
  --ink:#e7edea; --ink-2:#9db0a8; --ink-3:#7f918a;
  --line:#233029; --line-strong:#31423a;
  --accent:#49bd8e; --accent-soft:#13312a; --accent-ink:#8fe0bf;
  --up:#49bd8e; --down:#e0806e; --down-soft:#3a211c; --mine:#d8a44a; --mine-soft:#2b2317;
  --shadow:0 1px 2px rgba(0,0,0,.4), 0 8px 24px -16px rgba(0,0,0,.8);
}
*{box-sizing:border-box}
body{margin:0; background:var(--ground); color:var(--ink);
  font-family:"IBM Plex Sans",-apple-system,"Segoe UI",sans-serif; font-size:14px; line-height:1.5}
.wrap{max-width:1180px; margin:0 auto; padding-block:20px 56px; padding-inline:16px}
h1{font-family:Archivo,"IBM Plex Sans",sans-serif; font-weight:700; font-size:24px; margin:0; text-wrap:balance}
h2{font-family:Archivo,sans-serif; font-size:15px; margin:0 0 4px}
.prov{font-family:"IBM Plex Mono",monospace; font-size:12px; color:var(--ink-3); margin-top:4px}
.lede{max-width:70ch; color:var(--ink-2); margin:14px 0 18px}
.lede b{color:var(--ink)}
.summary{display:grid; grid-template-columns:repeat(auto-fit,minmax(230px,1fr)); gap:12px; margin-bottom:22px}
.card{background:var(--surface); border:1px solid var(--line); border-radius:10px; padding:12px 14px; box-shadow:var(--shadow)}
.card .lab{font-family:Archivo,sans-serif; font-size:10.5px; letter-spacing:.07em; text-transform:uppercase; color:var(--ink-3)}
.card .big{font-family:Archivo,sans-serif; font-size:22px; margin-top:2px}
.card .txt{font-size:12.5px; color:var(--ink-2); margin-top:4px}
.tabs{display:flex; gap:2px; overflow-x:auto; border-bottom:1px solid var(--line); margin-bottom:12px}
.tab{appearance:none; border:0; background:transparent; color:var(--ink-2); font:inherit; font-size:13px;
  padding:8px 12px 10px; cursor:pointer; border-bottom:2px solid transparent; white-space:nowrap}
.tab:hover{color:var(--ink); background:var(--surface-2)}
.tab[aria-selected="true"]{color:var(--accent-ink); border-bottom-color:var(--accent); font-weight:600}
.tab .cnt{font-family:"IBM Plex Mono",monospace; font-size:11px; color:var(--ink-3); margin-left:5px}
.tab:focus-visible,th button:focus-visible,.chip:focus-visible{outline:2px solid var(--accent); outline-offset:2px}
.note{max-width:78ch; color:var(--ink-2); font-size:13px; margin:0 0 10px}
.controls{display:flex; flex-wrap:wrap; gap:6px; margin-bottom:10px}
.chip{appearance:none; font:inherit; font-size:12px; padding:4px 10px; cursor:pointer; border-radius:999px;
  border:1px solid var(--line-strong); background:var(--surface); color:var(--ink-2)}
.chip[aria-pressed="true"]{background:var(--accent); border-color:var(--accent); color:var(--surface)}
.tablewrap{overflow-x:auto; background:var(--surface); border:1px solid var(--line); border-radius:10px; box-shadow:var(--shadow)}
table{border-collapse:separate; border-spacing:0; width:100%}
th{position:sticky; top:0; background:var(--surface); border-bottom:1px solid var(--line-strong); padding:0; text-align:right; white-space:nowrap}
th:first-child{text-align:left}
th button{appearance:none; border:0; background:transparent; width:100%; cursor:pointer; text-align:inherit;
  font-family:Archivo,sans-serif; font-weight:600; font-size:10.5px; letter-spacing:.06em; text-transform:uppercase;
  color:var(--ink-3); padding:10px}
th[aria-sort] button{color:var(--accent-ink)}
td{padding:8px 10px; border-bottom:1px solid var(--line); text-align:right; vertical-align:top;
  font-family:"IBM Plex Mono",monospace; font-variant-numeric:tabular-nums; font-size:12.5px}
tr:last-child td{border-bottom:0}
td.txt{text-align:left; font-family:"IBM Plex Sans",sans-serif; font-size:13px}
td.wide{min-width:280px; max-width:460px; white-space:normal; color:var(--ink-2)}
td.name{text-align:left; font-family:"IBM Plex Sans",sans-serif; font-weight:500; white-space:nowrap; font-size:13px}
.pos{display:inline-block; min-width:28px; text-align:center; font:600 10.5px "IBM Plex Mono",monospace;
  padding:1px 4px; border-radius:4px; background:var(--surface-2); color:var(--ink-2); border:1px solid var(--line); margin-right:6px}
.pill{display:inline-block; font:500 11px "IBM Plex Sans",sans-serif; padding:2px 8px; border-radius:999px; white-space:nowrap}
.pill.core{background:var(--accent-soft); color:var(--accent-ink); border:1px solid var(--accent)}
.pill.likely{background:var(--accent-soft); color:var(--accent-ink)}
.pill.bubble{background:var(--mine-soft); color:var(--mine)}
.pill.sell{background:var(--down-soft); color:var(--down); border:1px solid var(--down)}
.pill.depth{background:var(--surface-2); color:var(--ink-3)}
.up{color:var(--up)} .down{color:var(--down)} .dim{color:var(--ink-3)}
.tag{font:500 9.5px "IBM Plex Mono",monospace; letter-spacing:.04em; text-transform:uppercase; padding:0 4px;
  border-radius:3px; border:1px solid var(--line-strong); color:var(--ink-3); margin-left:5px}
.tag.warn{border-color:var(--down); color:var(--down)}
.meter{display:inline-flex; align-items:center; gap:6px; justify-content:flex-end}
.meter .tr{width:46px; height:5px; border-radius:3px; background:var(--surface-2); overflow:hidden}
.meter .fl{display:block; height:100%; background:var(--accent)}
.method{max-width:78ch; color:var(--ink-2)}
.method h2{margin-top:18px; color:var(--ink)}
.method p{margin:6px 0}
.method code{font-family:"IBM Plex Mono",monospace; font-size:12.5px}
.bartbl{max-width:720px; margin-top:8px}
.slots{display:flex; align-items:flex-end; gap:4px; height:46px; margin:6px 0 18px}
.sb{position:relative; flex:1; height:44px; display:flex; align-items:flex-end; background:var(--surface-2); border-radius:3px}
.sbf{display:block; width:100%; background:var(--accent); border-radius:3px}
.sb i{position:absolute; bottom:-15px; left:0; right:0; text-align:center; font:normal 10px "IBM Plex Mono",monospace; color:var(--ink-3)}
.subh{font-family:Archivo,sans-serif; font-size:13px; margin:22px 0 4px}
td.heat{color:var(--ink)}
@media (max-width:520px){ h1{font-size:20px} .wrap{padding-inline:12px} }
@media (prefers-reduced-motion:reduce){*{transition:none!important}}
</style>)---"

  body_bits <- paste0(r"---(<div class="wrap">
  <header>
    <h1>Galletti Keeper Outlook</h1>
    <div class="prov" id="prov"></div>
  </header>
  <p class="lede" id="lede"></p>
  <div class="summary" id="summary"></div>
  <nav class="tabs" id="tabs" role="tablist" aria-label="Sections"></nav>
  <div id="panel"></div>
</div>
<script id="data" type="application/json">)---", json, r"---(</script>
<script>
(function(){
  var D = JSON.parse(document.getElementById("data").textContent), M = D.meta;
  function esc(s){ return String(s).replace(/[&<>"]/g, function(c){
    return {"&":"&amp;","<":"&lt;",">":"&gt;","\"":"&quot;"}[c]; }); }
  function k(v){ return v==null ? "" : (Math.abs(v)>=1000 ? (v/1000).toFixed(1)+"k" : String(v)); }
  function signed(v){ if(v==null) return "<span class=\"dim\">&mdash;</span>";
    return "<span class=\""+(v>0?"up":(v<0?"down":"dim"))+"\">"+(v>0?"+":"")+k(v)+"</span>"; }
  function pct(v){ if(v==null) return "<span class=\"dim\">&mdash;</span>";
    return "<span class=\"meter\">"+Math.round(v*100)+"%<span class=\"tr\"><span class=\"fl\" style=\"width:"+
      Math.round(v*100)+"%\"></span></span></span>"; }
  function pill(v){ var c = /^sell/.test(v) ? "sell" : v==="core keeper" ? "core" : v==="likely keeper" ? "likely" :
      v==="bubble" ? "bubble" : "depth"; return "<span class=\"pill "+c+"\">"+esc(v||"")+"</span>"; }
  function rd(v){ return v==null ? "<span class=\"dim\">&mdash;</span>" : "R"+v; }

  document.getElementById("prov").textContent = "built " + M.generated + " · " + M.n_sims +
    " simulated seasons from the " + M.sim + " run · weeks 1-" + (M.first_sim_week-1) + " banked";
  document.getElementById("lede").innerHTML =
    "Who is worth keeping in 2027, what each player costs to keep (this year&rsquo;s draft round minus one, " +
    "and the round follows him through drops and trades), and which of your players to sell to contenders now " +
    "for cheap 2027 keepers. <b>"+esc(M.my_team)+"</b> projects to hold <b>"+M.n_keep+"</b> positive keepers, " +
    "ranking <b>"+M.my_rank+" of "+M.n_teams+"</b> on 2027 keeper surplus.";

  var slotBars = D.my_slots.map(function(s){
    return "<span class=\"sb\" title=\"slot "+s.slot+": "+Math.round(s.p*100)+"%\"><span class=\"sbf\" style=\"height:"+
      Math.max(2, Math.round(s.p*100))+"%\"></span><i>"+s.slot+"</i></span>"; }).join("");
  var top4 = D.my_slots.filter(function(s){ return s.slot<=4; }).reduce(function(a,s){ return a+s.p; }, 0);
  var sells = D.sell.slice(0,3).map(function(r){ return esc(r.player); }).join(", ");
  var bestPair = D.pairs.length ? D.pairs[0] : null;
  document.getElementById("summary").innerHTML =
    "<div class=\"card\"><div class=\"lab\">Your likely 2027 keepers</div><div class=\"big\">"+M.n_keep+"</div>"+
      "<div class=\"txt\">"+esc(M.keep_list||"none")+"</div></div>"+
    "<div class=\"card\"><div class=\"lab\">Your 2027 draft slot</div><div class=\"slots\">"+slotBars+"</div>"+
      "<div class=\"txt\">"+Math.round(top4*100)+"% you miss the playoffs and pick 1-4, set by the toilet bowl (its winner picks first). Early slots make odd-round keepers harder and even-round keepers easier.</div></div>"+
    "<div class=\"card\"><div class=\"lab\">Top sells</div><div class=\"big\">"+D.sell.length+"</div>"+
      "<div class=\"txt\">"+(sells||"none")+". Value to a contender this year, no keeper value to you next year.</div></div>"+
    "<div class=\"card\"><div class=\"lab\">Best-scoring pairing</div><div class=\"big\">"+
      (bestPair ? esc(bestPair.give)+" &rarr; "+esc(bestPair.get) : "&mdash;")+"</div>"+
      "<div class=\"txt\">"+(bestPair ? "with "+esc(bestPair.owner)+" ("+esc(bestPair.posture)+"); "+
        esc(bestPair.get)+" keeps at "+esc(bestPair.get_cost)+". Priced on FantasyCalc values, not simulated." : "")+"</div></div>";

  var POS = ["ALL","QB","RB","WR","TE"];
  var TABS = [
    {id:"mine", h:"Your roster", rows:D.mine, extra:"byslot", note:"Every player on your roster: what he costs to keep next year, the board rank the pick you would give up is expected to return (the bar, averaged over the slots you are likely to pick from), and his projected rank a year from now. <b>P(keep)</b> is the share of simulated futures where he beats the bar. <b>Option</b> is what the right to keep him is worth today: you keep him only in the futures where he is worth it, so it is never negative.",
     cols:[["player","Player","name"],["verdict","Verdict","pill"],["drafted","Drafted","draft"],["cost","Keep '27","cost"],
           ["bar","Bar","int"],["proj","Proj rank","int"],["p_keep","P(keep)","pct"],["option","Option","k"],
           ["surplus","Surplus","signed"],["redraft","2026 value","k"],["why","Why","wide"]], sort:"option"},
    {id:"sell", h:"Sell", rows:D.sell, note:"Players with real 2026 trade value (FantasyCalc redraft) who are not in your projected 2027 keeper set. Contenders pay for this year; you are paying for next year.",
     cols:[["player","Player","name"],["redraft","2026 value","k"],["option","2027 option","k"],["p_keep","P(keep)","pct"],["cost","Keep '27","cost"],["age","Age","int"]], sort:"redraft"},
    {id:"buy", h:"Buy", rows:D.buy, note:"The best 2027 keeper options on other rosters, priced as if he were on your roster. <b>Costs you</b> is the round you would actually forfeit: he joins your projected keeper set ("+esc(M.my_rounds)+") and if his round is already taken, one of them bumps a round earlier. <b>P(keep)</b> and <b>2027 option</b> use that round at your likely draft slot, not his owner&rsquo;s. Option is excess value in FantasyCalc points: the average of max(his value next year &minus; the value of that pick, 0) across simulated futures. <b>Owner option</b> is the same number on his current team. When your set is already full, a newcomer has to push out your weakest keeper by median surplus (<b>Bumps</b>); <b>2027 option (net)</b> subtracts that keeper<b>Owner option</b> is the same number on his current team.rsquo;s option, and <b>Before bump</b> is the figure without it. Low 2026 value and <i>owner keeps: no</i> are the easiest to pry loose.",
     cols:[["player","Player","name"],["owner","Owner","txt"],["posture","Posture","txt"],["cost","Keep '27","cost"],["costs_me","Costs you","cost"],["replaces","Bumps","txt"],
           ["p_keep","P(keep)","pct"],["option","2027 option (net)","k"],["option_gross","Before bump","k"],["owner_option","Owner option","k"],["redraft","2026 value","k"],["owner_keeps","Owner keeps","bool"],["age","Age","int"]], sort:"option"},
    {id:"wire", h:"Waiver wire", rows:D.fa, note:"Free agents who could be kept next year. The price follows the drafted round, so a dropped late-round pick keeps at that round minus one; a player nobody drafted (<i>ADP</i>) is priced off next year&rsquo;s ADP and brings no real discount. <b>Costs you</b> is the round you would actually forfeit once he joins your projected keeper set ("+esc(M.my_rounds)+"), after any bump. <b>Bar</b> is the board rank that pick is expected to return at your likely slot. <b>Margin</b> is in rank places: bar minus his FantasyCalc dynasty rank. <b>Excess</b> is the same gap in FantasyCalc value points, the unit the 2027 option uses elsewhere on this page, so a +20 margin in round 3 shows as far more excess than +20 in round 16. If your keeper set is full he pushes out your weakest keeper (<b>Bumps</b>), and <b>Net of bump</b> subtracts that keeperso a +20 margin in round 3 shows as far more excess than +20 in round 16.rsquo;s median surplus. <b>P(keep) est.</b> is not simulated: it reads each free agent off a fit of P(keep) on the rostered players (rank inside the bar, age, position; R&sup2; "+M.pk_r2+"). Usage and market columns come from the "+esc(M.fa_source||"")+" waiver watchlist.",
     cols:[["player","Player","name"],["team","Team","txt"],["drafted","Drafted","draft"],["cost","Keep '27","cost"],["costs_me","Costs you","cost"],["replaces","Bumps","txt"],
           ["dyn","Dyn rk","int"],["bar","Bar","int"],["margin","Margin","signed"],["excess","Excess","signed"],["excess_net","Net of bump","signed"],["p_keep","P(keep) est.","pct"],["own","Own%","int"],
           ["adds","Adds","k"],["snap","Snap%","pctplain"],["xfp","xFP/g","num1"],["redraft","2026 value","k"],
           ["age","Age","int"]], sort:"excess_net"},
    {id:"pairs", h:"Pairings", rows:D.pairs, note:"One-for-one swaps with contender and bubble teams: your sell for their keeper, only at a position they are short at. <b>Their 2026 gain</b> is the redraft value they add; <b>your 2027 gain</b> is the keeper option value you add. <b>Ratio</b> is their 2026 gain per unit of 2027 value they give up. These run on FantasyCalc values only; test a real offer in the trade tools before sending it.",
     cols:[["owner","Owner","txt"],["give","You give","txt"],["get","You get","txt"],["get_cost","Costs you","txt"],["replaces","Bumps","txt"],
           ["their_2026","Their 2026 gain","k"],["my_2027","Your 2027 gain","k"],["ratio","Ratio","num2"],["owner_keeps","Owner keeps","bool"]], sort:"my_2027", noPos:true},
    {id:"league", h:"League", rows:D.league, note:"Each franchise&rsquo;s likely 2027 keeper set: the best players by surplus, up to 8 keepers and one from this year&rsquo;s rounds 1-2, with collisions bumped up a round. Surplus and option are summed over the set.",
     cols:[["rank","#","int"],["franchise","Franchise","txt"],["posture","Posture","txt"],["playoff","Playoff 2026","pct"],
           ["slot","Exp. slot","num1"],["keepers","Keepers","int"],["surplus","Surplus","k"],["option","Option","k"],["list","Set","wide"]], sort:"surplus", noPos:true},
    {id:"method", h:"How it works"}
  ];
  var st = {tab:"mine", pos:"ALL", sort:{}, dir:{}};
  try { var h = (location.hash||"").slice(1); if(TABS.some(function(t){return t.id===h;})) st.tab = h; } catch(e){}

  function cellFor(r, c){
    var v = r[c[0]], t = c[2];
    if(t==="name") return "<td class=\"name\"><span class=\"pos\">"+esc(r.pos)+"</span>"+esc(v)+
      (r.top2 ? "<span class=\"tag warn\" title=\"a round 1-2 pick: only one can be kept\">top-2</span>" : "")+
      (r.source==="adp" ? "<span class=\"tag\" title=\"never drafted: priced off next year's ADP\">ADP</span>" : "")+"</td>";
    if(t==="pill") return "<td class=\"txt\">"+pill(v)+"</td>";
    if(t==="wide") return "<td class=\"txt wide\">"+esc(v==null?"":v)+"</td>";
    if(t==="txt") return "<td class=\"txt\">"+esc(v==null?"":v)+"</td>";
    if(t==="pct") return "<td>"+pct(v)+"</td>";
    if(t==="signed") return "<td>"+signed(v)+"</td>";
    if(t==="bool") return "<td class=\"txt\">"+(v==null?"":(v?"yes":"<b>no</b>"))+"</td>";
    if(v==null) return "<td class=\"dim\">&mdash;</td>";
    if(t==="k") return "<td>"+k(v)+"</td>";
    if(t==="cost") return "<td>R"+v+"</td>";
    if(t==="draft") return "<td>R"+v+(r.kept?" <span class=\"dim\" title=\"kept this year\">K</span>":"")+"</td>";
    if(t==="num2") return "<td>"+v.toFixed(2)+"</td>";
    if(t==="num1") return "<td>"+v.toFixed(1)+"</td>";
    if(t==="pctplain") return "<td>"+Math.round(v*100)+"</td>";
    return "<td>"+esc(v)+"</td>";
  }
  function drawTable(T){
    var key = st.sort[T.id] || T.sort, dir = st.dir[T.id] || -1;
    var rows = T.rows.filter(function(r){ return T.noPos || st.pos==="ALL" || r.pos===st.pos; });
    rows.sort(function(a,b){ var x=a[key], y=b[key];
      if(x==null && y==null) return 0; if(x==null) return 1; if(y==null) return -1;
      if(typeof x==="string") return dir*x.localeCompare(y);
      return dir*(x-y); });
    var head = T.cols.map(function(c){ var on = c[0]===key;
      return "<th"+(on?" aria-sort=\""+(dir>0?"ascending":"descending")+"\"":"")+"><button type=\"button\" data-k=\""+c[0]+"\">"+
        c[1].replace("'","&rsquo;")+(on?(dir>0?" &#9650;":" &#9660;"):"")+"</button></th>"; }).join("");
    var body = rows.map(function(r){ return "<tr>"+T.cols.map(function(c){ return cellFor(r,c); }).join("")+"</tr>"; }).join("");
    var chips = T.noPos ? "" : "<div class=\"controls\" id=\"poschips\">"+POS.map(function(p){
      return "<button type=\"button\" class=\"chip\" data-pos=\""+p+"\" aria-pressed=\""+(p===st.pos)+"\">"+p+"</button>"; }).join("")+"</div>";
    return "<p class=\"note\">"+T.note+"</p>"+chips+"<div class=\"tablewrap\"><table><thead><tr>"+head+"</tr></thead><tbody>"+
      (body || "<tr><td class=\"txt dim\" colspan=\""+T.cols.length+"\">Nothing here.</td></tr>")+"</tbody></table></div>"+
      (T.extra==="byslot" ? drawBySlot() : "");
  }
  // the same players if you picked from each slot: shaded by P(keep), and the
  // column you are most likely to pick from is marked
  function drawBySlot(){
    var n = M.n_teams, best = D.my_slots.reduce(function(a,s){ return s.p>a.p ? s : a; }, {p:-1});
    var hd = "<th><button type=\"button\">Player</button></th><th><button type=\"button\">Keep &rsquo;27</button></th>";
    for(var s=1; s<=n; s++){
      var p = (D.my_slots.filter(function(x){ return x.slot===s; })[0]||{p:0}).p;
      hd += "<th><button type=\"button\" title=\"you pick here in "+Math.round(p*100)+"% of simulations\">Slot "+s+
        (s===best.slot?" &#9733;":"")+"<br><span class=\"dim\">"+Math.round(p*100)+"%</span></button></th>";
    }
    var rows = D.by_slot.map(function(r){
      var cells = "<td class=\"name\">"+esc(r.player)+"</td><td>R"+r.keep_cost+"</td>";
      for(var s=1; s<=n; s++){ var v = r["slot"+s];
        cells += v==null ? "<td class=\"dim\">&mdash;</td>" :
          "<td class=\"heat\" style=\"background:color-mix(in srgb, var(--accent) "+Math.round(v*55)+"%, var(--surface))\">"+Math.round(v*100)+"%</td>"; }
      return "<tr>"+cells+"</tr>"; }).join("");
    return "<h2 class=\"subh\">P(keep) at each draft slot</h2><p class=\"note\">What each player&rsquo;s chance of beating his pick would be if you picked from that slot next year. Picking first makes a round-1 keeper have to beat the 1.01, but your round-2 pick becomes the 16th overall. The percentage under each slot is how often the simulations put you there.</p>"+
      "<div class=\"tablewrap\"><table><thead><tr>"+hd+"</tr></thead><tbody>"+rows+"</tbody></table></div>";
  }
  function drawMethod(){
    var n = M.n_teams, bh = "";
    for(var s=1; s<=n; s++) bh += "<th><button type=\"button\">Slot "+s+"</button></th>";
    var bar = D.bar.map(function(b, i){
      var bs = D.bar_slot[i], c = "";
      for(var s=1; s<=n; s++) c += "<td>"+bs["slot"+s]+"</td>";
      return "<tr><td>R"+b.round+"</td><td>"+b.bar+"</td>"+c+"<td>"+(b.raw==null?"&mdash;":b.raw)+"</td><td>"+b.n+"</td></tr>"; }).join("");
    return "<div class=\"method\">"+
      "<h2>The price</h2><p>Next year a player costs the round he went in this year&rsquo;s draft minus one, floored at round 1. That round follows him through drops, waivers and trades, so a dropped late-round pick is a cheap keeper for whoever claims him. Players nobody drafted are priced off next year&rsquo;s ADP, which gives no discount. Only one keeper may come from this year&rsquo;s rounds 1-2, at most 8, and two keepers in the same round bump one up a round.</p>"+
      "<h2>The bar</h2><p>About 50 players never reach the draft, so a pick returns a much worse player than its number suggests. The bar is the board rank a pick actually returned in this year&rsquo;s draft (non-keeper picks, Sleeper superflex ADP as of 2026-08-22), smoothed by a log-log fit on overall pick because each round has only 3-7 such picks. It is read at the pick the keeper&rsquo;s team would hold, so the slot matters: in a snake the 1.01 team also holds pick 16.</p>"+
      "<h2>The slot</h2><p>Next year&rsquo;s order follows this year&rsquo;s finish. Playoff teams pick 5-8, champion last. The other four pick 1-4 by the toilet bowl, a bracket among them seeded by record (5v8, 6v7), whose winner takes the 1.01. Each simulated season plays both brackets out, two weeks per round as in the playoffs, using that team&rsquo;s own simulated weekly scores. A player&rsquo;s P(keep) uses his team&rsquo;s slot in the same simulated season as his own projection.</p>"+
      "<div class=\"tablewrap bartbl\"><table><thead><tr><th><button type=\"button\">Round</button></th><th><button type=\"button\">Mid-round</button></th>"+bh+"<th><button type=\"button\">Raw median</button></th><th><button type=\"button\">Picks</button></th></tr></thead><tbody>"+bar+"</tbody></table></div>"+
      "<h2>The projection</h2><p>Each player&rsquo;s rank a year from now comes from the package&rsquo;s dynasty outlook: for each of "+M.n_sims+" simulated seasons (weeks 1-"+(M.first_sim_week-1)+" as actually played, the rest simulated), it draws his next-year rank from historical players with a similar position, age, rank and season. Ranks are on the FantasyCalc superflex dynasty ladder; the bar is on a redraft ADP ladder, which rates young players a little lower and veterans a little higher, so read young players&rsquo; P(keep) as slightly generous.</p>"+
      "<h2>Limits</h2><p>Pairings use FantasyCalc values, not simulated wins, and ignore roster fit beyond the contender&rsquo;s weakest positions. Opponents&rsquo; keeper sets are the greedy best by surplus; real managers will differ.</p></div>";
  }
  function draw(){
    document.getElementById("tabs").innerHTML = TABS.map(function(t){
      var n = t.rows ? t.rows.length : t.n;
      return "<button type=\"button\" class=\"tab\" role=\"tab\" data-t=\""+t.id+"\" aria-selected=\""+(t.id===st.tab)+"\">"+
        t.h+(n!=null?"<span class=\"cnt\">"+n+"</span>":"")+"</button>"; }).join("");
    var T = TABS.filter(function(t){ return t.id===st.tab; })[0];
    document.getElementById("panel").innerHTML = T.id==="method" ? drawMethod() : drawTable(T);
  }
  document.getElementById("tabs").addEventListener("click", function(e){
    var b = e.target.closest("[data-t]"); if(!b) return; st.tab = b.dataset.t; draw(); });
  document.getElementById("panel").addEventListener("click", function(e){
    var p = e.target.closest("[data-pos]"); if(p){ st.pos = p.dataset.pos; draw(); return; }
    var b = e.target.closest("th button[data-k]"); if(!b) return;
    var kk = b.dataset.k, id = st.tab;
    if((st.sort[id]||TABS.filter(function(t){return t.id===id;})[0].sort)===kk) st.dir[id] = -(st.dir[id]||-1);
    else { st.sort[id] = kk; st.dir[id] = /^(player|owner|give|get|franchise|rank|cost|drafted|bar|proj|age|posture|dyn|team)$/.test(kk) ? 1 : -1; }
    draw(); });
  draw();
})();
</script>)---")

  writeLines(paste0(head_bits, "\n", body_bits), file.path(out, "keeper_outlook_artifact.html"),
             useBytes = TRUE)
  writeLines(paste0('<!doctype html>\n<html lang="en">\n<head>\n<meta charset="utf-8">\n',
                    '<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">\n',
                    head_bits, "\n</head>\n<body>\n", body_bits, "\n</body>\n</html>"),
             file.path(out, "keeper_outlook.html"), useBytes = TRUE)
  invisible(file.path(out, "keeper_outlook.html"))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  rds <- if (length(args)) args[[1]] else
    sort(Sys.glob(here::here("dev", "league_sims", "keeper_outlook", "*", "keeper_outlook.rds")),
         decreasing = TRUE)[[1]]
  message("wrote ", keeper_outlook_page(rds))
}
