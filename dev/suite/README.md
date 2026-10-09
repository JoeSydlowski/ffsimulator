# League analysis suite

Self-service tooling built on this fork's simulation engine (v3 trajectory
projections, rank-based start/sit, calibrated knobs). Everything runs from
the repo root with `devtools::load_all()` — the runners do that for you.

## The one command

```
Rscript dev/suite/run_league_suite.R
```

Edit the `config` block at the top (or set env vars `FFS_LEAGUE_ID`,
`FFS_SEASON`, `FFS_MY_TEAM`, `FFS_NSEASONS`). Produces a dated folder under
`dev/league_sims/<league_id>/<date>/`:

| file | what it is |
|---|---|
| `simulation.rds` | full `ff_simulation` object (`return = "all"`) — feed it to the trade functions below |
| `summary_simulation.csv`, `playoff_odds.csv` | projected standings; playoff/top-seed/last-place rates |
| `wins.png`, `rank.png`, `points.png` | the package autoplots |
| `dynasty_outlook.csv` | per player: current dynasty value and the simulated post-season value distribution (mean/p10/p90, P(rise), P(exit)). Current values are anchored to real **FantasyCalc** market values when available, else a synthetic rank-decay curve |
| `dynasty_capital.csv` | team-level dynasty capital, now and post-season expected |
| `roster.csv` | **your roster, one coherent view**: market value + momentum, model outlook (`exp_change`, `p_rise`, `p_exit`), win value **to your team** (`value_to_me`, `playoff_delta_me`, `wins_per_1k`), drivers (`swing`), positional depth rank, a team-coherent `verdict`, `pareto_front` / `dominated_by` (efficiency×trajectory front on your own roster — who is out-wins-per-$'d *and* out-trajectory'd by a rostermate; a pure efficiency lens that **ignores concentration**, so it flags studs too — judge replaceability yourself), the value decomposition `mkt_winnow` (`cur_value − next_value_mean` = what the market charges for this-year production; ≤0 = a *future/growth* asset — judge on trajectory, not win-now) and `fit_residual` (win-now assets only: how much cheaper the market prices his win-now than the **playoff odds** he adds to *your* team warrant — strongly negative = win-now you're paying for but can't cash into playoff odds = sell-high. Based on `playoff_delta`, the true objective, so trustworthy only on rows valued at the standings n: the whole roster, but targets only where `confirmed` — run `FFS_TRADE_TARGET_CONFIRM_N=100`), and — for SELLs — `best_buyers` (the franchises he'd help most, with their playoff-odds gain) |
| `targets.csv` | players rostered elsewhere: value **to your team** (`value_to_you`, `playoff_delta_you`) vs value **to their team** (`value_to_owner`, `playoff_delta_owner`), `surplus`, `mkt_winnow`/`fit_residual` (the win-now/future decomposition — see roster.csv; **acquisition edge = the sheet leads with the highest `fit_residual`**, players whose win-now value to your lineup the market underprices; future assets carry NA), market price/retention/momentum, `pareto_front` (the reallocation frontier: `wins_per_1k` × next-year value change, magnitude folded into the ratio), and the sweet-spot columns (`robust_rank`, `sweet_spot`, `tilt`) plus `fade_flag` (model bearish, market still bidding — don't buy). `confirmed` = the shortlist (frontier + sweet-spot + top gettable) whose value/surplus/playoff were re-priced on the n=2000 standings sim; unconfirmed rows carry search-sim (n=60) value, which is noisier |
| `trades.csv` | complete value-matched packages with a `motive` column: `buy` (the classic builder) and `sell <player>` (deals forced to send that SELL player to his best buyers for similar-priced pieces that improve you). Both sides' re-optimized win + playoff deltas, `win_win`, `future_capital_delta`, `score` |
| `portfolio.csv` | team-level portfolio metrics: posture (contend/bubble/rebuild), capital now→next with downside (Σp10) and upside (Σp90), value-at-risk (Σ value×p_exit), top-3 concentration, age-weighted capital, positional allocation vs league median, capital by verdict |
| `pareto.png` | return-on-capital (`wins_per_1k`) vs next-year value change scatter; Pareto frontier and sweet-spot picks highlighted |
| `war_players.csv` | *(optional, `FFS_RUN_WAR=TRUE`)* generic owner-context leave-one-out WAR — see caveats |

## Trade intelligence (`dev/suite/trade_intel.R`)

One script produces roster.csv / targets.csv / trades.csv / portfolio.csv. It
runs as suite step 3 and also standalone off the newest saved report:

```
Rscript dev/suite/trade_intel.R
```

**Valuation semantics — the one rule.** Every number answers "impact on whose
team?" consistently: players on (or coming to) *your* roster are valued with
`ffs_player_value(sim, p, you)` — leave-one-out **on your team**; players on
(or going to) *another* roster are valued against **that team** (`value_to_owner`,
buyer playoff deltas, trade-eval opponent deltas). Generic `ff_wins_added`
(leave-one-out on whoever currently owns him) is *not* used: it measures
irreplaceability on someone else's bench, which inflates players stuck on thin
rosters and undervalues stars on deep ones.

**Two-stage valuation, both on the real schedule** (taking a good player from
a bubble team with an easy slate should show its true playoff impact):

- **Search** runs on a dedicated fast sim (`FFS_TRADE_NSIMS`, default n=60 from
  `dev/suite/valuation_convergence.R` — value *ranks* plateau there): buyer
  rankings (ordered by the faster-converging wins delta), target scans, package
  enumeration.
- **Report** comes from the n=400 standings sim: the roster values behind
  verdicts, and the deltas of every quoted deal — the best deal per motive plus
  the top buys (`FFS_TRADE_CONFIRM_N`, default 15) are re-evaluated with
  `ffs_trade_eval` on the same 400 draws (`confirmed` column; unconfirmed rows
  keep search-sim numbers and sort below). Search-sim playoff deltas carry
  ~±5% run-to-run SD — good enough to *find* deals, not to *quote* them;
  confirmed deltas are standings-grade (~±2%).

### Roster roles (two axes, not a directive ladder)

Every player is classified on two independent axes — does he help *your*
lineup win (leave-one-out ≥ 0.10 wins), and what is his value doing
(appreciating / holding / declining, the `trajectory` column). Trajectory is
measured **relative to the position's incumbent drift**: the projection
universe loses ~20% of capital per year by construction (next year's rookie
class takes rank slots, exits go to zero), so a raw −20% is *average*, not
fading — declining means ≥7.5% *beyond* the position drift, appreciating ≥7.5%
above it (`rel_change` and `pos_drift` are on the sheet next to the raw
`exp_change`):

|                | appreciating | holding | declining |
|----------------|--------------|---------|-----------|
| **wins now**   | CORE (wins + value) | CORE (wins + value) | RENTAL (win-now, fading) |
| **low wins**   | STASH (appreciating) | TRADE CHIP (surplus)¹ / hold | SELL (fading) |

¹ value ≥ 800 and buried behind a full position room (depth rank > starters+1).

**Urgency comes from the trajectory axis.** A `SELL (fading)` is capital
actually leaving — move it. A `TRADE CHIP` is parked, portable capital (often
worth 2× to a thinner roster): convert it only when the return upgrades your
lineup, otherwise it keeps. A `STASH` is appreciating — don't sell early. A
`RENTAL` is a keep while contending and a sell when banking value. `bust_flag`
(P(exit) ≥ 20%) rides alongside as a column rather than displacing the role;
`depth` = value < 150. This replaced an earlier if-else ladder after it
labeled a young appreciating WR "SELL (redundant)" on a knife-edge
wins-per-$ threshold — buried-but-rising is a chip, not a sale.

The biggest movable pieces (SELL + TRADE CHIP + RENTAL, top `FFS_SELL_MAX`,
default 4, by market value) get `best_buyers` — the three franchises he adds
the most wins to — and the sell matchmaker in trades.csv tries to construct
actual deals with those teams (send him, get back similar-priced pieces that
improve *your* lineup). If no value-matched return improves you, the console
says so: that's a "hold until the market improves," not a forced sale.

### Sweet-spot targets

The reallocation frontier collapses "improve my team / cost little" into one
axis — `wins_per_1k` (wins-added-to-me per $1k, so cost lives inside the ratio)
— against a second axis, next-year value change. Magnitude of value is *not* an
axis: it's fungible via trades, so what matters is return-on-capital and
trajectory. Beyond the front number, the script ranks candidates under a grid
of win-now × growth weightings on those same two axes: `robust_rank` is the
median rank across the grid, `sweet_spot` marks players in the top-10 under
≥75% of weightings (good under *any* strategy), and `tilt` labels weight-
sensitive picks (`win-now pick` / `growth pick`) so you can match them to your
posture. The value axis is the **raw** expected % change (`exp_change`), not the
drift-adjusted one: drift-adjusting forgives structural RB decline while cheap
RBs already win the `wins_per_1k` axis, double-counting them onto the front —
raw change keeps positions balanced (Pareto keeps the corners either way).

### End-of-roster shrinkage

Leave-one-out wins, market values, and any ratio of the two fluctuate wildly
near zero. Sub-150 players are labeled `depth` outright; wins-per-1k$ is shrunk
toward the positional median with weight `value/(value+1000)` (cheap players
need extreme evidence to earn an extreme label); edge and sweet-spot rankings
only run at value ≥ 300.

### Deal knobs (env vars)

- `FFS_TRADE_VALUE_BAND` (default `0.10`) — for **even** trades, how tightly
  send/receive must match on current dynasty value.
- `FFS_TRADE_UNEVEN_SHADE` (default = value band) — for **uneven** trades
  (2-for-1) the multi-player side must *overpay* by 0–this fraction; people
  dislike trading one stud for a package, so consolidation costs a premium.
- `FFS_TRADE_FUTURE_WEIGHT` (default `1`) — how heavily to weigh future
  dynasty value vs win-now, in SD units (`z(win) + w·z(future_capital_delta)`).
  `0` = pure win-now, `1` = equal weight, `2`+ = favour retention. Steers the
  screen, what gets confirmed, and the final ranking. Pure win-now surfaces
  aging-star consolidations (trade young WRs for a 30yo RB) most owners would
  refuse — hence the equal-weight default.
- `FFS_TRADE_MIN_FUTURE` (default `-500`) — hard floor: drop deals that bleed
  more future value than this. Set `-Inf` to disable.
- `FFS_TRADE_TOP_N` (default `50`) — how many screened targets to value.
- `FFS_TRADE_NSIMS` — search sim size (default set by the convergence study).
- `FFS_TRADE_CONFIRM_N` (default `15`) — how many deals get their deltas
  confirmed on the n=400 standings sim.
- `FFS_SELL_MAX` (default `4`) — how many SELLs get buyer scans + matchmaking.

## Waiver watchlist (`dev/suite/waiver_watchlist.R`) — the weekly one

Every ranked player in every league, with each signal that says something
different about him, and no simulation anywhere: it scrapes rather than
simulates, so all six leagues take about ninety seconds.

> **Per-game columns are season averages over games he was ACTIVE for.** Two
> things were wrong here and both are fixed in `ffs_usage_signals()`:
> `load_ff_opportunity` only carries rows for players with expected points, so a
> week of offensive snaps and no touch was missing entirely — 138 of 753
> snap-weeks in 2026 weeks 1–2 (18%), including Calvin Ridley's 64%-of-snaps Week
> 1 — which divided by the wrong denominator and roughly doubled per-game rates
> for boom-or-bust players. The game universe now comes from `load_snap_counts`
> (`offense_snaps > 0`) with the missing weeks added back as real zeros. And
> `snap_pct`/`xfp_g`/`fp_g` used to be the *recent window*, which early in the
> season collapses to a single game — the board was headlining last week under a
> per-game heading. They are now `snap_pct_avg`/`exp_pts_avg`/`fp_avg`/`fpoe_avg`,
> with the week-by-week values still in the cell. `*_recent`/`*_prior`/`*_trend`
> are unchanged and still drive the trend columns.

```
Rscript dev/suite/waiver_watchlist.R
```

| Signal | Column | Why it is there |
|---|---|---|
| rankings | `pos_rank` `ecr` `ecr_delta` | **live** FantasyPros rest-of-season scrape via `ffpros` — `ffs_latest_rankings()` reads DynastyProcess's mirror, which was four days stale the day this was written |
| market | `roster_pct` `trend_add` `trend_drop` | how much of the market owns him, and what Sleeper's whole user base claimed in the last 24h — the fastest signal there is |
| usage | `snap_pct` `xfp_g` `fp_g` | what his team actually gives him; moves first when a role changes |
| value | `redraft_value` `dynasty_value` | FantasyCalc, at each league's own size and format |

Outputs to `dev/league_sims/waivers/<date>/`: `watchlist.csv` (all leagues, one
table) and `waiver_watchlist.html` — a sortable page, one tab per league, with
a Free agents / My roster / Everyone toggle so you can size a claim against
your own bench. The `_artifact.html` twin is the same page shaped for
publishing.

`signal` averages the available components as percentile ranks **within
position**, over free agents only. It is an opening sort, not a projection —
every input is there to be sorted on itself.

Rest-of-season pages are scored per league (`ros-ppr-rb` vs
`ros-half-point-ppr-rb`), and each scrape and FantasyCalc pull is cached across
leagues that share a format.

## Playoff odds (`dev/suite/playoff_odds.R`) — where you stand, and who is selling

Rest-of-season odds for every franchise in every league, and a read on who is
buying and who is selling.

```
Rscript dev/suite/playoff_odds.R                 # n=1000 per league
FFS_NSEASONS=2000 Rscript dev/suite/playoff_odds.R
```

Only the games **left** are simulated, on the real schedule, from live
FantasyPros rest-of-season ranks (`ff_simulate(latest_rankings = ffs_ros_rankings())`).
The record already banked (`ffs_current_record()`, stored on the simulation as
`standings`) is added to every simulated season before seeding on wins, then
points-for. Berths come from the league (`playoff_teams`, read off Sleeper), and
the title odds play a fixed bracket of that size with byes to the top seeds. A
guillotine league (Sleeper type 3) gets survival odds instead
(`ffs_guillotine_odds()`).

Each franchise also gets a **posture** (`ffs_team_posture()`: Contender /
Bubble / Seller (aging roster) / Rebuilding / Long shot) and a **need**
(`ffs_team_needs()`: the position where its simulated starters score furthest
below the league median). From those, per league:

- **targets**: veterans (past a positional age line, or with redraft value at
  least 15% above dynasty value; 500+ redraft value either way) on teams that
  are out of it, flagged where they fill your need
- **pitches**: your veterans, and the contenders whose need they fill

Outputs to `dev/league_sims/playoff_odds/<date>/`: `odds.csv`, `needs.csv`,
`targets.csv`, `pitches.csv` and `playoff_odds.html` (a "My leagues" board, then
one tab per league). Every run appends to `dev/league_sims/playoff_odds/history.csv`,
which is where the page's "since last run" arrows come from.

The simulation is saved as `dev/league_sims/<league_id>/<date>/simulation.rds`,
the file `waiver_wire.R` reuses, so a waiver report run the same day prices
claims on the same base.

## Waiver wire (`dev/suite/waiver_wire.R`) — when you want a move priced

The watchlist says who is worth a look; this says what a specific swap is
worth, in the same currency as everything else: the change in **your**
simulated season. It costs a simulation per league, so it is for the shortlist,
not the weekly sweep.

```
Rscript dev/suite/waiver_wire.R                        # every league in the config
FFS_LEAGUE_ID=1312145437842497536 Rscript dev/suite/waiver_wire.R
```

League type (dynasty/keeper/redraft), superflex, PPR, team count and Sleeper's
position limits are read off the league — the only things to set are the league
ids and `FFS_MY_TEAM` (matched against Sleeper `user_name` first). It reuses
today's `simulation.rds` if the suite already built one. Outputs land in
`dev/league_sims/<league_id>/<date>/waivers/`:

| file | what it is |
|---|---|
| `pairs.csv` | ranked add/drop swaps: `h2h_wins_delta`, `playoff_pct_delta`, `champion_pct_delta` for **your** team, plus `value_delta` (FantasyCalc market swing), the add's ECR / roster% / snaps / xFP, and `score` (the ordering — see below). `exact` marks the rows that got a real swap evaluation; `confirmed` (when the search/confirm split is on) marks the ones re-priced on the standings sim |
| `adds.csv` | what each free agent is worth to your roster on his own |
| `drops.csv` | what dropping each of your players costs on its own (sorted least-costly first) |
| `board.csv` | the whole free-agent board: ROS ECR, FantasyPros roster% and rank movement, snap share, expected fantasy points, FantasyCalc value |
| `watchlist.csv` | unrostered usage risers — including players too unranked for the sim to price, who are a *watch*, not a move |

**How a claim gets priced.** `ffs_attach_free_agents()` simulates the free-agent
pool on the same season/week grid as the league sim and parks it under a phantom
franchise, so a claim is priced by exactly the machinery a trade is:
re-optimize your lineups with him in (and the dropped player out), re-summarise.
This is deliberately **not** `replacement_level = TRUE`, which hands the top free
agents to *every* team at once and prices every claim at ~0 by construction —
the runner refuses to report off such a simulation.

**Two screens, then exact math.** Simulating every free agent against every
droppable player is `n_add × n_drop` re-optimizations. Instead: a cheap
points-vs-your-starters proxy keeps the top `FFS_SCREEN_ADDS` (25) candidates;
each surviving add and each drop is valued once; their sum ranks the pairs; and
the best `FFS_TOP_PAIRS` (30) are re-evaluated as real simultaneous swaps
(`exact = TRUE` — trust those rows first). Measured screen-vs-exact error on
the confirmed rows: ~0.02 wins, max 0.035.

**Runtime.** A counterfactual costs one lineup re-solve per simulated
franchise-week, so it scales with `n` *and* with how often the player would
start: ~7s for a lineup-worthy add at n=400, ~0.5s for a bench drop. A full
league run at the defaults is ~9 minutes of evaluation on top of the
simulation. Two levers if that is too slow on a bigger league:
`FFS_SEARCH_NSEASONS` (< `FFS_NSEASONS`) values everything on a cheap sim and
re-prices only the top `FFS_CONFIRM_N` (10) pairings on the standings sim —
those rows come back marked `confirmed`; and `FFS_WORKERS` spreads the batch
over a PSOCK cluster (results are bit-identical to in-process). **Leave
`FFS_WORKERS=1` unless you have ~2.5 GB of free RAM per worker** — each worker
holds its own copy of the simulation, and a 6-worker run on a 32 GB box paged
so hard it ran ~10x slower than sequential.

**Ordering.** `score` blends win-now and market value as percentile ranks:
championship-odds delta only in a redraft league, and
`(1 − FFS_DYNASTY_WEIGHT)` win-now + `FFS_DYNASTY_WEIGHT` value swing (0.35 by
default) in a dynasty or keeper league, where a claim is partly an asset
acquisition. It is an ordering heuristic, not a unit — the underlying deltas are
the real numbers.

**What it cannot see.** Exclusivity and denial: the model prices the player you
gain, not the opponent who now cannot have him. Nor does it know your FAAB
budget or waiver priority.

## Config notes

- `replacement_level = FALSE` is the right setting for deep dynasty leagues:
  the "best available waiver" mechanism otherwise invents startable free
  agents. Set `TRUE` for shallow redraft leagues.
- `n_sims`: see `convergence.csv` from `dev/suite/convergence.R`. Playoff-odds
  SE is ~1/sqrt(n): roughly ±3.5% at n=200, ±2.5% at 400, ±1.7% at 800.
- `FFS_RUN_WAR=TRUE` re-enables the generic `ff_wins_added` step
  (war_players.csv). **Caveat:** it is leave-one-out on the player's *current*
  owner, so it reflects that roster's depth, not acquisition value — for any
  buy/sell decision use roster.csv / targets.csv instead.

### Speed notes

The lineup optimizer (an exact LP per team-week) is the main cost, and every
`ffs_player_value` / `ffs_trade_eval` call re-optimizes a franchise across all
simulated seasons — cost scales with the valuation sim's n. That's why trade
intelligence runs on a dedicated fast sim (`FFS_TRADE_NSIMS`) sized by
`dev/suite/valuation_convergence.R` rather than the n=400 standings sim.
Re-run individual pieces off a saved `simulation.rds` (trade intel, dynasty
outlook) rather than re-simulating.

## Evaluating a specific trade

```r
devtools::load_all()
sim <- readRDS("dev/league_sims/<league>/<date>/simulation.rds")

# who's who
unique(sim$franchises[, c("franchise_id", "franchise_name")])

ffs_trade_eval(
  sim,
  franchise_a = "05", gives_a = c("4046"),        # you give (player_ids)
  franchise_b = "02", gives_b = c("6786", "8112") # you get
)
```

Both rows report that side's before/after mean wins and playoff odds. For the
dynasty half of the ledger, look the players up in `dynasty_outlook.csv` —
win-now delta and dynasty-value delta are reported side by side on purpose;
how you weigh them is the actual trade decision.

Other pieces:

- `ffs_player_value(sim, player_id, franchise_id)` — what a player is worth
  *to any specific roster* (this is not the same as his WAR on his current
  team; that difference is where trades come from).
- `ffs_trade_targets(sim, franchise_id)` — the scan behind targets.csv
  (value_to_you/playoff_delta_you vs value_to_owner/playoff_delta_owner).
- `ffs_build_trades(sim, franchise_id)` — the package constructor behind
  trades.csv (accepts precomputed `targets`/`dynasty`; `must_send=` forces a
  player into every send package, `opponents=` restricts counterparties —
  the sell-matchmaker mode).
- `ffs_dynasty_outlook(sim)` — the dynasty projection behind the csv; anchors
  to live FantasyCalc values by default.
- `fc_dynasty_values(num_qbs, num_teams, ppr)` — scrape live FantasyCalc
  dynasty values, crosswalked to `fantasypros_id`.
- `ffs_pareto_front(objectives, maximize)` — general non-dominated sorting.

### FantasyCalc market values

The dynasty engine works in rank space and anchors dollars to real
**FantasyCalc** values: `ffs_dynasty_outlook()` (default
`dynasty_values = "fantasycalc"`) sets each player's `cur_value` to his actual
market value and re-estimates the rank→value curve empirically, so projected
`next_value` scales the real current value by the model's predicted rank
change. Per-player trajectory columns (`exp_change`, `rel_change`,
`growth_abs`, `retention`) are computed from the **median** simulated next
value: the curve is convex, so the mean is Jensen-inflated by draw spread
(the 11-holdout backtest found mean exp_change overstates realized change,
QB worst, while the median is ~unbiased — see
`dev/validate_outputs/dynasty_point_calibration.txt`). The mean
(`next_value_mean`) still backs additive capital numbers (portfolio totals,
trade-package future capital, `mkt_winnow`). The suite scrapes both QB formats automatically (set
`FFS_FANTASYCALC=0` for the synthetic curve) and appends dated snapshots to
`dev/data/fantasycalc_values.parquet` via `fc_snapshot_append()` — a growing
value time-series that also carries `trend_30day`, `redraft_value`,
volatility, tier and ADP.

## Dynasty mispricing — the fast signal leading the slow market

Three scripts. The question: which dynasty players are priced wrong *right now* —
falling cheaper than they should, or not rising fast enough with their production.
The existing `ffs_dynasty_outlook()` cannot answer it, because it anchors to the
market price and moves it with `(pos, age, market rank, format, simulated q)` and
sees nothing that has actually happened this season.

**`dev/suite/ecr_archive.R`** → `dev/data/ecr_panel.parquet`. Downloads
DynastyProcess's FantasyPros ECR archive (one 39MB parquet) and reshapes it. This
is the piece that unlocks everything: **323 weekly snapshots of the dynasty boards
(1QB `dynasty-overall`, superflex `dynasty-op`) AND the redraft board, 2020-10-16
to now, median gap 7 days**, keyed on `fantasypros_id`. The bundled
`fp_dynasty_history()` has one snapshot per *season*, and FantasyPros itself only
serves one per year (its `week=` argument is silently ignored), so scraping it
ourselves is not an option — this archive is the only route. Re-run weekly; it is
cached for 3 days (`FFS_ECR_REFRESH=1` forces).

> **Two traps, both learned the hard way, both now handled in the study and the
> board — anything else reading this panel must handle them too.**
>
> 1. **Never difference the stored `pct` across snapshots.** It is `rank/N` within
>    one snapshot and is only safe for ordering *inside* that snapshot. Compare two
>    snapshots by re-ranking on the players **common to both**.
> 2. **`redraft-overall` is two different FantasyPros pages.** It is the preseason
>    draft board (`ppr-cheatsheets.php`) until early September and the
>    rest-of-season board (`ros-ppr-overall.php` ) after it — a different question
>    over a smaller universe (525 → 404 players at the 2026-09-11 switch). Switch
>    dates: 2021-09-10, 2022-09-16, 2023-09-15, 2024-09-27, 2025-09-12, 2026-09-11.
>    A window must not cross one. `fp_page` is carried in the panel so you can check.
>
> Together these are not academic: dropping 121 players inflates every survivor's
> percentile *in proportion to his rank*, which a cross-sectional z-score cannot
> remove. It scored Dalton Kincaid as the biggest 1QB faller in the week his
> redraft ECR went 113.7 → 105.7 — a real rise. Fixing it made the measured effect
> **stronger**, so the bug was adding noise, not manufacturing signal.

**`dev/suite/market_snapshot.R`** → `dev/data/market_panel.parquet`. Weekly
FantasyCalc scrape via `fc_snapshot_append()`, unioned with the values that the
waiver watchlist CSVs happen to have logged since 2026-09-15. Nobody archives
*trade* prices — FantasyCalc and KTC serve only "current", and DynastyProcess's
`values-players.csv` is a strictly monotone transform of its own ECR (spearman
−0.998), so it carries no independent price information. **That series only exists
going forward, from whenever we start logging it**, which is why this runs weekly.
Carries `value_share` (share-of-pool) because the FantasyCalc pool deflates a
couple of percent a week and raw value deltas contain a drift that looks like
signal.

**`dev/suite/leadlag_study.R`** → `dev/validate_outputs/leadlag_study_<date>.txt`.
The gate. For every weekly anchor it measures the redraft percentile move over the
prior *h* weeks against the dynasty percentile move over the *next* h weeks,
partialling out the dynasty board's own momentum, with block-bootstrap inference
(overlapping windows make the naive t meaningless).

> **Result: all six format × horizon cells pass.** In-season partial IC +0.24 to
> +0.28 (block-boot t 12–19), positive in every complete season, decile spread
> positive in 88–98% of anchors. Out of season it roughly halves. **The signal is
> asymmetric**: redraft fallers lose ~1–1.3 dynasty percentile points over the
> following window, risers gain ~nothing — so the edge is mostly *sell before the
> dynasty board marks him down*, consistent with this repo's older finding that
> decline signals are firm and appreciation signals are shaky. Median realised
> slope ≈ 0.07–0.09; that is the shrinkage, already attenuated.

**The age split is the part that changes how you read the board.** Fit per band as
`dyn_fwd ~ b0 + b1·fast`, in-season:

| band | IC | baseline drift `b0` | risers | fallers |
|---|---|---|---|---|
| ≤23 | +0.19 | +0.005 | **−0.003** | +0.017 |
| 24–26 | +0.24 | +0.006 | **−0.007** | +0.019 |
| 27–29 | +0.24 | +0.011 | +0.003 | +0.027 |
| 30+ | +0.21 | +0.016 | +0.012 | +0.028 |

The *ranking* signal holds at every age — fallers always do worse than risers. But
the *level* is age-determined: baseline decay triples from ≤23 to 30+, and the
riser half **flips sign around 27**. Under 27 a redraft riser gains dynasty ground;
over 27 he still loses it, just more slowly. So "redraft rose and dynasty hasn't
followed" is a buy for a 24-year-old and a mirage for a 32-year-old — the dynasty
board is right not to follow him. The board charges each player his band's drift
before calling anything a BUY.

*What it claims:* redraft ECR predicts dynasty ECR — one analyst consensus leading
another. It is a forecast claim about **rankings**. Whether the trade price you
actually pay lags the same way is a separate question that no historical data can
settle; `market_snapshot.R`'s panel is what will answer it.

**`dev/suite/ep_gap_study.R`** → `dev/validate_outputs/ep_gap_study_<date>.txt`. The obvious follow-up —
does the market under-price *opportunity*? Signal is EP/g − FP/g season to date (expected fantasy points
from `load_ff_opportunity`, weeks finished before the anchor); outcome is the same forward dynasty
percentile move.

> **Result: FAILED, and it reverses.** Scoring *below* your expected points predicts your dynasty ranking
> getting **worse**, not better. Incremental IC — net of the board's own momentum *and* of the redraft-ECR
> signal we already trade — is **−0.140 (h=2) / −0.175 (h=4)**, block-boot t −7 to −10, **0 of 6 seasons
> positive**, both formats, all four age bands negative, survivor-only identical. So the
> regression-to-opportunity heuristic is backwards here: outperforming your opportunity (FPOE) is what
> predicts dynasty gains. Two mechanisms fit — efficiency is partly real (hands, YAC, QB, scheme), and
> opportunity *follows* production, so xFP is a lagging snapshot rather than a claim on future usage.
>
> This does **not** contradict the redraft-ECR lead-lag: `corr(gap, redraft ECR move) = +0.22`, i.e. both
> boards move against the underperformer. It does mean an "expected vs actual points" column — the staple
> of weekly analyst columns — can be right about next week's points and still wrong about the dynasty
> price. Any EP-versus-price chart must put FPOE, not the regression gap, on the buy axis.

**`dev/suite/usage_shape_gate.R`** → `dev/validate_outputs/usage_shape_gate_<date>.txt`. The successor to
the EP-gap null. If the gap fails *pooled* because it mixes noise with role loss, then the missing
conditioner might be usage **shape** — a player's role, measured from usage shares only. Tests whether
the `gap × shape` interaction predicts what happens to his *opportunity*. Uses no dynasty data at all
by design, so the panel runs 2009–2025 rather than being capped by the ECR archive.

> **Result: FAILED, across every arm.** Effect sizes (decile spread at a 1-SD gap shock, against a
> 0.5 EP/team-game threshold): RB tilt axis −0.195 (prior-season shape), −0.558 (early-season, but
> CI [−0.83, +0.26]), −0.349 (contemporaneous); WR aDOT axis −0.077 and +0.116 with permutation
> p = 0.73 and 0.46 — a clean null, not an underpowered one. Every bootstrap CI includes zero except
> the contemporaneous WR arm, which is the contaminated one.
>
> **The crude 3× slope spread that motivated this did not reproduce.** That spread (LOVOL-RECV −0.209
> to LOVOL-RUSH −0.611) came from buckets crossing volume with tilt, measured *during* the gap window
> and not residualised on volume. Once shape is (a) measured strictly before the shock, (b) volume-
> residualised and (c) level-controlled, it goes away. Usage at *t* is caused by efficiency at *t−1*,
> so a contemporaneous role label is entangled with the shock by construction — that entanglement was
> the effect.
>
> **Specification sensitivity, reported rather than hidden:** team×season FE is ~5.7 rows per level
> (490 levels / 2,800 RB rows), which absorbs signal and inflates variance — it is why the block-boot
> CIs run 3–4× the permutation SDs. Dropping it raises RB θ from −0.110 to −0.243 (SE 0.136, t −1.8),
> implying a decile spread near −0.43. Still under threshold, still not significant. The verdict does
> not depend on the specification.
>
> **One arm was a design bug, now fixed and re-run:** the WR tilt axis was near-degenerate, because
> WRs barely rush and percentile-ranking a near-constant rush share spreads ties arbitrarily
> (`cor(S, target share)` = 0.80 for RB but 0.14 for WR). WR is now tested on aDOT, the genuinely
> orthogonal receiver role axis.

Per the pre-registered abandon conditions, the market layer was **not built** — an effect this size is
not tradeable even if it were real. What survives from this line of work is the *lesson*, not a signal:
role labels must be measured strictly before the shock they are supposed to condition, or they are
just a restatement of it.

**`dev/suite/mispricing_board.R`** → `dev/league_sims/mispricing/<date>/board_<format>.csv`.
The weekly board, both formats, **no simulation** so it runs in a couple of
minutes. `score = z(fast) − z(slow)` over the same window, both legs measured —
"the price hasn't moved" is an assertion that sits inside weekly noise, so we
compare like with like. Negative = the redraft board marked him down and dynasty
has not caught up (SELL/FADE); positive = the reverse. A **BUY has to clear two
bars** — a positive divergence score *and* an `exp_pct_move` that actually gains
ground once his age band's baseline drift is charged; a riser who fails the second
is labelled `rising, below drift` rather than sold to you as a bargain. The drift
coefficients are read from the newest `leadlag_ageband_*.csv`, matched to the
horizon nearest the realised window and pro-rated to its length, so the board and
the study can never disagree. Columns include `age_band`, `b0`/`b1`,
`exp_pct_move` and `exp_value_chg` (priced through the market's own value ladder),
`price_moving` (is FantasyCalc already repricing him) and the `established` tier split — the cheap end of the board moves most and would
otherwise crowd out the names worth trading. Every run appends to
`dev/league_sims/mispricing/history.csv`: the formula is pre-registered, and that
file is the forward test of it. Expect a real read after ~9 independent windows,
i.e. an off-season verdict, not an October one.

The board **shrinks its own window** rather than crossing the September page
switch, and prints the span it actually used. Right after the switch that means a
one-week window, which is thin and noisy; it widens by a week per snapshot. Treat
early-September boards as provisional.

Knobs: `FFS_MB_WINDOW` (4 weeks, an upper bound not a promise), `FFS_MB_MIN_VALUE`
(1000 — below that, values swing tens of percent on nothing), `FFS_MB_TIER` (0.5),
`FFS_MB_ESTABLISHED` (2500), `FFS_MB_SLOPE` (0.06, only a fallback when no age fit
is on disk), `FFS_MB_SCRAPE=0` to reuse the stored panel.

The board ranks the market; it does not know your roster. Cross the shortlist with
a `trade_intel.R` run when you want to know whether a name helps *your* team, and
take the top ~15 to a manual qualitative pass — there is no text/transcript
pipeline here on purpose.

## Standalone studies

- `dev/suite/convergence.R` — replicate/bootstrap study of how many sims the
  *standings* need. Re-run after major model changes.
- `dev/suite/valuation_convergence.R` — how many sims the *valuations* need:
  rank stability of value_to_you/value_to_owner and playoff-delta SD across
  seeds, by n and by roster tier (top/mid/tail). Sets the `FFS_TRADE_NSIMS`
  default; results in `dev/validate_outputs/valuation_convergence.csv`.
  **2026-07-10 result (JML, real schedule): n=60 is the knee.** Value-rank
  Spearman plateaus there (.82/.87 vs .86/.91 at n=120, flat to 240) and
  top-tier value SD halves from n=30 (.25→.11) then stops improving; the
  residual instability is near-tie swapping among mid/tail players.
  Playoff-delta SD is pure 1/sqrt(n) (±7.3% @30, ±5.4% @60, ±3.3% @240) with
  no knee — deltas never converge at search-sim sizes, which is why the search
  sim only *finds* deals and every quoted number (roster values, deal deltas)
  is computed or confirmed on the n=400 standings sim.
- `dev/suite/dynasty_backtest.R` — the honesty check for the dynasty
  transition model, now **multi-year**: loops holdout pairs (1qb 2019→20 …
  2025→26, superflex 2022→25), pooled + per-holdout metrics
  (`FFS_DYN_HOLDOUTS`/`FFS_DYN_K`/`FFS_DYN_FEAT` env knobs). Verdicts as of
  2026-07-11: exit risk well calibrated, Q dose-response ~exact, rank
  intervals near-nominal on recent holdouts (weaker pre-2021 where training
  windows are short); 30+ intervals still narrow. **Decimal Sept-1 ages
  adopted** (cover80 ≥ integer-age baseline in 11/11 holdouts). **Five
  candidate kernel features all rejected** — years_exp, hard rookie flag,
  draft capital, momentum, ECR-sd each degraded coverage (0/11 holdout wins;
  the hard rookie flag lost even on the rookie subset): with a few hundred
  transitions per position the age+rank+Q kernel is at its conditioning
  capacity, and extra terms starve the pools. The machinery stays as opt-in
  `ffsimulator.dyn_*` options for future re-testing. Rookies DO transition
  differently (at matched rank veterans drift ~+15 ranks while rookies hold,
  and exit less) — the age kernel carries most of it; revisit as an additive
  drift term once more seasons accumulate. A/B logs in
  `dev/validate_outputs/dynasty_backtest_*.log`.

## Method caveats (short version)

- Projections resample whole historical player-seasons (v3) — calibrated on
  2019–2025 holdouts, but they know nothing about coaching changes, holdouts,
  or September depth charts beyond what FantasyPros ECR encodes.
- Trade values are marginal to the *current* rosters in the sim; after a big
  trade, re-run the suite rather than chaining evaluations.
- The dynasty transition model is trained on 2018+ FantasyPros dynasty ECR:
  it prices age curves and season outcomes the way the dynasty market
  historically has, including the market's biases.
- Verdicts are heuristics over calibrated inputs — the thresholds (`TH` in
  trade_intel.R) are editable, and the portfolio/posture context matters:
  a RENTAL is a keep for a contender and a sell for a rebuilder.
