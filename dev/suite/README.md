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
