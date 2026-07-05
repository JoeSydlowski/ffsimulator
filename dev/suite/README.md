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
| `roster_drivers.csv` | which of *your* players separate your playoff sims from your basement sims (swing = started points in top-quartile vs bottom-quartile team seasons) |
| `war_players.csv` | leave-one-out wins added for every rostered player, under your league's exact rules |
| `trade_targets.csv` | league scan: `value_to_you` vs `value_to_owner` (h2h wins), `surplus` = the trade-asymmetry column, plus dynasty value/trend of each target |
| `trade_offers.csv` | your roster ranked as trade bait: win value to you vs dynasty market value; `sell_score` highlights high-market-value / low-use / at-risk pieces |
| `dynasty_outlook.csv` | per player: current dynasty value and the simulated post-season value distribution (mean/p10/p90, P(rise), P(exit)) |
| `dynasty_capital.csv` | team-level dynasty capital, now and post-season expected |
| `pareto_targets.csv` | trade targets ranked by Pareto front over three goals at once — improve my team most (`value_to_you`), cost least (`dyn_value`), hold value best (`retention`). `front == 1` = the non-dominated shortlist (no target beats them on all three) |

### Pareto targets

The question "which players improve my team most, cost the least, and won't
fall off in value" is a three-objective trade-off. A player is *Pareto-optimal*
(front 1) when no other target is at least as good on all three and strictly
better on one — i.e. there is no strictly-better alternative. Everyone on a
later front is dominated by someone on front 1.

`dev/suite/pareto_targets.R` produces `pareto_targets.csv` and `pareto.png`
(the efficient frontier: cost on x, wins-added on y, point size = retention,
front-1 players connected and labeled). The suite runs a lightweight version
of it automatically; the standalone script also draws the plot and lets you
widen the candidate pool with `FFS_TRADE_TOP_N`.

`ffs_pareto_front(objectives, maximize)` is the general helper — pass any set
of columns and a max/min direction per column to get front numbers back.

## Config notes

- `replacement_level = FALSE` is the right setting for deep dynasty leagues:
  the "best available waiver" mechanism otherwise invents startable free
  agents (e.g. unrostered incoming rookies in the offseason). Set `TRUE` for
  shallow redraft leagues.
- `n_sims`: see `convergence.csv` from `dev/suite/convergence.R`. Playoff-odds
  SE is ~1/sqrt(n): roughly ±3.5% at n=200, ±2.5% at 400, ±1.7% at 800.
  400 is a good default; go 800+ for final published numbers.
- `n_sims_war = 50` — WAR values have run-to-run SD of roughly 0.01 allplay at
  n=40, fine for tiering; don't read the third decimal.
- **WAR scope** (`FFS_WAR_SCOPE`, default `mine`): leave-one-out cost scales
  with the number of players evaluated, so the suite defaults to computing WAR
  only for your roster plus the trade-target shortlist (~1 min) instead of all
  ~380 rostered players (~6 min). Set `FFS_WAR_SCOPE=all` for the whole league.
  Subset results are identical to the full run for the players included.
  Directly: `ff_wins_added(conn, players = c("id1","id2"), ...)`.

### Speed notes

The lineup optimizer (an exact LP per team-week) is the main cost; WAR re-runs
it per player, which is why WAR dominates. Fastest path to answers:
- keep `n_sims_war` low (WAR barely moves past n≈40) and standings `n_sims` at
  400 (±2.4% playoff odds; 800 for final numbers — see `convergence.csv`);
- scope WAR to the players you're actually deciding on (`FFS_WAR_SCOPE=mine`);
- re-run individual pieces off a saved `simulation.rds` (trade eval, dynasty
  outlook) rather than re-simulating.
A vectorized greedy optimizer was prototyped but diverges from the LP on ~0.4%
of team-weeks (bye ties / short rosters), so the exact LP is kept.

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

Both rows report that side's before/after mean wins and playoff odds. For
the dynasty half of the ledger, look the players up in `dynasty_outlook.csv`
(current value, expected post-season value, exit risk) — win-now delta and
dynasty-value delta are reported side by side on purpose; how you weigh them
is the actual trade decision.

Other pieces:

- `ffs_player_value(sim, player_id, franchise_id)` — what a player is worth
  *to any specific roster* (this is not the same as his WAR on his current
  team; that difference is where trades come from).
- `ffs_trade_targets(sim, franchise_id)` — the scan behind trade_targets.csv.
- `ffs_dynasty_outlook(sim)` — the dynasty projection behind the csv.

## Standalone studies

- `dev/suite/convergence.R` — replicate/bootstrap study of how many sims you
  need. Re-run after major model changes.
- `dev/suite/dynasty_backtest.R` — the honesty check for the dynasty
  transition model: trains on 2018→2024, predicts 2025→2026 from actual 2025
  seasons, scores against the real 2026 dynasty rankings. Current verdict:
  exit risk well calibrated; season-quality dose-response ~exact; rank
  intervals near-nominal for WR/QB, slightly narrow for TE and <=23yo.

## Method caveats (short version)

- Projections resample whole historical player-seasons (v3) — calibrated on
  2019–2025 holdouts, but they know nothing about coaching changes, holdouts,
  or September depth charts beyond what FantasyPros ECR encodes.
- Trade values are marginal to the *current* rosters in the sim; after a big
  trade, re-run the suite rather than chaining evaluations.
- The dynasty transition model is trained on 2018+ FantasyPros dynasty ECR:
  it prices age curves and season outcomes the way the dynasty market
  historically has, including the market's biases.
