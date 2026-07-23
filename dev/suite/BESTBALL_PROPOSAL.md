# Project proposal: a Best Ball Mania edge engine

## Thesis

Build a simulation-driven tool that evaluates best-ball rosters and draft decisions for
large-field tournaments (Underdog **Best Ball Mania**, "BBM"), on top of ffsimulator's
existing best-ball scoring engine. The tool's job is to answer one question with real
numbers: **does a given roster / draft strategy clear the rake, and by how much?**

We are *not* extending the redraft championship-odds function — that models a 12-team H2H
bracket, a fundamentally different game. This is a new build that reuses only the scoring
substrate (`ffs_optimise_lineups(best_ball = TRUE)`, `R/06-optimise.R`, plus the resample
pipeline).

The honest framing up front: **best-ball GPP edges are real but thin, and the rake is
large. The central deliverable is therefore a backtest harness** that measures our
strategy against the *actual historical fields* (we have them — the Underdog pick CSVs).
Everything else is in service of that gate. If it doesn't beat the vig in backtest, we
don't play.

---

## The game and the rake (what we're up against)

BBM is a top-heavy, multi-stage GPP. Roughly:

- 12-team drafts, 18 rounds, half-PPR, best-ball lineups (1QB/2RB/3WR/1TE/1FLEX).
  *(Settings from memory — the rules page blocks automated fetch; confirm before build.)*
- **Regular season (wks 1–14):** cumulative points within your 12-team pod; **top 2
  advance** (~1-in-6).
- **Playoffs (wks 15/16/17):** three sequential **single-week** rounds; each round is a
  group **max** — top score(s) advance — culminating in a finals week that pays the
  seven-figure top prize.
- Users enter **many** lineups (the tournament is a portfolio game, not a single bet).

**The vig.** Underdog's rake on these is roughly **~14–15%** (total entry fees vs. prize
pool). Combined with a payout curve where a small fraction of entries win most of the
money, this means: to profit we must be materially better than the field *and* have the
volume/bankroll to let a thin edge resolve. This number is the bar every proposed edge
below has to clear.

---

## Findings to date — the engine-free gate has run (see `dev/bestball/`)

We built the phase-0/1 machinery and ran the gate on **6 seasons** of the real field
(2020–25 ingested to Parquet; 5 full funnels). Headline results — these **revise the plan
below**, so read them first:

- **Game model validated:** advancement = top-2-of-pod by `roster_points` matches the real
  `made_playoffs` flag at **99.98%**. We can replay any year.
- **Stacking is NOT the edge we assumed (~13 tests, `03`–`13`).** The QB↔WR1 weekly
  correlation is real (**+0.37**, co-blow-up ~2× independent), but **same-team stacking
  produces no advancement or ceiling edge** for the regular season, QF, or SF — because best
  ball auto-optimises the weekly lineup, making correlated booms *redundant* vs. diversified
  ones. Confirmed via within-QB fix-the-anchor, a stack-size gradient (concentration *lowers*
  ceiling), WR-vs-TE, QB-tier, wk15 bring-back, archetype, and a random-assignment baseline
  (~54% of "stacking" is incidental; the intentional layer is large, growing, and **taxed
  ~1–1.5 picks of reach** for no payoff → *not* stacking is mildly +EV).
- **One real correlation edge, in the FINALS only.** Checking ETR's manifesto (`13`), wk17
  **game stacks** raise the finals top-10% rate monotonically (**7.3% → 12.3%** by
  game-stack count) — a single winner-take-most week won in the extreme tail. ETR otherwise
  aligns with us (13.7% don't stack ≈ our number; they flag the Kelce/TE confound too).
- **Net engine implication (updated):** correlation is worthless for the accumulation/
  advancement model — **don't build it there** — but a proper engine **should** model wk17
  game-stack correlation for the finals objective. This flips the premise of Q1 below.
- **Still un-de-confounded (the live frontier):** the raw phase-1 signals **positional
  structure** (Zero-RB / WR-heavy looked strong — ETR agrees) and **draft-value captured**
  need the same player-fixed-effects treatment before they can be called edges. **This is the
  next work**, and it's where an actual structural edge is most likely to be.

---

## Q1. How do we make correlation (stacking) work?

> **Empirical revision (see Findings above):** correlation turned out *not* to drive
> advancement — best ball neutralises it — so this is **no longer the priority fidelity gap**.
> Build correlation only into the **finals-week** objective (wk17 game stacks), not the
> accumulation model. The mechanics below still apply *there*.

This is the most important fidelity gap. ffsimulator draws each player's weekly outcome
**independently** from per-`pos`/`rank` pools (`R/01-outcomes_week.R`), so a QB can boom
while his own WR1 is drawn from an average week. In a ceiling-driven tournament that
understates exactly what wins — QB + pass-catchers (and both sides of a shootout) hitting
*together*. Three ways to add it, in rough order of preference:

1. **Gaussian copula over the existing empirical marginals (recommended).**
   Keep ffsimulator's per-rank empirical score pools — they already capture the right-skew
   we care about — but stop drawing them independently. Instead, for each simulated week:
   draw correlated standard normals for the players in a given NFL game (loadings by
   *role*: QB, WR1/2/3, TE, RB, and opponent "bring-back" roles), push them through the
   normal CDF to uniforms, and read each player's score at that uniform quantile of *his*
   empirical pool. This adds correlation **without touching the marginals** and is a
   localized change (a new correlated sampler feeding the existing pipeline).

2. **Game-block / team-week resampling.** Resample real historical *team-weeks* as blocks
   so co-occurring teammate performances travel together and the joint structure is
   empirical, not parameterized. Cleaner in theory, but harder to align: our drafted
   players aren't the historical players, so mapping a rostered offense onto a historical
   team-week archetype is fiddly.

3. **Parametric role-correlation matrix.** Estimate a fixed correlation matrix by role
   from nflfastR box scores (target values are well-known: same-game QB↔primary receiver
   ~0.3–0.5, QB↔TE lower, RB↔own-QB ~0 to slightly negative, opponent bring-back
   ~0.1–0.2) and impose it. Simplest, least faithful to tails/skew; a good sanity check
   against (1).

**Data the correlation layer needs that the sim doesn't currently carry:** each drafted
player's real **NFL team + weekly opponent** (the season schedule), so we know who stacks
with whom and which players share a game. That's a join to nflfastR schedules via
`ffscrapr` — additive, not a rewrite. **Recommendation:** implement (1), calibrate its
loadings so simulated same-game correlations match the historical box-score targets from
(3), and isolate it behind its own interface so the rest of the engine is unaffected.

---

## Q2. Do we need to simulate drafting the whole field? (and the ADP-shift problem)

**Short answer: no — and your ADP-shift instinct is exactly why not.**

Trying to predict ~58k opponent rosters from a moving ADP is both intractable and
unnecessary. Advancement isn't decided by *who* is in the field; it's decided by
**cutlines** — the points needed to (a) finish top-2-of-12 over wks 1–14, and (b) win
each single playoff week. Those cutlines are a **low-dimensional** object (a handful of
score quantiles per round) and are far more stable, year to year, than any individual
roster. So we estimate the cutlines, not the field. Two sources, used together:

- **Backtest / calibration — use the *real* field.** The Underdog pick CSVs *are* the
  realized field, with `roster_points` and `made_playoffs` per entry. We don't simulate
  opponents at all here; we read the true historical cutlines and outcomes straight off
  the data. This is what the backtest gate runs on.
- **Forward-looking — a modest ADP-sampled field.** To evaluate a roster *before* results
  exist, sample a few thousand opponent pods from the **current** ADP snapshot (with
  draft-slot noise), score them through the same engine, and read off cutline quantiles.
  A few thousand pods pins the cutline distribution tightly — you never need 700k, because
  the cutline is a property of the ADP *structure*, not of any single entry.

**How ADP shift is handled:** we re-estimate cutlines under whatever ADP snapshot we're
drafting into. We never try to forecast a September field from a July ADP — we condition
on the ADP regime in force at draft time. ADP movement then stops being a modeling
headache and becomes an **edge** (see Q3, "timing").

One place a field model *does* still matter: **leverage / uniqueness.** In the tail, your
equity depends on not being identical to everyone else. The CSVs give us historical
ownership, so we can score a roster's differentiation — a second-order refinement, not a
v1 requirement.

### What the real data actually gives us (6 years, verified)

The published files cover **six seasons**, in three tiers. **Five (2021–25) have complete
four-round funnels** (regular season + quarterfinals/semifinals/finals, i.e. single-week
playoff scores); 2020 is a minimal single-sheet workbook with advancement depth but no
playoff scores:

| Season | Tournament | Field files | Entries | Playoff scores | Schema tier |
|---|---|---|---|---|---|
| 2020 | BBM I | `BBMData.xlsx` (1 sheet, 753k rows) | ~42k | depth only | **minimal** — no ADP, name-only, round-level draft |
| 2021 | BBM II | reg 452 MB + QF/SF/Finals (57/6.4/0.36 MB) | (smaller) | ✓ | thin — no `player_id`/user/`source` |
| 2022 | BBM III | reg ~1.4 GB + QF/SF/Finals | ~680k | ✓ | thin |
| 2023 | BBM IV | rd1–4 (4.81 GB → 2.7 MB) | ~680k | ✓ | rich (full funnel) |
| 2024 | BBM V | rd1–4 (5.22 GB → 3.3 MB) | ~680k | ✓ | rich |
| 2025 | BBM VI | rd1–4 (5.13 GB → 4.1 MB) | ~680k | ✓ | rich |

Regular-season files are the full field (~680k × 18 picks in later years); the ~1/6 drop to
the quarterfinal file confirms top-2-of-12, then two single-week cuts to a few-hundred-entry
finals. **Five full funnels plus a sixth regular season** is a strong backtest set — a real
train/hold-out split (develop 2020–24, test 2025), not a one-year curve-fit. Note the
tournament **grew ~16×** (2020 ~42k entries → 2025 ~680k), so early years describe a much
softer, smaller field — relevant to any edge-decay story. Three data tiers:

- **Rich (2023–25):** `player_id` (clean nflfastR joins), `source` (auto/ownership),
  full pick numbering, and per-round scores ⇒ everything, including playoff-cutline and
  leverage work.
- **Thin but full funnel (2021–22):** `projection_adp`, full pick numbering, **and**
  single-week playoff scores (QF/SF/Finals dumps) ⇒ regular-season *and* playoff cutlines,
  ADP-value, construction. Missing only `player_id` (⇒ name-match) and user/`source`
  (⇒ no ownership/leverage). Naming is inconsistent — 2022's quarterfinals are split into
  `..._Part_NN` files like its regular season, while semis/finals are single dumps.
- **Minimal (2020):** `BBMData.xlsx`, 8 columns — `team`, `player` (name), `drafted_round`,
  `roster_points` (season total, mean ~1431), `pick_points`, `draft_time`, `playoff_round`
  (text `Lost`/`Round 2/3/4` = advancement depth), `made_playoffs`. **No ADP column**
  (derive empirical ADP from field draft positions), only *round-level* draft position, and
  **no per-round playoff scores** (advancement depth only). Good for regular-season
  cutlines, advancement rates, and coarse construction; not exact reach-vs-value or
  playoff-cutline work.

Per-pick columns we can exploit:
- `roster_points`, `made_playoffs` — outcome + regular-season advancement for **every**
  entry ⇒ the top-2-of-12 cutline is readable directly, no simulation.
- `projection_adp` **at the pick** — the ADP snapshot in force at draft time, i.e. the
  exact conditioning variable for the ADP-shift problem above.
- `source` = `user` vs `auto` — flags autodrafted (pure-ADP, disengaged) entries, a clean
  handle on **ownership / engagement leverage** and on how sharp the beatable field is.
- `overall_pick_number` / `team_pick_number` / `pick_order` — full draft reconstruction,
  so "reach vs. value by round" (Q3d) is measurable, not folklore.

Two things this imposes:
- **Volume.** ~15 GB across the three rd1 files alone means we cannot `fread` them into
  RAM. Ingest with **DuckDB or Arrow** (column-pruned, predicate-pushdown queries), convert
  to Parquet once. A real phase-1 engineering line item, not a footnote.
- **Schema drift across years.** Must be normalized on ingest, and it's not just cosmetic:
  2020 is a 37 MB **xlsx** (single 296 MB sheet) with only 8 columns, **no ADP**, name-only
  players, round-level draft position, and `playoff_round` as text; 2021–22 drop `player_id`
  (⇒ name-match), `username`/`user_id`, `source`, rename `made_playoffs`→`playoff_team`, add
  `bye_week`, and 2021 lives on a different host (`assets.underdogfantasy.com`); 2023 uses
  `clock`/ISO-`Z` timestamps. A per-year adapter to one canonical schema, with graceful
  degradation where columns are absent (features that need `player_id`/`source`/`adp`/
  playoff scores simply don't run on the thin/minimal years).

**Weekly granularity — resolved.** The round files carry no explicit per-week columns, but
they don't need to: `roster_points` is **round-scoped**, so the round partition *is* the
week partition for the playoffs. rd1 `roster_points` is the 14-week cumulative
(~1300–1600); rd2/rd3/rd4 `roster_points` are the single-week scores for wks 15/16/17
(a rd4 finals entry reads ~113, with per-player `pick_points` for that one week). So the
playoff cutlines read straight off rd2–4 `roster_points`, and the only thing the data does
*not* expose is a week-by-week split of the regular season — which the backtest doesn't
need (advancement there is on the cumulative, which we have). Minor ingest note: playoff
files blank out `projection_adp` and carry synthetic pick numbers (rosters carry over, they
aren't redrafted).

---

## Q3. Where are the edges?

### a) Roster construction — the biggest and most defensible edge
This is where a correlation-aware sim earns its keep, because the field systematically
under-optimizes it:
- **Structure / positional allocation** (how many QB/RB/WR/TE; hero-RB vs. zero-RB vs.
  balanced): known +EV shapes exist, and the sim can *rank* them by advance-rate/EV rather
  than by folklore.
- **Stacking:** correlation-aware construction directly raises ceiling — the thing that
  wins single-week playoff rounds. Only valuable once Q1 is in place; with it, this is our
  clearest model-driven edge.
- **Bye/bench construction:** startable lineup every week, no stacked byes.

### b) Projection-vs-ADP value
Taking players our projections like relative to their ADP. Real but hard — ADP is
efficient near the top. The edge concentrates where the market is noisiest (rookies, role
changes, injury returns), i.e. the later rounds.

### c) Reaching for correlation
Sometimes reaching a round or two early to complete a QB stack is +EV: the correlation
ceiling gain exceeds the small ADP "cost." The sim can price this tradeoff exactly —
a specific, defensible reason to reach.

### d) Draft-region edges (reaching vs. value by round)
- **Early (1–3):** market is efficient, players are high-floor studs — little edge, don't
  get cute, take value.
- **Middle (4–8):** structural bets and stack-completion decisions — moderate edge.
- **Late (9–18):** **the largest edge.** Projection-vs-ADP disagreement is widest, and
  best ball rewards weekly spikes, so upside/contingent-value picks (ambiguous roles,
  handcuffs, high-variance rookies) pay asymmetrically. Late-round process compounds.

### e) Timing / ADP-movement edge (doesn't even need the sim)
Because ADP shifts across July→September, drafting **early** lets you get ahead of
predictable news (a backup who will win a role), effectively buying future risers at a
discount. Sharp players exploit foreseeable ADP movement directly.

---

## Q4. Are these edges enough to beat the ~15% vig?

**Honestly: maybe — and we should assume "not proven" until the backtest says otherwise.**

- Documented sharp best-ball players *do* beat the rake, but reported edges are thin
  (roughly single-digit to ~20% ROI *before* variance), realized only over **large
  entry counts**. A thin edge on a top-heavy payout needs volume to show up at all.
- To clear ~15% rake, our entries must be ~15%+ better than field entries. Construction +
  correlation + late-round value edges *can plausibly* stack to that, but it is a
  hypothesis, not a given.
- **The gate:** measure it against the real historical fields — but note this is a bigger
  question than one test, and the cheapest test settles it first. See **Backtesting** below:
  the engine-free field study either shows construction predicts advancement (proceed) or it
  doesn't (stop), before we build anything.

This backtest-first posture is the point of the whole build.

---

## Q5. Contest selection & bankroll

Edge is thin and variance is huge, so contest choice should track bankroll size — smaller
rolls buy lower variance so the edge can realize before ruin. Rough tiers *(heuristics,
tune to real Underdog contest menu and fees)*:

| Bankroll | Play | Why |
|---|---|---|
| **Small** | H2H, 3-/6-man draft pools, small-field tournaments (flat payouts) | Edge realizes fast with low variance; the giant GPP with few entries busts you before variance resolves |
| **Medium** | Core in small/mid-field tournaments + a **measured** BBM allocation | Keep a floor, buy some top-end |
| **Large** | BBM at **volume** (many entries), plus mid-field | Only large samples turn a thin top-heavy EV into realized profit; volume also smooths field-outcome variance |

Sizing rules of thumb:
- Cap total exposure to any single contest structure at a **few percent** of roll.
- Size to survive the drawdown the payout curve implies — in a GPP where <5% of entries
  cash meaningfully, expect long dry stretches; the season's total best-ball spend should
  be money you can watch go to zero for months.
- In BBM specifically, **more entries** is not just more EV — it's lower variance on the
  *field* draw, which is what makes a thin edge bankable. Undercapitalized single-entry
  GPP play is -EV in practice even with a real edge.

---

## Backtesting: what's actually testable (the crux)

"Can we backtest this?" hides **three different tests**, and the cheapest one is the most
decisive — so we sequence them, cheap-and-killing first. The key realization: **the field
data is a natural experiment that already ran.** ~680k real entries × ~6 years drafted every
strategy, and we can look up whether each *actually advanced* against *real* cutlines — so
the most important measurement **needs no simulation engine at all.**

**Test 1 — Empirical field study (no engine). The phase-1 gate.**
Label every real entry with construction features (stack count/type, positional structure,
ADP-value captured, reach magnitude by round); measure realized advance-rate and
points-percentile as a function of those features. This answers "how much to stack / reach /
value-chase / how to allocate positions" as **measured coefficients from reality**. Runs
before any correlation code — **if construction features don't predict advancement here, no
simulator manufactures an edge, and we stop.**

**Test 2 — Engine calibration (needs the sim). Only if Test 1 shows signal.**
Score the whole historical field on vintage-only info and check **calibration** (teams rated
20% advance ~20%) and **discrimination** (does it rank advancers above non-advancers across
680k entries?). The engine's value *over* Test 1 is pricing constructions the field never
tried, and pricing correlation explicitly — but only once it's proven calibrated on a
held-out year, and only if it beats a plain points-projection baseline.

**Test 3 — End-to-end draft agent. Realistic, hardest, last.**
Run our drafting agent through simulated snake drafts against an ADP field, score the
rosters on *realized* player points vs. *real* cutlines, averaged over many draft slots to
remove seat luck. Opponent behavioral realism (how much real drafters deviate from ADP by
round) is **calibrated from the same field data** — closing the loop.

### Four hazards that will actually bite

- **The unit of replication is the season, not the entry (n ≈ 6).** Within a year every
  entry shares one realization of which players boomed, so "stacking worked in 2022" can
  just mean "Burrow–Chase hit." Entry-level n is millions; **edge-*persistence* n is ~6.**
  6/6-year robustness is real; 4/6 is a shrug. No amount of field size fixes this — it's the
  binding constraint.
- **Player-selection vs. structural confound (the deep one).** Construction features
  correlate with *which players* you rostered, and a specific player's realized season can
  masquerade as a structural rule. "Draft elite TE" looked great because *Kelce* boomed
  2020–22 — a **player-selection** win, not a structural one; it dies when he declines. Test 1
  must measure the *structural* component with specific-player realizations stripped out
  (see "Isolating structure from players" below). Trickiest and most important part of Test 1.
- **Field-softness drift.** 2020 was ~42k rec entries; 2025 was ~680k sharks. Edge measured
  on old cutlines overstates current edge — weight recent years, treat the trend as signal.
- **Lookahead.** Any ex-ante test must truncate the engine's resample pools *and* projections
  to pre-season data for the test year. ffsimulator uses all history by default; an
  un-vintaged backtest silently leaks the future and every number is garbage. Enforce
  structurally.

**Metric note:** never score a single sampled team on binary advancement — that's one coin
flip (a good team ≈18% vs. field 16.7%; invisible in one 0/1). Measure advance-*rates* over
many entries, or use **points-percentile-within-pod** as a much higher-signal proxy.

### Isolating *structure* from *players* (Test 1 identification)

Two edges hide in "construction," and only one is repeatable:
- **Player-selection edge** — picking players who beat ADP. Lives in the *projections*; a
  skill you must re-earn every year.
- **Structural edge** — arranging *any* player set to advance more (stacking, positional
  concentration, bye coverage). Independent of *which* players; the transferable thing.

"Draft elite TE" is a player-selection win (correctly valuing Kelce) in a structural
costume — it evaporates when he declines. Phase 1 measures the **structural component only**,
via (strongest → coarsest):

1. **Player fixed effects — the workhorse.** `advanced ~ structure + dummy(every rostered
   player)`. Structural coefficients are then identified only from teams that rostered the
   *same* players but differed structurally, so "you had Kelce" is absorbed into his dummy.
   The variance split (player dummies vs. structural features) also answers a first-order
   question by itself: **is BBM a player-picking contest (edge → projections) or a
   construction contest (edge → structure)?**
2. **Fix-the-anchor natural experiment (cleanest for stacking).** Condition on rostering QB
   X; compare entries that also rostered X's receivers vs. not. Same QB, **same realized
   season** — the gap is pure structure (correlated ceiling), no "you had the good QB"
   contamination. 680k entries give large samples on both sides for popular anchors.
3. **Effect-concentration diagnostic (the Kelce detector).** Decompose each feature's raw
   edge across the specific players/years instantiating it. Concentrated in ≤1–2 names or one
   season → flag as player-luck; diffuse across many players/years → structural.
4. **Cross-year sign-stability.** A structural edge recurs across the six years; player-luck
   shows up once and dies.

**Engine cross-check (with Test 2):** the resample engine draws from ADP/rank pools, not
specific players, so it measures the structural effect *by construction* (it replaces
"Kelce's 280" with "a draw from what TEs at that ADP actually do"). Therefore the **gap
between the empirical edge (Test 1) and the resampled edge (Test 2) is the player-luck
contribution** — a large gap means the historical edge was names, not structure.

*Limit:* player FE identify structure only where within-player structural variation exists —
ample for common players/structures (where the exploitable edge lives), useless for rare
ones. Acceptable.

---

## What we reuse vs. build

| Piece | Status |
|---|---|
| Field-data ingestion (~17 GB CSV/xlsx → Parquet via DuckDB/Arrow) | **New** — required first |
| **Empirical field study** (construction features → realized advancement; engine-free) | **New — the gate** |
| Best-ball weekly scoring (`ffs_optimise_lineups(best_ball = TRUE)`, `R/06-optimise.R`) | **Reuse** |
| Resample → weekly outcome pipeline (`R/01-outcomes_week.R`, projections) | **Reuse** (marginals) |
| Correlated sampler (Gaussian copula, role loadings) | **New** — Q1 |
| Team/opponent/schedule join for stacking | **New** (data join to nflfastR) |
| ADP field builder + cutline estimator | **New** — Q2 |
| Round-advancement model (wk1–14 sum → top-2-of-12; wk15/16/17 group-max) → $EV | **New** |
| **Backtest harness vs. real Underdog fields** | **New — the gate** |

---

## Proposed phases

0. **Ingest.** ✅ **DONE** — 6 seasons (2020–25) in canonical Parquet (`00`/`02` in
   `dev/bestball/R`); arrow streaming for the 5 GB rich years, per-year adapters for schema
   drift, two hosts, split parts.
1. **Empirical field study (Test 1) — the gate.** 🔶 **IN PROGRESS.** Game model validated
   (99.98%). **Stacking sub-question fully answered** (`03`–`13`): no advancement edge, one
   real finals-week game-stack edge (see Findings). **Remaining and next: de-confound
   positional structure (Zero-RB / WR-heavy) and draft-value** with player fixed effects — the
   live candidates for a real structural edge. Decision point still stands: if *structural*
   construction doesn't predict advancement out-of-sample, stop.
2. **Advancement + cutline model.** Partly done — advancement rule validated; formal cutline
   estimation for the single-week playoff rounds still to build.
3. **Correlation layer (Q1) + engine calibration (Test 2).** **Scope reduced by Findings:**
   correlation is a **finals-week-only** concern (wk17 game stacks); skip it in the
   accumulation model. Calibrate against wk17 game/bring-back correlation, not season-long.
4. **Draft agent + edge experiments (Test 3, Q3).** Draft against a behaviorally-calibrated
   ADP field; test construction/value rules on realized points vs. real cutlines and rake, on
   held-out years, with vintaged (no-lookahead) inputs. **De-prioritise stacking rules**
   (answered); prioritise structure/value and finals game-stacking.
5. **Bankroll / contest tooling (Q5).** Turn realized edge + variance into entry-count and
   contest-mix recommendations by bankroll.

Two hard gates: **phase 1** (does *structural* construction predict advancement?) and
**phase 4** (does it clear the vig on held-out years?). Fail either → stop. Phase-1 verdict so
far: stacking is not that edge; structure/value are the remaining hope.

---

## Open questions / risks

- **Settings confirmation:** verify BBM VII lineup/scoring/advancement against the official
  rules (page blocked automated fetch).
- **Data depth:** **six** seasons of regular-season fields (BBM I–VI, 2020–25), **five with
  full playoff funnels** (2021–25) — a strong train/hold-out set, no longer a top risk.
  Residual concerns: the tournament grew ~16× and sharpens each year (an edge trained on
  soft early fields may decay by 2025 — validate on the held-out year, don't over-tune);
  ownership/leverage work is limited to the rich years (2023–25); and 2020 has advancement
  depth but no playoff scores.
- **Data volume + schema drift:** ~17 GB of field files, a second host for 2021, split-part
  files for 2022, and real per-year schema differences force a DuckDB/Arrow ingest + a
  normalization adapter — budget in phase 1.

*(Resolved during scoping: weekly granularity — playoff cutlines read directly off
round-scoped `roster_points` in rd2–4; see the data section.)*
- **Projection quality is the ceiling on edge (b).** If our projections aren't better than
  ADP where it's noisy, several edges evaporate — worth measuring projection-vs-ADP
  accuracy early.
- **Correlation calibration is the riskiest new code** and the highest-leverage; budget
  for getting it wrong the first time.
