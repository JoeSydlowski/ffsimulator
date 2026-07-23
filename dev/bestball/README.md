# Best Ball Mania edge engine — working directory

Implementation of the proposal in [`../suite/BESTBALL_PROPOSAL.md`](../suite/BESTBALL_PROPOSAL.md).
Data landscape + backtest methodology live there; this README tracks build status.

## Layout
- `R/00_ingest.R` — **phase 0**: thin-year dumps (fread) → canonical Parquet.
- `R/02_ingest_rich.R` — **phase 0 (rich 5 GB years)**: arrow streaming ingest (column-
  pruned) → player dimension + fact parquet. fread would blow RAM.
- `R/01_field_study.R` — **phase 1 (gate)**: engine-free entry reconstruction + descriptive
  advancement analysis.
- `R/03_stacking.R` — **phase 1**: fix-the-anchor structural stack test (de-confounds QB
  quality from stacking) via nflreadr team mapping.
- `data/` — raw dumps + derived Parquet. **Gitignored** (GBs, re-downloadable).

## How to run (from `dev/bestball/`)
```sh
# 1. fetch a year's dump into data/raw/ (example: 2021 regular season, 452 MB)
curl -o data/raw/bbm2021_regular.csv \
  https://assets.underdogfantasy.com/underblog/BBM_II_Data_Dump_Regular_Season_01312022.csv
# 2. ingest -> data/parquet/2021_BBMII.parquet
Rscript R/00_ingest.R
# 3. field study
Rscript R/01_field_study.R
```

## Status
- [x] **Phase 0 ingest** — canonical schema + Parquet writer; per-year `specs` map handles
  schema drift. Validated on 2021 (2.8M picks → 40 MB Parquet, 11× compression).
- [~] **Phase 1 field study** — machinery working on 2021; descriptive/uncontrolled pass done.
  **Confound controls (player FE, fix-the-anchor stacks, concentration diagnostic) not yet built** — that's the real gate.
- [x] **Rich-year streaming ingest** (2023/2024/2025, ~5 GB each → ~45 s/yr; ~675k entries/yr).
- [x] **Fix-the-anchor stack test + ceiling test** (`stack_lib.R`), replicated across
  **4 years** (2021 thin + 2023/24/25 rich) in `04_stack_replication.R`.
- [x] **Playoff-week (single-week) ceiling test** on rd2 (wk15), 4 years — `sd_ratio ≈ 1.0`,
  no weekly ceiling from minimal stacks.
- [x] **Stack-size gradient**, 5 seasons (2021–25) — concentration *lowers* single-week
  variance (`sd_ratio` falls with stack size every year); 2022 (BBM III) ingested.
- [x] **Game-stack / bring-back**, WR-vs-TE, and QB-tier tests (`08`) — all null / no edge.
- [x] **Prevalence by round** (`09`) — stacking ~90% base rate; only slight, confound-sized
  rise to Finals; all winners stacked but so is everyone (survivorship).
- [x] **Raw weekly correlation** (`10`) — QB↔WR1 +0.37, co-blow-up ~2×: the correlation is
  REAL, but best ball's weekly auto-optimisation makes it valueless.
- [x] **Intent + reaching** (`11`) — ~54% of stacking is incidental, but the intentional layer
  is large and growing, and taxed ~1–1.5 picks of reach. Not stacking is mildly +EV.
- [x] **Archetype** (`12`) — no stack archetype helps; stud-WR stacks are the *worst* for ceiling.
- [x] **ETR manifesto check** (`13`) — most claims align; their **finals (wk17) game-stack** edge
  is REAL and we'd missed it (top-10% 7.3%→12.3% by game-stack count). Regular-season same-team
  stacking still no edge. **Two regimes: no edge for advancement, real edge for the finals week.**
- [ ] **Move the gate to the other raw signals** (positional structure, draft-value) with
  player fixed effects — still un-de-confounded and the most likely place for a real edge.
- [ ] Add 2020 (BBM I xlsx) for a sixth regular-season year (no single-week playoff scores).
- [ ] Sim engine (Test 2): toggle correlation with players held identical — the clean arbiter;
  seed copula loadings from the Underdog article (WR1↔WR2 +0.16, bring-back +0.09).

### Note: advancement in rich years
Rich rd1 files leave `made_playoffs = 0`; advancement is derived as **top-2-of-pod by
`roster_points`** (the rule validated at 99.98% on 2021). Thin years carry the real flag.

## First findings (2021, descriptive — NOT yet a proven edge)
- **Rule validated:** 155,375 entries in 12,948 pods, exactly 12/pod × 18 picks; advancement
  = top-2-of-pod by `roster_points` at **99.98%** agreement with `made_playoffs`. Our model
  of the game matches reality.
- **Cutline:** regular-season advance line (2nd-place points) median **1688.8** (P5–P95
  1621–1773).
- **Construction correlates with advancement (raw):**
  - Positional structure advance rates span **0.137 → 0.197** (baseline 0.167); 3-TE and
    2-QB builds skew high, 3-QB/light-RB low.
  - Draft-value captured (`overall_pick − adp`) is **monotonic**: reachiest quintile 0.134 →
    most-value quintile 0.190; same shape for late-round value.
- **Caveat (why this isn't the gate yet):** these are *uncontrolled*. Both signals are
  confounded — structure with *which players* (the Kelce problem), and draft-value with
  *drafter skill* (an engaged user, not an adoptable rule). Isolating the **structural**
  component (player fixed effects, fix-the-anchor stack tests, concentration diagnostic) is
  the next step and the actual go/no-go.

## Stacking result — 4-year fix-the-anchor + ceiling (KEY FINDING)

De-confounded within-QB stacking effect on **regular-season advancement**, by year:

| Year | match | de-conf. advance lift | % QBs +ve | tail_extra (ceiling) |
|---|---|---|---|---|
| 2021 | 0.99 | **+0.002** | 43% | −1.8 |
| 2023 | 0.84 | **+0.013** | 54% | −0.9 |
| 2024 | 0.91 | **−0.014** | 31% | −6.2 |
| 2025 | 0.85 | **+0.012** | 50% | −2.5 |

**Two robust conclusions:**
1. **Stacking is NOT a reliable structural edge for regular-season advancement.** Holding the
   QB fixed, the advance lift averages ≈ 0 and **flips sign** across years (strongly negative
   in 2024); % of QBs helped is a coin-flip (31–54%). It fails cross-year persistence. The
   naive edge (~+0.01–0.02) is mostly QB quality + receiver-realization noise — in the years
   it "worked," stacking just meant *you stacked an offense whose receivers hit* (Dak+Lamb
   +0.14, Purdy +0.13 in 2023; the opposite in 2024).
2. **No ceiling signature in season totals — robustly.** `tail_extra` (P95 lift − median lift)
   is **negative every year**: stacking, where it shifts anything, shifts the *level*, never
   fattening the upper tail.

**Reframe that motivated the playoff-week test:** `roster_points` above is a **14-week sum**,
which averages weekly correlation away (CLT). Correlation *redistributes* points into fewer,
bigger weeks, so it can leave the season-total tail flat while still fattening the
**single-week** tail — the mechanism that wins single-elimination playoff weeks. So we tested
the single week directly.

## Playoff-week (single-week) ceiling — `06_playoff_ceiling.R` (STRENGTHENS THE NULL)

Within-QB single-week (rd2 = wk15) score distribution, stacked vs not, 4 years:

| Year | single-week tail_extra | **sd_ratio** (stacked/unstacked wk variance) |
|---|---|---|
| 2021 | −1.2 | 0.98 |
| 2023 | +0.7 | 1.02 |
| 2024 | −1.0 | 0.99 |
| 2025 | −0.4 | 0.98 |

**`sd_ratio ≈ 1.00` every year is the result:** holding the QB fixed, stacked rosters have
essentially the **same single-week variance** as unstacked ones — no fatter weekly tail, no
extra ceiling. The correlation-ceiling mechanism is **not visible even at the single week**,
across 4 years, at neither horizon.

**Why plausibly:** best ball already auto-optimises the weekly lineup, so ceiling comes from
*whichever* players boom — an unstacked roster spreading pass-catchers across many games has
*more independent* shots at a boom, offsetting a stack's *correlated* boom. Stacking's
theoretical edge is largely neutralised by best ball's own mechanic (a known best-ball
counter-argument, now with field evidence).

## Stack-size gradient — 5 seasons (`07_stack_gradient.R`) — CONCENTRATION *LOWERS* CEILING

Tests whether **bigger** same-team stacks (QB + 1/2/3+ pass-catchers) behave differently
than the binary test, across 5 full-funnel years (2021–25; added 2022 BBM III).

- **Regular-season advance lift** by stack size is **inconsistent** — positive some years,
  negative others, and **3-stacks are negative in 3 of 5 years** (2023/24/25). No reliable
  advancement edge from concentration.
- **Single-week `sd_ratio` (stacked/no-stack weekly variance) DECREASES with stack size in
  *every* year:** ssize +1 ≈ 1.00 → +2 < 1 → +3 ≈ **0.90–0.99**. Concentrating on one team's
  receivers **reduces** your single-week ceiling — the *opposite* of the stack-for-ceiling thesis.

**Mechanism (now well-supported):** best ball auto-optimises the weekly lineup, so ceiling
comes from having **many independent boom sources**. A big same-team stack makes your
catchers boom/bust *together*, so a down week for that offense sinks the whole stack —
*fewer* effective shots at a spike, hence **lower** weekly variance. Diversified "mini
stacks" beat concentrated ones. This matches Underdog's own correlation research
([article](https://underdognetwork.com/football/best-ball-research/correlation-at-ceiling-outcomes-between-teammates-and-their-opponents):
WR1↔WR2 only +0.16; two WRs both 20+ just 1.1% of weeks; "spread mini-stacks, don't
concentrate") — now confirmed at the roster level over 5 seasons.

## More stacking angles — 5 seasons (`08_stack_evidence.R`)

- **(A) QB-WR vs QB-TE:** QB-**WR** advance lift is positive 4/5 years (~+0.004 avg) with
  `sd_ratio ≈ 1.0`; QB-**TE** advance lift ~0 and `sd_ratio < 1` **every year** (0.94–0.99) —
  TE stacks *reduce* ceiling. So QB-WR is mildly preferable and TE stacks are the worst form,
  matching the article's negative TE↔WR2 correlation. Still not an *edge*, just a preference.
- **(B) By QB draft tier:** elite-ADP-QB stack lift = +0.027, −0.003, +0.018, −0.027, +0.010
  across years — **no tier pattern**, year noise dominates. Stacking is **not** bigger for
  highly-drafted QBs.
- **(C) Bring-back / game stack** (own-stack + the QB's *week-15 opponent's* catcher, within
  QB): `sd_ratio ≈ 1.00` every year, wk15 points lift ≈ 0 — **null**, as season-level dilution
  (one matchup per pairing) predicts.

## Prevalence by round — the survivorship check (`09_stack_by_round.R`)

Do stacked teams get over-represented as you go deeper (field → QF → SF → Finals → winner)?

- **Stacking is near-universal:** ~**86–94%** of the *entire field* is "stacked" (≥1 same-team
  catcher). So "the winners were stacked" is almost tautological.
- Stack prevalence rises only **slightly** field→Finals (2022: 85.7%→92.1%; 2023: 89.5%→92.7%;
  2025 basically flat), and `mean_ss` a touch (e.g. 2022 1.36→1.56). A few points — exactly the
  size you'd expect from the confound (stackers draft good offenses), **not** a causal edge,
  consistent with the within-QB null.
- **All 5 winners had a QB-WR stack** (size 1–3); top-10 finalists ~80–100% stacked — but so is
  ~90% of everyone. Textbook survivorship/base-rate illusion.

## Why stacks blow up together yet it doesn't matter (`10_weekly_correlation.R`)

Raw nflverse weekly data, half-PPR, **all teams, no advancement filter** (2021–25):

- Same-team **weekly** correlation is real and strong: **QB1↔WR1 +0.37**, QB1↔WR2 +0.31,
  QB1↔TE1 +0.29 (WR1↔WR2 ≈ 0 — they compete for targets).
- **Co-blow-up:** when WR1 smashes (20+), his QB smashes **61.5%** of the time vs **32.1%**
  base; both-smash-same-week **9.3%** vs **4.8%** if independent (~2×).

**So the correlation is unambiguously real — stacks *do* explode together, on every team,
regardless of playoff luck.** The reconciliation with the null: **best ball auto-optimises the
weekly lineup, so correlated booms are *redundant*, not additive.** When your QB+WR both go for
30 in week 5 you bank one big week — but you'd likely have banked a big week anyway from *some*
of your 18 players. Independent boom sources spread across games give you a big week *more often*;
correlation concentrates booms into *fewer* weeks. The best-ball optimiser (top-8 of 18 each
week) routes around busts, absorbing the joint-variance a stack adds — which is exactly why
`sd_ratio ≈ 1` and stacking yields no edge. **The correlation is real; best ball neutralises its
value.**

## Is stacking intentional, and do people reach for it? (`11_stack_intent.R`)

- **Intentional vs incidental:** the ~90% "stacked" rate is inflated by chance — shuffling
  catchers' NFL teams across the field gives a **~54% random baseline** (with ~2.5 QBs + ~10
  catchers, incidental overlap is common). But observed sits **~35 pts above** random, so
  deliberate stacking is large and **growing every year** (intentional excess +0.32 in 2021 →
  +0.39 in 2024). The field is getting *more* stack-obsessed.
- **Reaching (the stack tax):** stack-completing catchers are drafted **~1–1.5 picks earlier**
  than comparable non-stack catchers (reach +1.7–2.0 vs +0.3–0.7; reached above ADP ~59–61% vs
  ~52%). A real, if modest, cost.
- **So: not stacking is not a disadvantage — it's mildly favourable.** The field pays a rising
  reach tax for a form with no edge; skipping it (draft best-available) fades a −EV behaviour.

## Does it depend on archetype? (`12_stack_archetype.R`)

Split each stack by its best same-team catcher's ADP: **stud_WR** (≤48), **mid_WR** (49–108),
**dart_WR** (>108), within QB, 5 seasons.

- **Advance lift:** no archetype is reliably positive; **stud_WR** (the "stud QB + stud WR"
  case) averages ~0 and is worst in 2024 (−0.033).
- **Single-week ceiling (sd_ratio):** **backwards from the intuition** — stud_WR is the *lowest*
  (~0.96, most below 1), dart_WR the most neutral (~1.00); none exceed 1. Concentrating two
  *expensive* players on one team reduces relative ceiling the most (fewest independent boom
  sources per dollar). "Stud QB + stud WR pop off" is real at the player level (+0.37) but
  *worse* for best-ball ceiling, not better.

## Does ETR's Best Ball Mania Manifesto hold up? (`13_finals_gamestack.R`)

Checked the [ETR manifesto](https://establishtherun.com/best-ball-mania-manifesto-a-guide-to-winning-big-on-underdog-fantasy/)
against our work (they analysed 75,200 BBM3 playoff teams).

**Agrees with us:** stacking is table stakes ("13.7% avoid stacking entirely" ≈ our ~10–15%);
the Kelce/TE confound is real (they flag it too); "live players / advance max teams" is our
diversification mechanism; ADP value matters; it's a volume game with a modest edge
("30 optimally-stacked ≈ 35 random", ~17%).

**The big one we had NOT tested — and it holds up:** ETR's headline is that **game stacks win
the FINALS (week 17), ~+50% win odds**, and are "less beneficial in quarters/semis." Our
bring-back test was at wk15 (quarters) — null, which *matches* them. Testing the **finals**
directly (a player is game-stacked if the entry also holds a pass-catcher on its wk17
opponent), pooled 2021–25: finals-score percentile and top-10% rate **rise monotonically** with
game-stacked players (top-10%: 7.3% at 0 → **12.3% at 7+**; mean finals pts 121 → 130;
cor(n_gs, pctile) +0.084). **A real finals-week edge our same-team/regular-season/QF tests
correctly did not see.** *(Descriptive, not de-confounded — but mechanism-consistent.)*

**Why it fits our mechanism:** the finals are a single, winner-take-most week decided in the
extreme right tail. Same-team stacks lose to diversification over 14 weeks and even in the QF
"beat your group" week — but to post a top-1-of-500 finals score you need to catch a *whole
game going nuclear*, and a game stack (both sides) is the correlated bet that delivers that
tail. Correlation is worthless for accumulation/advancement, valuable for the finals ceiling.

**One genuine disagreement:** ETR likes 3–5 same-team stacked skill players in the *regular*
season; our de-confounded within-QB gradient says same-team stack size doesn't help advance
(3-stacks negative 3/5 yrs). Their claim reads like the confounded EV comparison (stackers draft
good offences) our player-FE method dismantles. We stand by the regular-season null; the finals
game-stack edge is a separate, real thing.

## Final verdict on stacking (revised after the ETR check)

Two regimes, and they differ:

1. **Accumulation & advancement (regular season → QF → SF): same-team stacking is NOT an edge.**
   Across binary, size-gradient, WR-vs-TE, QB-tier, wk15 bring-back, prevalence-by-round,
   intentional-vs-incidental, reaching, archetype, season-total and single-week tests plus the
   raw weekly correlation (+0.37, real) over 4–5 seasons — no advancement or ceiling edge, because
   best ball's weekly auto-optimisation makes correlated booms redundant vs. diversified ones.
   Deliberate same-team stacking is large, growing, and taxed (~1–1.5-pick reach) for no payoff,
   so **not stacking is mildly +EV** through the QF/SF. (Nuance: prefer WR over TE; a cheap
   dart-WR stack is more ceiling-neutral than a stud-WR one.)
2. **The FINALS (week 17): GAME stacks ARE an edge.** A single winner-take-most week decided in
   the extreme tail — game-stacking both sides of a wk17 shootout raises the finals ceiling
   monotonically (top-10% 7.3%→12.3%), confirming ETR.

**Engine implication (updated):** correlation is worthless for the accumulation/advancement
model, so don't build it in there — but a proper BBM engine **should model wk17 game-stack
correlation for the finals-week objective**. Same-team QB↔WR1 +0.37 (regular weeks) and
game/bring-back correlation (finals) are the loadings that matter, in that second regime only.
