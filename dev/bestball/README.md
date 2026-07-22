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
- [ ] **Playoff-week ceiling test** — rerun the ceiling test on rd2–4 *single-week* scores
  (the correct venue; season totals wash out weekly correlation via CLT).
- [ ] Add 2020 xlsx + 2022 split-parts for two more regular-season years.
- [ ] Control *receiver* quality (not just QB) for a tighter stack estimate.

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

**Critical caveat / reframe (why this isn't the whole story):** `roster_points` here is a
**14-week sum**, which averages weekly correlation away (CLT) — so this correctly shows
stacking doesn't help you make your pod's top-2, but it *cannot* test the mechanism that
actually wins a top-heavy GPP: does stacking raise **single-week playoff ceiling** and thus
deep-run/win rate? That lives in the **rd2–4 single-week scores** (which we have) and/or the
sim engine (toggle correlation, players held identical). **That playoff-week ceiling test is
the next build** — this regular-season result localizes where the real question must be asked.
