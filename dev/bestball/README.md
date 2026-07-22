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
- [x] **Rich-year streaming ingest** (2023 BBM IV, 4.81 GB → 43 s; 677k entries).
- [x] **Fix-the-anchor stack test** on 2023 (nflreadr team match 83.5%).
- [ ] Add remaining years (2020 xlsx, 2022 split-parts, 2024–25 rich) for cross-year replication.
- [ ] Cleaner stack identification: control *receiver* quality too (not just QB); measure the
  ceiling mechanism directly (upper-tail roster_points), not just mean advance rate.

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

## Stacking result (2023 BBM IV, fix-the-anchor)
- **Naive (confounded):** any QB-stack 0.168 vs none 0.156. (Loose definition — ~90% of
  entries have *some* QB↔same-team-catcher overlap by chance; the per-QB test below is cleaner.)
- **Fix-the-anchor (hold the QB fixed):** across 37 QBs with ≥150 entries each side, within-QB
  stack lift is **small and heterogeneous** — median ≈ 0, mean **+0.016**, weighted **+0.013**,
  only 54% of QBs positive.
- **The pattern is the story:** stacking helped where the *receivers hit* (Dak+Lamb **+0.14**,
  Purdy **+0.13**, Stroud **+0.12**, Tua **+0.10**) and hurt where they didn't (Mahomes **−0.05**,
  Wilson/Burrow/Rodgers ≈ **−0.04**; Rodgers tore his Achilles wk 1). So holding the QB fixed
  still leaves a **receiver-realization** confound: a single-year stack "edge" is largely *did
  you stack the right offense*. Net structural benefit in 2023 is mildly positive but not a
  proven edge. **Next:** replicate across years + control receiver quality + test the ceiling
  mechanism (does stacking fatten the upper tail of `roster_points`, holding quality fixed?).
