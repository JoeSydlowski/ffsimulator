# Trade-intel field guide: how to read `roster.csv` and `targets.csv`

Every buy/sell call reduces to **three signals**. Everything else was noise and was
removed. There is no black-box composite score — each row's rank traces to a visible
field.

## The three signals

| Field | What it is | Its one job |
|---|---|---|
| **`win_now_value`** | `cur_value − next_value_mean`: the share of his market price that is *this-year* production (also = his expected price melt). | **Gate.** `> 0` = a win-now asset (the framework applies). `≤ 0` = a future asset — ignore win-now, judge on year+1 only. |
| **`win_now_edge`** | How his win-now value to **my** lineup compares to what the market charges (the residual off one league price line, in value units, signed). | **The spine of both boards.** Sells: `< 0` = overpriced to me. Buys: `> 0` = underpriced to me. |
| **year+1** (`exp_change`, `rel_change`, `growth_abs`, `p_rise`, `p_exit`) | His value trajectory next season, vs his position's drift. | **The clock.** Declining + you're contending = sell high. Appreciating = hold / buy. |

`cur_value` sizes the trade and sets the sell *type* (valuable → sell high; cheap →
throw-in). On the roster, **`playoff_add`** (playoff odds this player adds to your lineup,
n=2000) is shown as context — it tempers the studs: a very negative `win_now_edge` with a
*low* `playoff_add` is genuinely redundant; a *high* `playoff_add` means "expensive, but
you rely on him." On targets the raw win/playoff numbers stay hidden — `win_now_edge`
already prices win-now against cost and breaks sort ties internally.

## The decision plane: `win_now_edge` × trajectory × `cur_value`

`win_now_edge < 0` = the market overvalues him for *your* lineup (a reallocation
candidate). What you *do* then depends on trajectory (urgency) and `cur_value` (sell-high
vs throw-in):

| | declining | holding | appreciating |
|---|---|---|---|
| **edge ≤ −250, cur ≥ 3000** | **SELL HIGH** (urgent) | **SELL HIGH** (value arb) | STASH |
| **edge ≤ −250, 1.5k–3k** | **DUMP** | PARKED CHIP | STASH |
| **edge ≤ −250, cur < 1.5k** | **SWEETENER** | PARKED CHIP | STASH |
| **−250 < edge < 0** | **CASH RENTAL** | **PARKED CHIP** | STASH |
| **edge ≥ 0** | **CORE** (keep) | **CORE** | **CORE / STASH** |
| **edge > 0** (target) | win-now rental buy | solid buy | **PREMIUM buy** |

Key idea: **trajectory decides urgency; `win_now_edge` decides *whether he's a sell at all*
and the *floor*; `cur_value` decides sell-high vs throw-in.** A *positive* edge means he's
underpriced to you — **keep him even if he's declining** (that's why the declining QBs with
+edge read CORE, not "sell").

## SELL board order (`roster.csv`, top = act first)

1. **SELL HIGH** — `edge ≤ −250` & `cur_value ≥ 3000`: valuable *and* market-overvalued for your lineup → cash at market (someone without your depth pays sticker). Declining = urgent, holding = a pure value arb. Read `playoff_add`: low = truly redundant, high = a bold reallocation.
2. **CASH RENTAL** — `−250 < edge < 0` & declining: fairly priced but melting → full value only. Sorted so the *replaceable* one leads (deeper `my_pos_rank` first).
3. **DUMP** — `edge ≤ −250` & `1500 ≤ cur_value < 3000` & declining: mid-value stranded → offload.
4. **PARKED CHIP** — `edge ≤ −150` & holding: surplus, no urgency.
5. **SWEETENER** — declining & `cur_value < 1500`: melting throw-ins.
6. **CORE / STASH / hold / depth** — `edge ≥ 0` (underpriced → keep even if declining) or appreciating: sinks to the bottom.

## BUY board order (`targets.csv`, top = pursue first)

Sorted **`win_now_edge` descending** (biggest bargain to my lineup) among `confirmed`
rows, ties broken by the hidden objective. `growth_abs`/`exp_change` stay visible so you
apply the appreciating tilt by eye. Gettability is judged by the paired `ffs_trade_eval`,
not a leave-one-out column.

## Worked cases

**Lamb vs Warren — same melt, `win_now_edge` splits the sell type.** Both decline ~−33/−34%
next year, so year+1 alone can't separate them. `win_now_edge` does:
- **Warren** `edge −603`, TE2 behind Loveland, but `cur_value` 4,210 → **SELL HIGH**. You pay
  starter-TE price for a bench TE — deeply overvalued to *your* stacked TE room, yet still
  valuable, so a TE-needy team pays sticker. Cash him at full market — `playoff_add` 0.03
  confirms he's redundant to you — not a discount dump.
- **Lamb** `edge −67` (≈0), WR1, fully deployed → **CASH RENTAL**. Earns his price, so the
  ≈0 edge is the field telling you *demand equal young value back* (Achane/Hampton/Jeanty),
  never a dump.

**Higgins — the cash-the-rental case.** `edge −102` (≈0), declining −35%, but **WR3**
(replaceable). CASH RENTAL, and sorted *above* Lamb because he's the more expendable
rental — move him if you can't land an elite RB for Lamb.

**Jennings — same edge as Warren, opposite value.** `edge −566` (nearly Warren's −603) and
−72% year+1 scream sell just as loud, but `cur_value` is ~1,000, not 4,000 → **SWEETENER**,
not SELL HIGH. Identical stranded-surplus signal; `cur_value` alone decides throw-in vs
cash-at-market, so Jennings just rides along in a package (Lamb + Jennings → Hampton).

**Hampton vs Achane — the buy board.** Hampton `edge +613` sorts **above** Achane `edge +342`:
the bigger bargain to your lineup. Gettability (from the paired eval) then favours Achane —
so you pursue Hampton for the value and settle for Achane for the certainty.
