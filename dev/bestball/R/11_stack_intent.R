# Phase 1 - is the ~90% "stack" rate INTENTIONAL or just incidental overlap?
# (A) random baseline: shuffle catchers' NFL teams across the field (breaking any
#     deliberate QB-pairing, preserving team frequencies) and recompute the stack
#     rate. obs - rand = the intentional excess.
# (B) reaching: do the pass-catchers that COMPLETE a stack get drafted above ADP
#     (reach = adp - overall_pick > 0) more than non-stack catchers?

source("R/stack_lib.R")

srate <- function(qt, ct, n_entry) {
  j <- merge(qt, ct, by = c("entry_id","team"))
  uniqueN(j$entry_id) / n_entry
}

baseline <- function(fact, season, nperm = 5L) {
  n_entry <- uniqueN(fact$entry_id)
  qt <- unique(fact[pos == "QB" & !is.na(nfl_team), .(entry_id, team = nfl_team)])
  ct <- fact[pos %in% c("WR","TE") & !is.na(nfl_team), .(entry_id, team = nfl_team)]
  obs  <- srate(qt, ct, n_entry)
  rand <- mean(replicate(nperm, srate(qt, data.table(entry_id = ct$entry_id, team = sample(ct$team)), n_entry)))
  comp <- fact[!is.na(nfl_team), .(nqb = sum(pos == "QB"),
                                   nqbteam = uniqueN(nfl_team[pos == "QB"]),
                                   ncatch = sum(pos %in% c("WR","TE"))), by = entry_id]
  data.table(season, n_entry,
             obs_stack = round(obs, 3), rand_stack = round(rand, 3),
             intentional_excess = round(obs - rand, 3),
             mean_nQB = round(mean(comp$nqb), 2),
             mean_QBteams = round(mean(comp$nqbteam), 2),
             mean_nCatch = round(mean(comp$ncatch), 2))
}

reaching <- function(fact, season) {
  f <- fact[!is.na(nfl_team) & !is.na(adp) & !is.na(overall_pick)]
  cat_pick <- f[pos %in% c("WR","TE"), .(entry_id, overall_pick, reach = adp - overall_pick, cteam = nfl_team)]
  qteams <- unique(f[pos == "QB", .(entry_id, qteam = nfl_team)])
  m <- merge(cat_pick, qteams, by = "entry_id", allow.cartesian = TRUE)
  onqb <- m[, .(stack = any(cteam == qteam), reach = reach[1]), by = .(entry_id, overall_pick)]
  data.table(season,
             reach_stack_catcher = round(onqb[stack == TRUE, mean(reach)], 2),
             reach_nonstack      = round(onqb[stack == FALSE, mean(reach)], 2),
             pct_reached_stack   = round(onqb[stack == TRUE, mean(reach > 0)], 3),
             pct_reached_nonstk  = round(onqb[stack == FALSE, mean(reach > 0)], 3))
}

cfg <- list(
  list(y = 2021L, kind = "thin", f = "data/parquet/2021_BBMII.parquet"),
  list(y = 2022L, kind = "thin", f = "data/parquet/2022_BBMIII.parquet"),
  list(y = 2023L, kind = "rich", f = "data/parquet/2023_BBMIV"),
  list(y = 2024L, kind = "rich", f = "data/parquet/2024_BBMV"),
  list(y = 2025L, kind = "rich", f = "data/parquet/2025_BBMVI")
)
A <- list(); B <- list()
for (c in cfg) {
  cat(sprintf("[year] %d\n", c$y))
  fact <- if (c$kind == "rich") prep_rich(c$f, c$y)$fact else prep_thin(c$f, c$y)$fact
  A[[as.character(c$y)]] <- baseline(fact, c$y)
  B[[as.character(c$y)]] <- reaching(fact, c$y)
  rm(fact); gc(FALSE)
}
cat("\n===== (A) is stacking INTENTIONAL? observed vs random-assignment baseline =====\n")
print(rbindlist(A))
cat("\n===== (B) do stack-completing catchers get REACHED for? (reach = adp - pick) =====\n")
print(rbindlist(B))
cat("\nreach>0 = drafted earlier than ADP. If reach_stack_catcher > reach_nonstack, people reach to stack.\n")
