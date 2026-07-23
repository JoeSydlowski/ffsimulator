# Phase 1 - does stacking matter for particular ARCHETYPES? Split each (entry,QB)
# stack by the ADP of its best same-team catcher: stud_WR (<=48), mid_WR (49-108),
# dart_WR (>108). Within QB, compare advance lift (regular season) and single-week
# sd_ratio vs NO-stack, plus a stud-QB x stud-WR cell.
#
# NOTE: playoff dumps blank projection_adp, so for the single-week analyses we
# pass an adp_map (player -> regular-season ADP) to tier the catchers.

source("R/stack_lib.R")

# tag each (entry,QB) with stack archetype. adp_map (pkey -> p_adp) fills ADP when
# the fact's own adp is blank (playoff rounds).
archetype_tag <- function(fact, adp_map = NULL) {
  qb <- fact[pos == "QB" & !is.na(nfl_team), .(entry_id, qb = pkey, qb_team = nfl_team, qb_adp = adp)]
  catch <- fact[pos %in% c("WR","TE") & !is.na(nfl_team), .(entry_id, pkey, cteam = nfl_team, cadp = adp)]
  if (!is.null(adp_map)) {
    qb[, qb_adp := NULL]
    qb <- merge(qb, adp_map, by.x = "qb", by.y = "pkey", all.x = TRUE); setnames(qb, "p_adp", "qb_adp")
    catch[, cadp := NULL]
    catch <- merge(catch, adp_map, by = "pkey", all.x = TRUE); setnames(catch, "p_adp", "cadp")
  }
  m <- merge(qb[, .(entry_id, qb, qb_team)], catch, by = "entry_id", allow.cartesian = TRUE)
  sm <- m[cteam == qb_team, .(min_cadp = suppressWarnings(min(cadp, na.rm = TRUE)), n_c = .N),
          by = .(entry_id, qb)]
  qb <- merge(qb, sm, by = c("entry_id","qb"), all.x = TRUE)
  qb[, stacked := !is.na(n_c)]
  qb[, arche := fifelse(!stacked, "0_no_stack",
              fifelse(is.na(min_cadp) | is.infinite(min_cadp), "9_unk",
              fifelse(min_cadp <= 48, "1_stud_WR",
              fifelse(min_cadp <= 108, "2_mid_WR", "3_dart_WR"))))]
  qb[]
}

adp_map_of <- function(fact) fact[!is.na(adp), .(p_adp = median(adp)), by = .(pkey)]

arch_adv <- function(prep, season, min_cell = 100L) {
  fact <- prep$fact
  em <- unique(fact[, .(entry_id, draft_id, roster_points)])
  em[, adv := as.integer(frank(-roster_points, ties.method = "first") <= 2L), by = draft_id]
  qb <- merge(archetype_tag(fact), em[, .(entry_id, adv)], by = "entry_id")
  cell <- qb[arche != "9_unk", .(n = .N, adv = mean(adv)), by = .(qb, arche)]
  base <- cell[arche == "0_no_stack", .(qb, adv0 = adv, n0 = n)]
  g <- merge(cell[arche != "0_no_stack"], base, by = "qb")
  g <- g[n >= min_cell & n0 >= min_cell][, `:=`(lift = adv - adv0, wt = pmin(n, n0))]
  g[, .(season = season, n_qb = .N, adv_lift = round(weighted.mean(lift, wt), 4)), by = arche][order(arche)]
}

arch_week <- function(prep, adp_map, season, min_cell = 50L) {
  fact <- prep$fact
  score <- unique(fact[, .(entry_id, s = roster_points)])
  qb <- merge(archetype_tag(fact, adp_map), score, by = "entry_id")
  cell <- suppressWarnings(qb[arche != "9_unk", .(n = .N, sd = sd(s)), by = .(qb, arche)])
  base <- cell[arche == "0_no_stack", .(qb, sd0 = sd, n0 = n)]
  g <- merge(cell[arche != "0_no_stack"], base, by = "qb")
  g <- g[n >= min_cell & n0 >= min_cell][, `:=`(sd_ratio = sd / sd0, wt = pmin(n, n0))]
  g[, .(season = season, n_qb = .N, sd_ratio = round(weighted.mean(sd_ratio, wt), 3)), by = arche][order(arche)]
}

cfg <- list(
  list(y = 2021L, kind = "thin", reg = "data/parquet/2021_BBMII.parquet",  wk = "data/raw/bbm2021_rd2.csv"),
  list(y = 2022L, kind = "thin", reg = "data/parquet/2022_BBMIII.parquet", wk = sprintf("data/raw/bbm2022_rd2_%02d.csv", 0:2)),
  list(y = 2023L, kind = "rich", reg = "data/parquet/2023_BBMIV",  wk = "data/parquet/2023_BBMIV_rd2"),
  list(y = 2024L, kind = "rich", reg = "data/parquet/2024_BBMV",   wk = "data/parquet/2024_BBMV_rd2"),
  list(y = 2025L, kind = "rich", reg = "data/parquet/2025_BBMVI",  wk = "data/parquet/2025_BBMVI_rd2")
)
reg_prep <- function(c) if (c$kind == "rich") prep_rich(c$reg, c$y) else prep_thin(c$reg, c$y)
wk_prep  <- function(c) if (c$kind == "rich") prep_rich(c$wk, c$y)  else prep_thin_raw(c$wk, c$y)

ADV <- list(); WK <- list()
for (c in cfg) {
  cat(sprintf("[year] %d\n", c$y))
  rp <- reg_prep(c)
  ADV[[as.character(c$y)]] <- arch_adv(rp, c$y)
  amap <- adp_map_of(rp$fact); rm(rp); gc(FALSE)
  WK[[as.character(c$y)]] <- arch_week(wk_prep(c), amap, c$y); rm(amap); gc(FALSE)
}
cat("\n===== advance lift by stack archetype (vs no-stack, within QB) =====\n")
print(dcast(rbindlist(ADV), season ~ arche, value.var = "adv_lift"))
cat("\n===== single-week sd_ratio by stack archetype (vs no-stack, within QB) =====\n")
cat("(ADP tiered from regular season; sd_ratio>1 => that archetype fattens the weekly ceiling)\n")
print(dcast(rbindlist(WK), season ~ arche, value.var = "sd_ratio"))
