# Reusable stacking analysis - shared by the single-year detail (03) and the
# cross-year replication (04). Two tests, both holding the QB fixed:
#
#   fix-the-anchor advance lift : within a QB, does also rostering his pass-
#       catchers raise advance rate? (de-confounds QB quality)
#   ceiling test                : within a QB, does stacking lift the UPPER TAIL
#       of season roster_points MORE than the median? A pure "better receivers"
#       effect shifts median and P95 together (tail_extra ~ 0); a correlation-
#       CEILING effect fattens the right tail (tail_extra > 0). This is the
#       field-level proxy for the mechanism - the clean test is the sim engine,
#       which can toggle correlation with players held identical.
#
# Works on rich years (pkey = player_id) and thin years (pkey = player_name).

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(dplyr); library(nflreadr)
})

.clean <- function(x) {
  x <- tolower(trimws(x)); x <- gsub("[.'’`-]", "", x)
  x <- gsub("\\s+(jr|sr|ii|iii|iv|v)$", "", x); gsub("\\s+", " ", x)
}

# name+pos -> NFL team for a season (mid-season movers: first listed team)
nfl_teammap <- function(season) {
  ros <- as.data.table(load_rosters(season))[position %in% c("QB","RB","WR","TE")]
  ros[, key := .clean(full_name)]
  ros[, .(nfl_team = team[1]), by = .(key, pos = position)]
}

# prep -> list(fact[entry_id,draft_id,pkey,pos,roster_points,nfl_team], match_rate)
prep_rich <- function(prefix, season) {
  players <- as.data.table(read_parquet(paste0(prefix, "_players.parquet")))
  players[, key := .clean(player_name)]
  players <- merge(players, nfl_teammap(season), by = c("key","pos"), all.x = TRUE)
  fact <- as.data.table(open_dataset(paste0(prefix, "_fact")) %>%
    select(entry_id, draft_id, player_id, pos, roster_points) %>% collect())
  fact <- merge(fact, players[, .(player_id, nfl_team)], by = "player_id", all.x = TRUE)
  setnames(fact, "player_id", "pkey")
  list(fact = fact, match_rate = mean(!is.na(players$nfl_team)))
}

prep_thin <- function(parquet, season) {
  d <- as.data.table(read_parquet(parquet))[round == 1L,
        .(entry_id, draft_id, pkey = player_name, pos, roster_points)]
  d[, key := .clean(pkey)]
  d <- merge(d, nfl_teammap(season), by = c("key","pos"), all.x = TRUE)
  d[, key := NULL]
  list(fact = d, match_rate = d[, mean(!is.na(nfl_team))])
}

# thin playoff dumps (2021/2022): fread a raw CSV directly (name-keyed)
prep_thin_raw <- function(csv, season) {
  d <- fread(csv, showProgress = FALSE)
  d <- d[, .(entry_id = tournament_entry_id, draft_id,
             pkey = player_name, pos = position_name, roster_points)]
  d[, key := .clean(pkey)]
  d <- merge(d, nfl_teammap(season), by = c("key", "pos"), all.x = TRUE)
  d[, key := NULL]
  list(fact = d, match_rate = d[, mean(!is.na(nfl_team))])
}

# entry x QB stack flag: TRUE if the entry rosters a WR/TE on that QB's team
.qb_stack <- function(fact) {
  catch <- unique(fact[pos %in% c("WR","TE") & !is.na(nfl_team), .(entry_id, team = nfl_team)])
  catch[, has_catcher := TRUE]
  qbs <- fact[pos == "QB" & !is.na(nfl_team), .(entry_id, qb = pkey, qb_team = nfl_team)]
  qbs <- merge(qbs, catch, by.x = c("entry_id","qb_team"),
               by.y = c("entry_id","team"), all.x = TRUE)
  qbs[, stacked := !is.na(has_catcher)]
  qbs[, .(entry_id, qb, stacked)]
}

# run both tests; returns list(summary = 1 row, per_qb = detail)
stack_tests <- function(prep, season, min_side = 150L) {
  fact <- prep$fact
  em <- unique(fact[, .(entry_id, draft_id, roster_points)])
  em[, pod_rank := frank(-roster_points, ties.method = "first"), by = draft_id]
  em[, advanced := as.integer(pod_rank <= 2L)]

  qbs <- merge(.qb_stack(fact), em[, .(entry_id, roster_points, advanced)],
               by = "entry_id", all.x = TRUE)

  # naive (confounded), for contrast
  es <- qbs[, .(has_stack = any(stacked)), by = entry_id]
  naive <- merge(em[, .(entry_id, advanced)], es, by = "entry_id", all.x = TRUE)
  naive[is.na(has_stack), has_stack := FALSE]
  naive_lift <- naive[has_stack == TRUE, mean(advanced)] - naive[has_stack == FALSE, mean(advanced)]

  # per-QB: advance lift + ceiling (quantile) lifts
  q95 <- function(x) as.numeric(quantile(x, .95, names = FALSE))
  perqb <- suppressWarnings(qbs[, {
    si <- stacked; s <- roster_points[si]; u <- roster_points[!si]
    list(n_yes = length(s), n_no = length(u),
         adv_yes = mean(advanced[si]), adv_no = mean(advanced[!si]),
         med_yes = median(s), med_no = median(u),
         p95_yes = q95(s), p95_no = q95(u))
  }, by = qb])
  w <- perqb[n_yes >= min_side & n_no >= min_side]
  w[, `:=`(adv_lift = adv_yes - adv_no, med_lift = med_yes - med_no,
           p95_lift = p95_yes - p95_no)]
  w[, tail_extra := p95_lift - med_lift]   # ceiling lift beyond the median shift
  w[, wt := pmin(n_yes, n_no)]

  summary <- data.table(
    season       = season,
    match_rate   = round(prep$match_rate, 3),
    n_entry      = nrow(em),
    adv_rate     = round(mean(em$advanced), 4),
    naive_lift   = round(naive_lift, 4),
    n_qb         = nrow(w),
    pct_pos      = round(mean(w$adv_lift > 0), 3),
    adv_lift_w   = round(weighted.mean(w$adv_lift, w$wt), 4),  # de-confounded advance lift
    med_lift_w   = round(weighted.mean(w$med_lift, w$wt), 1),  # level shift (pts)
    p95_lift_w   = round(weighted.mean(w$p95_lift, w$wt), 1),  # ceiling shift (pts)
    tail_extra_w = round(weighted.mean(w$tail_extra, w$wt), 1) # ceiling beyond level
  )
  list(summary = summary, per_qb = w[order(-adv_lift)])
}

# SINGLE-WEEK ceiling test (playoff rounds). fact$roster_points is the entry's
# single-week score. Correlation theory: stacking redistributes points into
# fewer, bigger weeks -> should fatten the SINGLE-WEEK upper tail even though it
# left the 14-week season total's tail flat. We test that here, within QB.
#   tail_extra > 0  and  sd_ratio > 1  => stacking fattens the weekly ceiling.
# Caveat: playoff entries are advancers (selected on weeks 1-14); the scored week
# (15/16/17) is out-of-sample vs that selection, but receiver-quality still varies.
single_week_ceiling <- function(prep, season, min_side = 100L) {
  fact <- prep$fact
  score <- unique(fact[, .(entry_id, s = roster_points)])
  qbs <- merge(.qb_stack(fact), score, by = "entry_id", all.x = TRUE)
  q95 <- function(x) as.numeric(quantile(x, .95, names = FALSE))
  perqb <- suppressWarnings(qbs[, {
    si <- stacked; y <- s[si]; n <- s[!si]
    list(n_yes = length(y), n_no = length(n),
         med_yes = median(y), med_no = median(n),
         p95_yes = q95(y), p95_no = q95(n),
         sd_yes = sd(y), sd_no = sd(n))
  }, by = qb])
  w <- perqb[n_yes >= min_side & n_no >= min_side]
  w[, `:=`(med_lift = med_yes - med_no, p95_lift = p95_yes - p95_no,
           sd_ratio = sd_yes / sd_no)]
  w[, tail_extra := p95_lift - med_lift]
  w[, wt := pmin(n_yes, n_no)]
  data.table(
    season       = season,
    match_rate   = round(prep$match_rate, 3),
    n_entry      = nrow(score),
    n_qb         = nrow(w),
    med_lift_w   = round(weighted.mean(w$med_lift, w$wt), 1),   # weekly level shift
    p95_lift_w   = round(weighted.mean(w$p95_lift, w$wt), 1),   # weekly ceiling shift
    tail_extra_w = round(weighted.mean(w$tail_extra, w$wt), 1), # ceiling beyond level
    sd_ratio_w   = round(weighted.mean(w$sd_ratio, w$wt), 3),   # >1 = more weekly variance
    pct_tail_pos = round(mean(w$tail_extra > 0), 3)
  )
}
