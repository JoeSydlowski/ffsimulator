# Two stress tests on a trade_deepdive.R deal that mean points cannot answer:
#
#  (1) CEILING SHAPE. A flex slot that switches from a WR to a TE can gain mean
#      points and still lose boom weeks, because TE weekly scores are tighter.
#      Mean pts/start hides this entirely. Here every started player's weekly
#      score DISTRIBUTION is reported (sd, p90, p95, boom rates), the weekly team
#      delta is split by whether the incoming TE started, and the 2-TE weeks are
#      isolated so the "don't start two tight ends" instinct can be checked
#      against this roster's own simulated weeks rather than assumed.
#
#  (2) DEPTH SATURATION. Bench points going UP after a trade can mean two very
#      different things: dead capital added, or good players pushed down by
#      better ones. Bench points are decomposed per player, and roster capital is
#      split by how often each player actually cracks the lineup, so "am I too
#      deep to use more depth" gets a capital answer, not a points answer.
#
# Usage: same env vars as dev/suite/trade_deepdive.R (run that first).

suppressMessages({
  library(data.table)
  devtools::load_all(here::here(), quiet = TRUE)
})
options(ffsimulator.verbose = FALSE)

league_id  <- Sys.getenv("FFS_LEAGUE_ID", "1359546500786434048")
league_dir <- here::here("dev", "league_sims", league_id)
sims <- Sys.glob(file.path(league_dir, "*", "simulation.rds"))
out  <- dirname(sims[order(file.info(sims)$mtime, decreasing = TRUE)][1])
tag  <- Sys.getenv("FFS_DD_TAG", "puka_mcbride_flowers")
dd   <- readRDS(file.path(out, paste0(tag, "_deepdive.rds")))
me <- dd$me; send_ids <- dd$send_ids; recv_ids <- dd$recv_ids
n_seasons <- dd$n_seasons

sim <- readRDS(file.path(out, "simulation.rds"))
rs  <- as.data.table(sim$roster_scores)
lc  <- as.data.table(sim$lineup_constraints)

mine_before <- rs[franchise_id == me]
mine_after  <- rbind(
  rs[franchise_id == me & !player_id %in% send_ids],
  local({ inc <- copy(rs[player_id %in% recv_ids])
          for (cc in c("franchise_id", "franchise_name"))
            set(inc, j = cc, value = mine_before[[cc]][1]); inc }))
nm <- unique(rbind(mine_before, mine_after)[, list(player_id = as.character(player_id),
                                                   player_name, pos)], by = "player_id")

opt_of <- function(roster_rows, label) {
  message("optimising ", label, " lineups @ ", Sys.time())
  ffsimulator:::.ffs_optimise_started(roster_rows, lc)
}
ob <- opt_of(mine_before, "BEFORE"); oa <- opt_of(mine_after, "AFTER")
st_of <- function(o) o[, list(player_id = unlist(starter_player_id)),
                       by = list(season, week)][!is.na(player_id)]
sb <- st_of(ob); sa <- st_of(oa)
pb <- merge(sb, mine_before[, list(season, week, player_id, projected_score)],
            by = c("season", "week", "player_id"))
pa <- merge(sa, mine_after[,  list(season, week, player_id, projected_score)],
            by = c("season", "week", "player_id"))

## ---- 1a. per-player STARTED-week score distribution ------------------------------
shape <- function(p, label) {
  z <- p[, list(when = label, starts = .N / n_seasons,
                mean = mean(projected_score), sd = stats::sd(projected_score),
                p50 = stats::quantile(projected_score, .50),
                p90 = stats::quantile(projected_score, .90),
                p95 = stats::quantile(projected_score, .95),
                boom20 = mean(projected_score >= 20),
                boom25 = mean(projected_score >= 25)), by = player_id]
  merge(nm, z, by = "player_id")
}
sh <- rbind(shape(pb, "before"), shape(pa, "after"))
sh[, cv := sd / mean]
fwrite(sh, file.path(out, paste0(tag, "_start_shape.csv")))

cat("\n== weekly score SHAPE among started weeks (AFTER roster, >=1 start/season) ==\n")
print(sh[when == "after" & starts >= 1][order(-mean)][, list(
  player_name, pos, starts = round(starts, 2), mean = round(mean, 2), sd = round(sd, 2),
  cv = round(cv, 3), p90 = round(p90, 1), p95 = round(p95, 1),
  boom20 = round(100 * boom20, 1), boom25 = round(100 * boom25, 1))])

cat("\n== the substitution that matters: McBride vs the bodies he displaces ==\n")
key_players <- c(nm[player_id %in% recv_ids]$player_name, nm[player_id %in% send_ids]$player_name,
                 "George Kittle", "Dallas Goedert", "Harold Fannin",
                 "Jordyn Tyson", "Denzel Boston", "Calvin Ridley", "Carnell Tate", "Jack Bech")
print(sh[player_name %in% key_players & ((when == "after" & !player_id %in% send_ids) |
                                         (when == "before" &  player_id %in% send_ids))][
  order(-mean)][, list(player_name, pos, mean = round(mean, 2), sd = round(sd, 2),
                       cv = round(cv, 3), p90 = round(p90, 1), p95 = round(p95, 1),
                       boom20 = round(100 * boom20, 1), boom25 = round(100 * boom25, 1))])

cat("\n== is the 'TE has a lower ceiling' instinct true on THIS roster? ==\n")
cat("   (started weeks, players with >=2 starts/season, after-trade roster)\n")
print(sh[when == "after" & starts >= 2, list(
  players = .N, mean = round(mean(mean), 2), cv = round(mean(cv), 3),
  p90_over_p50 = round(mean(p90 / p50), 3), boom25 = round(100 * mean(boom25), 1)), by = pos][order(-cv)])

## ---- 1b. weekly team delta, split by what the lineup looked like -----------------
wk <- merge(ob[, list(season, week, before = actual_score)],
            oa[, list(season, week, after  = actual_score)], by = c("season", "week"))
wk[, delta := after - before]
mcb <- nm[player_name == "Trey McBride"]$player_id
te_ct <- merge(pa, nm[, list(player_id, pos)], by = "player_id")[
  pos == "TE", list(n_te = .N), by = list(season, week)]
wk <- merge(wk, te_ct, by = c("season", "week"), all.x = TRUE)
wk[is.na(n_te), n_te := 0]
wk[, mcb_started := paste(season, week) %in% sa[player_id %in% mcb, paste(season, week)]]

cat("\n== weekly delta split by how many TEs the post-trade lineup started ==\n")
print(wk[, list(share_of_weeks = round(100 * .N / nrow(wk), 1),
                mean_before = round(mean(before), 1), mean_after = round(mean(after), 1),
                mean_delta = round(mean(delta), 2),
                p90_before = round(stats::quantile(before, .9), 1),
                p90_after  = round(stats::quantile(after,  .9), 1)), by = n_te][order(n_te)])

cat("\n== weekly delta in BOOM weeks vs BUST weeks (deciles of the before-score) ==\n")
wk[, dec := cut(before, breaks = stats::quantile(before, seq(0, 1, .1)),
                include.lowest = TRUE, labels = paste0("D", 1:10))]
print(wk[, list(before = round(mean(before), 1), after = round(mean(after), 1),
                delta = round(mean(delta), 2),
                pct_weeks_improved = round(100 * mean(delta > 0), 1)), by = dec][order(dec)])

## ---- 2. depth saturation ---------------------------------------------------------
bench_by_player <- function(st, roster_rows, label) {
  z <- merge(roster_rows[, list(season, week, player_id, projected_score)],
             st[, list(season, week, player_id, started = TRUE)],
             by = c("season", "week", "player_id"), all.x = TRUE)
  z[is.na(started), started := FALSE]
  z[, list(when = label,
           bench_pts = sum(projected_score[!started]) / n_seasons,
           start_pts = sum(projected_score[started]) / n_seasons,
           weeks_rostered = .N / n_seasons,
           weeks_started = sum(started) / n_seasons), by = player_id]
}
bp <- rbind(bench_by_player(sb, mine_before, "before"), bench_by_player(sa, mine_after, "after"))
bp <- merge(nm, bp, by = "player_id")
bw <- dcast(bp, player_id + player_name + pos ~ when, value.var = c("bench_pts", "weeks_started"))
for (cc in c("bench_pts_before", "bench_pts_after", "weeks_started_before", "weeks_started_after"))
  bw[is.na(get(cc)), (cc) := 0]
bw[, d_bench := bench_pts_after - bench_pts_before]
setorder(bw, -d_bench)
fwrite(bw, file.path(out, paste0(tag, "_bench_by_player.csv")))
cat("\n== who accounts for the extra bench points? ==\n")
print(head(bw[, list(player_name, pos, bench_b = round(bench_pts_before, 1),
                     bench_a = round(bench_pts_after, 1), d_bench = round(d_bench, 1),
                     wks_b = round(weeks_started_before, 2),
                     wks_a = round(weeks_started_after, 2))], 12))

# capital view: how much market value sits on players who rarely start
dyn <- fread(file.path(out, "dynasty_outlook.csv"),
             colClasses = list(character = c("player_id", "fantasypros_id")))
cap <- merge(bw, dyn[, list(player_id, cur_value)], by = "player_id", all.x = TRUE)
cap[is.na(cur_value), cur_value := 0]
band <- function(w) fcase(w >= 8, "core starter (>=8 wk)", w >= 4, "rotation (4-8 wk)",
                          w >= 1, "fill-in (1-4 wk)", default = "never starts (<1 wk)")
capb <- rbind(
  cap[player_id %in% c(unique(mine_before$player_id)),
      list(when = "before", band = band(weeks_started_before), cur_value)],
  cap[player_id %in% c(unique(mine_after$player_id)),
      list(when = "after",  band = band(weeks_started_after),  cur_value)])
capt <- dcast(capb[, list(capital = sum(cur_value), n = .N), by = list(when, band)],
              band ~ when, value.var = c("capital", "n"))
capt[, d_capital := capital_after - capital_before]
cat("\n== roster CAPITAL by how often the player actually starts ==\n")
print(capt)
cat(sprintf("\ncapital that never cracks the lineup: %.0f -> %.0f (%.1f%% -> %.1f%% of roster)\n",
            capt[band == "never starts (<1 wk)"]$capital_before,
            capt[band == "never starts (<1 wk)"]$capital_after,
            100 * capt[band == "never starts (<1 wk)"]$capital_before / sum(capt$capital_before, na.rm = TRUE),
            100 * capt[band == "never starts (<1 wk)"]$capital_after  / sum(capt$capital_after,  na.rm = TRUE)))
fwrite(capt, file.path(out, paste0(tag, "_capital_bands.csv")))

saveRDS(list(sh = sh, wk = wk, bw = bw, capt = capt),
        file.path(out, paste0(tag, "_ceiling_depth.rds")))
message("wrote ceiling/depth artifacts for tag ", tag)
