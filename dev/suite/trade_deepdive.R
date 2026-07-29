# Deep-dive ONE proposed trade on the big standings sim: not "is the score
# positive" but WHERE the points come from.
#
# player_trade_board.R ranks packages and confirm_trade_board.R re-prices them on
# the n=2000 sim. Both answer "how much". This answers "why": it re-optimises my
# starting lineup week-by-week before and after the swap and reports, per player,
# how many weeks he starts and how many points he puts in the lineup. A stud is
# only worth breaking up when the two pieces coming back BOTH clear the lineup
# cut-line often enough that the second one is not dead weight - this script
# shows that directly instead of asserting it.
#
# Outputs (into the newest report folder):
#   <tag>_lineup_shift.csv     per-player weeks-started + points-started, before/after
#   <tag>_slot_mix.csv         positional composition of the starting 10, before/after
#   <tag>_headline.csv         ffs_trade_eval on the n=2000 sim
#   <tag>_score_dist.csv       team weekly/season score distribution, before/after
#   <tag>_value_dist.csv       post-trade roster dynasty value distribution (p10/med/p90)
#   <tag>_deepdive.rds         everything above, for the plotting script
#
# Usage:
#   FFS_LEAGUE_ID=1359546500786434048 FFS_MY_TEAM=sox05syd \
#   FFS_DD_OPP="MacDaddyNation" FFS_DD_SEND="Puka Nacua" \
#   FFS_DD_RECV="Trey McBride + Zay Flowers" \
#     Rscript dev/suite/trade_deepdive.R

suppressMessages({
  library(data.table)
  devtools::load_all(here::here(), quiet = TRUE)
})
options(ffsimulator.verbose = FALSE)

league_id  <- Sys.getenv("FFS_LEAGUE_ID", "1359546500786434048")
league_dir <- here::here("dev", "league_sims", league_id)
sims <- Sys.glob(file.path(league_dir, "*", "simulation.rds"))
stopifnot("no saved simulation.rds for this league" = length(sims) > 0)
out    <- dirname(sims[order(file.info(sims)$mtime, decreasing = TRUE)][1])
config <- readRDS(file.path(out, "config.rds"))
my_team <- Sys.getenv("FFS_MY_TEAM", config$my_team %||% "")

opp_name  <- Sys.getenv("FFS_DD_OPP",  "MacDaddyNation")
send_txt  <- Sys.getenv("FFS_DD_SEND", "Puka Nacua")
recv_txt  <- Sys.getenv("FFS_DD_RECV", "Trey McBride + Zay Flowers")
split_pl  <- function(x) trimws(strsplit(x, "\\+")[[1]])
send_names <- split_pl(send_txt); recv_names <- split_pl(recv_txt)
tag <- Sys.getenv("FFS_DD_TAG",
                  gsub("[^A-Za-z0-9]+", "_", paste0(send_txt, "_for_", recv_txt)))

message("report folder: ", out)
message("deal: ", my_team, " sends [", send_txt, "] to ", opp_name,
        " for [", recv_txt, "]")

sim <- readRDS(file.path(out, "simulation.rds"))
rs  <- as.data.table(sim$roster_scores)
lc  <- as.data.table(sim$lineup_constraints)
n_seasons <- uniqueN(rs$season)
fr  <- unique(rs[, list(franchise_id, franchise_name)])
me  <- fr[franchise_name == my_team]$franchise_id[1]
opp <- fr[franchise_name == opp_name]$franchise_id[1]
stopifnot("my_team not found"   = !is.na(me),
          "FFS_DD_OPP not found" = !is.na(opp))

pid_of <- function(nm, fid) {
  hit <- unique(rs[player_name == nm & franchise_id == fid]$player_id)
  if (!length(hit)) stop(sprintf("'%s' is not on %s's roster in the sim", nm,
                                 fr[franchise_id == fid]$franchise_name[1]))
  as.character(hit[1])
}
send_ids <- vapply(send_names, pid_of, character(1), fid = me,  USE.NAMES = FALSE)
recv_ids <- vapply(recv_names, pid_of, character(1), fid = opp, USE.NAMES = FALSE)

## ---- 1. headline: the confirmed trade evaluation -------------------------------
message("ffs_trade_eval on n=", n_seasons, " @ ", Sys.time())
te <- as.data.table(ffs_trade_eval(sim, me, send_ids, opp, recv_ids))
te <- merge(te, fr, by = "franchise_id")
fwrite(te, file.path(out, paste0(tag, "_headline.csv")))
print(te[, list(franchise_name, h2h_wins_delta = round(h2h_wins_delta, 3),
                points_delta = round(points_delta, 1),
                playoff_before = round(100 * playoff_pct_before, 1),
                playoff_after  = round(100 * playoff_pct_after, 1),
                champ_before   = round(100 * champion_pct_before, 1),
                champ_after    = round(100 * champion_pct_after, 1))])

## ---- 2. lineup reallocation: who plays more / less ------------------------------
# Re-set my starting lineup in EVERY franchise-week, before and after, with the
# same rank-based manager the simulation used. Both sides are recomputed (rather
# than reusing the base starters) so the comparison is exactly like-for-like.
mine_before <- rs[franchise_id == me]
mine_after  <- rbind(
  rs[franchise_id == me & !player_id %in% send_ids],
  local({
    inc <- copy(rs[player_id %in% recv_ids])
    for (cc in c("franchise_id", "franchise_name")) set(inc, j = cc, value = mine_before[[cc]][1])
    inc
  }))

started_of <- function(roster_rows, label) {
  message("optimising ", label, " lineups (", uniqueN(roster_rows$season), " seasons) @ ", Sys.time())
  st <- ffsimulator:::.ffs_optimise_started(roster_rows, lc)
  list(
    weekly = st[, list(season, week, actual_score)],
    starts = st[, list(player_id = unlist(starter_player_id)), by = list(season, week)][
      !is.na(player_id)])
}
b <- started_of(mine_before, "BEFORE")
a <- started_of(mine_after,  "AFTER")

# points a started player actually put in the lineup
pts_of <- function(starts, roster_rows) {
  merge(starts, roster_rows[, list(season, week, player_id, projected_score)],
        by = c("season", "week", "player_id"))
}
pb <- pts_of(b$starts, mine_before); pa <- pts_of(a$starts, mine_after)

per_player <- function(p, roster_rows) {
  z <- p[, list(weeks = .N, pts = sum(projected_score)), by = list(season, player_id)]
  # players never started in a season still count as 0 for that season
  grid <- CJ(season = unique(roster_rows$season),
             player_id = unique(roster_rows$player_id), unique = TRUE)
  z <- merge(grid, z, by = c("season", "player_id"), all.x = TRUE)
  z[is.na(weeks), weeks := 0][is.na(pts), pts := 0]
  z[, list(weeks_started = mean(weeks), pts_started = mean(pts)), by = player_id]
}
lb <- per_player(pb, mine_before); la <- per_player(pa, mine_after)
setnames(lb, c("weeks_started", "pts_started"), c("weeks_before", "pts_before"))
setnames(la, c("weeks_started", "pts_started"), c("weeks_after",  "pts_after"))

nm <- unique(rbind(mine_before, mine_after)[, list(player_id = as.character(player_id),
                                                   player_name, pos, age)])
nm <- unique(nm, by = "player_id")
shift <- merge(lb, la, by = "player_id", all = TRUE)
shift[is.na(weeks_before), `:=`(weeks_before = 0, pts_before = 0)]
shift[is.na(weeks_after),  `:=`(weeks_after  = 0, pts_after  = 0)]
shift <- merge(nm, shift, by = "player_id", all.y = TRUE)
shift[, `:=`(role = fcase(player_id %in% send_ids, "OUT (sent)",
                          player_id %in% recv_ids, "IN (received)",
                          default = "stays"),
             d_weeks = weeks_after - weeks_before,
             d_pts   = pts_after - pts_before)]
setorder(shift, -d_pts)
fwrite(shift[, list(player_name, pos, age, role,
                    weeks_before = round(weeks_before, 2), weeks_after = round(weeks_after, 2),
                    d_weeks = round(d_weeks, 2),
                    pts_before = round(pts_before, 1), pts_after = round(pts_after, 1),
                    d_pts = round(d_pts, 1), player_id)],
       file.path(out, paste0(tag, "_lineup_shift.csv")))

cat("\n== lineup reallocation (per season, mean of ", n_seasons, " sims) ==\n", sep = "")
pr <- shift[abs(d_pts) >= 1 | role != "stays"]
for (i in seq_len(nrow(pr))) {
  r <- pr[i]
  cat(sprintf("  %-22s %-3s %-14s weeks %5.2f -> %5.2f (%+5.2f)   pts %6.1f -> %6.1f (%+6.1f)\n",
              r$player_name, r$pos, r$role, r$weeks_before, r$weeks_after, r$d_weeks,
              r$pts_before, r$pts_after, r$d_pts))
}
cat(sprintf("  %-41s %-14s %28s %+6.1f\n", "TOTAL", "", "",
            sum(shift$d_pts)))

## ---- 3. positional mix of the starting 10 ---------------------------------------
mixb <- merge(pb, nm[, list(player_id, pos)], by = "player_id")[
  , list(starters = .N), by = list(season, week, pos)][
  , list(mean_starters = sum(starters) / (n_seasons * 14)), by = pos][, when := "before"]
mixa <- merge(pa, nm[, list(player_id, pos)], by = "player_id")[
  , list(starters = .N), by = list(season, week, pos)][
  , list(mean_starters = sum(starters) / (n_seasons * 14)), by = pos][, when := "after"]
mix <- dcast(rbind(mixb, mixa), pos ~ when, value.var = "mean_starters")
mix[is.na(before), before := 0][is.na(after), after := 0][, delta := after - before]
fwrite(mix, file.path(out, paste0(tag, "_slot_mix.csv")))
cat("\n== starting-10 positional mix (mean starters per week) ==\n")
print(mix[order(-delta)])

## ---- 4. score distribution: floor and ceiling shift ------------------------------
wk <- merge(b$weekly[, list(season, week, before = actual_score)],
            a$weekly[, list(season, week, after  = actual_score)],
            by = c("season", "week"))
sea <- wk[, list(before = sum(before), after = sum(after)), by = season]
qs <- c(.05, .10, .25, .50, .75, .90, .95)
dist <- rbind(
  data.table(scale = "weekly", q = qs, before = quantile(wk$before,  qs),
             after = quantile(wk$after,  qs)),
  data.table(scale = "season", q = qs, before = quantile(sea$before, qs),
             after = quantile(sea$after, qs)))
dist[, delta := after - before]
fwrite(dist, file.path(out, paste0(tag, "_score_dist.csv")))
cat("\n== team score distribution ==\n"); print(dist)
cat(sprintf("\nweeks my score improves: %.1f%%  |  mean weekly delta %+.2f pts\n",
            100 * mean(wk$after > wk$before), mean(wk$after - wk$before)))

## ---- 5. post-trade dynasty value distribution -----------------------------------
dyn <- fread(file.path(out, "dynasty_outlook.csv"),
             colClasses = list(character = c("player_id", "fantasypros_id")))
post_ids <- unique(as.character(mine_after$player_id))
vd <- dyn[player_id %in% c(post_ids, send_ids)][, list(
  player_name, pos, age, player_id, cur_value,
  next_value_mean, next_value_med, next_value_p10, next_value_p90, p_rise, p_exit)]
vd[, role := fcase(player_id %in% send_ids, "OUT (sent)",
                   player_id %in% recv_ids, "IN (received)", default = "stays")]
setorder(vd, -cur_value)
fwrite(vd, file.path(out, paste0(tag, "_value_dist.csv")))

saveRDS(list(te = te, shift = shift, mix = mix, wk = wk, sea = sea, dist = dist,
             vd = vd, me = me, opp = opp, my_team = my_team, opp_name = opp_name,
             send_names = send_names, recv_names = recv_names,
             send_ids = send_ids, recv_ids = recv_ids, out = out, tag = tag,
             n_seasons = n_seasons),
        file.path(out, paste0(tag, "_deepdive.rds")))
message("\nwrote deep-dive artifacts with tag '", tag, "' to ", out)
