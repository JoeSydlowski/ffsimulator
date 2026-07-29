# Second pass on a trade_deepdive.R deal: WHO gets displaced, at what rate per
# start, and what the post-trade roster's year+1 value distribution looks like.
#
# trade_deepdive.R answers "how many weeks does each player start, before and
# after". This answers the follow-on question a manager actually asks: when the
# incoming player starts, whose spot did he take, and is the swap a points-per-
# start upgrade or just a reshuffle? It recomputes the same before/after starting
# lineups and diffs them week by week.
#
# Outputs (into the report folder):
#   <tag>_per_start.csv       points per start, before/after, per player
#   <tag>_displacement.csv    when an incoming player starts, who left the lineup
#   <tag>_bench_waste.csv     points left on the bench, before/after
#   <tag>_lineup_shift.png    who plays more / less
#   <tag>_value_dist.png      year+1 value distribution for the post-trade roster
#   <tag>_score_dist.png      weekly team score: floor/ceiling shift
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

sim <- readRDS(file.path(out, "simulation.rds"))
rs  <- as.data.table(sim$roster_scores)
lc  <- as.data.table(sim$lineup_constraints)
n_seasons <- dd$n_seasons

mine_before <- rs[franchise_id == me]
mine_after  <- rbind(
  rs[franchise_id == me & !player_id %in% send_ids],
  local({ inc <- copy(rs[player_id %in% recv_ids])
          for (cc in c("franchise_id", "franchise_name"))
            set(inc, j = cc, value = mine_before[[cc]][1]); inc }))

nm <- unique(rbind(mine_before, mine_after)[, list(player_id = as.character(player_id),
                                                   player_name, pos)], by = "player_id")
starters_of <- function(roster_rows, label) {
  message("optimising ", label, " lineups @ ", Sys.time())
  st <- ffsimulator:::.ffs_optimise_started(roster_rows, lc)
  st[, list(player_id = unlist(starter_player_id)), by = list(season, week)][!is.na(player_id)]
}
sb <- starters_of(mine_before, "BEFORE")
sa <- starters_of(mine_after,  "AFTER")

## ---- 1. points per start ---------------------------------------------------------
pps <- function(st, roster_rows) {
  merge(st, roster_rows[, list(season, week, player_id, projected_score)],
        by = c("season", "week", "player_id"))[
    , list(starts = .N, pts = sum(projected_score)), by = player_id][
    , list(player_id, starts_per_season = starts / n_seasons,
           pts_per_start = pts / starts)]
}
per_start <- merge(pps(sb, mine_before), pps(sa, mine_after),
                   by = "player_id", all = TRUE, suffixes = c("_before", "_after"))
per_start <- merge(nm, per_start, by = "player_id", all.y = TRUE)
per_start[, role := fcase(player_id %in% send_ids, "OUT (sent)",
                          player_id %in% recv_ids, "IN (received)", default = "stays")]
setorder(per_start, -pts_per_start_after, na.last = TRUE)
fwrite(per_start, file.path(out, paste0(tag, "_per_start.csv")))
cat("\n== points per START (the rate that actually matters) ==\n")
print(per_start[, list(player_name, pos, role,
                       starts_b = round(starts_per_season_before, 2),
                       ppst_b   = round(pts_per_start_before, 2),
                       starts_a = round(starts_per_season_after, 2),
                       ppst_a   = round(pts_per_start_after, 2))])

## ---- 2. displacement: when an incoming player starts, who left the lineup? --------
# Diff the two starting elevens week by week. `dropped` = in the base lineup but
# not in the post-trade lineup; that set is what the incoming pieces displaced.
key <- c("season", "week")
setkeyv(sb, key); setkeyv(sa, key)
bl <- sb[, list(before = list(player_id)), by = key]
al <- sa[, list(after  = list(player_id)), by = key]
wkl <- merge(bl, al, by = key)
disp <- rbindlist(lapply(seq_len(nrow(wkl)), function(i) {
  b <- wkl$before[[i]]; a <- wkl$after[[i]]
  d <- setdiff(b, a); g <- setdiff(a, b)
  if (!length(d)) return(NULL)
  data.table(season = wkl$season[i], week = wkl$week[i],
             dropped = d, n_added = length(g),
             added = paste(sort(g), collapse = "|"))
}))
disp <- merge(disp, nm[, list(dropped = player_id, dropped_name = player_name,
                              dropped_pos = pos)], by = "dropped")
# ignore the traded-away player himself: he is gone by construction, not displaced
disp_real <- disp[!dropped %in% send_ids]
dsum <- disp_real[, list(weeks_bumped_per_season = .N / n_seasons),
                  by = list(dropped_name, dropped_pos)][order(-weeks_bumped_per_season)]
fwrite(dsum, file.path(out, paste0(tag, "_displacement.csv")))
cat("\n== displacement: weeks per season this player loses his lineup spot ==\n")
print(dsum)

# how often does each incoming piece actually start, and both together?
inc <- rbindlist(lapply(recv_ids, function(p)
  data.table(player_id = p, starts = sa[player_id == p, .N] / n_seasons)))
inc <- merge(nm[, list(player_id, player_name)], inc, by = "player_id")
both <- sa[player_id %in% recv_ids, list(n = uniqueN(player_id)), by = key][n == length(recv_ids), .N] / n_seasons
cat("\n== how often the incoming pieces start (weeks per 14-week season) ==\n")
print(inc); cat(sprintf("  both in the same lineup: %.2f weeks/season\n", both))

## ---- 3. bench waste: points scored by rostered non-starters -----------------------
bench <- function(st, roster_rows, label) {
  all_pts <- roster_rows[, list(season, week, player_id, projected_score)]
  b <- merge(all_pts, st[, list(season, week, player_id, started = TRUE)],
             by = c("season", "week", "player_id"), all.x = TRUE)
  b[is.na(started), started := FALSE]
  data.table(when = label,
             started_pts = b[started == TRUE,  sum(projected_score)] / n_seasons,
             bench_pts   = b[started == FALSE, sum(projected_score)] / n_seasons)
}
bw <- rbind(bench(sb, mine_before, "before"), bench(sa, mine_after, "after"))
bw[, bench_share := bench_pts / (bench_pts + started_pts)]
fwrite(bw, file.path(out, paste0(tag, "_bench_waste.csv")))
cat("\n== bench waste (points per season scored by players who did not start) ==\n")
print(bw)

## ---- plots -----------------------------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  th <- theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey40", size = 9))
  rolecol <- c("IN (received)" = "#1b9e77", "OUT (sent)" = "#d95f02", "stays" = "#7570b3")

  # (a) lineup shift: who plays more / less, in points
  sh <- copy(dd$shift)[abs(d_pts) >= 0.5 | role != "stays"]
  sh[, lab := sprintf("%s (%s)", player_name, pos)]
  sh[, lab := factor(lab, levels = sh[order(d_pts)]$lab)]
  p1 <- ggplot(sh, aes(lab, d_pts, fill = role)) +
    geom_col(width = 0.72) + coord_flip() +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
    scale_fill_manual(values = rolecol, name = NULL) +
    labs(title = sprintf("Where the points move: %s -> %s",
                         paste(dd$send_names, collapse = " + "),
                         paste(dd$recv_names, collapse = " + ")),
         subtitle = sprintf(paste0("Change in points each player puts INTO my starting lineup ",
                                   "per season (mean of %d sims).\nNet = %+.1f pts/season."),
                            n_seasons, sum(dd$shift$d_pts)),
         x = NULL, y = "change in points started per season") + th
  ggsave(file.path(out, paste0(tag, "_lineup_shift.png")), p1,
         width = 10, height = 7, dpi = 150, bg = "white")

  # (b) year+1 value distribution for the post-trade roster
  vd <- copy(dd$vd)[cur_value > 0]
  setorder(vd, cur_value)
  vd[, lab := factor(sprintf("%s (%s)", player_name, pos),
                     levels = sprintf("%s (%s)", player_name, pos))]
  p2 <- ggplot(vd, aes(y = lab, colour = role)) +
    geom_segment(aes(x = next_value_p10, xend = next_value_p90, yend = lab), linewidth = 1.6,
                 alpha = 0.45) +
    geom_point(aes(x = next_value_med), size = 2.6) +
    geom_point(aes(x = cur_value), shape = 124, size = 4, colour = "grey25") +
    scale_colour_manual(values = rolecol, name = NULL) +
    labs(title = "Value after this season: where each asset can land",
         subtitle = paste0("Bar = 10th-90th percentile of next-year dynasty value over ",
                           n_seasons, " simulated seasons; dot = median; ",
                           "grey tick = today's market value.\n",
                           "A bar that sits entirely left of the tick is an asset the market ",
                           "is paying you to sell."),
         x = "dynasty value after this season", y = NULL) + th
  ggsave(file.path(out, paste0(tag, "_value_dist.png")), p2,
         width = 11, height = 8.5, dpi = 150, bg = "white")

  # (c) weekly team score: floor vs ceiling
  wk <- copy(dd$wk)
  qq <- seq(0.02, 0.98, by = 0.01)
  qd <- data.table(q = qq, before = quantile(wk$before, qq), after = quantile(wk$after, qq))
  qd[, delta := after - before]
  p3 <- ggplot(qd, aes(100 * q, delta)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
    geom_line(colour = "#1b9e77", linewidth = 1) +
    geom_area(aes(fill = delta > 0), alpha = 0.18, position = "identity", show.legend = FALSE) +
    scale_fill_manual(values = c(`TRUE` = "#1b9e77", `FALSE` = "#d95f02")) +
    labs(title = "Which weeks get better: the trade buys floor, not ceiling",
         subtitle = paste0("Change in my weekly team score at each percentile of the ",
                           "weekly-score distribution (", n_seasons, " seasons x 14 weeks).\n",
                           "Positive on the left = bad weeks get better."),
         x = "percentile of my weekly team score", y = "points gained at that percentile") + th
  ggsave(file.path(out, paste0(tag, "_score_dist.png")), p3,
         width = 10, height = 6, dpi = 150, bg = "white")
  message("wrote plots for tag ", tag)
}

saveRDS(list(per_start = per_start, dsum = dsum, inc = inc, both = both, bw = bw),
        file.path(out, paste0(tag, "_deepdive2.rds")))
