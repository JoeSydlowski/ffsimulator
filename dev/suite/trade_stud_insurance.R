# The "why break up a stud" test: does the package protect you in the seasons
# where the stud disappoints, and does it cost you in the seasons where he hits?
#
# The headline playoff delta from ffs_trade_eval is an average over all simulated
# seasons, which is exactly where a concentration argument hides. This splits the
# n=2000 seasons into quintiles of the OUTGOING player's own simulated production
# and reports, in each quintile, my points-for and playoff rate with and without
# the trade. A stud worth breaking up is one whose bad quintiles are catastrophic
# for the team and whose good quintiles are worth less than they look.
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
me <- dd$me; opp <- dd$opp; send_ids <- dd$send_ids; recv_ids <- dd$recv_ids

sim <- readRDS(file.path(out, "simulation.rds"))
rs  <- as.data.table(sim$roster_scores)
lc  <- as.data.table(sim$lineup_constraints)
n_seasons <- dd$n_seasons

## ---- league-wide standings, before and after ------------------------------------
retag <- function(rows, fid) {
  rows <- copy(rows); tm <- rs[franchise_id == fid][1]
  for (cc in c("franchise_id", "franchise_name")) set(rows, j = cc, value = tm[[cc]]); rows
}
rs2 <- rbind(
  rs[franchise_id == me  & !player_id %in% send_ids], retag(rs[player_id %in% recv_ids], me),
  rs[franchise_id == opp & !player_id %in% recv_ids], retag(rs[player_id %in% send_ids], opp))
rs2[order(-projected_score), pos_rank := seq_len(.N),
    by = c("league_id", "franchise_id", "pos", "season", "week")]
message("re-optimising the two traded rosters @ ", Sys.time())
reopt <- rbindlist(lapply(c(me, opp), function(f)
  ffsimulator:::.ffs_optimise_started(rs2[franchise_id == f], lc)))

standings <- function(opt) {
  sw <- as.data.table(ffs_summarise_week(optimal_scores = opt, schedules = sim$schedules))
  ss <- as.data.table(ffs_summarise_season(summary_week = sw))
  ss[, lg_rank := frank(list(-h2h_wins, -points_for), ties.method = "first"), by = season]
  ss[franchise_id == me, list(season, wins = h2h_wins, pf = points_for,
                              made = as.integer(lg_rank <= 6))]
}
base_opt  <- as.data.table(sim$optimal_scores)
st_before <- standings(base_opt)
st_after  <- standings(rbind(base_opt[!franchise_id %in% c(me, opp)], reopt, fill = TRUE))

## ---- stratify by the outgoing player's own simulated season --------------------
stud <- rs[player_id %in% send_ids, list(stud_pts = sum(projected_score)), by = season]
z <- merge(merge(st_before, st_after, by = "season", suffixes = c("_b", "_a")), stud, by = "season")
z[, quint := cut(stud_pts, breaks = quantile(stud_pts, seq(0, 1, .2)),
                 include.lowest = TRUE, labels = c("worst 20%", "20-40%", "40-60%",
                                                   "60-80%", "best 20%"))]
tab <- z[, list(
  seasons      = .N,
  stud_pts     = mean(stud_pts),
  pf_before    = mean(pf_b),   pf_after   = mean(pf_a),   d_pf   = mean(pf_a - pf_b),
  wins_before  = mean(wins_b), wins_after = mean(wins_a), d_wins = mean(wins_a - wins_b),
  playoff_before = 100 * mean(made_b), playoff_after = 100 * mean(made_a),
  d_playoff      = 100 * mean(made_a - made_b)), by = quint][order(quint)]
fwrite(tab, file.path(out, paste0(tag, "_stud_insurance.csv")))
cat("\n== seasons split by ", paste(dd$send_names, collapse = " + "),
    "'s own production (n=", n_seasons, ") ==\n", sep = "")
print(tab[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 1) else x)])

cat(sprintf("\noverall: playoff %.1f%% -> %.1f%% (%+.1f)  |  points-for %+.1f/season\n",
            100 * mean(z$made_b), 100 * mean(z$made_a),
            100 * mean(z$made_a - z$made_b), mean(z$pf_a - z$pf_b)))
cat(sprintf("seasons the trade helps my playoff outcome: %.1f%%  hurts: %.1f%%\n",
            100 * mean(z$made_a > z$made_b), 100 * mean(z$made_a < z$made_b)))

## ---- plot -----------------------------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  pl <- melt(tab[, list(quint, before = playoff_before, after = playoff_after)],
             id.vars = "quint", variable.name = "when", value.name = "playoff")
  p <- ggplot(pl, aes(quint, playoff, fill = when)) +
    geom_col(position = position_dodge(width = .78), width = .72) +
    geom_text(aes(label = sprintf("%.0f%%", playoff)),
              position = position_dodge(width = .78), vjust = -0.4, size = 3.2, colour = "grey25") +
    scale_fill_manual(values = c(before = "#d95f02", after = "#1b9e77"), name = NULL,
                      labels = c(before = "keep Puka", after = "trade for McBride + Flowers")) +
    labs(title = sprintf("Insurance: playoff odds split by how %s's own season goes",
                         paste(dd$send_names, collapse = " + ")),
         subtitle = paste0("The 2,000 simulated seasons ranked by the outgoing player's own ",
                           "fantasy production, in quintiles.\nThe trade is protection: it pays ",
                           "most where he lets you down, and costs least where he carries you."),
         x = paste0("quintile of ", paste(dd$send_names, collapse = " + "), "'s simulated season"),
         y = "my playoff odds (%)") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "top",
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey40", size = 9))
  ggsave(file.path(out, paste0(tag, "_stud_insurance.png")), p,
         width = 10, height = 6, dpi = 150, bg = "white")
  message("wrote ", file.path(out, paste0(tag, "_stud_insurance.png")))
}
