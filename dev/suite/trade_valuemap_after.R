# Re-draw the win-now value map for the roster you would have AFTER a trade.
#
# trade_intel.R's value_map.png prices my CURRENT roster against the league's
# win-now price line. After a trade the interesting question is different: does
# the new roster still sit on the right side of that line, and did the pieces I
# took back land above it? This rebuilds the simulation with the swap applied
# (both franchises re-optimised), re-values every player I would own with
# ffs_player_value on the n=2000 sim, and plots BEFORE vs AFTER against the same
# fixed league line (the market for players elsewhere doesn't move because I
# traded, so holding the line fixed is what makes the two panels comparable).
#
# Usage: same env vars as dev/suite/trade_deepdive.R (run that first - this reads
# its <tag>_deepdive.rds for the deal definition).

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
me <- dd$me; opp <- dd$opp
send_ids <- dd$send_ids; recv_ids <- dd$recv_ids

message("report folder: ", out, "  tag: ", tag)
sim <- readRDS(file.path(out, "simulation.rds"))
rs  <- as.data.table(sim$roster_scores)

## ---- rebuild the simulation with the swap applied --------------------------------
retag <- function(rows, fid) {
  rows <- copy(rows)
  tm <- rs[franchise_id == fid][1]
  for (cc in c("franchise_id", "franchise_name")) set(rows, j = cc, value = tm[[cc]])
  rows
}
rs2 <- rbind(
  rs[!franchise_id %in% c(me, opp)],
  rs[franchise_id == me  & !player_id %in% send_ids], retag(rs[player_id %in% recv_ids], me),
  rs[franchise_id == opp & !player_id %in% recv_ids], retag(rs[player_id %in% send_ids], opp))
# pos_rank must reflect the modified rosters (the optimiser's candidate trim uses it)
rs2[order(-projected_score), pos_rank := seq_len(.N),
    by = c("league_id", "franchise_id", "pos", "season", "week")]

# Rank-only re-optimisation (.ffs_optimise_started): one LP per franchise-week
# instead of ffs_optimise_lineups' two, and it returns the two columns everything
# downstream reads - actual_score (all ffs_summarise_week needs) and
# starter_player_id (how .ffs_counterfactual_rows finds the weeks a valuation
# actually changes). The hindsight optimal_score is never consumed here, and
# computing it for 56k franchise-weeks is what made the full path unusable.
message("re-optimising both franchises' lineups on n=", dd$n_seasons, " @ ", Sys.time())
reopt <- rbindlist(lapply(c(me, opp), function(f)
  ffsimulator:::.ffs_optimise_started(
    rs2[franchise_id == f], ffsimulator:::.ffs_sim_lineup_constraints(sim))))

sim2 <- sim
sim2$roster_scores  <- rs2
sim2$optimal_scores <- rbind(
  as.data.table(sim$optimal_scores)[!franchise_id %in% c(me, opp)], reopt, fill = TRUE)

## ---- value every player on the post-trade roster ---------------------------------
post_ids <- unique(rs2[franchise_id == me &
                       !grepl("^(QB|RB|WR|TE|K)_\\d+$", player_id)]$player_id)
message("valuing ", length(post_ids), " post-trade players @ ", Sys.time())
base_me2 <- ffsimulator:::.ffs_franchise_summary(sim2, me)
after_val <- rbindlist(lapply(post_ids, function(p) {
  v <- ffs_player_value(sim2, p, me, base_summary = base_me2)
  data.table(player_id = as.character(p), playoff_delta_me = v$playoff_pct,
             value_to_me = v$h2h_wins, champ_delta_me = v$champion_pct)
}))
nm <- unique(rs2[, list(player_id = as.character(player_id), player_name, pos)], by = "player_id")
after_val <- merge(after_val, nm, by = "player_id")

## ---- same win-now definition + same fixed league price line ----------------------
dyn <- fread(file.path(out, "dynasty_outlook.csv"),
             colClasses = list(character = c("player_id", "fantasypros_id")))
after_val <- merge(after_val, dyn[, list(player_id, cur_value, next_value_med)],
                   by = "player_id", all.x = TRUE)
after_val[, win_now_value := cur_value - next_value_med]

tg <- fread(file.path(out, "targets.csv"), colClasses = list(character = "player_id"))
mkt_wn   <- tg[win_now_value > 0 & is.finite(playoff_delta_you) & confirmed == TRUE]
mkt_coef <- coef(lm(win_now_value ~ playoff_delta_you, mkt_wn))
message("league win-now price line (confirmed targets, n=", nrow(mkt_wn), "): ",
        sprintf("win_now = %.0f + %.0f * playoff_delta", mkt_coef[[1]], mkt_coef[[2]]))
edge_of <- function(d, valcol) {
  e <- rep(NA_real_, nrow(d))
  w <- which(d$win_now_value > 0 & is.finite(d[[valcol]]))
  e[w] <- (mkt_coef[[1]] + mkt_coef[[2]] * d[[valcol]][w]) - d$win_now_value[w]
  e
}
after_val[, win_now_edge := edge_of(after_val, "playoff_delta_me")]

before_val <- fread(file.path(out, "roster.csv"), colClasses = list(character = "player_id"))
before_val <- before_val[, list(player_id, player_name, pos, cur_value, win_now_value,
                                playoff_delta_me = playoff_add, verdict)]
before_val[, win_now_edge := edge_of(before_val, "playoff_delta_me")]

vm <- rbind(
  before_val[, list(player_name, pos, cur_value, win_now_value, win_now_edge,
                    pd = playoff_delta_me, panel = "BEFORE (today's roster)",
                    role = fifelse(player_id %in% send_ids, "OUT (sent)", "stays"))],
  after_val[, list(player_name, pos, cur_value, win_now_value, win_now_edge,
                   pd = playoff_delta_me, panel = "AFTER (post-trade roster)",
                   role = fifelse(player_id %in% recv_ids, "IN (received)", "stays"))])
vm[, panel := factor(panel, levels = c("BEFORE (today's roster)", "AFTER (post-trade roster)"))]
vm[, cat := fcase(win_now_value <= 0, "future (judge on trajectory)",
                  win_now_edge > 0,  "win-now bargain (edge)",
                  default = "win-now priced-rich")]
fwrite(vm, file.path(out, paste0(tag, "_valuemap.csv")))

## ---- plot ------------------------------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  yv <- seq(0, max(vm$pd, na.rm = TRUE), length.out = 60)
  ln <- rbindlist(lapply(levels(vm$panel), function(p)
    data.table(pd = yv, win_now_value = mkt_coef[[1]] + mkt_coef[[2]] * yv,
               panel = factor(p, levels = levels(vm$panel)))))
  p <- ggplot2::ggplot(vm, ggplot2::aes(win_now_value, pd)) +
    ggplot2::geom_vline(xintercept = 0, color = "#dddddd", linewidth = 0.4) +
    ggplot2::geom_line(data = ln, color = "#333333", linewidth = 0.6, linetype = "21") +
    ggplot2::geom_point(ggplot2::aes(color = cat, size = cur_value, shape = role), alpha = 0.85) +
    ggplot2::facet_wrap(~panel) +
    ggplot2::scale_color_manual(values = c("win-now bargain (edge)" = "#1b9e77",
      "win-now priced-rich" = "#d95f02", "future (judge on trajectory)" = "#8c8c8c"), name = NULL) +
    ggplot2::scale_shape_manual(values = c("stays" = 16, "OUT (sent)" = 4,
                                           "IN (received)" = 17), name = NULL) +
    ggplot2::scale_size_continuous(range = c(1.4, 7), guide = "none") +
    ggplot2::labs(
      title = sprintf("Win-now value map before vs after: %s -> %s",
                      paste(dd$send_names, collapse = " + "),
                      paste(dd$recv_names, collapse = " + ")),
      subtitle = paste0("Dashed line = the league's win-now price (fixed, from confirmed targets). ",
                        "Above/left of it = you get his win-now cheap.\n",
                        "Left of 0 = future asset. Point size = market value."),
      x = "win_now_value = market's charge for this-year production (cur - next median)",
      y = "playoff odds this player adds to YOUR team (n=2000)") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(color = "#555555", size = 9))
  if (requireNamespace("ggrepel", quietly = TRUE))
    p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = player_name, color = cat),
      size = 2.6, min.segment.length = 0, max.overlaps = 40, seed = 1,
      show.legend = FALSE, segment.color = "#bbbbbb", segment.size = 0.2)
  png <- file.path(out, paste0(tag, "_valuemap.png"))
  ggplot2::ggsave(png, p, width = 14, height = 7, dpi = 150, bg = "white")
  message("wrote ", png)
}

saveRDS(list(vm = vm, mkt_coef = mkt_coef, after_val = after_val, before_val = before_val),
        file.path(out, paste0(tag, "_valuemap.rds")))
