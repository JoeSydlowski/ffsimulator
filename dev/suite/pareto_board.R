#!/usr/bin/env Rscript
# Pareto frontier of a whole roster trade board on the two objectives Joe
# actually trades off: win-now (playoff-odds delta) and reliability-adjusted
# future capital. Everything not on the frontier is beaten on BOTH at once and
# can be dropped from consideration without a judgement call.
#
# The board's own `score` collapses the two with the engine's exchange rate
# (playoff_value / my_haircut adj_fut per playoff point). The frontier is the
# weight-free view: it says which deals could be optimal under ANY weighting.
#
#   Rscript dev/suite/pareto_board.R [sim_dir]

suppressMessages({library(data.table); library(ggplot2)})

my_team <- Sys.getenv("FFS_MY_TEAM", "Rootin' Tuten")
dir_in <- commandArgs(TRUE)[1]
if (is.na(dir_in) || !nzchar(dir_in))
  dir_in <- "dev/league_sims/1312145437842497536/2026-09-03"

# player_trade_board.R bakes a +0.5 win-win bump into score; it is a
# negotiability nudge, not part of the model, so back it out.
board_winwin <- 0.5
winwin_bonus <- as.numeric(Sys.getenv("FFS_PARETO_WINWIN_BONUS", "0"))
# targets judged unavailable no matter what the board says
exclude <- trimws(strsplit(Sys.getenv("FFS_PARETO_EXCLUDE", ""), ",", fixed = TRUE)[[1]])
exclude <- exclude[nzchar(exclude)]

b <- fread(file.path(dir_in, "roster_trade_board.csv"))
if (length(exclude)) b <- b[!receive %in% exclude]
b[, score := round(score - (board_winwin - winwin_bonus) * (win_win %in% TRUE), 2)]

my_pl <- fread(file.path(dir_in, "playoff_odds.csv"))[franchise_name == my_team, playoff_pct]
my_haircut <- min(max(1.30 - my_pl, 0.60), 1.00)
playoff_value <- 68
fut_per_pl <- playoff_value / my_haircut

# --- frontier ---------------------------------------------------------------
# dominated = some other deal is >= on both axes and > on at least one
b[, pareto := vapply(seq_len(.N), function(i)
  !any((mine_pl >= mine_pl[i] & adj_fut >= adj_fut[i]) &
       (mine_pl >  mine_pl[i] | adj_fut >  adj_fut[i])), logical(1))]

setorder(b, -score)
cat(sprintf("board: %d deals, %d on the Pareto frontier\n", nrow(b), sum(b$pareto)))
cat(sprintf("exchange rate: %.1f%% playoff odds -> haircut %.2f -> 1 playoff pt = %.0f adj_fut\n\n",
            100 * my_pl, my_haircut, fut_per_pl))

fr <- b[(pareto)][order(-mine_pl)]
cat("-- the frontier, most win-now first --\n")
print(fr[, .(team = substr(team, 1, 16), send = substr(send, 1, 32), receive = substr(receive, 1, 18),
             mine_pl, opp_pl, adj_fut, score, win_win)])

cat("\n-- rate paid stepping toward win-now (adj_fut given up per playoff pt) --\n")
w <- b[(pareto)][order(mine_pl)]
for (i in seq_len(nrow(w) - 1)) {
  r <- abs((w$adj_fut[i+1] - w$adj_fut[i]) / (w$mine_pl[i+1] - w$mine_pl[i]))
  cat(sprintf("  %-30s -> %-30s  %+.1f%% for %+5d = %6.0f fut/pt  %s\n",
      substr(paste0(w$sending[i], " -> ", w$receive[i]), 1, 30),
      substr(paste0(w$sending[i+1], " -> ", w$receive[i+1]), 1, 30),
      w$mine_pl[i+1] - w$mine_pl[i], w$adj_fut[i+1] - w$adj_fut[i], r,
      if (r < fut_per_pl) "<- cheap, step up" else ""))
}

sfx <- if (length(exclude)) "_avail" else ""
out <- file.path(dir_in, paste0("pareto_board", sfx, ".csv"))
fwrite(b[, .(pareto, team, sending, send, receive, overpay, opp_edge, mine_pl, opp_pl,
             fut, adj_fut, score, grade, win_win, backfill, opp_drop)], out)
cat("\nwrote ", out, "\n", sep = "")

# --- plot -------------------------------------------------------------------
iso <- with(b, seq(floor(min(adj_fut + fut_per_pl * mine_pl) / fut_per_pl) * fut_per_pl,
                   ceiling(max(adj_fut + fut_per_pl * mine_pl) / fut_per_pl) * fut_per_pl,
                   by = 2 * fut_per_pl))
b[, lab := fifelse(pareto, paste0(sending, " -> ", receive), NA_character_)]

p <- ggplot(b, aes(mine_pl, adj_fut)) +
  geom_abline(data = data.frame(bb = iso), aes(slope = -fut_per_pl, intercept = bb),
    colour = "grey90", linewidth = 0.3, linetype = "22") +
  geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.4) +
  geom_point(data = b[!(pareto)], colour = "grey72", size = 1.9, alpha = 0.7) +
  geom_step(data = b[(pareto)][order(mine_pl)], direction = "vh",
    colour = "#C0651F", linewidth = 0.6, alpha = 0.6) +
  geom_point(data = b[(pareto)], aes(fill = win_win), size = 3.6, shape = 21,
    colour = "#8A4513", stroke = 0.7) +
  scale_fill_manual(values = c(`TRUE` = "#5E9C82", `FALSE` = "white"),
    labels = c(`TRUE` = "they gain too", `FALSE` = "they give up value"),
    name = "Win-win?") +
  ggrepel::geom_text_repel(aes(label = lab), size = 2.9, colour = "grey25",
    na.rm = TRUE, max.overlaps = Inf, box.padding = 0.55, min.segment.length = 0,
    segment.color = "grey65", seed = 1) +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")) +
  labs(title = sprintf("%s: the efficient frontier of the whole trade board", my_team),
    subtitle = sprintf("%d gettable 2-for-1s; %d are Pareto-optimal. Dotted diagonals are equal value at the engine's rate (1 playoff pt = %.0f adj_fut)",
                       nrow(b), sum(b$pareto), fut_per_pl),
    x = "My playoff-odds change (win-now)",
    y = "Reliability-adjusted future capital gained (adj_fut)",
    caption = "grey = dominated on both axes • labels are the asset leaving -> the asset arriving • n=2000 exact evals, win-win bonus removed") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank(),
        plot.caption = element_text(colour = "grey55"))

png <- file.path(dir_in, paste0("pareto_board", sfx, ".png"))
ggsave(png, p, width = 15, height = 9.5, dpi = 150, bg = "white")
cat("wrote ", png, "\n", sep = "")
