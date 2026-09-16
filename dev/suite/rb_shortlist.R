#!/usr/bin/env Rscript
# RB consolidation shortlist: two of mine for one RB.
#
# Reads a roster trade board (dev/suite/player_trade_board.R output) and keeps the
# 2-for-1 deals that buy an RB without either side taking a real playoff hit, then
# maps them on win-now (x) against reliability-adjusted future capital (y).
#
# adj_fut, not fut, is what the engine actually scores on: fut is the raw next-year
# value swing, adj_fut discounts the projected *move* per position (QB ~0.55 in
# superflex) and level-discounts draft picks. Plotting adj_fut is plotting the
# number the ranking believes.
#
#   Rscript dev/suite/rb_shortlist.R [sim_dir]

suppressMessages({library(data.table); library(ggplot2)})

my_team <- Sys.getenv("FFS_RB_MY_TEAM", "Rootin' Tuten")
dir_in <- commandArgs(TRUE)[1]
if (is.na(dir_in) || !nzchar(dir_in))
  dir_in <- "dev/league_sims/1312145437842497536/2026-09-03"

# --- knobs ------------------------------------------------------------------
max_pl_drop <- as.numeric(Sys.getenv("FFS_RB_MAX_PL_DROP", "1"))   # neither side may lose more than this many playoff pts
min_opp_edge <- as.numeric(Sys.getenv("FFS_RB_MIN_OPP_EDGE", "0")) # they must be getting at least fair value
# future-value gate: "either" keeps a deal if fut OR adj_fut is positive;
# "adj" is the stricter cut on the number that scores; "raw" is the old fut-only rule.
fut_rule <- Sys.getenv("FFS_RB_FUT_RULE", "either")
# targets Joe judges unavailable regardless of what the board says - the engine
# only knows value and playoff odds, not whether an owner will pick up the phone.
exclude <- Sys.getenv("FFS_RB_EXCLUDE", "Jeremiyah Love,Jadarian Price,Ashton Jeanty")
exclude <- trimws(strsplit(exclude, ",", fixed = TRUE)[[1]])
exclude <- exclude[nzchar(exclude)]
# player_trade_board.R bakes a +0.5 win-win ranking bump into score. It is a
# negotiability nudge, not part of the win-now/future model, so back it out and
# rank on the two objectives alone; win_win stays in the output to read directly.
board_winwin <- 0.5
winwin_bonus <- as.numeric(Sys.getenv("FFS_RB_WINWIN_BONUS", "0"))

board <- fread(file.path(dir_in, "roster_trade_board.csv"))
dyn   <- fread(file.path(dir_in, "dynasty_outlook.csv"))
rbs   <- unique(dyn[pos == "RB", player_name])

fut_ok <- switch(fut_rule,
  either = board$fut > 0 | board$adj_fut > 0,
  adj    = board$adj_fut > 0,
  raw    = board$fut > 0,
  stop("FFS_RB_FUT_RULE must be one of: either, adj, raw"))

s <- board[shape == "2-for-1" & gettable & receive %in% rbs &
           !receive %in% exclude &
           opp_edge > min_opp_edge &
           mine_pl > -max_pl_drop & opp_pl > -max_pl_drop &
           fut_ok]
s[, score := round(score - (board_winwin - winwin_bonus) * (win_win %in% TRUE), 2)]
s[, grade := data.table::fcase(score >= 18, "A", score >= 10, "B",
                               score >= 4, "C", score >= 0, "D", default = "F")]
setorder(s, -score)
s[, rank := .I]

# --- Pareto frontier on (win-now, adj_fut) ----------------------------------
# a deal is dominated if another is at least as good on BOTH axes and strictly
# better on one. The frontier is the only set worth choosing between; everything
# else is beaten outright on both objectives at once.
s[, pareto := vapply(seq_len(.N), function(i)
  !any((mine_pl >= mine_pl[i] & adj_fut >= adj_fut[i]) &
       (mine_pl >  mine_pl[i] | adj_fut >  adj_fut[i])), logical(1))]

# the rate the engine itself trades one against the other: score is
# 100*playoff_delta + adj_fut * my_haircut / playoff_value, so one playoff point
# is worth playoff_value / my_haircut points of adjusted future capital.
my_pl <- fread(file.path(dir_in, "playoff_odds.csv"))[franchise_name == my_team, playoff_pct]
my_haircut <- min(max(1.30 - my_pl, 0.60), 1.00)
playoff_value <- 68
fut_per_pl <- playoff_value / my_haircut

out_cols <- c("rank", "pareto", "team", "send", "receive", "overpay", "fair", "opp_edge",
              "send_value", "recv_value", "mine_pl", "opp_pl", "fut", "adj_fut",
              "score", "grade", "win_win", "backfill", "opp_drop")
csv <- file.path(dir_in, "rb_shortlist.csv")
fwrite(s[, ..out_cols], csv)
message("wrote ", nrow(s), " deals to ", csv, "  (rule: ", fut_rule, ")")

# --- plot -------------------------------------------------------------------
s[, lab := paste0(rank, ". ", send, " -> ", receive)]

# iso-value grid spanning the data, spaced one full playoff point apart
iso <- with(s, seq(floor(min(adj_fut + fut_per_pl * mine_pl) / fut_per_pl) * fut_per_pl,
                   ceiling(max(adj_fut + fut_per_pl * mine_pl) / fut_per_pl) * fut_per_pl,
                   by = fut_per_pl))

p <- ggplot(s, aes(mine_pl, adj_fut)) +
  geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.4) +
  # iso-value lines at the engine's own exchange rate: every deal on one line is
  # worth the same, so a deal is better than another only by sitting on a HIGHER
  # line, not by being further right or further up.
  geom_abline(data = data.frame(b = iso), aes(slope = -fut_per_pl, intercept = b),
    colour = "grey88", linewidth = 0.35, linetype = "22") +
  geom_step(data = s[(pareto)][order(mine_pl)], direction = "vh",
    colour = "#C0651F", linewidth = 0.5, alpha = 0.5) +
  geom_point(aes(colour = overpay, size = score, shape = pareto), alpha = 0.95) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1),
    labels = c(`TRUE` = "on the frontier", `FALSE` = "dominated"), name = NULL) +
  ggrepel::geom_text_repel(aes(label = lab), size = 3, colour = "grey25",
    max.overlaps = Inf, box.padding = 0.7, min.segment.length = 0,
    segment.color = "grey70", seed = 1) +
  scale_colour_gradient(low = "#5E9C82", high = "#C0651F",
    name = "I overpay by (%)\n10 = the fair rate") +
  scale_size_continuous(range = c(3, 8), name = "Score") +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")) +
  labs(
    title = "Goofball: two of mine for one RB",
    subtitle = sprintf(
      "Future value survives the reliability haircut, neither side loses more than %g playoff point%s, and I pay at or above the fair 10%% consolidation premium",
      max_pl_drop, if (max_pl_drop == 1) "" else "s"),
    x = "My playoff-odds change (win-now)",
    y = "Reliability-adjusted future capital gained (adj_fut)",
    caption = sprintf("all %d qualifying deals labelled • y is adj_fut: QB projections cut ~45%%, picks ~45%%%s • n=2000 on the bye-fixed sim, no draft picks",
      nrow(s), if (length(exclude)) paste0(" • excluded as unavailable: ", paste(exclude, collapse = ", ")) else "")) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank(),
        plot.caption = element_text(colour = "grey55"))

# Windows keeps a hard lock on a PNG that is open in a viewer, and ggsave only
# warns - so verify the file actually moved and fall back to a suffixed name.
png <- file.path(dir_in, "rb_shortlist.png")
before <- if (file.exists(png)) file.mtime(png) else NA
suppressWarnings(ggsave(png, p, width = 17, height = 10.5, dpi = 150, bg = "white"))
if (!is.na(before) && identical(file.mtime(png), before)) {
  png <- file.path(dir_in, "rb_shortlist_adjfut.png")
  ggsave(png, p, width = 17, height = 10.5, dpi = 150, bg = "white")
  message("rb_shortlist.png was locked (close the viewer); wrote ", png, " instead")
} else message("wrote plot ", png)

# what the rule change actually did
cat("\n-- rows by rule --\n")
for (r in c("raw", "either", "adj")) {
  ok <- switch(r, either = board$fut > 0 | board$adj_fut > 0,
                  adj = board$adj_fut > 0, raw = board$fut > 0)
  n <- nrow(board[shape == "2-for-1" & gettable & receive %in% rbs & !receive %in% exclude & opp_edge > min_opp_edge &
                  mine_pl > -max_pl_drop & opp_pl > -max_pl_drop & ok])
  cat(sprintf("  %-7s %d\n", r, n))
}
cat("\n-- deals where fut and adj_fut disagree most --\n")
print(s[order(fut - adj_fut, decreasing = TRUE)][1:5,
  .(rank, send = substr(send, 1, 34), receive, fut, adj_fut, gap = fut - adj_fut, score)])

cat(sprintf("\n-- exchange rate: %.0f%% playoff odds -> haircut %.2f -> 1 playoff point = %.0f adj_fut --\n",
            100 * my_pl, my_haircut, fut_per_pl))
cat("\n-- Pareto frontier (win-now vs adj_fut) --\n")
print(s[(pareto)][order(-mine_pl), .(rank, send = substr(send, 1, 34), receive,
        mine_pl, adj_fut, score, win_win = NULL)])
cat("\n-- rate you pay stepping along the frontier (fut given up per playoff pt gained) --\n")
fr <- s[(pareto)][order(mine_pl)]
if (nrow(fr) > 1) for (i in 2:nrow(fr))
  cat(sprintf("  #%d -> #%d : %+.1f%% win-now for %+d adj_fut = %.0f fut/pt  (engine charges %.0f) -> %s\n",
      fr$rank[i-1], fr$rank[i], fr$mine_pl[i] - fr$mine_pl[i-1],
      fr$adj_fut[i] - fr$adj_fut[i-1],
      abs((fr$adj_fut[i] - fr$adj_fut[i-1]) / (fr$mine_pl[i] - fr$mine_pl[i-1])), fut_per_pl,
      if (abs((fr$adj_fut[i]-fr$adj_fut[i-1])/(fr$mine_pl[i]-fr$mine_pl[i-1])) < fut_per_pl)
        "cheap win-now, step up" else "expensive win-now, stay put"))
