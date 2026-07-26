# Plot the fair Puka trade board: win-now (x) vs future value (y), colored by how
# the OTHER team fares (acceptability), shaped by deal type.
suppressMessages({library(data.table); library(ggplot2)})
has_repel <- requireNamespace("ggrepel", quietly = TRUE)
BASE <- "dev/league_sims/1359546500786434048"
b <- fread(file.path(BASE, "trade_board_v2.csv"))
b <- b[mine_pl >= -8 & opp_pl >= -8]   # reasonable for BOTH sides

b[, headliner := tstrsplit(receive, " \\+ ", keep = 1)]
b[, deal_type := ifelse(src == "qb", "QB upgrade (send Jones back)", "WR surplus -> RB/WR")]
# one label per team: its best (highest score) fair idea
b[, is_best := seq_len(.N) == which.max(score), by = team]
b[, lab := ifelse(is_best, paste0(team, ": ", headliner), NA_character_)]

p <- ggplot(b, aes(x = mine_pl, y = fut)) +
  geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.4) +
  geom_point(aes(colour = opp_pl, shape = deal_type), size = 4.2, stroke = 0.6) +
  scale_colour_gradient2(
    low = "#B45309", mid = "grey78", high = "#1D6D9C", midpoint = 0,
    name = "Their playoffΔ (accept?)",
    breaks = c(-6, -3, 0, 3, 6),
    labels = c("-6% (hard)", "-3%", "0", "+3%", "+6% (easy)")) +
  scale_shape_manual(values = c("QB upgrade (send Jones back)" = 17,
                                "WR surplus -> RB/WR" = 16), name = "Deal type")
if (has_repel) {
  p <- p + ggrepel::geom_text_repel(aes(label = lab), size = 3.1, colour = "grey20",
             na.rm = TRUE, box.padding = 0.5, max.overlaps = 20, seg.color = "grey70")
} else {
  p <- p + geom_text(aes(label = lab), size = 3.0, colour = "grey20", vjust = -0.9, na.rm = TRUE)
}
p <- p +
  annotate("text", x = max(b$mine_pl), y = max(b$fut)*1.02, hjust = 1,
           label = "win now + bank future ↗", colour = "grey55", size = 3, fontface = 3) +
  annotate("text", x = min(b$mine_pl), y = max(b$fut)*1.02, hjust = 0,
           label = "↖ future play (costs wins now)", colour = "grey55", size = 3, fontface = 3) +
  labs(
    title = "Fair Puka trade offers (Jon, superflex)",
    subtitle = "x = your win-now impact  •  y = future value banked  •  colour = will the other side accept",
    x = "Your playoff-odds change (win-now)", y = "Future dynasty capital gained",
    caption = "n=60 search estimates • fair to both: gap ≤8% and neither side below -8% playoff") +
  scale_x_continuous(labels = function(x) paste0(ifelse(x>0,"+",""), x, "%")) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey93"),
        legend.position = "right",
        plot.caption = element_text(colour = "grey55"))

outpng <- file.path(BASE, "trade_plot.png")
ggsave(outpng, p, width = 9.5, height = 6.2, dpi = 150, bg = "white")
cat("wrote", outpng, "| ggrepel:", has_repel, "| points:", nrow(b), "\n")
