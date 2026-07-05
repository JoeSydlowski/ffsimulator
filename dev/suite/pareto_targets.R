# Pareto-optimal trade targets for your team.
#
# Three objectives, all from pieces the suite already computes:
#   1. improve my team   -> value_to_you  (h2h wins added to MY roster)   [max]
#   2. cost the least     -> dyn_value     (dynasty market value = price)  [min]
#   3. don't fall off     -> retention = next_value_mean / cur_value       [max]
#
# Front-1 players are Pareto-optimal: no other target beats them on all three
# at once. They are the shortlist worth pursuing; everyone else is dominated
# by someone strictly better.
#
# Usage: FFS_LEAGUE_ID=<id> FFS_MY_TEAM=<team> Rscript dev/suite/pareto_targets.R
# Reads the newest saved simulation; writes pareto_targets.csv + pareto.png.

library(data.table)
library(ggplot2)
devtools::load_all(here::here(), quiet = TRUE)
options(ffsimulator.verbose = FALSE)

league_id <- Sys.getenv("FFS_LEAGUE_ID", "1359546500786434048")
my_team <- Sys.getenv("FFS_MY_TEAM", "sox05syd")
top_n <- as.integer(Sys.getenv("FFS_TRADE_TOP_N", "60"))

league_dir <- here::here("dev", "league_sims", league_id)
sims <- Sys.glob(file.path(league_dir, "*", "simulation.rds"))
stopifnot(length(sims) > 0)
out <- dirname(sims[order(file.info(sims)$mtime, decreasing = TRUE)][1])

# trade valuation re-optimizes a franchise per candidate, so it scales with
# the sim's season count; target *rankings* are stable well below the
# standings-grade 400, so value on a fast dedicated sim (see convergence.csv)
n_trade <- as.integer(Sys.getenv("FFS_TRADE_NSIMS", "60"))
message("building n=", n_trade, " valuation sim @ ", Sys.time())
conn <- ffscrapr::sleeper_connect(season = as.integer(Sys.getenv("FFS_SEASON", "2026")),
                                  league_id = league_id)
sim <- ff_simulate(conn, n_seasons = n_trade, version = "v3",
                   lineup_method = "rank", replacement_level = FALSE, return = "all")

fr <- as.data.table(sim$franchises)
me <- fr[franchise_name == my_team, franchise_id][1]
stopifnot(!is.na(me))

## ---- axis 1+2 owner side: value to me and to current owner --------------------
message("valuing trade targets @ ", Sys.time())
targets <- as.data.table(ffs_trade_targets(sim, me, top_n = top_n))

## ---- axis 2+3 dynasty: market value and retention (superflex-aware) -----------
message("dynasty outlook @ ", Sys.time())
dyn <- as.data.table(ffs_dynasty_outlook(sim))  # format auto-detected

cand <- merge(
  targets,
  dyn[, list(player_id, dyn_value = cur_value, dyn_next = next_value_mean,
             p_rise, p_exit)],
  by = "player_id", all.x = TRUE
)

# keep players who (a) actually improve me and (b) have a dynasty market value
cand <- cand[value_to_you > 0 & !is.na(dyn_value) & dyn_value > 0]
cand[, retention := dyn_next / dyn_value]

## ---- Pareto frontier over the three objectives --------------------------------
cand[, front := ffs_pareto_front(
  cand[, list(value_to_you, dyn_value, retention)],
  maximize = c(TRUE, FALSE, TRUE)   # improve up, cost down, retention up
)]
setorder(cand, front, -value_to_you)

# a simple efficiency score for ordering within/among fronts: wins added per
# 1000 units of dynasty cost, tilted by retention
cand[, wins_per_1k := value_to_you / (dyn_value / 1000)]
cand[, pareto_score := scale(value_to_you) - scale(dyn_value) + scale(retention)]

# a trade only clears if the player is worth more to me than to his owner
cand[, gettable := surplus > 0]
fwrite(cand, file.path(out, "pareto_targets.csv"))

show_cols <- function(dt) dt[, list(
  player_name, pos, owner = owner_id,
  adds = round(value_to_you, 2), owner_val = round(value_to_owner, 2),
  surplus = round(surplus, 2), cost = round(dyn_value),
  retention = round(retention, 2))]

cat("\n==== Pareto front 1 (", sum(cand$front == 1), "players ) - no target beats them on all 3 ====\n")
cat("With 3 objectives the frontier is broad; the shortlist is front-1 AND gettable (surplus > 0):\n\n")
print(show_cols(cand[front == 1 & gettable == TRUE][order(-value_to_you)]))

cat("\n==== frontier corners (best on each single axis, among gettable) ====\n")
g <- cand[gettable == TRUE]
corners <- unique(rbind(
  g[which.max(value_to_you)][, tag := "most improvement"],
  g[which.min(dyn_value)][, tag := "cheapest"],
  g[which.max(retention)][, tag := "best value retention"],
  g[which.max(surplus)][, tag := "easiest to pry loose"]
))
print(corners[, list(tag, player_name, pos, adds = round(value_to_you, 2),
                     cost = round(dyn_value), retention = round(retention, 2),
                     surplus = round(surplus, 2))])

## ---- plot: efficient frontier -------------------------------------------------
f1 <- cand[front == 1][order(dyn_value)]
p <- ggplot(cand, aes(x = dyn_value, y = value_to_you)) +
  geom_point(aes(size = retention, color = factor(front)), alpha = 0.7) +
  geom_line(data = f1, color = "#1b9e77", linewidth = 0.8) +
  ggrepel::geom_text_repel(
    data = cand[front == 1],
    aes(label = player_name), size = 3, max.overlaps = 20
  ) +
  scale_color_manual(values = c("1" = "#1b9e77", "2" = "#7570b3",
                                "3" = "#999999", "4" = "#cccccc"),
                     name = "Pareto front") +
  scale_size_continuous(name = "value retention", range = c(1.5, 6)) +
  labs(
    title = paste0(my_team, ": Pareto trade targets"),
    subtitle = "up = improves my team more; left = cheaper; bigger = holds value better",
    x = "acquisition cost (dynasty market value)",
    y = "wins added to my roster (value_to_you)"
  ) +
  theme_minimal(base_size = 12)

has_repel <- requireNamespace("ggrepel", quietly = TRUE)
if (!has_repel) {
  p <- ggplot(cand, aes(x = dyn_value, y = value_to_you)) +
    geom_point(aes(size = retention, color = factor(front)), alpha = 0.7) +
    geom_line(data = f1, color = "#1b9e77", linewidth = 0.8) +
    labs(title = paste0(my_team, ": Pareto trade targets"),
         x = "acquisition cost (dynasty value)", y = "wins added to my roster") +
    theme_minimal(base_size = 12)
}
ggsave(file.path(out, "pareto.png"), p, width = 10, height = 7, dpi = 150)
cat("\nwrote", file.path(out, "pareto_targets.csv"), "and pareto.png\n")
