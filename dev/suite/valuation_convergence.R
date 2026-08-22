# Valuation convergence: how many simulated seasons do trade valuations need?
#
# ffs_player_value / ffs_trade_eval re-optimize lineups across every simulated
# season, so their cost scales with n_seasons. This study finds the smallest n
# where the *decisions* stabilize: for n in {30,60,120,240} x 3 seeds it builds
# a valuation sim (v3, rank lineups, real schedule) and evaluates a fixed panel:
#   (a) value_to_you / value_to_owner for candidates spanning the value spectrum
#       (top / mid / tail of roster - the tail is expected to be noisiest), and
#   (b) my/opp playoff deltas for fixed 1:1 deals.
# Reports rank stability (Spearman across seed pairs), run-to-run SD by tier,
# playoff-delta SD, and wall clock. The chosen n becomes the default
# FFS_TRADE_NSIMS for dev/suite/trade_intel.R.
#
# Usage: Rscript dev/suite/valuation_convergence.R
# Writes dev/validate_outputs/valuation_convergence.csv
# Env: FFS_LEAGUE_ID, FFS_MY_TEAM, FFS_SEASON, FFS_CONV_NS (csv), FFS_CONV_SEEDS,
#      FFS_CONV_PANEL (candidates per tier), FFS_CONV_DEALS

library(data.table)
devtools::load_all(here::here(), quiet = TRUE)
# ffscrapr reads a dead nflverse asset, so ff_scoringhistory() returns ZERO rows
# for 2025+ and every simulation silently loses its most recent season.
source(here::here("dev", "suite", "scoring_history_shim.R")); install_shim()
options(ffsimulator.verbose = FALSE)

league_id <- Sys.getenv("FFS_LEAGUE_ID", "1326464763936403456")
my_team   <- Sys.getenv("FFS_MY_TEAM", "sox05syd")
season    <- as.integer(Sys.getenv("FFS_SEASON", "2026"))
ns        <- as.integer(strsplit(Sys.getenv("FFS_CONV_NS", "30,60,120,240"), ",")[[1]])
n_seeds   <- as.integer(Sys.getenv("FFS_CONV_SEEDS", "3"))
per_tier  <- as.integer(Sys.getenv("FFS_CONV_PANEL", "6"))
n_deals   <- as.integer(Sys.getenv("FFS_CONV_DEALS", "6"))

out_dir <- here::here("dev", "validate_outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

conn <- ffscrapr::sleeper_connect(season = season, league_id = league_id)

build_sim <- function(n, seed) {
  set.seed(seed)
  ff_simulate(conn, n_seasons = n, version = "v3", lineup_method = "rank",
              replacement_level = FALSE, actual_schedule = TRUE, return = "all")
}

## ---- fixed panel: candidates + deals, chosen once from a reference sim --------
message("reference sim for panel selection @ ", Sys.time())
ref <- build_sim(min(ns), 1L)
fr <- as.data.table(ref$franchises)
me <- fr[fr$franchise_name == my_team][["franchise_id"]][1]
stopifnot(!is.na(me))

qb_fmt <- ffsimulator:::.ffs_detect_qb_format(ref$lineup_constraints)
fc <- as.data.table(fc_dynasty_values(num_qbs = if (qb_fmt == "superflex") 2L else 1L))
rosters <- unique(as.data.table(ref$rosters)[
  , list(player_id, fantasypros_id, player_name, pos, franchise_id)])
# only players the sim actually scores (deep stashes with no projection are in
# rosters but not roster_scores, and ffs_player_value asserts membership)
rosters <- rosters[player_id %in% unique(as.data.table(ref$roster_scores)$player_id)]
rosters[, fantasypros_id := as.character(fantasypros_id)]
fc[, fantasypros_id := as.character(fantasypros_id)]
vals <- merge(rosters, fc[, list(fantasypros_id, value)], by = "fantasypros_id")
vals <- vals[!is.na(value) & value > 0][order(-value)]

# candidates on OTHER teams spanning the value spectrum
oth <- vals[franchise_id != me]
mid_start <- max(1L, floor(nrow(oth) / 2) - floor(per_tier / 2))
panel <- rbind(
  oth[seq_len(per_tier)][, tier := "top"],
  oth[seq(mid_start, length.out = per_tier)][, tier := "mid"],
  oth[seq(nrow(oth) - per_tier + 1L, nrow(oth))][, tier := "tail"]
)
panel <- unique(panel, by = "player_id")
message("panel: ", nrow(panel), " candidates (",
        paste(panel[, .N, by = tier][["N"]], collapse = "/"), " top/mid/tail)")

# fixed 1:1 deals: my top priced players swapped for the nearest-value candidate
mine <- vals[franchise_id == me][seq_len(min(n_deals, .N))]
deals <- lapply(seq_len(nrow(mine)), function(i) {
  cand <- oth[which.min(abs(oth$value - mine$value[i]))]
  list(my = mine$player_id[i], my_name = mine$player_name[i],
       opp = cand$franchise_id[1], their = cand$player_id[1],
       their_name = cand$player_name[1])
})
message("deals: ", length(deals), " fixed 1:1 swaps")

## ---- evaluate the panel on one sim --------------------------------------------
eval_panel <- function(sim) {
  t0 <- Sys.time()
  pv <- rbindlist(lapply(seq_len(nrow(panel)), function(i) {
    p <- panel$player_id[i]
    vy <- ffs_player_value(sim, p, me)
    vo <- ffs_player_value(sim, p, panel$franchise_id[i])
    data.table(player_id = p, tier = panel$tier[i],
               value_to_you = vy$h2h_wins, playoff_you = vy$playoff_pct,
               value_to_owner = vo$h2h_wins, playoff_owner = vo$playoff_pct)
  }))
  dl <- rbindlist(lapply(deals, function(d) {
    te <- as.data.table(ffs_trade_eval(sim, me, d$my, d$opp, d$their))
    data.table(deal = paste0(d$my_name, ">", d$their_name),
               my_playoff_delta = te[te$franchise_id == me][["playoff_pct_delta"]],
               opp_playoff_delta = te[te$franchise_id != me][["playoff_pct_delta"]])
  }))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  list(pv = pv, dl = dl, secs = secs)
}

## ---- main loop -----------------------------------------------------------------
results <- list()
for (n in ns) {
  for (seed in seq_len(n_seeds)) {
    message("n=", n, " seed=", seed, " @ ", Sys.time())
    sim <- if (n == min(ns) && seed == 1L) ref else build_sim(n, seed)
    r <- eval_panel(sim)
    r$pv[, `:=`(n = n, seed = seed)]
    r$dl[, `:=`(n = n, seed = seed)]
    results[[paste(n, seed)]] <- r
    rm(sim); gc(verbose = FALSE)
  }
}

pv <- rbindlist(lapply(results, `[[`, "pv"))
dl <- rbindlist(lapply(results, `[[`, "dl"))
secs <- rbindlist(lapply(names(results), function(k) {
  data.table(n = as.integer(strsplit(k, " ")[[1]][1]), secs = results[[k]]$secs)
}))

## ---- summaries ------------------------------------------------------------------
# rank stability: Spearman of value_to_you across every seed pair, per n
pairs <- utils::combn(seq_len(n_seeds), 2)
rank_stab <- rbindlist(lapply(ns, function(nn) {
  rbindlist(lapply(seq_len(ncol(pairs)), function(j) {
    a <- pv[n == nn & seed == pairs[1, j]][order(player_id)]
    b <- pv[n == nn & seed == pairs[2, j]][order(player_id)]
    data.table(
      n = nn,
      sp_value_to_you = cor(a$value_to_you, b$value_to_you, method = "spearman"),
      sp_value_to_owner = cor(a$value_to_owner, b$value_to_owner, method = "spearman")
    )
  }))
}))
rank_sum <- rank_stab[, list(sp_value_to_you = mean(sp_value_to_you),
                             sp_value_to_owner = mean(sp_value_to_owner)), by = n]

# run-to-run SD of each player's value across seeds, averaged by tier
tier_sd <- pv[, list(sd_vy = sd(value_to_you), sd_po_you = sd(playoff_you)),
              by = list(n, player_id, tier)][
  , list(sd_value_to_you = mean(sd_vy), sd_playoff_you = mean(sd_po_you)),
  by = list(n, tier)]

# playoff-delta SD across seeds per deal, averaged
deal_sd <- dl[, list(sd_my = sd(my_playoff_delta), sd_opp = sd(opp_playoff_delta)),
              by = list(n, deal)][
  , list(sd_my_playoff_delta = mean(sd_my), sd_opp_playoff_delta = mean(sd_opp)),
  by = n]

wall <- secs[, list(panel_secs = mean(secs)), by = n]

summary_long <- rbindlist(list(
  melt(rank_sum, id.vars = "n", variable.name = "metric")[, group := "overall"],
  melt(tier_sd, id.vars = c("n", "tier"), variable.name = "metric")[
    , group := tier][, tier := NULL],
  melt(deal_sd, id.vars = "n", variable.name = "metric")[, group := "overall"],
  melt(wall, id.vars = "n", variable.name = "metric")[, group := "overall"]
), use.names = TRUE)
setorder(summary_long, metric, group, n)
fwrite(summary_long, file.path(out_dir, "valuation_convergence.csv"))

cat("\n==== rank stability (mean Spearman across seed pairs) ====\n")
print(rank_sum)
cat("\n==== value SD by tier (top/mid/tail of roster) ====\n")
print(dcast(tier_sd, n ~ tier, value.var = "sd_value_to_you"))
cat("\n==== playoff-delta SD (fixed 1:1 deals) ====\n")
print(deal_sd)
cat("\n==== mean panel wall-clock (secs) ====\n")
print(wall)

# recommendation: the value-rank plateau. Rank stability is what gates decision
# quality (which players/deals get considered); playoff-delta SD is pure
# 1/sqrt(n) Monte-Carlo with no knee, so it's bought per-deal when confirming.
# Pick the smallest n whose mean Spearman is within 0.05 of the best tested.
rec <- merge(rank_sum, deal_sd, by = "n")
best_sp <- max(rec$sp_value_to_you)
ok <- rec[sp_value_to_you >= best_sp - 0.05]
cat("\nrecommended n (value-rank plateau): ", min(ok$n),
    " (Spearman ", round(rec[n == min(ok$n)]$sp_value_to_you, 2),
    " vs best ", round(best_sp, 2),
    "; my-playoff-delta SD there ", round(rec[n == min(ok$n)]$sd_my_playoff_delta, 3),
    ")\n", sep = "")
cat("wrote", file.path(out_dir, "valuation_convergence.csv"), "\n")
