# How many simulated seasons do you need? Convergence study.
#
# Runs the league simulation 8 times at n=200 with different seeds:
#   - the 8 true replicates measure the real run-to-run SD at n=200
#   - pooled (1600 seasons), bootstrap subsampling traces SE(n) for any n
#   - the bootstrap SE at n=200 is validated against the true replicate SD
# Separately, a mini-study measures per-player WAR noise (the leave-one-out
# delta) across 3 replicate base sims.
#
# Usage: FFS_LEAGUE_ID=<id> Rscript dev/suite/convergence.R
# Outputs: dev/league_sims/<id>/convergence/*.csv

library(data.table)
devtools::load_all(here::here(), quiet = TRUE)
# ffscrapr reads a dead nflverse asset, so ff_scoringhistory() returns ZERO rows
# for 2025+ and every simulation silently loses its most recent season.
source(here::here("dev", "suite", "scoring_history_shim.R")); install_shim()
options(ffsimulator.verbose = FALSE)

league_id <- Sys.getenv("FFS_LEAGUE_ID", "1359546500786434048")
season <- as.integer(Sys.getenv("FFS_SEASON", "2026"))
n_rep <- 8L
n_per_rep <- 200L

out <- here::here("dev", "league_sims", league_id, "convergence")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

conn <- ffscrapr::sleeper_connect(season = season, league_id = league_id)

## ---- replicate sims ---------------------------------------------------------

seasons_all <- list()
for (rep in seq_len(n_rep)) {
  message("replicate ", rep, "/", n_rep, " @ ", Sys.time())
  sim <- ff_simulate(conn, n_seasons = n_per_rep, version = "v3",
                     lineup_method = "rank", seed = 1000L + rep)
  ss <- as.data.table(sim$summary_season)
  ss[, rep := rep]
  seasons_all[[rep]] <- ss
}
seasons_all <- rbindlist(seasons_all)
fwrite(seasons_all, file.path(out, "replicate_seasons.csv"))

league_size <- length(unique(seasons_all$franchise_id))
# wins then points-for, deterministic (matches .ffs_franchise_summary; random
# tie-breaks would inflate the measured noise floor)
seasons_all[, lg_rank := frank(list(-h2h_wins, -points_for), ties.method = "first"),
            by = list(rep, season)]
seasons_all[, playoff := lg_rank <= 6]

## ---- true replicate SD at n=200 --------------------------------------------

per_rep <- seasons_all[, list(
  playoff_pct = mean(playoff),
  mean_wins = mean(h2h_wins),
  mean_pf = mean(points_for)
), by = list(rep, franchise_name)]

true_sd <- per_rep[, list(
  sd_playoff = sd(playoff_pct),
  sd_wins = sd(mean_wins),
  sd_pf = sd(mean_pf)
), by = franchise_name]
cat("\n==== true run-to-run SD at n=200 (8 replicates) ====\n")
print(true_sd[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 4) else x)])

## ---- bootstrap SE(n) curves from the pooled 1600 seasons --------------------

pool <- seasons_all # 1600 league-seasons
grid_n <- c(50, 100, 200, 400, 800, 1600)
n_boot <- 400

boot_se <- rbindlist(lapply(grid_n, function(n) {
  reps <- rbindlist(lapply(seq_len(n_boot), function(b) {
    # resample whole league-seasons (all 12 team rows together)
    keys <- unique(pool[, list(rep, season)])
    take <- keys[sample.int(nrow(keys), n, replace = TRUE)]
    s <- pool[take, on = c("rep", "season")]
    s[, list(playoff_pct = mean(playoff), mean_wins = mean(h2h_wins)),
      by = franchise_name][, b := b]
  }))
  reps[, list(
    se_playoff = sd(playoff_pct),
    se_wins = sd(mean_wins)
  ), by = franchise_name][, n_sims := n]
}))
fwrite(boot_se, file.path(out, "bootstrap_se.csv"))

se_curve <- boot_se[, list(
  se_playoff = mean(se_playoff),
  se_wins = mean(se_wins)
), by = n_sims]
cat("\n==== bootstrap SE(n), averaged over franchises ====\n")
print(se_curve[, lapply(.SD, round, 4)])
cat("\nvalidation: bootstrap SE at n=200 =", round(se_curve[n_sims == 200, se_playoff], 4),
    "vs true replicate SD =", round(mean(true_sd$sd_playoff), 4), "\n")

## ---- WAR noise mini-study ----------------------------------------------------

message("WAR noise mini-study @ ", Sys.time())
war_reps <- list()
for (rep in 1:3) {
  base <- ff_simulate(conn, n_seasons = 40, version = "v3",
                      lineup_method = "rank", seed = 2000L + rep, return = "all")
  rosters <- as.data.table(base$rosters)
  # a spread of players: best/mid/depth from one franchise + stars elsewhere
  sample_players <- rosters[pos %in% c("QB", "RB", "WR", "TE")][
    order(player_name)][seq(1, .N, length.out = 8)]
  wr <- rbindlist(lapply(seq_len(nrow(sample_players)), function(i) {
    p <- sample_players[i]
    d <- ffsimulator:::.ffs_win_add(
      p[, list(player_id, player_name, franchise_id)], base
    )
    data.table(rep = rep, player_name = p$player_name,
               allplay_delta = d$allplay_winpct)
  }))
  war_reps[[rep]] <- wr
}
war_reps <- rbindlist(war_reps)
war_sd <- war_reps[, list(mean_war = mean(allplay_delta), sd_war = sd(allplay_delta)), by = player_name]
fwrite(war_reps, file.path(out, "war_replicates.csv"))
cat("\n==== WAR (allplay delta) run-to-run SD at n=40, 3 replicates ====\n")
print(war_sd[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 4) else x)])

cat("\nDONE\n")
