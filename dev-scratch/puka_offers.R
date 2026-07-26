# Best REALISTIC Puka offers in Jon: fair value (not +11% to me), meatier shapes
# (2-2, 2-3, 3-2), QB-for-QB balance handled by the opp-playoff filter (sending a
# startable QB like Daniel Jones back keeps the other side whole).
suppressMessages({library(data.table); pkgload::load_all(".", quiet=TRUE); library(ffscrapr)})
options(ffsimulator.verbose = FALSE)
BASE <- "dev/league_sims/1359546500786434048"
cfg  <- readRDS(file.path(BASE, "2026-07-21", "config.rds"))
conn <- sleeper_connect(season = cfg$season, league_id = cfg$league_id)

# fast n=60 SEARCH sim (rankings are stable at n=60; confirm on n=2000 later)
sim <- ff_simulate(conn, n_seasons = 60, version = "v3", lineup_method = "rank",
                   return = "all", actual_schedule = TRUE, replacement_level = FALSE)

dyn <- fread(file.path(BASE, "2026-07-20", "dynasty_outlook.csv"))
dyn[, `:=`(player_id = as.character(player_id))]
joe   <- dyn[franchise_name == "sox05syd"]$franchise_id[1]
joe   <- as.character(joe)
nacua <- dyn[player_name == "Puka Nacua"]$player_id[1]

offers <- ffs_build_trades(
  sim, joe, dynasty = dyn, must_send = nacua,
  shapes = list(c(2,2), c(2,3), c(3,2)),
  value_band = 0.07, uneven_shade = 0.07, consolidation_penalty = 0.03,
  future_weight = 1, min_future_delta = -750, max_opp_drop = 0.20,
  winwin_bonus = 0.5, screen_n = 60L, top_n = 25L)
setDT(offers)

fr <- unique(dyn[, .(franchise_id = as.character(franchise_id), franchise_name)])
offers <- merge(offers, fr, by.x = "opponent", by.y = "franchise_id", all.x = TRUE)
offers[, gap_pct := round(100 * value_gap / recv_value, 1)]
setorder(offers, -score)
cat("\n== best realistic Puka offers (n=60 search; fair value, band 7%) ==\n")
print(offers[, .(opp = franchise_name, send, receive,
                 sv = round(send_value), rv = round(recv_value), gap = gap_pct,
                 mine_pl = round(100*my_playoff_delta,1), opp_pl = round(100*opp_playoff_delta,1),
                 fut = round(future_capital_delta), ww = win_win, score = round(score,2))],
      nrow = 25)
saveRDS(offers, "dev-scratch/puka_offers.rds")
