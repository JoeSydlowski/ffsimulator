# Phase 1 - test ETR's headline claim: GAME STACKS in the FINALS (week 17) drive
# finals wins (they claim ~+50% win odds, "6-9 game-stacked players", and that it
# is much weaker in quarters/semis - which matched our null wk15 bring-back test).
# Here we go straight at the finals (rd4). A player is "game-stacked" if the entry
# also rosters a pass-catcher on that player's WEEK-17 opponent. We compare each
# finalist's finals-week score percentile (within year) by game-stack count.

source("R/stack_lib.R")

fin_csv <- list("2021" = "data/raw/bbm2021_rd4.csv", "2022" = "data/raw/bbm2022_rd4.csv",
                "2023" = "data/raw/bbm2023_rd4.csv", "2024" = "data/raw/bbm2024_rd4.csv",
                "2025" = "data/raw/bbm2025_rd4.csv")

gs_finals <- function(csv, season) {
  fact <- prep_csv(csv, season)
  sched <- as.data.table(nflreadr::load_schedules(season))[week == 17]
  opp <- rbind(sched[, .(team = home_team, oppo = away_team)],
               sched[, .(team = away_team, oppo = home_team)])
  # each rostered QB/WR/TE with its wk17 opponent
  pl <- merge(fact[pos %in% c("QB","WR","TE") & !is.na(nfl_team), .(entry_id, team = nfl_team, pos)],
              opp, by = "team", all.x = TRUE)
  # teams on which the entry holds a pass-catcher (the bring-back partners)
  ct <- unique(fact[pos %in% c("WR","TE") & !is.na(nfl_team), .(entry_id, ct = nfl_team)])
  # a player is game-stacked if its wk17 opponent is a team the entry has a catcher on
  gs <- merge(pl, ct, by.x = c("entry_id","oppo"), by.y = c("entry_id","ct"))
  cnt <- gs[, .(n_gs = .N), by = entry_id]
  score <- unique(fact[, .(entry_id, s = roster_points)])
  score <- merge(score, cnt, by = "entry_id", all.x = TRUE)
  score[is.na(n_gs), n_gs := 0L]
  score[, `:=`(season = season, pct = frank(s) / .N)]  # finals-week score percentile within year
  score[]
}

all <- rbindlist(lapply(names(fin_csv), function(y) gs_finals(fin_csv[[y]], as.integer(y))))
all[, bucket := cut(n_gs, c(-1, 0, 3, 6, 100), labels = c("0", "1-3", "4-6", "7+"))]

cat("===== finalists by # game-stacked players (pooled 2021-25) =====\n")
print(all[, .(n_finalists = .N,
              mean_score_pctile = round(mean(pct), 3),
              pct_top10 = round(mean(pct >= 0.90), 3),
              mean_finals_pts = round(mean(s), 1)), by = bucket][order(bucket)])

cat("\n===== the actual WINNER each year (top finals score) =====\n")
print(all[, .SD[which.max(s)], by = season][, .(season, winner_pts = round(s,1),
          winner_n_gamestack = n_gs)][order(season)])

cat("\ncorrelation(n_gs, finals-score percentile), pooled: ",
    round(cor(all$n_gs, all$pct), 3), "\n")
cat("mean game-stacked players - top-10% finals vs rest: ",
    round(all[pct >= 0.9, mean(n_gs)], 2), " vs ", round(all[pct < 0.9, mean(n_gs)], 2), "\n")
