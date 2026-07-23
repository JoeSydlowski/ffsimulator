# Phase 1 - RAW weekly teammate correlation from nflverse, independent of any
# best-ball outcome or playoff advancement. Answers: do QB-WR stacks actually
# blow up together (and on ALL teams, incl. ones that never made a pod's top-2)?
# This measures the MECHANISM INPUT (correlation is real) to reconcile with the
# roster-outcome finding (it doesn't create a best-ball edge). Half-PPR to match
# Underdog. No advancement filter - every team-season-week.

suppressPackageStartupMessages({ library(data.table); library(nflreadr) })

ps <- as.data.table(load_player_stats(2021:2025))
tmcol <- intersect(c("recent_team","team"), names(ps))[1]
setnames(ps, tmcol, "team")
ps[, half := (fantasy_points + fantasy_points_ppr) / 2]          # Underdog 0.5 PPR
ps <- ps[position %in% c("QB","WR","TE") & week <= 18 & !is.na(team) & !is.na(half)]

# season-long roles per team: QB1, WR1, WR2, TE1 (by total half-PPR)
tot <- ps[, .(tot = sum(half)), by = .(season, team, position, player_id)]
tot[, rk := frank(-tot, ties.method = "first"), by = .(season, team, position)]
roles <- tot[(position == "QB" & rk == 1) | (position == "WR" & rk <= 2) | (position == "TE" & rk == 1)]
roles[, role := fifelse(position == "QB", "QB1", fifelse(position == "TE", "TE1", paste0("WR", rk)))]

psr <- merge(ps[, .(season, team, week, player_id, half)],
             roles[, .(season, team, player_id, role)], by = c("season","team","player_id"))
w <- dcast(psr, season + team + week ~ role, value.var = "half")

corr1 <- function(a, b) if (sum(!is.na(a) & !is.na(b)) >= 6) cor(a, b, use = "complete.obs") else NA_real_
cc <- w[, .(qb_wr1 = corr1(QB1, WR1), qb_wr2 = corr1(QB1, WR2),
            qb_te1 = corr1(QB1, TE1), wr1_wr2 = corr1(WR1, WR2)),
        by = .(season, team)]

cat("===== same-team WEEKLY correlation (half-PPR, 2021-25, all teams) =====\n")
cat("mean over team-seasons:\n")
print(cc[, lapply(.SD, function(x) round(mean(x, na.rm = TRUE), 3)),
         .SDcols = c("qb_wr1","qb_wr2","qb_te1","wr1_wr2")])
cat("\nby season (qb_wr1):\n")
print(cc[, .(qb_wr1 = round(mean(qb_wr1, na.rm = TRUE), 3), n_teams = .N), by = season][order(season)])

# co-blow-up: when WR1 smashes, does his QB come with him? (no advancement filter)
bb <- w[!is.na(QB1) & !is.na(WR1)]
cat("\n===== QB-WR1 co-blow-up (>= 20 half-PPR = a 'smash') =====\n")
cat(sprintf("P(QB1 smash) overall            : %.3f\n", mean(bb$QB1 >= 20)))
cat(sprintf("P(QB1 smash | WR1 smashed)      : %.3f\n", mean(bb[WR1 >= 20]$QB1 >= 20)))
cat(sprintf("mean QB1 pts when WR1 smashed   : %.1f  (overall %.1f)\n",
            mean(bb[WR1 >= 20]$QB1), mean(bb$QB1)))
cat(sprintf("P(both QB1 & WR1 smash same wk) : %.3f  (independent would be %.3f)\n",
            mean(bb$QB1 >= 20 & bb$WR1 >= 20), mean(bb$QB1 >= 20) * mean(bb$WR1 >= 20)))
