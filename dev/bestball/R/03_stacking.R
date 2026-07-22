# Phase 1 - the FIX-THE-ANCHOR stack test (the de-confounded structural gate).
#
# Naive "stacked entries advanced more" is confounded: stacking correlates with
# rostering good QBs. The clean test holds the QB FIXED: among all entries that
# rostered QB X, compare advance rate of those that ALSO rostered one of X's
# pass-catchers vs those that didn't. Same QB => same realized season => the gap
# is the STRUCTURAL value of the stack, not "you had the good QB". Averaging the
# within-QB lift across many QBs de-confounds player-selection from structure.
#
# Team mapping: Underdog player_id is its own UUID, so we join player NAME+pos to
# nflreadr season rosters to get NFL team. Requires 02_ingest_rich.R outputs.

suppressPackageStartupMessages({ library(data.table); library(arrow); library(dplyr); library(nflreadr) })

SEASON <- 2023L
PREFIX <- "data/parquet/2023_BBMIV"

# --- player -> NFL team via nflreadr name match ----------------------------
clean <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[.'’`-]", "", x)
  x <- gsub("\\s+(jr|sr|ii|iii|iv|v)$", "", x)
  gsub("\\s+", " ", x)
}
players <- as.data.table(read_parquet(paste0(PREFIX, "_players.parquet")))
players[, key := clean(player_name)]

ros <- as.data.table(load_rosters(SEASON))[position %in% c("QB", "RB", "WR", "TE")]
ros[, key := clean(full_name)]
# one team per (name, pos); mid-season movers -> first listed
teammap <- ros[, .(nfl_team = team[1]), by = .(key, pos = position)]
players <- merge(players, teammap, by = c("key", "pos"), all.x = TRUE)
cat(sprintf("team match rate: %.3f (%d of %d players)\n",
            mean(!is.na(players$nfl_team)),
            sum(!is.na(players$nfl_team)), nrow(players)))

# --- fact: entry x player, with team -----------------------------------------
fact <- as.data.table(
  open_dataset(paste0(PREFIX, "_fact")) %>%
    select(entry_id, draft_id, player_id, pos, roster_points) %>% collect()
)
fact <- merge(fact, players[, .(player_id, nfl_team)], by = "player_id", all.x = TRUE)

# advancement: rich rd1 leaves made_playoffs = 0, so derive it - top-2-of-pod by
# roster_points (this rule validated on 2021 at 99.98% vs the real flag).
em <- unique(fact[, .(entry_id, draft_id, roster_points)])
em[, pod_rank := frank(-roster_points, ties.method = "first"), by = draft_id]
em[, advanced := as.integer(pod_rank <= 2L)]
cat(sprintf("entries: %s   derived adv rate: %.4f\n",
            format(nrow(em), big.mark = ","), mean(em$advanced)))

# entry x NFL team that has a rostered pass-catcher (WR/TE)
catch <- unique(fact[pos %in% c("WR", "TE") & !is.na(nfl_team), .(entry_id, team = nfl_team)])
catch[, has_catcher := TRUE]

# one row per rostered QB (with team): is the entry stacked on that QB?
qbs <- fact[pos == "QB" & !is.na(nfl_team), .(entry_id, qb_id = player_id, qb_team = nfl_team)]
qbs <- merge(qbs, catch, by.x = c("entry_id", "qb_team"),
             by.y = c("entry_id", "team"), all.x = TRUE)
qbs[, stacked := !is.na(has_catcher)]
qbs <- merge(qbs, em[, .(entry_id, advanced)], by = "entry_id", all.x = TRUE)

# --- naive (confounded) comparison, for contrast ----------------------------
entry_stack <- qbs[, .(has_stack = any(stacked)), by = entry_id]
em2 <- merge(em[, .(entry_id, advanced)], entry_stack, by = "entry_id", all.x = TRUE)
em2[is.na(has_stack), has_stack := FALSE]
cat("\n--- NAIVE (confounded): any QB-stack vs none ---\n")
print(em2[, .(n = .N, adv = round(mean(advanced), 4)), by = has_stack][order(has_stack)])

# --- fix-the-anchor (de-confounded): within-QB stacked vs not ---------------
agg <- qbs[, .(n = .N, adv = mean(advanced)), by = .(qb_id, stacked)]
wide <- dcast(agg, qb_id ~ stacked, value.var = c("n", "adv"))
setnames(wide, c("n_FALSE","n_TRUE","adv_FALSE","adv_TRUE"),
         c("n_no","n_yes","adv_no","adv_yes"), skip_absent = TRUE)
# keep QBs with enough sample on both sides to estimate a within-QB lift
MIN <- 150L
w <- wide[!is.na(n_no) & !is.na(n_yes) & n_no >= MIN & n_yes >= MIN]
w[, lift := adv_yes - adv_no]
cat(sprintf("\n--- FIX-THE-ANCHOR: within-QB stack lift (QBs with >= %d entries each side) ---\n", MIN))
cat(sprintf("QBs tested            %d\n", nrow(w)))
cat(sprintf("QBs with +ve lift     %d (%.0f%%)\n", w[lift > 0, .N], 100 * mean(w$lift > 0)))
cat(sprintf("mean within-QB lift   %+.4f  (advance-rate pts)\n", mean(w$lift)))
cat(sprintf("median within-QB lift %+.4f\n", median(w$lift)))
# sample-weighted (by smaller side) so big-N QBs count more
w[, wt := pmin(n_no, n_yes)]
cat(sprintf("weighted mean lift    %+.4f\n", weighted.mean(w$lift, w$wt)))

cat("\ntop / bottom QBs by within-QB stack lift:\n")
w2 <- merge(w, unique(players[, .(qb_id = player_id, player_name)]), by = "qb_id", all.x = TRUE)
setorder(w2, -lift)
show <- rbind(head(w2, 5), tail(w2, 5))
print(show[, .(player_name, n_no, adv_no = round(adv_no,3),
               n_yes, adv_yes = round(adv_yes,3), lift = round(lift,3))])
