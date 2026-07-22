# Phase 1 - empirical field study (ENGINE-FREE), the project's first gate.
# Reconstruct entries from the canonical Parquet, validate our understanding of
# the advancement rule against reality, and produce a first descriptive read of
# how roster CONSTRUCTION relates to realized advancement. No simulation here.
#
# This pass is deliberately descriptive/uncontrolled - it proves the machinery
# and sizes the raw signals. Isolating STRUCTURE from PLAYER-SELECTION (player
# fixed effects, fix-the-anchor stack tests, concentration diagnostic) is the
# next step; see dev/suite/BESTBALL_PROPOSAL.md "Isolating structure from players".

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

pq <- "data/parquet/2021_BBMII.parquet"
d <- as.data.table(arrow::read_parquet(pq))
reg <- d[round == 1L]  # regular season

rule <- function(x) cat(strrep("-", 68), "\n", sep = "")
hdr  <- function(s) { rule(); cat(s, "\n"); rule() }

hdr("1. FIELD STRUCTURE")
n_pick  <- nrow(reg)
n_entry <- reg[, uniqueN(entry_id)]
n_pod   <- reg[, uniqueN(draft_id)]
cat(sprintf("picks           %s\n", format(n_pick, big.mark = ",")))
cat(sprintf("entries         %s\n", format(n_entry, big.mark = ",")))
cat(sprintf("pods (drafts)   %s\n", format(n_pod, big.mark = ",")))
cat(sprintf("picks / entry   %.2f (expect 18)\n", n_pick / n_entry))
cat(sprintf("entries / pod   %.2f (expect 12)\n", n_entry / n_pod))

# entry-level table: outcome is constant within entry_id
entries <- reg[, .(
  draft_id      = draft_id[1],
  roster_points = roster_points[1],
  made_playoffs = made_playoffs[1],
  nQB = sum(pos == "QB"), nRB = sum(pos == "RB"),
  nWR = sum(pos == "WR"), nTE = sum(pos == "TE"),
  n_players = .N
), by = entry_id]

hdr("2. ADVANCEMENT RULE CHECK  (is it top-2-of-pod by roster_points?)")
# rank entries within pod by points; the rule says top 2 advance
entries[, pod_rank := frank(-roster_points, ties.method = "first"), by = draft_id]
entries[, pred_advance := as.integer(pod_rank <= 2L)]
agree <- entries[, mean(pred_advance == made_playoffs)]
adv_rate <- entries[, mean(made_playoffs)]
cat(sprintf("made_playoffs rate      %.4f (top-2-of-12 => 0.1667)\n", adv_rate))
cat(sprintf("top-2 == made_playoffs  %.4f agreement\n", agree))
cat(sprintf("mean pod size           %.2f\n", entries[, .N, by = draft_id][, mean(N)]))

hdr("3. REGULAR-SEASON CUTLINE  (2nd-place roster_points per pod)")
cut <- entries[pod_rank == 2L, roster_points]
cat(sprintf("n pods            %s\n", format(length(cut), big.mark = ",")))
cat(sprintf("cutline median    %.1f\n", median(cut, na.rm = TRUE)))
cat(sprintf("cutline mean      %.1f\n", mean(cut, na.rm = TRUE)))
print(round(quantile(cut, c(.05, .25, .5, .75, .95), na.rm = TRUE), 1))

hdr("4. POSITIONAL STRUCTURE  (advance rate by QB/RB/WR/TE counts)")
entries[, structure := sprintf("%dQB/%dRB/%dWR/%dTE", nQB, nRB, nWR, nTE)]
struct <- entries[n_players == 18L, .(
  n = .N, adv = mean(made_playoffs)
), by = structure][order(-n)]
struct[, share := n / sum(n)]
cat("most common structures (share of field, advance rate vs 0.1667 baseline):\n")
print(head(struct[, .(structure, n, share = round(share, 3), adv = round(adv, 4))], 12))

hdr("5. DRAFT CAPITAL: VALUE vs REACH  (overall_pick - adp; +ve = fell to you)")
# per-pick draft value where adp is present; aggregate to entry by round region
reg[, region := fifelse(overall_pick <= 36L, "early(1-3)",
                 fifelse(overall_pick <= 96L, "mid(4-8)", "late(9-18)"))]
reg[, pick_value := overall_pick - adp]  # +ve = drafted later than ADP (value)
ev <- reg[!is.na(pick_value), .(value = sum(pick_value)), by = .(entry_id, region)]
ev <- dcast(ev, entry_id ~ region, value.var = "value", fill = 0)
entries <- merge(entries, ev, by = "entry_id", all.x = TRUE)

# advance rate by total-value quintile (raw / uncontrolled)
tot <- reg[!is.na(pick_value), .(tot_value = sum(pick_value)), by = entry_id]
entries <- merge(entries, tot, by = "entry_id", all.x = TRUE)
entries[!is.na(tot_value), vq := cut(tot_value, quantile(tot_value, 0:5/5),
                                     include.lowest = TRUE, labels = paste0("Q", 1:5))]
cat("advance rate by total draft-value quintile (Q1=most reachy, Q5=most value):\n")
print(entries[!is.na(vq), .(n = .N, adv = round(mean(made_playoffs), 4),
                            mean_value = round(mean(tot_value), 1)), by = vq][order(vq)])

cat("\nadvance rate by LATE-round value quintile (rounds 9-18):\n")
entries[!is.na(`late(9-18)`), lq := cut(`late(9-18)`, quantile(`late(9-18)`, 0:5/5),
                                        include.lowest = TRUE, labels = paste0("Q", 1:5))]
print(entries[!is.na(lq), .(n = .N, adv = round(mean(made_playoffs), 4)),
              by = lq][order(lq)])

# persist entry-level features for later modeling (player-FE, stacks, etc.)
arrow::write_parquet(entries, "data/parquet/2021_entry_features.parquet")
cat(sprintf("\n[saved] entry features -> data/parquet/2021_entry_features.parquet (%s entries)\n",
            format(nrow(entries), big.mark = ",")))
