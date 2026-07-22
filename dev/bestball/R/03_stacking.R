# Phase 1 - single-year detailed stack view (2023 BBM IV). Logic lives in
# stack_lib.R; this prints the per-QB breakdown that makes the confound visible.
# For the cross-year summary, see 04_stack_replication.R.

source("R/stack_lib.R")

res <- stack_tests(prep_rich("data/parquet/2023_BBMIV", 2023L), 2023L)

cat("=== 2023 BBM IV summary ===\n"); print(res$summary)

cat("\ntop / bottom QBs by within-QB advance lift (stacking helped / hurt):\n")
d <- res$per_qb
# label rich pkey (player_id) with names
players <- as.data.table(read_parquet("data/parquet/2023_BBMIV_players.parquet"))
d <- merge(d, unique(players[, .(qb = player_id, player_name)]), by = "qb", all.x = TRUE)
setorder(d, -adv_lift)
show <- rbind(head(d, 6), tail(d, 6))
print(show[, .(player_name, n_no, n_yes,
               adv_no = round(adv_no,3), adv_yes = round(adv_yes,3), adv_lift = round(adv_lift,3),
               med_lift = round(med_lift,0), p95_lift = round(p95_lift,0),
               tail_extra = round(tail_extra,0))])

cat("\nCeiling read: p95_lift_w vs med_lift_w - if the ceiling lifts MORE than the\n",
    "median (tail_extra_w > 0), that's the correlation signature; ~0 means stacking\n",
    "just changed the level (receiver quality), not the tail.\n", sep = "")
