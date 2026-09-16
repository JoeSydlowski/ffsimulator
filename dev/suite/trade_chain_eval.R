# Price a proposed trade on the n=2000 standings sim AFTER trades you have
# already executed.
#
# The saved simulation.rds is a snapshot of the rosters on the day it was built.
# Once a deal actually goes through, every board and confirm script is quietly
# answering the wrong question: it still prices the next move against the old
# roster. Re-scraping and re-running ff_simulate is the honest fix but costs the
# whole suite; this script instead REPLAYS the executed deals inside the saved
# simulation - moving the rows, re-optimising both franchises' lineups, and
# applying the same waiver backfill / cut-to-size the deal was priced under -
# and then evaluates the new proposal against that updated baseline.
#
# What it does NOT do: refresh projections or rankings. The sim's player pool and
# weekly scores are whatever the underlying run scraped. That is fine for a deal
# made days later, and wrong once the rankings have really moved - rerun
# run_league_suite.R then.
#
# Outputs (into the newest report folder):
#   <tag>_chain_headline.csv   ffs_deal_scores row for the proposal, n=2000
#   <tag>_chain_lineup.csv     per-player weeks-started + points-started, before/after
#
# Usage:
#   FFS_LEAGUE_ID=1312145437842497536 \
#   FFS_DONE="K Stafford isn't Worthy|Jaylen Warren + Courtland Sutton|Bucky Irving" \
#   FFS_DD_OPP="That's so Ravens" \
#   FFS_DD_SEND="DeVonta Smith + Rashee Rice" FFS_DD_RECV="Jeremiyah Love" \
#   FFS_WAIVER=me FFS_OPP_CUT=1 \
#     Rscript dev/suite/trade_chain_eval.R
#
# FFS_DONE is "opponent|what I sent|what I got", several deals separated by ";".
# It may be empty, in which case this is just a single-deal confirm with a
# lineup breakdown.

suppressMessages({
  library(data.table)
  devtools::load_all(here::here(), quiet = TRUE)
})
options(ffsimulator.verbose = FALSE)

league_id  <- Sys.getenv("FFS_LEAGUE_ID", "1312145437842497536")
league_dir <- here::here("dev", "league_sims", league_id)
sims <- Sys.glob(file.path(league_dir, "*", "simulation.rds"))
stopifnot("no saved simulation.rds for this league" = length(sims) > 0)
out    <- dirname(sims[order(file.info(sims)$mtime, decreasing = TRUE)][1])
config <- readRDS(file.path(out, "config.rds"))
my_team <- Sys.getenv("FFS_MY_TEAM", config$my_team %||% "")

waiver_mode <- Sys.getenv("FFS_WAIVER", "me")
opp_cut     <- as.integer(Sys.getenv("FFS_OPP_CUT", "1")) == 1L
# replay the executed deals under the same roster-churn rules they were priced
# under, so the baseline is the roster he actually has
done_waiver <- Sys.getenv("FFS_DONE_WAIVER", waiver_mode)
done_cut    <- as.integer(Sys.getenv("FFS_DONE_CUT", if (opp_cut) "1" else "0")) == 1L

opp_name   <- Sys.getenv("FFS_DD_OPP",  "That's so Ravens")
send_txt   <- Sys.getenv("FFS_DD_SEND", "DeVonta Smith + Rashee Rice")
recv_txt   <- Sys.getenv("FFS_DD_RECV", "Jeremiyah Love")
split_pl   <- function(x) trimws(strsplit(x, "\\+")[[1]])
send_names <- split_pl(send_txt); recv_names <- split_pl(recv_txt)
tag <- Sys.getenv("FFS_CHAIN_TAG",
                  gsub("[^A-Za-z0-9]+", "_", paste0(send_txt, "_for_", recv_txt)))

message("report folder: ", out)
message("loading n=2000 standings sim @ ", Sys.time())
sim <- readRDS(file.path(out, "simulation.rds"))
rs0 <- as.data.table(sim$roster_scores)
lc  <- ffsimulator:::.ffs_sim_lineup_constraints(sim, roster_scores = rs0)
n2k <- uniqueN(rs0$season)
message("n = ", n2k, " seasons")

# franchise names carry stray whitespace on Sleeper ("That's so Ravens "), so
# match on the trimmed name and keep the sim's own spelling for output
fr <- unique(rs0[, list(franchise_id, franchise_name)])
fr[, key := trimws(franchise_name)]
fid_of_team <- function(nm) {
  hit <- fr[key == trimws(nm)]$franchise_id
  if (!length(hit)) stop(sprintf("franchise '%s' not in the sim; have: %s", nm,
                                 paste(fr$key, collapse = ", ")))
  hit[1]
}
me <- fid_of_team(my_team)

pid_of <- function(nm, rs, fid) {
  hit <- unique(rs[player_name == nm & franchise_id == fid]$player_id)
  if (!length(hit)) stop(sprintf("'%s' is not on %s's roster at this point in the chain",
                                 nm, fr[franchise_id == fid]$franchise_name[1]))
  as.character(hit[1])
}
name_of <- function(pid, rs) {
  h <- unique(rs[player_id == pid]$player_name)
  if (length(h)) h[1] else pid
}

## ---- replay the executed deals ---------------------------------------------------
retag <- function(rows, fid, rs) {
  rows <- copy(rows)
  tm <- rs[franchise_id == fid][1]
  for (cc in intersect(c("franchise_id", "franchise_name", "league_id"), names(rows)))
    set(rows, j = cc, value = tm[[cc]])
  rows
}
reoptimise <- function(rs_new, fids) {
  rbindlist(lapply(fids, function(f)
    ffsimulator:::.ffs_optimise_started(rs_new[franchise_id == f], lc)), fill = TRUE)
}
rebuild <- function(sim_in, rs_new, fids) {
  # pos_rank must reflect the modified rosters (the optimiser's candidate trim reads it)
  rs_new[order(-projected_score), pos_rank := seq_len(.N),
         by = c("league_id", "franchise_id", "pos", "season", "week")]
  message("  re-optimising ", length(fids), " franchises' lineups @ ", Sys.time())
  reopt <- reoptimise(rs_new, fids)
  s <- sim_in
  s$roster_scores  <- rs_new
  s$optimal_scores <- rbind(
    as.data.table(sim_in$optimal_scores)[!franchise_id %in% fids], reopt, fill = TRUE)
  s
}

done_spec <- trimws(Sys.getenv("FFS_DONE", ""))
rs <- copy(rs0)
sim_mid <- sim
touched <- integer(0)
if (nzchar(done_spec)) {
  for (one in trimws(strsplit(done_spec, ";")[[1]])) {
    if (!nzchar(one)) next
    parts <- trimws(strsplit(one, "\\|")[[1]])
    stopifnot("FFS_DONE entry must be 'opponent|sent|received'" = length(parts) == 3)
    o <- fid_of_team(parts[1])
    s_ids <- vapply(split_pl(parts[2]), pid_of, character(1), rs = rs, fid = me,
                    USE.NAMES = FALSE)
    r_ids <- vapply(split_pl(parts[3]), pid_of, character(1), rs = rs, fid = o,
                    USE.NAMES = FALSE)
    message("EXECUTED: sent [", parts[2], "] to ", parts[1], " for [", parts[3], "]")
    rs <- rbind(
      rs[!franchise_id %in% c(me, o)],
      rs[franchise_id == me & !player_id %in% s_ids], retag(rs[player_id %in% r_ids], me, rs),
      rs[franchise_id == o  & !player_id %in% r_ids], retag(rs[player_id %in% s_ids], o, rs))
    touched <- union(touched, c(me, o))
    attr(rs, "last_deal") <- list(o = o, s = s_ids, r = r_ids)
  }
  sim_mid <- rebuild(sim, rs, touched)
}

## ---- waiver wire, measured on the post-executed-trade roster ---------------------
v_pos <- config$pos_filter %||% c("QB", "RB", "WR", "TE", "K")
if (waiver_mode != "none" || done_waiver != "none") {
  sim_mid$waiver_scores <- ffs_waiver_proxies(
    sim_mid, n_per_pos = as.integer(Sys.getenv("FFS_WAIVER_N", "3")),
    pos_filter = intersect(v_pos, c("QB", "RB", "WR", "TE")))
  message("ranking the wire for my roster @ ", Sys.time())
  sim_mid$waiver_scores <- ffs_waiver_rank(sim_mid, me)
  message("wire (best first): ", paste(unique(
    as.data.table(sim_mid$waiver_scores)[order(-value_w)][donor_rank == 1L]$player_name),
    collapse = ", "))
}

## ---- churn from the executed deals: my backfill, their cut -----------------------
if (nzchar(done_spec) && (done_waiver != "none" || done_cut)) {
  ld <- attr(rs, "last_deal")
  ch <- ffsimulator:::.ffs_roster_churn(
    ffsimulator:::.ffs_waiver_index(sim_mid$waiver_scores), rs,
    me, ld$s, ld$o, ld$r, waiver = done_waiver, cut_to_size = done_cut)
  ws <- as.data.table(sim_mid$waiver_scores)
  if (length(ch$add_a)) {
    add <- retag(ws[player_id %in% ch$add_a, intersect(names(rs), names(ws)), with = FALSE],
                 me, rs)
    message("  I signed off the wire: ",
            paste(unique(add$player_name), collapse = ", "))
    rs <- rbind(rs, add, fill = TRUE)
    # that slot is spent - do not let the next deal sign a second clone of him
    sim_mid$waiver_scores <- ws[!waiver_slot %in% ws[player_id %in% ch$add_a]$waiver_slot]
  }
  if (length(ch$cut_b)) {
    message("  ", fr[franchise_id == ld$o]$franchise_name[1], " released: ",
            paste(vapply(ch$cut_b, name_of, character(1), rs = rs), collapse = ", "))
    rs <- rs[!(franchise_id == ld$o & player_id %in% ch$cut_b)]
  }
  sim_mid <- rebuild(sim_mid, rs, touched)
}

## ---- what the executed deals did to the standings --------------------------------
if (nzchar(done_spec)) {
  b0 <- as.data.table(ffsimulator:::.ffs_summarise_optimal(
    sim, as.data.table(sim$optimal_scores), touched))
  b1 <- as.data.table(ffsimulator:::.ffs_summarise_optimal(
    sim_mid, as.data.table(sim_mid$optimal_scores), touched))
  cmp <- merge(b0[, list(franchise_id, pl0 = playoff_pct, ch0 = champion_pct)],
               b1[, list(franchise_id, pl1 = playoff_pct, ch1 = champion_pct)],
               by = "franchise_id")
  cmp <- merge(cmp, fr[, list(franchise_id, franchise_name)], by = "franchise_id")
  cat("\n== baseline shift from the executed trade(s), n=", n2k, " ==\n", sep = "")
  for (i in seq_len(nrow(cmp))) cat(sprintf(
    "  %-24s playoff %5.1f%% -> %5.1f%% (%+.1f)   champ %4.1f%% -> %4.1f%% (%+.1f)\n",
    trimws(cmp$franchise_name[i]), 100 * cmp$pl0[i], 100 * cmp$pl1[i],
    100 * (cmp$pl1[i] - cmp$pl0[i]), 100 * cmp$ch0[i], 100 * cmp$ch1[i],
    100 * (cmp$ch1[i] - cmp$ch0[i])))
}

## ---- the proposal ----------------------------------------------------------------
opp <- fid_of_team(opp_name)
send_ids <- vapply(send_names, pid_of, character(1), rs = rs, fid = me,  USE.NAMES = FALSE)
recv_ids <- vapply(recv_names, pid_of, character(1), rs = rs, fid = opp, USE.NAMES = FALSE)
cat("\nPROPOSAL: send [", send_txt, "] to ", trimws(opp_name), " for [", recv_txt, "]\n",
    sep = "")
message("ffs_trade_eval on n=", n2k, " (waiver=", waiver_mode, ", opp_cut=", opp_cut,
        ") @ ", Sys.time())
ev <- as.data.table(ffs_trade_eval(sim_mid, me, send_ids, opp, recv_ids,
                                   waiver = waiver_mode, cut_to_size = opp_cut))
m  <- ev[franchise_id == me]; op <- ev[franchise_id == opp]

## ---- score it with the same function every board uses ----------------------------
dyn <- fread(file.path(out, "dynasty_outlook.csv"),
             colClasses = list(character = c("player_id", "fantasypros_id")))
dyn[, player_id := as.character(player_id)]
fmt <- tryCatch(as.character(ffsimulator:::.ffs_detect_qb_format(sim$lineup_constraints)),
                error = function(e) "superflex")
val <- function(ids, col) sum(dyn[player_id %in% ids][[col]], na.rm = TRUE)

r <- data.table(
  team = trimws(opp_name), shape = paste0(length(send_ids), "-for-", length(recv_ids)),
  send = send_txt, receive = recv_txt,
  send_ids = list(send_ids), recv_ids = list(recv_ids),
  send_value = val(send_ids, "cur_value"), recv_value = val(recv_ids, "cur_value"),
  future_capital_delta = val(recv_ids, "next_value_mean") - val(send_ids, "next_value_mean"),
  my_playoff_delta = m$playoff_pct_delta, my_champ_delta = m$champion_pct_delta,
  opp_playoff_delta = op$playoff_pct_delta, opp_champ_delta = op$champion_pct_delta,
  opp_playoff_before = op$playoff_pct_before,
  you_h2h = round(m$h2h_wins_delta, 2),
  b_you = round(100 * m$playoff_pct_before, 1), b_opp = round(100 * op$playoff_pct_before, 1))
r <- ffs_deal_scores(
  r, dyn, my_haircut = ffsimulator:::.ffs_haircut(m$playoff_pct_before[1]),
  playoff_value = as.numeric(Sys.getenv("FFS_TRADE_PLAYOFF_VALUE", "68")),
  format = fmt,
  # the shape-fair overpay a package must carry. 0.10 is now the package default
  # too (it used to be 0.05, and this script's local 0.10 was the only thing
  # matching the boards), so this is here for explicitness, not to override.
  fair_premium = as.numeric(Sys.getenv("FFS_FAIR_PREMIUM", "0.10")),
  opp_edge_tol = as.numeric(Sys.getenv("FFS_OPP_EDGE_TOL", "0.05")),
  max_opp_drop = as.numeric(Sys.getenv("FFS_BOARD_MAX_OPP_DROP", "0.15")))
fwrite(r[, setdiff(names(r), c("send_ids", "recv_ids")), with = FALSE],
       file.path(out, paste0(tag, "_chain_headline.csv")))

cat(sprintf("\n== %s CONFIRMED on n=%d (post-executed-trade baseline) ==\n", tag, n2k))
cat(sprintf("  grade %s  score %+.1f  %s\n", r$grade, r$score,
            if (isTRUE(r$gettable)) "GETTABLE on the market model" else "they say no (market model)"))
cat(sprintf("  me    playoff %5.1f%% -> %5.1f%% (%+.1f)  champ %4.1f%% -> %4.1f%% (%+.1f)  wins %+.2f\n",
            100 * m$playoff_pct_before, 100 * m$playoff_pct_after, 100 * m$playoff_pct_delta,
            100 * m$champion_pct_before, 100 * m$champion_pct_after,
            100 * m$champion_pct_delta, m$h2h_wins_delta))
cat(sprintf("  them  playoff %5.1f%% -> %5.1f%% (%+.1f)  champ %4.1f%% -> %4.1f%% (%+.1f)\n",
            100 * op$playoff_pct_before, 100 * op$playoff_pct_after, 100 * op$playoff_pct_delta,
            100 * op$champion_pct_before, 100 * op$champion_pct_after,
            100 * op$champion_pct_delta))
cat(sprintf("  value send %d vs recv %d | overpay %.1f%% vs fair %.1f%% -> their edge %+.1f%% (%+d pts)\n",
            round(r$send_value), round(r$recv_value), 100 * r$prem, 100 * r$fair_gap,
            100 * r$opp_edge, round(r$opp_surplus)))
cat(sprintf("  future %+d raw / %+d reliability-adjusted -> %+.1f of the score at haircut %.2f\n",
            round(r$future_capital_delta), round(r$adj_future_capital),
            r$adj_future_capital * ffsimulator:::.ffs_haircut(m$playoff_pct_before[1]) / 68,
            ffsimulator:::.ffs_haircut(m$playoff_pct_before[1])))

## ---- why: who actually plays -----------------------------------------------------
# A 2-for-1 only works if the single piece coming back clears the lineup cut-line
# often enough to cover BOTH departures. Re-set my lineup in every franchise-week
# before and after (same rank manager the sim used) and count starts and points.
churn <- ffsimulator:::.ffs_roster_churn(
  ffsimulator:::.ffs_waiver_index(sim_mid$waiver_scores), rs, me, send_ids, opp, recv_ids,
  waiver = waiver_mode, cut_to_size = opp_cut)
rs_w <- ffsimulator:::.ffs_rs_with_waivers(sim_mid, rs)
mine_before <- rs[franchise_id == me]
mine_after  <- rbind(
  rs[franchise_id == me & !player_id %in% c(send_ids, churn$cut_a)],
  retag(rs_w[player_id %in% c(recv_ids, churn$add_a)], me, rs), fill = TRUE)

started_of <- function(rows, label) {
  message("optimising ", label, " lineups @ ", Sys.time())
  st <- ffsimulator:::.ffs_optimise_started(rows, lc)
  s <- st[, list(player_id = unlist(starter_player_id)), by = list(season, week)][!is.na(player_id)]
  merge(s, rows[, list(season, week, player_id, projected_score)],
        by = c("season", "week", "player_id"))
}
pb <- started_of(mine_before, "BEFORE"); pa <- started_of(mine_after, "AFTER")
per <- function(p, rows) {
  z <- p[, list(weeks = .N / n2k, pts = sum(projected_score) / n2k), by = player_id]
  merge(unique(rows[, list(player_id, player_name, pos)], by = "player_id"), z,
        by = "player_id", all.x = TRUE)[is.na(weeks), c("weeks", "pts") := 0]
}
sh <- merge(per(pb, mine_before)[, list(player_id, player_name, pos, w0 = weeks, p0 = pts)],
            per(pa, mine_after)[, list(player_id, w1 = weeks, p1 = pts)],
            by = "player_id", all = TRUE)
sh[is.na(w0), c("w0", "p0") := 0][is.na(w1), c("w1", "p1") := 0]
sh[is.na(player_name), player_name := vapply(player_id, name_of, character(1), rs = rs_w)]
sh[is.na(pos), pos := ""]
sh[, `:=`(dw = w1 - w0, dp = p1 - p0)]
setorder(sh, -dp)
fwrite(sh, file.path(out, paste0(tag, "_chain_lineup.csv")))

cat(sprintf("\n  starts / points per 14-week season (my lineup, %d seasons)\n", n2k))
cat("  player                 pos  starts before -> after   points before -> after\n")
show <- rbind(sh[player_id %in% c(send_ids, recv_ids, churn$add_a)],
              sh[!player_id %in% c(send_ids, recv_ids, churn$add_a)][abs(dp) > 1])
for (i in seq_len(nrow(show))) cat(sprintf("  %-22s %-4s %5.1f -> %5.1f (%+.1f)     %6.1f -> %6.1f (%+.1f)\n",
    substr(show$player_name[i], 1, 22), show$pos[i], show$w0[i], show$w1[i], show$dw[i],
    show$p0[i], show$p1[i], show$dp[i]))
cat(sprintf("  TOTAL starting points %.1f -> %.1f (%+.1f per season)\n",
            sum(sh$p0), sum(sh$p1), sum(sh$p1) - sum(sh$p0)))
message("\nwrote ", file.path(out, paste0(tag, "_chain_headline.csv")))
