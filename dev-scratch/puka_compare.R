# Compare two ways to trade Puka Nacua in the Jon (superflex) league:
#  (1) Puka -> Omarion Hampton + Jared Goff  (win-now, from NFC Nostalgia)
#  (2) Puka -> 3 first-round picks + a ~Pearsall WR from the projected-WORST team,
#      re-slotting those picks AFTER Puka makes that team finish higher.
suppressMessages({library(data.table); pkgload::load_all(".", quiet=TRUE); library(ffscrapr)})
BASE <- "dev/league_sims/1359546500786434048"
sim  <- readRDS(file.path(BASE, "2026-07-21", "simulation.rds"))
conn <- ff_connect(platform="sleeper", league_id="1359546500786434048", season=2026)
dyn  <- fread(file.path(BASE, "2026-07-20", "dynasty_outlook.csv"))  # 07-21 CSV has conflict markers
dyn[, `:=`(player_id = as.character(player_id), franchise_id = as.character(franchise_id))]

pid  <- function(nm) dyn[player_name == nm]$player_id[1]
joe  <- dyn[franchise_name == "sox05syd"]$franchise_id[1]
nfc  <- dyn[franchise_name == "NFC Nostalgia"]$franchise_id[1]
nacua   <- pid("Puka Nacua"); hampton <- pid("Omarion Hampton"); goff <- pid("Jared Goff")
nxt <- function(p) dyn[player_id == p]$next_value_mean[1]
cur <- function(p) dyn[player_id == p]$cur_value[1]

# --- base finish distribution -> identify the rebuilder (worst projected) ---
fd <- as.data.table(ffsimulator:::.ffs_finish_distribution(sim))
ef <- fd[, .(exp_finish = sum(lg_rank * prob)), by = franchise_id]
fr <- unique(dyn[, .(franchise_id, franchise_name)])
ef <- merge(ef, fr, by = "franchise_id")[order(-exp_finish)]
alfa <- ef$franchise_id[1]                          # worst projected = "AlfaByte"
cat("Rebuilder counterparty (worst projected):", ef$franchise_name[1],
    "exp_finish", round(ef$exp_finish[1], 1), "\n")

# the rebuilder's own next-three first-round picks (Sleeper pick data isn't
# retrievable for this league, so construct the hypothetical directly)
picks_df <- data.table(season = c(2027L, 2028L, 2029L), round = 1L,
                       franchise_id = alfa, original_franchise_id = alfa)
fc_pk    <- fc_dynasty_values(num_qbs = 2)
pvf <- function(s) as.data.table(ffs_pick_values(s, picks = picks_df,
          fc_pick_values = fc_pk, pick_curve = "dev/data/pick_value_curve.csv",
          format = "superflex"))
pv_base <- pvf(sim)

# alfa's 3 most valuable first-round picks + a ~Pearsall throw-in WR from alfa
alfa_firsts <- pv_base[franchise_id == alfa & round == 1][order(-cur_value)][1:3]
throw <- dyn[franchise_id == alfa & pos == "WR" & cur_value > 1200 & cur_value < 2800][
             order(abs(cur_value - 1865))][1]
cat("throw-in (~Pearsall):", throw$player_name, "cur", round(throw$cur_value), "\n\n")

# --- post-trade: put Puka on alfa, recompute finish, re-slot alfa's firsts ---
base_opt <- as.data.table(sim$optimal_scores)
joe_rows  <- ffsimulator:::.ffs_counterfactual_rows(sim, joe,  remove_ids = nacua)
alfa_rows <- ffsimulator:::.ffs_counterfactual_rows(sim, alfa, add_ids   = nacua)
after_opt <- rbind(base_opt[!franchise_id %in% c(joe, alfa)], joe_rows, alfa_rows, fill = TRUE)
sim2 <- sim; sim2$optimal_scores <- after_opt
ef2 <- as.data.table(ffsimulator:::.ffs_finish_distribution(sim2))[
         , .(exp_finish = sum(lg_rank * prob)), by = franchise_id]
pv_post <- pvf(sim2)
alfa_firsts_post <- pv_post[franchise_id == alfa & round == 1][
                      player_id %in% alfa_firsts$player_id]

cat("== how Puka moves the rebuilder's draft slot ==\n")
cat(" exp_finish:", round(ef[franchise_id==alfa]$exp_finish,1), "->",
    round(ef2[franchise_id==alfa]$exp_finish,1), "(higher finish = later picks)\n")
cmp <- merge(alfa_firsts[, .(player_id, season, exp_pick_b=exp_pick, cur_b=cur_value)],
             alfa_firsts_post[, .(player_id, exp_pick_a=exp_pick, cur_a=cur_value)],
             by="player_id")[order(season)]
print(cmp[, .(season, exp_pick_b=round(exp_pick_b,1), exp_pick_a=round(exp_pick_a,1),
              cur_before=round(cur_b), cur_after=round(cur_a),
              lost=round(cur_b-cur_a))])
picks_val_after <- sum(alfa_firsts_post$next_value_mean)
picks_cur_after <- sum(alfa_firsts_post$cur_value)

# ================= SCENARIO 1: Puka -> Hampton + Goff =====================
cat("\n== SCENARIO 1: Puka -> Hampton + Goff (NFC Nostalgia) ==\n")
te1 <- as.data.table(ffs_trade_eval(sim, joe, nacua, nfc, c(hampton, goff)))
fut1 <- (nxt(hampton) + nxt(goff)) - nxt(nacua)
cat(" Joe playoff delta:", sprintf("%+.1f%%", 100*te1[franchise_id==joe]$playoff_pct_delta),
    "| h2h", round(te1[franchise_id==joe]$h2h_wins_delta,2), "\n")
cat(" value swap: recv", round(cur(hampton)+cur(goff)), "vs Puka", round(cur(nacua)),
    "| future_capital_delta", round(fut1), "\n")

# ================= SCENARIO 2: Puka -> 3 firsts + throw-in ================
cat("\n== SCENARIO 2: Puka -> 3 firsts + ", throw$player_name, " (rebuilder) ==\n", sep="")
te2 <- as.data.table(ffs_trade_eval(sim, joe, nacua, alfa, throw$player_id))  # picks win-neutral
fut2 <- (picks_val_after + nxt(throw$player_id)) - nxt(nacua)
cat(" Joe playoff delta:", sprintf("%+.1f%%", 100*te2[franchise_id==joe]$playoff_pct_delta),
    "| h2h", round(te2[franchise_id==joe]$h2h_wins_delta,2), "\n")
cat(" value swap: recv", round(picks_cur_after + cur(throw$player_id)),
    "(picks", round(picks_cur_after), "+ WR", round(cur(throw$player_id)),
    ") vs Puka", round(cur(nacua)), "| future_capital_delta", round(fut2), "\n")
cat(" (picks valued AFTER Puka devalues them: naive-slot value would be",
    round(sum(alfa_firsts$cur_value)), ")\n")
