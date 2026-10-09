# Next year's keepers, league-wide - and what to trade now to get them.
#
#   Rscript dev/suite/keeper_outlook.R
#
# For a keeper league mid-season: every rostered player's price to keep NEXT
# year (this year's draft round - 1, the round follows him through drops and
# trades; see keeper_next_year.R), a projected next-preseason value
# distribution from ffs_dynasty_outlook() on the newest saved season
# simulation, and from those:
#
#   P(keep)   chance he is worth his price next year: that his projected rank
#             beats what a pick in his keeper round actually returned in this
#             year's draft (the round bar)
#   surplus   median projected value minus the value of that pick
#   option    E[max(next value - pick value, 0)]: what the RIGHT to keep him is
#             worth today, since you only keep him if he turns out worth it.
#             This is the 2027 number a trade should be priced on.
#
# Then every franchise's likely keeper set (greedy on surplus under the max-8 /
# one-top-two-pick rules), my plan, what to sell to contenders (2026 value, no
# 2027 keeper value to me), what to buy from them (cheap now, keeper next year)
# and pairings between the two.
#
# No new simulation: reuses dev/league_sims/<league>/<newest date>/simulation.rds
# from dev/suite/playoff_odds.R. Pairings run on FantasyCalc values only - they
# are conversation starters, not a simulated trade verdict.
#
# Outputs dev/league_sims/keeper_outlook/<date>/: league_keepers.csv,
# players.csv, my_plan.csv, sell.csv, buy.csv, pairs.csv, keeper_outlook.html
# (+ _artifact.html shaped for publishing).

suppressPackageStartupMessages(library(data.table))
devtools::load_all(here::here(), quiet = TRUE)
source(here::here("dev", "suite", "keeper_next_year.R"))

config <- list(
  league_id = Sys.getenv("FFS_KEEP_LEAGUE", "1339817277222031360"),
  season = as.integer(Sys.getenv("FFS_SEASON", "2026")),
  my_franchise = Sys.getenv("FFS_KEEP_ME", "7"),
  max_keepers = as.integer(Sys.getenv("FFS_KEEP_MAX", "8")),
  max_top2 = as.integer(Sys.getenv("FFS_KEEP_MAX_TOP2", "1")),
  season_weeks = 14L,
  # below this 2026 redraft value a player is not worth a contender's time
  sell_min_redraft = as.numeric(Sys.getenv("FFS_SELL_MIN", "1000")),
  seed = as.integer(Sys.getenv("FFS_KEEP_SEED", "20260929"))
)
day <- format(Sys.Date())
out <- here::here("dev", "league_sims", "keeper_outlook", day)
dir.create(out, recursive = TRUE, showWarnings = FALSE)

## ---- inputs ------------------------------------------------------------------
sims <- Sys.glob(here::here("dev", "league_sims", config$league_id, "*", "simulation.rds"))
if (!length(sims)) stop("no saved simulation for league ", config$league_id,
                        " - run dev/suite/playoff_odds.R first")
sim_path <- sort(sims, decreasing = TRUE)[[1]]
message("simulation: ", sim_path)
sim <- readRDS(sim_path)
n_sims <- as.integer(sim$simulation_params$n_seasons)

picks <- keeper_draft_picks(config$league_id)
bar <- keeper_round_bar(config$league_id, config$season, picks = picks)
n_teams <- attr(picks, "n_teams"); n_rounds <- attr(picks, "n_rounds")
fc <- as.data.table(fc_dynasty_values(num_qbs = 2, num_teams = n_teams, ppr = 1))

# the weeks already played, in this league's scoring, for the season-quality
# read; the simulation only covers the rest
first_sim_week <- min(sim$projected_scores$week)
ids <- as.data.table(ffscrapr::dp_playerids())[!is.na(gsis_id) & !is.na(fantasypros_id),
                                                list(gsis_id, fantasypros_id = as.character(fantasypros_id))]
banked <- merge(as.data.table(sim$scoring_history)[season == config$season & week < first_sim_week,
                                                   list(gsis_id, points)],
                unique(ids, by = "gsis_id"), by = "gsis_id")[
  , list(points = sum(points)), by = fantasypros_id]
# the preseason expectation the season is judged against (not today's ROS rank)
pre <- as.data.table(fp_rankings_history())[season == config$season,
                                            list(fantasypros_id = as.character(fantasypros_id),
                                                 redraft_rank = rank)]
message("banked weeks 1-", first_sim_week - 1, " for ", nrow(banked), " players; ",
        nrow(pre), " preseason ranks")

message("dynasty outlook (", n_sims, " seasons) ...")
set.seed(config$seed)
ol <- ffs_dynasty_outlook(sim, format = "superflex", dynasty_values = fc,
                          banked_points = banked, season_weeks = config$season_weeks,
                          redraft_ranks = pre)
draws <- as.data.table(attr(ol, "draws"))
ol <- as.data.table(ol)

## ---- every rostered player: price, bar, projection ------------------------------
ros <- as.data.table(sim$rosters)[pos %in% c("QB", "RB", "WR", "TE"),
                                  list(franchise_id = as.character(franchise_id), franchise_name,
                                       fantasypros_id = as.character(fantasypros_id),
                                       player = player_name, pos, team)]
ros <- unique(ros, by = c("franchise_id", "fantasypros_id"))
priced <- keeper_next_year(ros, picks, bar, fc)
priced <- merge(priced, fc[!is.na(fantasypros_id), list(fantasypros_id = as.character(fantasypros_id),
                                                        age, redraft_value, dynasty_value = value)] |>
                  unique(by = "fantasypros_id"), by = "fantasypros_id", all.x = TRUE)

# the market ladder the outlook's next_rank lives on, as rank -> value
ladder <- fc[!is.na(overall_rank) & value > 0][order(overall_rank)]
val_at <- approxfun(ladder$overall_rank, ladder$value, rule = 2)

# next year's draft slot, per franchise per simulated season: the same season
# the player's own projection is drawn from, so a lost year for a team's
# players also moves its pick up (see keeper_next_slots())
slots <- keeper_next_slots(sim, seed = config$seed)
bar_slot <- keeper_round_bar(config$league_id, config$season, picks = picks, slot = seq_len(n_teams))
slot_dist <- dcast(slots[, .N, by = list(franchise_id, slot)], franchise_id ~ slot,
                   value.var = "N", fill = 0)
slot_mean <- slots[, list(exp_slot = mean(slot), p_top4 = mean(slot <= 4)), by = franchise_id]

# per draw: a drafted player's price is fixed; a never-drafted one is priced off
# next year's ADP, which is his own projected rank in that draw. The bar is the
# pick his team actually holds in that round, in that simulated season.
d <- merge(draws, priced[, list(fantasypros_id, franchise_id, keep_cost, cost_source)] |>
             unique(by = "fantasypros_id"), by = "fantasypros_id")
d <- merge(d, slots[, list(sim_season, franchise_id, slot)], by = c("sim_season", "franchise_id"))
d[, cost_d := fifelse(cost_source == "adp" & !exited,
                      pmin(n_rounds, as.integer(ceiling(next_rank / n_teams))), keep_cost)]
d[, cost_d := pmin(pmax(cost_d, 1L), n_rounds)]
d[bar_slot, bar_d := i.bar_rank, on = c(cost_d = "round", slot = "slot")]
d[, bar_val := val_at(bar_d)]
d[, gain := next_value - bar_val]
# the same player at every slot, for reading off "what if I pick first"
by_slot_all <- rbindlist(lapply(seq_len(n_teams), function(s) {
  b <- bar_slot[slot == s][order(round)]
  d[, list(slot = s, p_keep = mean(!exited & next_rank <= b$bar_rank[cost_d])), by = fantasypros_id]
}))
proj <- d[, list(p_keep = mean(!exited & next_rank <= bar_d),
                 bar_exp = mean(bar_d),
                 surplus = median(gain),
                 option = mean(pmax(gain, 0)),
                 next_rank_med = as.numeric(median(fifelse(exited, NA_real_, next_rank), na.rm = TRUE)),
                 p_exit = mean(exited)),
          by = fantasypros_id]
P <- merge(priced, proj, by = "fantasypros_id", all.x = TRUE)
P <- merge(P, ol[, list(fantasypros_id, next_value_med, next_value_p10, next_value_p90)],
           by = "fantasypros_id", all.x = TRUE)
P[, mine := franchise_id == config$my_franchise]
P <- merge(P, slot_mean, by = "franchise_id", all.x = TRUE)
message(P[!is.na(p_keep), .N], "/", nrow(P), " rostered players projected")

## ---- every franchise's likely 2027 keeper set ---------------------------------
sets <- rbindlist(lapply(split(P, P$franchise_id), function(f) {
  cand <- f[!is.na(keep_cost) & !is.na(surplus),
            list(player_id = fantasypros_id, base_round = keep_cost, top2_prev = top2, surplus)]
  chosen <- ffsimulator:::.ffs_greedy_keepers(cand, "surplus", max_keepers = config$max_keepers,
                                              max_top2 = config$max_top2, positive_only = TRUE)
  a <- ffs_assign_keeper_rounds(cand, chosen, max_keepers = config$max_keepers,
                                max_top2 = config$max_top2)
  stopifnot(a$feasible)
  if (!nrow(a$assignment)) return(NULL)
  data.table(franchise_id = f$franchise_id[[1]], fantasypros_id = a$assignment$player_id,
             keep_round = a$assignment$keeper_round)
}))
P <- merge(P, sets, by = c("franchise_id", "fantasypros_id"), all.x = TRUE)
P[, in_set := !is.na(keep_round)]

odds_files <- sort(Sys.glob(here::here("dev", "league_sims", "playoff_odds", "*", "odds.csv")),
                   decreasing = TRUE)
odds <- NULL; needs <- NULL
fnames <- unique(P$franchise_name)
for (of in odds_files) {
  o <- fread(of)[, franchise_id := as.character(franchise_id)]
  # the league whose franchise names are exactly this league's
  lg <- o[, list(hit = setequal(franchise_name, fnames)), by = league][hit == TRUE]$league
  if (length(lg)) {
    odds <- o[league == lg[[1]]]
    nf <- file.path(dirname(of), "needs.csv")
    if (file.exists(nf)) needs <- fread(nf)[league == o$league[[1]]][, franchise_id := as.character(franchise_id)]
    message("odds: ", of)
    break
  }
}
posture <- if (is.null(odds)) data.table(franchise_id = unique(P$franchise_id), posture = NA_character_,
                                         playoff_pct = NA_real_, champion_pct = NA_real_) else
  odds[, list(franchise_id, posture, playoff_pct, champion_pct)]
P <- merge(P, posture, by = "franchise_id", all.x = TRUE)

league <- P[, list(keepers = sum(in_set), keeper_surplus = sum(surplus[in_set]),
                   keeper_option = sum(option[in_set]),
                   keepers_list = paste(sprintf("%s R%d", player[in_set][order(keep_round[in_set])],
                                                sort(keep_round[in_set])), collapse = ", "),
                   redraft_now = sum(redraft_value, na.rm = TRUE)),
            by = list(franchise_id, franchise_name, posture, playoff_pct, champion_pct,
                      exp_slot, p_top4)][order(-keeper_surplus)]
league[, keeper_rank := seq_len(.N)]
league <- merge(league, slot_dist, by = "franchise_id", all.x = TRUE)[order(keeper_rank)]

## ---- my plan, sell, buy, pairs ---------------------------------------------------
mine <- P[mine == TRUE][order(-in_set, keep_round, -option)]
# outside the keeper set, a player worth at least twice as much to a contender
# this year as his keeper option is worth to me is a sell, whatever his P(keep)
sellable <- function(d) !d$in_set & !is.na(d$redraft_value) &
  d$redraft_value >= config$sell_min_redraft & d$redraft_value >= 2 * fcoalesce(d$option, 0)
is_sell <- sellable(mine)
mine[, verdict := fifelse(in_set & p_keep >= 0.7, "core keeper",
                  fifelse(in_set, "likely keeper",
                  fifelse(is_sell, "sell",
                  fifelse(fcoalesce(p_keep, 0) >= 0.3, "bubble", "depth"))))]
# why, in words: the price he has to beat, and whether the top-two rule (not
# his own value) is what keeps him out
# say WHICH pick: the overall picks this round lands on at my likely slots
# (those the sims put me in at least 10% of the time), and what a pick there
# returns - a board rank, which sits below the pick number because players
# ranked above it were kept or reached for
my_p <- slots[franchise_id == config$my_franchise, list(p = .N / n_sims), by = slot]
likely <- sort(my_p[p >= 0.10]$slot)
p_likely <- sum(my_p[slot %in% likely]$p)
bar_range <- function(r) {
  b <- round(bar_slot[round == r & slot %in% likely]$bar_rank)
  if (!length(b)) return(NA_character_)
  if (min(b) == max(b)) paste0("#", min(b)) else paste0("#", min(b), "-", max(b))
}
pick_range <- function(r) {
  pk <- bar_slot[round == r & slot %in% likely]$pick
  if (!length(pk)) return(NA_character_)
  if (min(pk) == max(pk)) as.character(min(pk)) else paste0(min(pk), "-", max(pk))
}
mine[, why := sprintf("R%s costs your pick %s (slots %s, %.0f%% of sims); a pick there returns about the %s player. %.0f%% chance he beats that.",
                      keep_cost, vapply(keep_cost, pick_range, character(1)),
                      paste(range(likely), collapse = "-"), 100 * p_likely,
                      vapply(keep_cost, bar_range, character(1)), 100 * p_keep)]
mine[is.na(p_keep), why := "no projection"]
if (any(mine$top2 & mine$in_set)) {
  mine[top2 == TRUE & in_set == FALSE & surplus > 0,
       why := paste0("worth keeping, but only one top-two pick can be kept and ",
                     mine[top2 & in_set]$player[[1]], " is the better one")]
}

sell <- mine[verdict == "sell"][order(-redraft_value)]

contenders <- posture[posture %in% c("Contender", "Bubble") & franchise_id != config$my_franchise]$franchise_id

## ---- what a newcomer would cost ME ---------------------------------------------
# A player I trade for or claim does not cost his own round in a vacuum: he joins
# my projected keeper set, and if that round is taken (Michael Wilson at R16) the
# bump-up rule pushes one of them a round earlier. The pick I actually give up is
# the round newly forfeited once he is added - and it is MY pick, at MY slot.
my_set <- P[mine == TRUE & in_set == TRUE,
            list(player_id = fantasypros_id, base_round = keep_cost, top2_prev = top2)]
my_rounds <- ffs_assign_keeper_rounds(my_set, max_keepers = config$max_keepers,
                                      max_top2 = config$max_top2)$rounds_used
# When my set is already full (or a top-two slot is taken), a newcomer has to
# REPLACE someone: try dropping my keepers weakest-first (by median surplus)
# and take the first that makes room. His gain is then net of what the dropped
# keeper was worth to me.
my_weak <- P[mine == TRUE & in_set == TRUE][order(surplus)]$fantasypros_id
marginal <- function(cost, top2) {
  none <- list(marg_round = NA_integer_, replaces = NA_character_)
  if (is.na(cost)) return(none)
  new_row <- data.table(player_id = ".new", base_round = as.integer(cost), top2_prev = isTRUE(top2))
  for (drop in c(NA_character_, my_weak)) {
    base <- if (is.na(drop)) my_set else my_set[player_id != drop]
    base_rounds <- if (is.na(drop)) my_rounds else
      ffs_assign_keeper_rounds(base, max_keepers = config$max_keepers, max_top2 = config$max_top2)$rounds_used
    a <- ffs_assign_keeper_rounds(rbind(base, new_row), max_keepers = config$max_keepers,
                                  max_top2 = config$max_top2)
    if (!a$feasible) next
    new <- setdiff(a$rounds_used, base_rounds)
    return(list(marg_round = as.integer(if (length(new) == 1L) new else max(new)), replaces = drop))
  }
  none
}
marginal_round <- function(cost, top2) marginal(cost, top2)$marg_round
add_marginal <- function(dt) {
  mm <- rbindlist(Map(marginal, dt$keep_cost, dt$top2))
  dt[, `:=`(marg_round = mm$marg_round, replaces_id = mm$replaces)]
  rep <- P[mine == TRUE, list(replaces_id = fantasypros_id, replaces = player,
                              rep_option = option, rep_surplus = surplus)]
  dt[rep, `:=`(replaces = i.replaces, rep_option = i.rep_option, rep_surplus = i.rep_surplus),
     on = "replaces_id"]
  dt[]
}
my_slot_s <- slots[franchise_id == config$my_franchise, list(sim_season, my_slot = slot)]
my_slot_p <- my_slot_s[, list(p = .N / n_sims), by = list(slot = my_slot)]
my_bar <- merge(bar_slot, my_slot_p, by = "slot")[, list(my_bar = sum(bar_rank * p)), by = round]

# trade targets, re-priced as mine: the marginal round, my slot in each season
buy <- add_marginal(P[mine == FALSE & !is.na(keep_cost)])
db <- merge(d[fantasypros_id %in% buy[!is.na(marg_round)]$fantasypros_id,
              list(fantasypros_id, sim_season, exited, next_rank, next_value, cost_d, keep_cost)],
            buy[, list(fantasypros_id, marg_round)], by = "fantasypros_id")
db <- merge(db, my_slot_s, by = "sim_season")
# a never-drafted player's cost moves with his projected rank; keep the same bump
db[, r_me := pmax(1L, cost_d - (keep_cost - marg_round))]
db[bar_slot, bar_me := i.bar_rank, on = c(r_me = "round", my_slot = "slot")]
db[, gain := next_value - val_at(bar_me)]
buy <- merge(buy, db[, list(p_keep_me = mean(!exited & next_rank <= bar_me),
                            option_me = mean(pmax(gain, 0)), surplus_me = median(gain),
                            bar_me = mean(bar_me)), by = fantasypros_id],
             by = "fantasypros_id")
# net of the keeper he would push out of my set, if any
buy[, option_net := option_me - fcoalesce(rep_option, 0)]
buy[, surplus_net := surplus_me - fcoalesce(rep_surplus, 0)]
buy <- buy[option_net > 0][order(-option_net)]
buy[, owner_keeps := in_set]
buy[, from_contender := franchise_id %in% contenders]

pairs <- rbindlist(lapply(contenders, function(cf) {
  their_needs <- if (is.null(needs)) character(0) else needs[franchise_id == cf & vs_median < 0]$pos
  s <- sell[if (length(their_needs)) pos %in% their_needs else TRUE]
  b <- buy[franchise_id == cf]
  if (!nrow(s) || !nrow(b)) return(NULL)
  g <- CJ(i = seq_len(nrow(s)), j = seq_len(nrow(b)))
  data.table(owner = b$franchise_name[g$j], posture = b$posture[g$j],
             give = s$player[g$i], give_pos = s$pos[g$i],
             give_redraft = s$redraft_value[g$i], give_option = s$option[g$i],
             get = b$player[g$j], get_pos = b$pos[g$j], get_cost = paste0("R", b$marg_round[g$j]),
             get_redraft = b$redraft_value[g$j], get_option = b$option_net[g$j], replaces = b$replaces[g$j],
             get_p_keep = b$p_keep_me[g$j], owner_keeps = b$owner_keeps[g$j])
}))
if (nrow(pairs)) {
  pairs[, their_gain_2026 := give_redraft - fcoalesce(get_redraft, 0)]
  pairs[, my_gain_2027 := get_option - fcoalesce(give_option, 0)]
  pairs <- pairs[their_gain_2026 > 0 & my_gain_2027 > 0]
  # what they get for what they give up: 2026 value gained per unit of 2027
  # keeper value sent over. A contender should take a high ratio.
  pairs[, ratio := their_gain_2026 / my_gain_2027]
  # both sides have to want it: rank on the smaller of the two gains
  pairs[, score := pmin(their_gain_2026, my_gain_2027)]
  setorder(pairs, -score)
}

## ---- the waiver wire: free agents who could be cheap keepers -----------------------
# Free agents are not in the season simulation, so their P(keep) is ESTIMATED:
# fit P(keep) on the rostered players against how far their dynasty rank sits
# inside their bar (log bar / rank), age and position, then read free agents
# off that fit at the marginal round they would cost me, at my slot. Usage and
# market columns come from the newest waiver watchlist.
fitd <- P[!is.na(p_keep) & !is.na(dyn_rank) & !is.na(bar_exp) & !is.na(age),
          list(p_keep, x = log(bar_exp / dyn_rank), age, pos)]
pk_fit <- suppressWarnings(glm(p_keep ~ x + age + pos, family = quasibinomial(), data = fitd))
pk_r2 <- cor(fitted(pk_fit), fitd$p_keep)^2
message(sprintf("free-agent P(keep) fit on %d rostered players: R2 %.2f", nrow(fitd), pk_r2))

wl_files <- sort(Sys.glob(here::here("dev", "league_sims", "waivers", "*", "watchlist.csv")),
                 decreasing = TRUE)
fa <- data.table()
for (wf in wl_files) {
  w <- fread(wf)
  lg <- w[, list(hit = all(setdiff(unique(rostered_by), c(NA, "")) %in% fnames) &&
                   any(rostered_by %in% fnames)), by = league][hit == TRUE]$league
  if (!length(lg)) next
  w <- w[league == lg[[1]] & status == "free agent" & pos %in% c("QB", "RB", "WR", "TE")]
  w[, fantasypros_id := as.character(fantasypros_id)]
  drop <- intersect(c("draft_rd", "kept_now", "keep_cost", "top2", "cost_source", "dyn_rank",
                      "keep_bar", "keep_margin", "age"), names(w))
  if (length(drop)) w[, (drop) := NULL]
  fa <- keeper_next_year(w, picks, bar, fc)
  fa <- merge(fa, unique(fc[!is.na(fantasypros_id), list(fantasypros_id = as.character(fantasypros_id), age)],
                         by = "fantasypros_id"), by = "fantasypros_id", all.x = TRUE)
  fa <- fa[!is.na(dyn_rank)]
  fa <- add_marginal(fa)
  fa[, bumped := !is.na(marg_round) & marg_round != keep_cost]
  fa <- merge(fa, my_bar, by.x = "marg_round", by.y = "round", all.x = TRUE)
  # rank places he clears the pick by, and the same gap in trade-value points
  fa[, my_margin := round(my_bar - dyn_rank)]
  fa[, excess := round(val_at(dyn_rank) - val_at(my_bar))]
  fa[, excess_net := round(excess - fcoalesce(rep_surplus, 0))]
  fa[, p_keep_est := NA_real_]
  ok <- !is.na(fa$my_bar) & !is.na(fa$age) & fa$pos %in% fitd$pos
  fa[ok, p_keep_est := predict(pk_fit, newdata = data.frame(x = log(my_bar / dyn_rank), age = age, pos = pos),
                               type = "response")]
  fa <- fa[order(-excess_net)]
  attr(fa, "source") <- wf
  message("waiver wire: ", nrow(fa), " ranked free agents from ", wf)
  break
}

## ---- write -----------------------------------------------------------------------
pcols <- c("franchise_name", "player", "pos", "age", "team", "draft_rd", "kept_now", "keep_cost",
           "top2", "cost_source", "keep_bar", "bar_exp", "exp_slot", "dyn_rank", "next_rank_med",
           "p_keep", "p_exit",
           "surplus", "option", "redraft_value", "dynasty_value", "in_set", "keep_round", "posture")
fwrite(P[order(franchise_name, -option), intersect(pcols, names(P)), with = FALSE],
       file.path(out, "players.csv"))
fwrite(league, file.path(out, "league_keepers.csv"))
fwrite(mine[, c(intersect(pcols, names(mine)), "verdict", "why"), with = FALSE], file.path(out, "my_plan.csv"))
fwrite(sell[, intersect(pcols, names(sell)), with = FALSE], file.path(out, "sell.csv"))
fwrite(buy[, c(intersect(pcols, names(buy)), "marg_round", "replaces", "bar_me", "p_keep_me", "option_me", "option_net",
               "surplus_me", "from_contender"), with = FALSE], file.path(out, "buy.csv"))
fwrite(pairs, file.path(out, "pairs.csv"))
if (nrow(fa)) fwrite(fa[, intersect(c("player", "pos", "team", "age", "draft_rd", "keep_cost", "marg_round",
                                      "bumped", "replaces", "cost_source", "dyn_rank", "my_bar", "my_margin", "excess_net",
                                      "excess", "p_keep_est", "pos_rank", "roster_pct",
                                      "trend_add", "snap_pct", "xfp_g", "fp_g", "injury",
                                      "redraft_value", "dynasty_value"), names(fa)), with = FALSE],
                     file.path(out, "waiver_keepers.csv"))
fwrite(bar_slot, file.path(out, "round_bar.csv"))
# my players' P(keep) if I picked from each slot, whatever the sims say
my_by_slot <- dcast(merge(by_slot_all, mine[, list(fantasypros_id, player, keep_cost)],
                          by = "fantasypros_id"),
                    player + keep_cost ~ paste0("slot", slot), value.var = "p_keep")
my_by_slot <- my_by_slot[order(keep_cost)]
sc <- grep("^slot", names(my_by_slot), value = TRUE)
my_by_slot[, (sc) := lapply(.SD, round, 2), .SDcols = sc]
fwrite(my_by_slot, file.path(out, "my_p_keep_by_slot.csv"))
my_slots <- slots[franchise_id == config$my_franchise, list(p = .N / n_sims), by = slot][order(slot)]

## ---- console ---------------------------------------------------------------------
cat("\n== round bar: the board rank a pick in each round returned (non-keepers, fitted) ==\n")
print(bar[, list(round, bar = round(bar_rank), raw_median = med_rank, n)])
cat("\n== 2027 keeper capital by franchise (exp_slot = expected 2027 draft slot) ==\n")
print(league[, list(keeper_rank, franchise_name, posture, keepers, exp_slot = round(exp_slot, 1),
                    surplus = round(keeper_surplus), option = round(keeper_option))])
cat("\n== my 2027 draft slot (toilet bowl winner picks 1st) ==\n")
print(my_slots[, list(slot, p = round(p, 3))])
cat("\n== my players' P(keep) at each slot ==\n")
print(my_by_slot)
cat("\n== my roster ==\n")
print(mine[, list(player, pos, drafted = draft_rd, cost = keep_cost, bar = round(bar_exp),
                  proj_rank = round(next_rank_med), p_keep = round(p_keep, 2),
                  surplus = round(surplus), option = round(option), redraft = redraft_value, verdict)])
print(mine[, list(player, why)])
cat("\n== sell (2026 value, not in my 2027 set) ==\n")
print(sell[, list(player, pos, redraft = redraft_value, option = round(option), p_keep = round(p_keep, 2))])
cat("\n== my projected keeper rounds: ", paste(sort(my_rounds), collapse = ", "), " ==\n", sep = "")
cat("\n== buy: best 2027 keeper options on other rosters, priced as MINE ==\n")
print(head(buy[, list(player, pos, owner = franchise_name, posture, cost = keep_cost, costs_me = marg_round,
                      replaces, p_keep_me = round(p_keep_me, 2), option_me = round(option_me), option_net = round(option_net),
                      owner_option = round(option), redraft = redraft_value, owner_keeps)], 25))
cat(sprintf("\n== waiver wire: free agents by excess value (P(keep) est. fit R2 %.2f) ==\n", pk_r2))
if (nrow(fa)) print(head(fa[, list(player, pos, team, drafted = draft_rd, cost = keep_cost, costs_me = marg_round, replaces,
                                   src = cost_source, dyn_rank, my_bar = round(my_bar), margin = my_margin,
                                   excess, p_keep = round(p_keep_est, 2), own = roster_pct,
                                   snap = round(snap_pct, 2))], 25))
cat("\n== pairings with contenders (FantasyCalc values, not simulated) ==\n")
if (nrow(pairs)) print(head(pairs[, list(owner, give, get, get_cost, replaces, their_gain_2026 = round(their_gain_2026),
                        my_gain_2027 = round(my_gain_2027))], 20))

saveRDS(list(P = P, league = league, mine = mine, sell = sell, buy = buy, pairs = pairs, bar = bar,
             bar_slot = bar_slot, my_by_slot = my_by_slot, my_slots = my_slots,
             fa = fa, fa_source = attr(fa, "source"),
             # per-draw projections, for head-to-head questions after the fact
             draws = d[, list(fantasypros_id, franchise_id, sim_season, slot, exited, next_rank,
                              next_value, keep_cost, cost_d)],
             slots = slots,
             meta = list(sim = sim_path, n_sims = n_sims, first_sim_week = first_sim_week,
                         my_rounds = sort(my_rounds), pk_r2 = pk_r2,
                         generated = format(Sys.time(), "%Y-%m-%d %H:%M"))),
        file.path(out, "keeper_outlook.rds"))
cat("\nwrote ", out, "\n", sep = "")

source(here::here("dev", "suite", "keeper_outlook_page.R"))
message("page: ", keeper_outlook_page(file.path(out, "keeper_outlook.rds")))
