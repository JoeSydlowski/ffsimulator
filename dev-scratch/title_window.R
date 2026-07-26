# LIGHTWEIGHT 5-year title-odds estimate for three Puka strategies (Jon, sflx).
# NOT a full multi-year sim - a transparent forward projection:
#   strength_f,y = sum of a team's top-K starters' REDRAFT (win-now) values,
#                  each aged through a position production curve; picks convert
#                  to developing rookies in their draft year.
#   champ_odds_f,y = strength^gamma / sum(strength^gamma), gamma calibrated so
#                  year-0 odds match the sim's ACTUAL championship odds.
#   expected titles (window) = sum_y champ_odds(Joe, y).
suppressMessages({library(data.table); pkgload::load_all(".", quiet=TRUE); library(ffscrapr)})
BASE <- "dev/league_sims/1359546500786434048"
sim  <- readRDS(file.path(BASE, "2026-07-21", "simulation.rds"))
dyn  <- fread(file.path(BASE, "2026-07-20", "dynasty_outlook.csv"))
dyn[, `:=`(player_id = as.character(player_id), franchise_id = as.character(franchise_id),
           fantasypros_id = as.character(fantasypros_id))]
fc   <- as.data.table(fc_dynasty_values(num_qbs = 2))          # superflex redraft values
fc[, fantasypros_id := as.character(fantasypros_id)]
WINDOW <- 0:4                                                   # 5-year window (2026-2030)

pid <- function(nm) dyn[player_name == nm]$player_id[1]
joe <- dyn[franchise_name=="sox05syd"]$franchise_id[1]
nfc <- dyn[franchise_name=="NFC Nostalgia"]$franchise_id[1]
nacua<-pid("Puka Nacua"); hampton<-pid("Omarion Hampton"); goff<-pid("Jared Goff")

# --- roster table: player, franchise, pos, age, redraft (win-now) value --------
ros <- merge(dyn[, .(player_id, fantasypros_id, player_name, pos, age, franchise_id, cur_value)],
             fc[, .(fantasypros_id, redraft_value)], by="fantasypros_id", all.x=TRUE)
ros <- ros[!is.na(redraft_value) & redraft_value > 0 & !is.na(age)]
K <- 10L                                                        # ~superflex starters
med_start_rd <- median(ros[, .(v=max(redraft_value)), by=franchise_id]$v)  # scale ref

# production multiplier vs peak (1.0 at peak); crude per-position curves
age_mult <- function(pos, age){
  peak <- c(QB=27,RB=24,WR=26,TE=27)[pos]; up <- c(QB=.05,RB=.09,WR=.07,TE=.06)[pos]
  dn   <- c(QB=.045,RB=.12,WR=.08,TE=.06)[pos]
  m <- ifelse(age<=peak, 1-up*(peak-age), 1-dn*(age-peak)); pmax(m,.12)
}
# a player's win-now value y years out (age forward)
proj_rd <- function(pos, age, rd, y){
  a0<-age; a1<-age+y; rd * age_mult(pos,a1)/age_mult(pos,a0)
}

# --- rebuilder + its post-Puka finish already known: pick slots ~ mid-first ----
# converted rookie: enters age 22 as a WR-curve developing starter; peak win-now
# value scaled from the pick's dynasty value; ramps 0.45/0.8/1.0 over first 3 yrs
rebuilder <- "mkbarz"
alfa <- dyn[franchise_name==rebuilder]$franchise_id[1]
pick_dyn_after <- c(3503, 3261, 2392)         # from puka_compare (post-Puka devalued cur)
dyn2rd <- med_start_rd / median(ros[, .(v=max(cur_value)), by=franchise_id]$v)
pick_peak_rd <- pick_dyn_after * dyn2rd
draft_year   <- c(1L,2L,3L)                    # 2027/28/29 relative to 2026 (y0)
ramp <- function(k) c(.45,.8,1.0)[pmin(k,3)]   # k = seasons since drafted (1-based)

# strength of a franchise in year y given a roster table (player rows) + extra picks
strength <- function(rtab, y, picks=NULL){
  v <- rtab[, proj_rd(pos, age, redraft_value, y)]
  vals <- sort(v, decreasing=TRUE)
  s <- sum(head(vals, K))
  if(!is.null(picks)) for(i in seq_along(picks$peak)){
    k <- y - picks$dy[i] + 1L                  # seasons since this pick drafted
    if(k>=1) s <- s + picks$peak[i]*ramp(k)*age_mult("WR",22+k-1)/age_mult("WR", 24)
  }
  s
}

# --- year-0 champ odds from the sim, to calibrate gamma ------------------------
sw <- as.data.table(ffs_summarise_week(sim$optimal_scores, sim$schedules))
ss <- as.data.table(ffs_summarise_season(summary_week=sw))
ss[, lg_rank := frank(list(-h2h_wins,-points_for), ties.method="first"), by=season]
champ0 <- as.data.table(ffsimulator:::.ffs_champion_pct(sw, ss))   # franchise_id, champion_pct

base_str0 <- ros[, .(S=strength(.SD,0)), by=franchise_id]
cal <- merge(base_str0, champ0[,.(franchise_id, champ=champion_pct)], by="franchise_id")
fit_gamma <- optimize(function(g){ p<-cal$S^g/sum(cal$S^g); sum((p-cal$champ)^2)}, c(.5,8))$minimum
cat("calibrated gamma:", round(fit_gamma,2), "| K:", K, "| year-0 sim champ(Joe):",
    round(champ0[franchise_id==joe]$champion_pct,3), "\n")

# --- scenario roster builders --------------------------------------------------
base_ros <- copy(ros)
sc_status <- copy(base_ros)                                           # keep Puka
sc_win    <- copy(base_ros)                                           # +Hampton+Goff -Puka
sc_win[player_id==nacua, franchise_id := nfc]
sc_win[player_id %in% c(hampton,goff), franchise_id := joe]
sc_reb    <- copy(base_ros)                                           # -Puka (to mkbarz) + picks
sc_reb[player_id==nacua, franchise_id := alfa]
reb_picks <- list(peak=pick_peak_rd, dy=draft_year)

# --- project champ odds each year & sum -----------------------------------------
proj <- function(rtab, joe_picks=NULL){
  sapply(WINDOW, function(y){
    S <- rtab[, .(S=strength(.SD, y, picks=if(.BY$franchise_id==joe) joe_picks else NULL)),
              by=franchise_id]
    p <- S$S^fit_gamma / sum(S$S^fit_gamma)
    p[match(joe, S$franchise_id)]
  })
}
c_status <- proj(sc_status)
c_win    <- proj(sc_win)
c_reb    <- proj(sc_reb, reb_picks)

yr <- 2026 + WINDOW
out <- data.table(year=yr,
  status_quo = round(c_status,3), win_now = round(c_win,3), rebuild = round(c_reb,3))
cat("\n== projected championship odds for Joe, by year ==\n"); print(out)
cat("\n== expected TITLES over the 5-year window (sum of annual odds) ==\n")
cat(sprintf("  status quo (keep Puka):        %.2f\n", sum(c_status)))
cat(sprintf("  win-now (Hampton+Goff):        %.2f\n", sum(c_win)))
cat(sprintf("  rebuild (3 firsts + throw-in): %.2f\n", sum(c_reb)))
cat(sprintf("\n  rebuild - win-now titles: %+.2f  (near-term yr0 loss %+.3f, later-year gain %+.3f)\n",
    sum(c_reb)-sum(c_win), c_reb[1]-c_win[1], sum(c_reb[-1])-sum(c_win[-1])))
