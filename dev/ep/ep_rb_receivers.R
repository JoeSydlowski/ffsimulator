# ESPN RB receiving scores (Capture.PNG, transcribed) vs FantasyPros price and 2026 role.
#   Rscript dev/ep/ep_rb_receivers.R
suppressPackageStartupMessages(library(data.table))
S <- here::here("dev", "ep")
espn <- fread(text = "
name,season,espn_team,overall,open,catch,yac
Christian McCaffrey,2025,SF,98,49,99,56
Bijan Robinson,2025,ATL,89,42,86,80
Jahmyr Gibbs,2026,DET,87,67,99,55
Kenneth Walker,2025,SEA,83,48,64,83
Rhamondre Stevenson,2025,NE,82,55,99,59
Bucky Irving,2025,TB,79,39,52,90
Dylan Sampson,2025,CLE,77,66,66,68
Tyjae Spears,2025,TEN,77,69,79,58
Kenneth Gainwell,2025,PIT,76,58,99,46
Bijan Robinson,2026,ATL,76,35,66,79
Breece Hall,2025,NYJ,75,36,74,73
Ashton Jeanty,2026,LV,75,60,85,57
Jaylen Warren,2026,PIT,72,31,70,75
Jaylen Warren,2025,PIT,71,46,66,69
Jonathan Taylor,2026,IND,70,66,68,59
Jeremy McNichols,2025,WAS,69,77,59,58
Jahmyr Gibbs,2025,DET,69,61,81,51
Chuba Hubbard,2025,CAR,66,61,38,74
Michael Carter,2025,ARI,64,74,56,55
Jonathan Taylor,2025,IND,64,42,72,60
Josh Jacobs,2025,GB,64,55,73,53
Omarion Hampton,2025,LAC,63,52,69,56
Travis Etienne,2025,JAX,62,63,61,54
Ashton Jeanty,2025,LV,61,52,62,58
RJ Harvey,2025,DEN,60,37,75,55
D'Andre Swift,2025,CHI,60,53,68,51
RJ Harvey,2026,DEN,60,64,71,45
Christian McCaffrey,2026,SF,60,48,80,48
Aaron Jones,2025,MIN,57,54,60,52
Quinshon Judkins,2025,CLE,55,46,60,54
Kyren Williams,2026,LAR,55,47,99,29
Rico Dowdle,2025,CAR,54,38,73,48
Javonte Williams,2026,DAL,54,65,70,39
D'Andre Swift,2026,CHI,54,59,65,44
Justice Hill,2025,BAL,53,48,45,60
Kimani Vidal,2025,LAC,52,35,51,60
Kenneth Walker,2026,KC,52,46,63,49
Rhamondre Stevenson,2026,NE,52,65,57,43
Cam Skattebo,2025,NYG,50,50,53,50
")
# best ESPN score per player (2025 or 2026) for the screen
best <- espn[order(-overall), .SD[1], by = name][, .(name, espn_best = overall, espn_season = season, espn_team, open, catch, yac)]

p26 <- readRDS(file.path(S, "ep_panel_2026.rds"))
role <- p26[position == "RB", .(team26 = names(which.max(table(team))), games26 = .N,
  carry_share26 = sum(carries) / pmax(sum(team_carries), 1), targets_g26 = mean(targets), rec_xfp_g26 = mean(rec_xfp),
  xfp_g26 = mean(xfp), fp_g26 = mean(fp), fp_id = fp_id[!is.na(fp_id)][1]), by = .(name26 = name, gsis_id)]
# fuzzy join on surname + first initial (handles "Kenneth Walker III", "Kenny Gainwell")
key <- function(x) { x <- gsub(" (Jr\\.|Sr\\.|II|III|IV)$", "", x); paste(substr(x, 1, 3), sub(".* ", "", x)) }
best[, k := key(name)]; role[, k := key(name26)]
m <- merge(best, role, by = "k", all.x = TRUE)
pan <- as.data.table(arrow::read_parquet(here::here("dev", "data", "ecr_panel.parquet"), mmap = FALSE))
last <- function(pg) { d <- pan[page_type == pg & pos == "RB"]; d <- d[scrape_date == max(scrape_date)]
  d[, .(fp_id = fantasypros_id, rk = frank(ecr, ties.method = "first"), snap = scrape_date[1])] }
ros <- last("redraft-rb"); dyn <- last("dynasty-rb")
cat("FantasyPros boards: ROS RB", format(ros$snap[1]), " dynasty RB", format(dyn$snap[1]), "\n")
m <- merge(m, ros[, .(fp_id, ros_rb = rk)], by = "fp_id", all.x = TRUE)
m <- merge(m, dyn[, .(fp_id, dyn_rb = rk)], by = "fp_id", all.x = TRUE)
lb <- fread(file.path(S, "rb_bench_leaderboard_2026_wk5_k5.csv"))
lead_risk <- lb[role == "lead", .(team26 = team, lead = name, lead_p = p_bench, lead_x = x_base)][order(-lead_p)][, .SD[1], by = team26]
m <- merge(m, lead_risk, by = "team26", all.x = TRUE)
m[lead == name26, `:=`(lead = NA, lead_p = NA, lead_x = NA)]
out <- m[order(-espn_best), .(name, espn = espn_best, espn_season, open, catch, yac, team26, ros_rb, dyn_rb,
  carry_sh = round(carry_share26, 2), tgt_g = round(targets_g26, 1), rec_xfp_g = round(rec_xfp_g26, 1), xfp_g = round(xfp_g26, 1),
  fp_g = round(fp_g26, 1), behind = lead, lead_bench = round(lead_p, 3))]
print(as.data.frame(out))
fwrite(out, file.path(S, "rb_receivers_vs_price_2026.csv"))
