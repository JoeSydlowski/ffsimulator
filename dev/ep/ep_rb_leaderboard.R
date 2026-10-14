# 2026 RB benching leaderboard.
#   Rscript dev/ep/ep_rb_leaderboard.R   (needs ep_rb_bench3.rds and ep_panel_2026.rds)
# Model: fit on 2013-25 RB windows with injury-driven role loss removed (back himself
# on the report in week t..t+4, or a teammate RB Out/Doubtful in the trailing weeks).
# P(bench) = chance his carry share over the next 4 games falls below 60% of the last 4,
# for a HEALTHY back with no teammate returning. Inputs: rushing and receiving yards
# over expected (per carry / per target), the teammate RB's rushing efficiency, and
# his current role (carry share, snap share, expected points).
suppressPackageStartupMessages(library(data.table))
S <- here::here("dev", "ep")
K <- as.integer(Sys.getenv("FFS_EP_K", "4"))
h <- readRDS(file.path(S, if (K == 4) "ep_rb_bench3.rds" else sprintf("ep_rb_bench3_k%d.rds", K)))$d[own_hurt == FALSE & mate_out == FALSE]
st <- h[, .(m_r = mean(ryoe_att), s_r = sd(ryoe_att), m_c = mean(recoe_tgt, na.rm = TRUE), s_c = sd(recoe_tgt, na.rm = TRUE),
            m_t = mean(tm_ryoe, na.rm = TRUE), s_t = sd(tm_ryoe, na.rm = TRUE))]
prep <- function(x) {
  x[, `:=`(z_rush = (ryoe_att - st$m_r) / st$s_r,
           z_rec = fifelse(is.na(recoe_tgt), 0, (recoe_tgt - st$m_c) / st$s_c), few_tgt = is.na(recoe_tgt),
           z_tm = fifelse(is.na(tm_ryoe), 0, (tm_ryoe - st$m_t) / st$s_t), no_mate = is.na(tm_ryoe))]
}
prep(h)
fit <- glm(benched ~ z_rush + z_rec + few_tgt + z_tm + no_mate + carry_share_tr + snap_tr + xfp_tr, binomial, h)
cat("fit on", nrow(h), "healthy windows (2013-25), base benched rate", round(mean(h$benched), 3), "\n")
print(round(cbind(OR = exp(coef(fit)), confint.default(fit) |> exp()), 2)[c("z_rush", "z_rec", "z_tm", "carry_share_tr", "snap_tr"), ])

# ---- 2026 trailing window --------------------------------------------------------
p <- readRDS(file.path(S, "ep_panel_2026.rds")); p[is.na(snap_pct), snap_pct := 0]
wk <- max(p$week)
w <- p[week > wk - K & week <= wk]
rb <- w[position == "RB", .(name = name[1], g_tr = .N, carries = sum(carries), targets = sum(targets),
  ryoe = sum(rush_yds_oe), recoe = sum(rec_yds_oe), team_carries = sum(team_carries),
  snap_tr = mean(snap_pct), xfp_tr = mean(xfp), fp_tr = mean(fp), team = names(which.max(table(team)))), by = gsis_id]
room <- rb[, .(room_ryoe = sum(ryoe), room_car = sum(carries)), by = team]
rb <- merge(rb, room, by = "team")
rb[, `:=`(tm_car = room_car - carries, tm_ryoe_yds = room_ryoe - ryoe)]
# the most-used OTHER back on his team (his starter if he is the backup)
top2 <- rb[order(-carries), .(gsis_id, name, carries), by = team][, .SD[1:2], by = team]
rb[, mate := {
  cand <- top2[team == .BY$team & gsis_id != .BY$gsis_id]
  if (nrow(cand)) cand$name[1] else NA_character_
}, by = .(team, gsis_id)]
rb <- rb[g_tr >= K - 1 & carries >= 20]
rb[, `:=`(ryoe_att = ryoe / carries, recoe_tgt = fifelse(targets >= 5, recoe / targets, NA_real_),
          carry_share_tr = carries / pmax(team_carries, 1), tm_ryoe = fifelse(tm_car >= 10, tm_ryoe_yds / tm_car, NA_real_))]
prep(rb)
rb[, p_bench := predict(fit, rb, type = "response")]
# latest injury report, so hurt backs are flagged rather than scored as healthy
inj <- tryCatch(as.data.table(nflreadr::load_injuries(seasons = 2026))[game_type == "REG"], error = function(e) NULL)
if (!is.null(inj) && nrow(inj)) {
  last <- inj[, .SD[which.max(week)], by = gsis_id][, .(gsis_id, inj_week = week, status = report_status, injury = report_primary_injury)]
  rb <- merge(rb, last, by = "gsis_id", all.x = TRUE)
} else rb[, `:=`(inj_week = NA_integer_, status = NA_character_, injury = NA_character_)]
rb[, flag := fifelse(!is.na(status) & status %in% c("Questionable", "Doubtful", "Out"), paste0(status, " wk", inj_week, " (", injury, ")"), "")]
cat(sprintf("\n==== 2026 RB benching leaderboard (weeks %d-%d, >= 20 carries, >= %d games; model trained on %d-week windows)\n",
            wk - K + 1, wk, K - 1, K))
cat("p_bench = P(carry share next 4 games < 60% of the last 4 | healthy, no teammate returning)\n")
out <- rb[order(-p_bench), .(rank = .I, name, team, carries, carry_share = round(carry_share_tr, 2), snap = round(snap_tr, 2),
  rush_yoe_att = round(ryoe_att, 2), rec_yoe_tgt = round(recoe_tgt, 2), mate, mate_yoe_att = round(tm_ryoe, 2),
  p_bench = round(p_bench, 3), injury = flag)]
out[, role := fifelse(carry_share >= 0.45, "lead", "committee/backup")]
base <- h[, .(base = mean(benched)), by = .(role = fifelse(carry_share_tr >= 0.45, "lead", "committee/backup"))]
out <- merge(out, base, by = "role")[, x_base := round(p_bench / base, 1)]
for (rl in c("lead", "committee/backup")) {
  cat(sprintf("\n-- %s backs (historical healthy base rate %.3f); x_base = multiple of that base\n", rl, base[role == rl]$base))
  print(out[role == rl][order(-p_bench), .(name, team, carries, carry_share, snap, rush_yoe_att, rec_yoe_tgt, mate, mate_yoe_att,
                                            p_bench, x_base, injury)], nrows = 40)
}
fwrite(out, file.path(S, sprintf("rb_bench_leaderboard_2026_wk%d_k%d.csv", wk, K)))
cat("wrote dev/ep/", sprintf("rb_bench_leaderboard_2026_wk%d_k%d.csv", wk, K), "\n", sep = "")
