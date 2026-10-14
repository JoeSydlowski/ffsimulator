# Player-game panel for testing the ffopportunity expected-points models out of
# sample. The models were frozen in Jan 2022 (v1.0.0-model is still `latest`), so
# 2022+ is untouched by training; 2021 is kept and flagged in-sample.
#
# THE DENOMINATOR FIX. load_ff_opportunity() only has rows for players who were
# involved in a play, so a game where a player was on the field and never got the
# ball is missing - averaging over its rows divides by too few games and inflates
# per-game rates, most for boom-or-bust players. The game universe here comes from
# load_snap_counts() (offense_snaps > 0, regular season), bridged pfr -> gsis, and
# a snap game with no opportunity row is a real zero.
#   Rscript dev/ep/ep_panel.R      -> dev/ep/ep_panel.rds
suppressPackageStartupMessages(library(data.table))
SEASONS <- eval(parse(text = Sys.getenv("FFS_EP_SEASONS", "2021:2025")))
OUT_FILE <- Sys.getenv("FFS_EP_OUT", "ep_panel.rds")
out <- here::here("dev", "ep")
dir.create(out, showWarnings = FALSE)

opp <- as.data.table(nflreadr::load_ff_opportunity(seasons = SEASONS, stat_type = "weekly", model_version = "latest"))
opp <- opp[position %in% c("QB", "RB", "WR", "TE"),
  .(season = as.integer(season), week = as.integer(week), game_id, gsis_id = player_id, name = full_name, position, team = posteam,
    fp = total_fantasy_points, xfp = total_fantasy_points_exp,
    targets = rec_attempt, carries = rush_attempt, pass_att = pass_attempt,
    rec_xfp = rec_fantasy_points_exp, rush_xfp = rush_fantasy_points_exp, pass_xfp = pass_fantasy_points_exp,
    rec_fp = rec_fantasy_points, rush_fp = rush_fantasy_points, pass_fp = pass_fantasy_points,
    receptions, rec_yards = rec_yards_gained, rec_td = rec_touchdown, rush_yards = rush_yards_gained, rush_td = rush_touchdown,
    # over-expected components (actual - expected), natural units
    catch_oe = receptions_diff, rec_yds_oe = rec_yards_gained_diff, rec_td_oe = rec_touchdown_diff,
    rec_fd_oe = rec_first_down_diff, rush_yds_oe = rush_yards_gained_diff, rush_td_oe = rush_touchdown_diff,
    rush_fd_oe = rush_first_down_diff, pass_yds_oe = pass_yards_gained_diff, pass_td_oe = pass_touchdown_diff,
    pass_int_oe = pass_interception_diff,
    team_xfp = total_fantasy_points_exp_team)]
opp <- unique(opp, by = c("game_id", "gsis_id"))

snaps <- as.data.table(nflreadr::load_snap_counts(seasons = SEASONS))[
  # PFR labels some backs "HB" from 2025 on (Chase Brown 2026) - drop it and the
  # whole snap history for that player silently disappears
  game_type == "REG" & offense_snaps > 0 & position %in% c("QB", "RB", "HB", "WR", "TE", "FB"),
  .(season = as.integer(season), week = as.integer(week), game_id, pfr_id = pfr_player_id, snap_name = player, snap_pos = position, snap_team = team,
    snaps = offense_snaps, snap_pct = offense_pct)]
ids <- unique(as.data.table(nflreadr::load_ff_playerids())[!is.na(pfr_id) & !is.na(gsis_id),
  .(pfr_id, gsis_id, fantasypros_id = as.character(fantasypros_id))], by = "pfr_id")
snaps <- merge(snaps, ids, by = "pfr_id", all.x = TRUE)
cat(sprintf("snap rows: %d, bridged to gsis: %.1f%%\n", nrow(snaps), 100 * mean(!is.na(snaps$gsis_id))))
reg_games <- unique(snaps$game_id)
opp <- opp[game_id %chin% reg_games]

# universe = every snap game (bridged) plus any opportunity row the bridge missed
p <- merge(snaps[!is.na(gsis_id)], opp, by = c("game_id", "gsis_id", "season", "week"), all = TRUE)
p[, `:=`(no_touch = is.na(fp), no_snaps = is.na(snaps))]
zero_cols <- c("fp", "xfp", "targets", "carries", "pass_att", "rec_xfp", "rush_xfp", "pass_xfp", "rec_fp", "rush_fp", "pass_fp",
               "receptions", "rec_yards", "rec_td", "rush_yards", "rush_td", "catch_oe", "rec_yds_oe", "rec_td_oe", "rec_fd_oe",
               "rush_yds_oe", "rush_td_oe", "rush_fd_oe", "pass_yds_oe", "pass_td_oe", "pass_int_oe")
for (v in zero_cols) set(p, which(is.na(p[[v]])), v, 0)
p[is.na(position), position := fifelse(snap_pos %in% c("FB", "HB"), "RB", snap_pos)]
p[is.na(name), name := snap_name]; p[is.na(team), team := snap_team]
p <- p[position %in% c("QB", "RB", "WR", "TE")]
# team expected points per game, for opportunity share (fill from teammates when the
# player's own row is missing)
tx <- opp[, .(team_xfp_g = max(team_xfp, na.rm = TRUE)), by = .(game_id, team)]
p <- merge(p[, -"team_xfp"], tx, by = c("game_id", "team"), all.x = TRUE)
# team carries + targets per game (all skill players on the panel), for touch share
tt <- p[, .(team_carries = sum(carries), team_targets = sum(targets)), by = .(game_id, team)]
p <- merge(p, tt, by = c("game_id", "team"), all.x = TRUE)

ps <- as.data.table(nflreadr::load_player_stats(seasons = SEASONS))[season_type == "REG",
  .(gsis_id = player_id, season = as.integer(season), week = as.integer(week), target_share, air_yards_share, wopr)]
p <- merge(p, unique(ps, by = c("gsis_id", "season", "week")), by = c("gsis_id", "season", "week"), all.x = TRUE)
for (v in c("target_share", "air_yards_share", "wopr")) set(p, which(is.na(p[[v]]) & !p$no_snaps), v, 0)
p[, `:=`(fpoe = fp - xfp, opp_share = xfp / team_xfp_g, in_sample = season <= 2021)]

# preseason redraft positional rank (FantasyPros), for "top X by rank" subsets
devtools::load_all(here::here(), quiet = TRUE)
rk <- as.data.table(fp_rankings_history())[page_pos == pos & season %in% SEASONS,
  .(season, fantasypros_id = as.character(fantasypros_id), pre_rank = rank)]
fpid <- unique(as.data.table(nflreadr::load_ff_playerids())[!is.na(gsis_id) & !is.na(fantasypros_id),
  .(gsis_id, fp_id = as.character(fantasypros_id))], by = "gsis_id")
p <- merge(p, fpid, by = "gsis_id", all.x = TRUE)
p <- merge(p, unique(rk, by = c("season", "fantasypros_id")), by.x = c("season", "fp_id"), by.y = c("season", "fantasypros_id"), all.x = TRUE)

# ---- how much does the fix matter? -----------------------------------------
cat(sprintf("player-games: %d   snap games with no opportunity row (real zeros): %d (%.1f%%)   opportunity rows with no snap row: %d\n",
            nrow(p), sum(p$no_touch), 100 * mean(p$no_touch), sum(p$no_snaps)))
cat("\nshare of a position's player-games that are snap-but-no-touch:\n")
print(p[, .(games = .N, no_touch = round(mean(no_touch), 3)), by = position][order(position)])
ss <- p[season >= 2022, .(g_true = .N, g_opp = sum(!no_touch), fp = sum(fp), xfp = sum(xfp)), by = .(season, gsis_id, name, position)]
ss[, `:=`(fpg_true = fp / g_true, fpg_opp = fp / pmax(g_opp, 1), inflate = g_true / pmax(g_opp, 1) - 1)]
cat("\nper-game inflation from the old denominator, players with >= 8 snap games, 2022-25:\n")
print(ss[g_true >= 8, .(n = .N, median = round(median(inflate), 3), p90 = round(quantile(inflate, .9), 3),
                        share_over_10pct = round(mean(inflate > .1), 3)), by = position][order(position)])
cat("\nbiggest inflation among fantasy-relevant players (>= 8 games, >= 6 fp/g):\n")
print(ss[g_true >= 8 & fpg_true >= 6][order(-inflate)][1:12, .(season, name, position, g_true, g_opp,
      fpg_true = round(fpg_true, 1), fpg_opp = round(fpg_opp, 1))])
saveRDS(p, file.path(out, OUT_FILE))
cat("\nsaved dev/ep/", OUT_FILE, "\n", sep = "")
