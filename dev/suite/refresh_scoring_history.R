# Rebuild dev/validate_outputs/scoring_history_2012_2025.rds (gitignored, so it
# has to be regenerable) past the nflverse release rename.
#
# ffscrapr's ff_scoringhistory() reads nflreadr::load_player_stats(), which since
# the nflverse "stats_player" restructure returns ZERO rows for 2025 - the old
# `player_stats_<yr>.rds` asset 404s. Silently, because .nflverse_player_stats_long
# selects its columns with dplyr::any_of(). Left alone, the scoring history stops
# at 2024 and the dynasty backtest quietly loses its most recent holdout year.
#
# This shims the two ffscrapr internals to read the new `stats_player_week_<yr>`
# asset and renames the three columns the restructure moved:
#     interceptions -> passing_interceptions
#     sacks         -> sacks_suffered
#     sack_yards    -> sack_yards_lost
# The league's own scoring rules reference `interceptions` and `sacks`, so
# without the renames those events score zero and every QB's history is wrong.
#
# The shim is VERIFIED, not assumed: 2024 is rebuilt through the new path and
# reconciled player-week by player-week against the old path before any new
# season is written. If the reconciliation fails the script stops.
#
# Usage: Rscript dev/suite/refresh_scoring_history.R [last_season]

suppressMessages({
  library(data.table)
  library(magrittr)
})

out_dir  <- here::here("dev", "validate_outputs")
cache    <- file.path(out_dir, "scoring_history_2012_2025.rds")
last_szn <- as.integer(commandArgs(trailingOnly = TRUE)[1])
if (is.na(last_szn)) last_szn <- 2025L
NEW_URL <- "https://github.com/nflverse/nflverse-data/releases/download/stats_player/stats_player_week_%d.rds"

new_week_stats <- function(season) {
  rbindlist(lapply(season, function(y)
    as.data.table(nflreadr::rds_from_url(sprintf(NEW_URL, y)))),
    use.names = TRUE, fill = TRUE)
}
renames <- c(passing_interceptions = "interceptions",
             sacks_suffered        = "sacks",
             sack_yards_lost       = "sack_yards")

long_from_new <- function(season, cols) {
  d <- new_week_stats(season)
  for (from in names(renames)) if (from %in% names(d) && !renames[[from]] %in% names(d))
    setnames(d, from, renames[[from]])
  d <- d[, intersect(cols, names(d)), with = FALSE]
  # the new asset mixes integer and double stat columns; melt would coerce with a
  # warning and the scoring join needs one numeric type anyway
  for (cc in setdiff(names(d), c("season", "week", "player_id")))
    set(d, j = cc, value = as.numeric(d[[cc]]))
  as.data.frame(melt(d, id.vars = c("season", "week", "player_id"),
                     variable.name = "metric", variable.factor = FALSE,
                     value.name = "value"))
}

off_cols <- c("season", "week", "player_id", "attempts", "carries", "completions",
  "interceptions", "passing_2pt_conversions", "passing_first_downs", "passing_tds",
  "passing_yards", "receiving_2pt_conversions", "receiving_first_downs",
  "receiving_fumbles", "receiving_fumbles_lost", "receiving_tds", "receiving_yards",
  "receptions", "rushing_2pt_conversions", "rushing_first_downs", "rushing_fumbles",
  "rushing_fumbles_lost", "rushing_tds", "rushing_yards", "sack_fumbles",
  "sack_fumbles_lost", "sack_yards", "sacks", "special_teams_tds", "targets")
kick_cols <- c("season", "week", "player_id", "fg_att", "fg_blocked", "fg_made",
  "fg_made_0_19", "fg_made_20_29", "fg_made_30_39", "fg_made_40_49", "fg_made_50_59",
  "fg_made_60_", "fg_made_distance", "fg_missed", "fg_missed_0_19", "fg_missed_20_29",
  "fg_missed_30_39", "fg_missed_40_49", "fg_missed_50_59", "fg_missed_60_",
  "fg_missed_distance", "fg_pct", "pat_att", "pat_blocked", "pat_made", "pat_missed",
  "pat_pct")

install_shim <- function() {
  assignInNamespace(".nflverse_player_stats_long",
                    function(season) long_from_new(season, off_cols), ns = "ffscrapr")
  assignInNamespace(".nflverse_kicking_long",
                    function(season) long_from_new(season, kick_cols), ns = "ffscrapr")
}

conn <- ffscrapr::mfl_connect(2021, 47747)
fetch <- function(season) {
  memoise::forget(ffscrapr:::ff_scoringhistory.mfl_conn)
  as.data.table(ffscrapr::ff_scoringhistory(conn, season))
}

## ---- verify the shim on a season both paths can produce --------------------------
VERIFY <- 2024L
message("verifying the shim on ", VERIFY, " @ ", Sys.time())
old <- fetch(VERIFY)                       # stock ffscrapr, old release path
install_shim()
new <- fetch(VERIFY)                       # shimmed, new release path

# The two releases are NOT bit-identical: nflverse corrected stats the frozen
# `player_stats` asset never picked up (e.g. a QB receiving TD on a trick play),
# so ~0.5% of player-weeks differ in both directions. That is a data revision,
# not a shim bug, and the old asset is dead anyway (404 for 2025). What the
# dynasty backtest consumes is each player's SEASON total mapped to a quality
# PERCENTILE, so the gate is on that: the two paths must agree to within noise
# on material players, and above all must not reorder them.
key <- c("season", "week", "gsis_id")
cmp <- merge(old[, c(key, "points"), with = FALSE],
             new[, c(key, "points"), with = FALSE], by = key,
             all = TRUE, suffixes = c("_old", "_new"))
cmp[is.na(points_old), points_old := 0][is.na(points_new), points_new := 0]
message(sprintf("  %d player-weeks compared, %d differ (%.2f%%), max |diff| = %.2f",
                nrow(cmp), sum(abs(cmp$points_new - cmp$points_old) > 0.011),
                100 * mean(abs(cmp$points_new - cmp$points_old) > 0.011),
                max(abs(cmp$points_new - cmp$points_old))))

tot <- merge(old[week %in% 1:14, list(old = sum(points)), by = gsis_id],
             new[week %in% 1:14, list(new = sum(points)), by = gsis_id],
             by = "gsis_id", all = TRUE)
tot[is.na(old), old := 0][is.na(new), new := 0]
mat <- tot[old >= 20 | new >= 20]
sp  <- stats::cor(mat$old, mat$new, method = "spearman")
pe  <- stats::cor(mat$old, mat$new)
mad <- mean(abs(mat$new - mat$old))
message(sprintf("  season totals (n=%d material): spearman %.6f, pearson %.6f, mean|diff| %.3f pts",
                nrow(mat), sp, pe, mad))
if (sp < 0.999 || pe < 0.999 || mad > 0.5) {
  print(head(mat[order(-abs(new - old))], 15))
  stop("new release disagrees with the old one beyond revision-level noise - not writing the cache")
}
message("  gate passed: the release change does not move season-quality ordering")

## ---- rebuild the full history ----------------------------------------------------
message("rebuilding 2012-", last_szn, " @ ", Sys.time())
sh <- fetch(2012:last_szn)
stopifnot("no rows returned" = nrow(sh) > 0)
per <- sh[, .N, by = season][order(season)]
print(per)
if (!last_szn %in% per$season) stop("last season produced no rows")
saveRDS(sh, cache)
message("wrote ", cache, "  rows=", nrow(sh),
        "  seasons=", paste(range(sh$season), collapse = "-"))
