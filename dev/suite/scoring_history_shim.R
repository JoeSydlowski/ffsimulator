# Session shim: restore post-rename nflverse stats to ffscrapr::ff_scoringhistory().
#
# ffscrapr reads nflreadr::load_player_stats(), whose `player_stats_<yr>.rds`
# asset 404s since the nflverse "stats_player" restructure - so every season from
# 2025 on comes back with ZERO rows, silently (.nflverse_player_stats_long selects
# with dplyr::any_of()). Any ff_simulate() run in 2026 therefore builds its
# outcome distributions on 2012-2024 and quietly drops the most recent season.
#
# This is the shim half of dev/suite/refresh_scoring_history.R (which also
# verifies it against 2024 and writes the backtest cache) pulled out so a live
# run can install it before calling ff_simulate():
#
#   source(here::here("dev", "suite", "scoring_history_shim.R")); install_shim()
#
# ff_scoringhistory is memoised per connection class, so install_shim() forgets
# the sleeper/mfl memo caches to make sure a pre-shim result is not reused.
suppressMessages({
  library(data.table)
})

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

# The stock implementations, captured at source() time so verify_shim() can run
# the old path even after the shim is installed.
.orig <- list(
  player_stats = get(".nflverse_player_stats_long", envir = asNamespace("ffscrapr")),
  kicking      = get(".nflverse_kicking_long",      envir = asNamespace("ffscrapr"))
)

.memo_names <- c("ff_scoringhistory.sleeper_conn", "ff_scoringhistory.mfl_conn",
                 "ff_scoringhistory.espn_conn", "ff_scoringhistory.flea_conn")

# ff_scoringhistory is memoised per connection class; a cached pre-shim result
# would otherwise survive the swap and hide it.
forget_scoringhistory <- function() {
  for (m in .memo_names) {
    f <- try(get(m, envir = asNamespace("ffscrapr")), silent = TRUE)
    if (!inherits(f, "try-error") && memoise::is.memoised(f)) memoise::forget(f)
  }
  invisible(TRUE)
}

install_shim <- function(quiet = FALSE) {
  assignInNamespace(".nflverse_player_stats_long",
                    function(season) long_from_new(season, off_cols), ns = "ffscrapr")
  assignInNamespace(".nflverse_kicking_long",
                    function(season) long_from_new(season, kick_cols), ns = "ffscrapr")
  forget_scoringhistory()
  if (!quiet) message("scoring-history shim installed (nflverse stats_player path)")
  invisible(TRUE)
}

uninstall_shim <- function() {
  assignInNamespace(".nflverse_player_stats_long", .orig$player_stats, ns = "ffscrapr")
  assignInNamespace(".nflverse_kicking_long",      .orig$kicking,      ns = "ffscrapr")
  forget_scoringhistory()
  invisible(TRUE)
}

#' Assert a connection's scoring history actually covers `seasons`.
#'
#' The failure this guards is silent: the dead asset 404s and ff_scoringhistory
#' returns a short frame rather than erroring, so a simulation quietly loses its
#' most recent - and most predictive - season. Call it right after install_shim()
#' with the same `base_seasons` the simulation will use.
assert_seasons <- function(conn, seasons) {
  sh <- data.table::as.data.table(ffscrapr::ff_scoringhistory(conn, seasons))
  got <- if (nrow(sh)) sort(unique(sh$season)) else integer()
  missing <- setdiff(seasons, got)
  if (length(missing))
    stop("scoring history is empty for season(s): ", paste(missing, collapse = ", "),
         " - the nflverse asset for those years is not being read. ",
         "Check dev/suite/scoring_history_shim.R against the current release layout.",
         call. = FALSE)
  message("scoring history covers ", min(got), "-", max(got),
          " (", nrow(sh), " rows, ", length(got), " seasons)")
  invisible(sh)
}

#' Reconcile the shimmed path against the stock one on a season both can produce.
#'
#' The two releases are not bit-identical - nflverse corrected stats the frozen
#' asset never picked up - so the gate is on season-quality ORDERING of material
#' players, not exact equality. Returns the comparison invisibly; stops on fail.
verify_shim <- function(conn, season = 2024L,
                        min_spearman = 0.999, min_pearson = 0.999, max_mad = 0.5) {
  fetch <- function() {
    forget_scoringhistory()
    data.table::as.data.table(ffscrapr::ff_scoringhistory(conn, season))
  }
  uninstall_shim(); old <- fetch()
  install_shim(quiet = TRUE); new <- fetch()

  tot <- merge(old[week %in% 1:14, list(old = sum(points)), by = "gsis_id"],
               new[week %in% 1:14, list(new = sum(points)), by = "gsis_id"],
               by = "gsis_id", all = TRUE)
  tot[is.na(old), old := 0][is.na(new), new := 0]
  mat <- tot[old >= 20 | new >= 20]
  sp  <- stats::cor(mat$old, mat$new, method = "spearman")
  pe  <- stats::cor(mat$old, mat$new)
  mad <- mean(abs(mat$new - mat$old))
  message(sprintf("shim gate on %d: n=%d material, spearman %.6f, pearson %.6f, mean|diff| %.3f pts",
                  season, nrow(mat), sp, pe, mad))
  # non-zero rows are the real payload; the new asset adds IDP/fringe rows that
  # score 0 under offense-only rules, so raw row counts are expected to differ
  message(sprintf("  scoring rows: old %d, new %d (zero-point rows: %d -> %d)",
                  sum(old$points != 0), sum(new$points != 0),
                  sum(old$points == 0), sum(new$points == 0)))
  if (sp < min_spearman || pe < min_pearson || mad > max_mad) {
    print(utils::head(mat[order(-abs(mat$new - mat$old))], 15))
    stop("shimmed path disagrees with the stock one beyond revision-level noise", call. = FALSE)
  }
  message("  gate passed")
  invisible(list(old = old, new = new, material = mat,
                 spearman = sp, pearson = pe, mad = mad))
}
