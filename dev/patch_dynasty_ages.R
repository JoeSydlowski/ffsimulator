# Patch fp_dynasty_history.rds ages to decimal as-of-Sept-1 ages, completing
# birthdates from three sources, without re-scraping FantasyPros:
#
#   1. dp_playerids birthdate (fantasypros_id join) - covers ~97% of rows, but
#      NOT the current rookie/devy class (dp lags new classes; audited
#      2026-07-11: 92% of missing rows were season-2026 rookies)
#   2. Sleeper players API birth_date (name+pos match) - carries rookie
#      classes early
#   3. MFL players API birthdate (name+pos match, epoch seconds)
#   4. rows still without a birthdate keep their existing age (FP current-age
#      arithmetic; college/devy players are in no NFL database)
#
# The old birth-year arithmetic lost month/day: +-0.5y scatter around the true
# Sept-1 age, ~20% of the age-kernel bandwidth (h_age=3). Multi-year backtest
# gate (dev/validate_outputs/dynasty_feature_study.txt): decimal ages improved
# cover80 in 11/11 holdouts.
#
# Usage: Rscript dev/patch_dynasty_ages.R
# Re-run after any inst/update_rankings_data.R dynasty rebuild.

library(data.table)
devtools::load_all(here::here(), quiet = TRUE)
rlang::check_installed(c("httr", "jsonlite"), "to fetch platform player databases")

path <- here::here("inst", "pkgdata", "fp_dynasty_history.rds")
d <- readRDS(path)
stopifnot(all(c("season", "fantasypros_id", "player_name", "pos", "age") %in% names(d)))
key_name <- nflreadr::clean_player_names(d$player_name)

## ---- source 1: dp_playerids by fantasypros_id ----------------------------------
ids <- data.table::as.data.table(ffscrapr::dp_playerids())
ids <- unique(ids[!is.na(ids$fantasypros_id) & !is.na(ids$birthdate),
                  list(fantasypros_id = as.character(fantasypros_id), birthdate)],
              by = "fantasypros_id")
bd <- ids$birthdate[match(as.character(d$fantasypros_id), ids$fantasypros_id)]
cat("dp_playerids birthdates:", sum(!is.na(bd)), "of", nrow(d), "rows\n")

## ---- source 2: Sleeper players (name + pos) -------------------------------------
sleeper_bd <- tryCatch({
  resp <- httr::GET("https://api.sleeper.app/v1/players/nfl")
  httr::stop_for_status(resp)
  sl <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
  sl <- data.table::rbindlist(lapply(sl, function(p) data.table::data.table(
    name = if (is.null(p$full_name)) NA_character_ else p$full_name,
    pos = if (is.null(p$position)) NA_character_ else p$position,
    birth_date = if (is.null(p$birth_date)) NA_character_ else p$birth_date
  )), fill = TRUE)
  sl <- sl[!is.na(name) & !is.na(pos) & !is.na(birth_date)]
  sl[, key := paste(nflreadr::clean_player_names(name), pos)]
  # drop ambiguous name+pos duplicates rather than risk a wrong birthdate
  sl <- sl[, if (.N == 1 || data.table::uniqueN(birth_date) == 1) .SD[1], by = key]
  sl
}, error = function(e) { message("Sleeper unavailable: ", conditionMessage(e)); NULL })
if (!is.null(sleeper_bd)) {
  hit <- match(paste(key_name, d$pos), sleeper_bd$key)
  fill <- is.na(bd) & !is.na(hit)
  bd[fill] <- sleeper_bd$birth_date[hit[fill]]
  cat("filled from Sleeper:", sum(fill), "\n")
}

## ---- source 3: MFL players (name + pos) -----------------------------------------
mfl_bd <- tryCatch({
  yr <- max(d$season)
  resp <- httr::GET(sprintf(
    "https://api.myfantasyleague.com/%d/export?TYPE=players&DETAILS=1&JSON=1", yr))
  httr::stop_for_status(resp)
  mf <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
  mf <- data.table::as.data.table(mf$players$player)
  mf <- mf[!is.na(name) & !is.na(position) & !is.na(birthdate)]
  # MFL names are "Last, First"; birthdate is epoch seconds
  mf[, name_fl := sub("^(.*), (.*)$", "\\2 \\1", name)]
  mf[, key := paste(nflreadr::clean_player_names(name_fl), position)]
  mf[, birth_date := format(as.Date(as.POSIXct(as.numeric(birthdate),
                                               origin = "1970-01-01", tz = "UTC")))]
  mf <- mf[, if (.N == 1 || data.table::uniqueN(birth_date) == 1) .SD[1], by = key]
  mf
}, error = function(e) { message("MFL unavailable: ", conditionMessage(e)); NULL })
if (!is.null(mfl_bd)) {
  hit <- match(paste(key_name, d$pos), mfl_bd$key)
  fill <- is.na(bd) & !is.na(hit)
  bd[fill] <- mfl_bd$birth_date[hit[fill]]
  cat("filled from MFL:", sum(fill), "\n")
}

## ---- apply ------------------------------------------------------------------------
old_age <- d$age
dec <- suppressWarnings(
  as.numeric(as.Date(sprintf("%d-09-01", d$season)) - as.Date(bd)) / 365.25)
dec[!is.na(dec) & (dec < 15 | dec > 50)] <- NA  # refuse implausible matches
d$age <- ifelse(!is.na(dec), round(dec, 1), old_age)

cat("\nrows:", nrow(d),
    "| decimal:", sum(!is.na(dec)),
    "| age fallback:", sum(is.na(dec) & !is.na(old_age)),
    "| still NA:", sum(is.na(d$age)), "\n")
delta <- d$age - old_age
cat("age - old_age: mean", round(mean(delta, na.rm = TRUE), 3),
    "| range", paste(round(range(delta, na.rm = TRUE), 2), collapse = " .. "), "\n")
stopifnot(all(abs(delta) < 1.51, na.rm = TRUE))  # sanity: platform fills can move
                                                 # FP-arithmetic ages by a bit more
                                                 # than the birth-year rounding

saveRDS(d, path)
cat("patched", path, "\n")
