# adp_draftsharks.R -- DraftSharks ADP as a live draft board
#
# The Sleeper ADP sheet (adp_board.R) is the only source with dated historical
# snapshots, so reach calibration has to come from there. But its newest tab can
# be weeks old, and DraftSharks publishes a board for the exact league shape
# (superflex / PPR / Sleeper / 12-team, set key "12::107::12") that is current.
#
# The API is POST-only and behind a session cookie plus CSRF token, both of
# which expire, so nothing here fetches on demand: refresh writes a dated CSV
# into dev/data/ and the pipeline reads the newest one. Credentials are passed
# in at call time and are never written to disk or committed.
#
#   Refresh (needs a fresh cookie + token from a logged-in browser session):
#     source("dev/suite/adp_draftsharks.R")
#     ds_refresh(cookie = "PHPSESSID=...", csrf = "...")
#
#   Use:
#     b <- ds_board_cached()

suppressPackageStartupMessages({
  library(data.table)
})

DS_CACHE <- here::here("dev", "data")
DS_SET <- "12::107::12"   # superflex, ppr, sleeper, 12 teams
DS_PAGE <- "https://www.draftsharks.com/adp/superflex/sleeper/12"
DS_API <- "https://www.draftsharks.com/api/v1/adp/sets"

# The embedded JSON arrives with unicode escapes intact (Ja'Marr Chase).
# Left undecoded, every apostrophe name silently fails the crosswalk.
ds_unescape <- function(x) {
  bs <- intToUtf8(92)
  x <- gsub(paste0(bs, bs), bs, x, fixed = TRUE)
  x <- gsub(paste0(bs, "u0027"), "'", x, fixed = TRUE)
  x <- gsub(paste0(bs, "u0026"), "&", x, fixed = TRUE)
  x <- gsub(paste0(bs, "/"), "/", x, fixed = TRUE)
  x
}

ds_fetch <- function(cookie, csrf, set = DS_SET) {
  ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Gecko/20100101 Firefox/155.0"

  hp <- curl::new_handle()
  curl::handle_setheaders(hp, "User-Agent" = ua, "Cookie" = cookie)
  html <- rawToChar(curl::curl_fetch_memory(DS_PAGE, handle = hp)$content)

  ha <- curl::new_handle()
  curl::handle_setheaders(ha, "User-Agent" = ua, "Accept" = "*/*",
                          "Content-Type" = "application/json",
                          "Referer" = DS_PAGE, "Origin" = "https://www.draftsharks.com",
                          "X-CSRF-Token" = csrf, "Cookie" = cookie)
  curl::handle_setopt(ha, post = TRUE,
                      postfields = sprintf('{"adpSets":["%s"]}', set))
  r <- curl::curl_fetch_memory(DS_API, handle = ha)
  if (r$status_code != 200L) stop("DraftSharks API returned ", r$status_code)
  list(html = html, json = rawToChar(r$content))
}

# The player dictionary is embedded in the page, keyed by the same ids the ADP
# payload uses; the payload itself carries no names.
ds_parse <- function(html, json, set = DS_SET, pos_keep = c("QB", "RB", "WR", "TE")) {
  pat <- '"([0-9]+)":[{]"fn":"([^"]*)","ln":"([^"]*)","tm":"([^"]*)","pos":"([^"]*)"'
  m <- regmatches(html, gregexpr(pat, html, perl = TRUE))[[1]]
  if (!length(m)) stop("no player dictionary found in the DraftSharks page")
  players <- rbindlist(lapply(m, function(s) {
    g <- regmatches(s, regexec(pat, s, perl = TRUE))[[1]]
    data.table(id = g[2], fn = g[3], ln = g[4], tm = g[5], pos = g[6])
  }))
  players <- unique(players, by = "id")

  adp <- as.data.table(jsonlite::fromJSON(json)$adpSets[[set]])
  if (is.null(adp) || !nrow(adp)) stop("ADP set ", set, " came back empty")
  adp[, id := as.character(id)]

  b <- merge(adp, players, by = "id", all.x = TRUE)
  miss <- sum(is.na(b$pos))
  if (miss) cli::cli_alert_warning("{miss} ADP row{?s} had no player entry")
  b[, player := ds_unescape(paste(fn, ln))]
  b <- b[!is.na(pos) & pos %in% pos_keep]
  setorder(b, pick)
  b[, adp_rank := seq_len(.N)]
  b[, list(adp_rank, adp = pick, player, pos, team = tm,
           ds_rank = dsRank, market_index = marketIndex, ds_id = id)]
}

ds_refresh <- function(cookie, csrf, set = DS_SET, on = Sys.Date()) {
  raw <- ds_fetch(cookie, csrf, set)
  b <- ds_parse(raw$html, raw$json, set)
  b[, scrape_date := as.character(on)]
  f <- file.path(DS_CACHE, sprintf("adp_draftsharks_%s.csv", on))
  dir.create(DS_CACHE, recursive = TRUE, showWarnings = FALSE)
  fwrite(b, f)
  cli::cli_alert_success("DraftSharks board: {nrow(b)} players -> {f}")
  invisible(b)
}

# Newest cached board, with the fantasypros_id crosswalk applied by name + pos
# (DraftSharks carries no sleeper_id, so there is no id-based path).
ds_board_cached <- function(latest_rankings = NULL, quiet = FALSE) {
  fs <- list.files(DS_CACHE, pattern = "^adp_draftsharks_.*[.]csv$", full.names = TRUE)
  if (!length(fs)) stop("no cached DraftSharks board - run ds_refresh(cookie, csrf)")
  f <- fs[order(basename(fs), decreasing = TRUE)][1]
  b <- fread(f)
  b[, nm := nflreadr::clean_player_names(player)]

  if (!is.null(latest_rankings)) {
    lk <- unique(as.data.table(latest_rankings)[
      , list(fantasypros_id = as.character(fantasypros_id),
             nm = nflreadr::clean_player_names(player), pos)])
    lk <- lk[!duplicated(paste(nm, pos))]
    b <- merge(b, lk, by = c("nm", "pos"), all.x = TRUE)

    # DraftSharks uses legal first names where FantasyPros uses the nickname
    # (Cameron/Cam Skattebo, Nathaniel/Tank Dell). Fall back to last name + pos,
    # accepting only unambiguous matches against ids not already claimed.
    if (anyNA(b$fantasypros_id)) {
      last_of <- function(s) tolower(sub("^.*[ ]", "", s))
      lk2 <- lk[!fantasypros_id %in% b$fantasypros_id]
      lk2[, ln2 := last_of(nm)]
      lk2 <- lk2[, if (.N == 1L) .SD, by = list(ln2, pos)]
      b[, ln2 := last_of(nm)]
      b <- merge(b, lk2[, list(ln2, pos, fp_by_last = fantasypros_id)],
                 by = c("ln2", "pos"), all.x = TRUE)
      n_fixed <- sum(is.na(b$fantasypros_id) & !is.na(b$fp_by_last))
      b[is.na(fantasypros_id), fantasypros_id := fp_by_last]
      b[, c("ln2", "fp_by_last") := NULL]
      if (n_fixed && !quiet) cli::cli_alert_info("last-name fallback matched {n_fixed} more")
    }
    setorder(b, adp_rank)
    if (!quiet) {
      cli::cli_alert_info(
        "DraftSharks {b$scrape_date[[1]]}: {sum(!is.na(b$fantasypros_id))}/{nrow(b)} crosswalked")
    }
  }
  b[]
}
