# adp_board.R -- Sleeper ADP boards, past and present
#
# Sleeper publishes its official ADP as a Google Sheet with one tab per scrape
# date. The tabs are the only source in this project for "who actually went
# when" at a point in time, which is what makes reach-vs-ADP measurable: pair a
# season's draft with the snapshot taken just before it.
#
# Everything here is read-only HTTP plus a local cache. Sourced by
# draft_strategy.R; keeper_intel.R's inline crosswalk is the same logic.

suppressPackageStartupMessages({
  library(data.table)
})

ADP_SHEET_ID <- "1wmjxi3K5rjIYME_lskUvquLbN331YV0vi-kg5VakpdY"
ADP_CACHE <- here::here("dev", "data", "sleeper_adp")

adp_url <- function(gid) {
  sprintf("https://docs.google.com/spreadsheets/d/%s/export?format=csv&gid=%s",
          ADP_SHEET_ID, gid)
}

#' Every tab in the sheet, as gid -> scrape date
#'
#' Discovered from the sheet's own HTML rather than hardcoded, so a tab added
#' later is picked up. Cached, because it costs one request per tab.
adp_tab_index <- function(refresh = FALSE) {
  dir.create(ADP_CACHE, recursive = TRUE, showWarnings = FALSE)
  idx_file <- file.path(ADP_CACHE, "_tab_index.csv")
  if (!refresh && file.exists(idx_file)) return(fread(idx_file, colClasses = "character"))

  html <- paste(readLines(
    sprintf("https://docs.google.com/spreadsheets/d/%s/htmlview", ADP_SHEET_ID),
    warn = FALSE), collapse = "\n")
  gids <- unique(sub("^gid=", "", regmatches(html, gregexpr("gid=[0-9]+", html))[[1]]))
  if (!length(gids)) stop("no gids found in the ADP sheet html")

  idx <- rbindlist(lapply(gids, function(g) {
    hdr <- tryCatch(fread(adp_url(g), nrows = 1L), error = function(e) NULL)
    if (is.null(hdr) || !nrow(hdr) || !"Date" %in% names(hdr)) {
      return(data.table(gid = g, date = NA_character_, cols = NA_character_))
    }
    data.table(gid = g, date = as.character(hdr$Date[[1]]),
               cols = paste(names(hdr), collapse = "|"))
  }))
  idx <- idx[!is.na(date)]
  idx[, scrape_date := as.character(as.Date(date, format = "%B %d, %Y"))]
  setorder(idx, scrape_date)
  fwrite(idx, idx_file)
  idx[]
}

#' The tab taken closest before `on_date` (the one a drafter would have seen)
adp_tab_before <- function(on_date, idx = adp_tab_index()) {
  cand <- idx[!is.na(scrape_date) & as.Date(scrape_date) <= as.Date(on_date)]
  if (!nrow(cand)) stop("no ADP snapshot on or before ", on_date)
  setorder(cand, scrape_date)
  cand[.N]
}

#' Raw rows for one tab, cached to dev/data/sleeper_adp/<date>_<gid>.csv
adp_fetch <- function(gid, scrape_date = NULL, refresh = FALSE) {
  dir.create(ADP_CACHE, recursive = TRUE, showWarnings = FALSE)
  gid_chr <- as.character(gid)
  if (is.null(scrape_date)) {
    # NB: `gid == gid_chr`, never `gid == as.character(gid)` - inside a
    # data.table both sides of the latter resolve to the COLUMN, so it matches
    # every row and silently returns the first tab
    i <- adp_tab_index()[gid == gid_chr]
    scrape_date <- if (nrow(i)) i$scrape_date[[1]] else "unknown"
  }
  f <- file.path(ADP_CACHE, sprintf("%s_%s.csv", scrape_date, gid))
  if (refresh || !file.exists(f)) {
    d <- fread(adp_url(gid_chr))
    fwrite(d, f)
  }
  d <- fread(f)
  d[, scrape_date := scrape_date]
  d[]
}

#' A draft board: one row per player, ordered by ADP, keyed to fantasypros_id
#'
#' Crosswalks Sleeper ids to fantasypros ids via dp_playerids(), falling back to
#' cleaned name + position for players the id map has not caught up on (the
#' perennial rookie lag). `latest_rankings` supplies the name lookup; pass NULL
#' to skip the fallback.
#'
#' @param pos_keep optional positions to keep BEFORE ranking - pass the league's
#'   own pos_filter so K/DEF do not consume board slots nobody can draft
#' @return fantasypros_id, sleeper_id, player, pos, adp (raw), adp_rank (1..N)
adp_board <- function(gid, adp_col = "Redraft SF ADP", latest_rankings = NULL,
                      scrape_date = NULL, pos_keep = NULL, quiet = FALSE) {
  s <- adp_fetch(gid, scrape_date)
  if (!adp_col %in% names(s)) {
    stop("tab ", gid, " has no column '", adp_col, "' - has: ",
         paste(names(s), collapse = ", "))
  }
  setnames(s, adp_col, "adp")
  s[, sleeper_id := as.character(`Player Id`)]
  s[, nm := nflreadr::clean_player_names(paste(`Player First Name`, `Player Last Name`))]
  s[, pos := `Fantasy Player Position`]
  s[, player := paste(`Player First Name`, `Player Last Name`)]
  s <- s[!is.na(adp)]
  # Sleeper ranks K and DEF on the same board. A league with no K/DEF slot can
  # never draft them, so leaving them in inflates every skill players board rank
  # and makes the league look like it reaches. Rank AFTER filtering.
  if (!is.null(pos_keep)) s <- s[pos %in% pos_keep]

  xw <- as.data.table(ffscrapr::dp_playerids())[
    , list(sleeper_id = as.character(sleeper_id), fantasypros_id = as.character(fantasypros_id))]
  s <- merge(s, xw[!is.na(sleeper_id)], by = "sleeper_id", all.x = TRUE)

  if (!is.null(latest_rankings)) {
    nm_lookup <- unique(as.data.table(latest_rankings)[
      , list(fantasypros_id = as.character(fantasypros_id),
             nm = nflreadr::clean_player_names(player), pos)])
    nm_lookup <- nm_lookup[!duplicated(paste(nm, pos))]
    s <- merge(s, nm_lookup[, list(nm, pos, fp_by_name = fantasypros_id)],
               by = c("nm", "pos"), all.x = TRUE)
    s[is.na(fantasypros_id), fantasypros_id := fp_by_name]
  }

  matched <- sum(!is.na(s$fantasypros_id))
  if (!quiet) {
    cli::cli_alert_info("ADP {s$scrape_date[[1]]}: {matched}/{nrow(s)} rows crosswalked")
  }

  setorder(s, adp)
  # adp_rank is the board position, assigned over EVERY player with an ADP -
  # including ones with no fantasypros_id, so a rank still means "the Nth player
  # off the board" even where the crosswalk failed
  s[, adp_rank := seq_len(.N)]
  out <- s[, list(fantasypros_id, sleeper_id, player, pos, adp, adp_rank,
                  scrape_date)]
  out[!duplicated(paste(out$sleeper_id))][]
}
