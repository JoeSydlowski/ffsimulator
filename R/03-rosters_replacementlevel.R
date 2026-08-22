#' Add replacement level players to each roster
#'
#' Adds the same N free-agent starters to each roster to represent being able to
#' churn the waiver wire for starters, where N is the maximum number of players
#' that could start in a given position
#'
#' @param rosters a dataframe of rosters as created by `ffs_rosters()`
#' @param franchises a dataframe of franchises as created by `ffs_franchises()`
#' @param latest_rankings a dataframe of latest rankings as created by `ff_latest_rankings()`
#' @param lineup_constraints a dataframe of lineup constraints as created by `ffs_starter_positions`
#' @param pos_filter a character vector of positions to filter to, defaults to c("QB","RB","WR","TE","K")
#'
#' @export
#' @return a dataframe of rosters with replacements
ffs_add_replacement_level <- function(rosters,
                                      latest_rankings,
                                      franchises,
                                      lineup_constraints,
                                      pos_filter = c("QB", "RB", "WR", "TE")
                                      ) {

  assert_df(franchises, c("franchise_id", "franchise_name", "league_id"))
  assert_df(rosters, c("pos", "fantasypros_id", "franchise_id", "franchise_name"))
  assert_df(latest_rankings, c("fantasypros_id"))
  assert_df(lineup_constraints, c("pos", "min", "max"))

  pos <- NULL
  franchise_id <- NULL
  pos_rank <- NULL
  max <- NULL
  ecr <- NULL
  player <- NULL
  fantasypros_id <- NULL

  r <- data.table::as.data.table(rosters)
  f <- data.table::as.data.table(franchises)[
    , c("franchise_id", "franchise_name", "league_id")
  ][, `:=`(joinkey = 1)]

  lr <- data.table::as.data.table(latest_rankings)[pos %in% pos_filter]
  lc <- data.table::as.data.table(lineup_constraints)[
    pos %in% pos_filter,
    c("pos", "min", "max")]

  fa <- r[
    , c("fantasypros_id", "franchise_id")
  ][lr, on = c("fantasypros_id")
  ][is.na(franchise_id)
  ][order(pos, ecr)
  ][, pos_rank := seq_len(.N), by = c("pos")
  ][lc, on = "pos"
  ][pos_rank <= max
  ][, list(
    player_id = paste(pos, pos_rank, sep = "_"),
    player_name = paste("Replacement", pos, "-", pos_rank, player),
    pos,
    team = NA,
    age = NA,
    fantasypros_id,
    joinkey = 1
  )]

  fa <- merge(f, fa, by = "joinkey", allow.cartesian = TRUE)[, -"joinkey"]

  out <- data.table::rbindlist(list(r, fa), use.names = TRUE, fill = TRUE)

  return(out)
}

#' Top up franchises that cannot field a required starter
#'
#' [ffs_add_replacement_level()] hands the same free agents to *every* roster, which
#' models an open waiver wire. This is the narrower case: a league that requires a
#' starting slot (typically K) where some managers are simply carrying nobody there.
#' The lineup optimiser has no notion of streaming - it leaves the slot **empty every
#' week** - so an unfilled franchise is modelled as punting a starter all season.
#' That is not a small effect: on a 10-team league with two kicker-less managers it
#' moved their playoff odds by over 40 points each and inflated everyone else's.
#'
#' Only franchises below `min` at a position are topped up, and only to `min`. The
#' fillers are the best-ranked *unrostered* players at that position, handed out in
#' ECR order so two needy franchises do not both receive the same player.
#'
#' @param rosters a dataframe of rosters as created by `ffs_rosters()`
#' @param latest_rankings a dataframe of rankings as created by `ffs_latest_rankings()`
#' @param franchises a dataframe of franchises as created by `ffs_franchises()`
#' @param lineup_constraints a dataframe as created by `ffs_starter_positions()`
#' @param pos_filter a character vector of positions to consider
#'
#' @return the rosters dataframe with fillers appended, carrying `replacement = TRUE`
#' @export
ffs_fill_missing_starters <- function(rosters,
                                      latest_rankings,
                                      franchises,
                                      lineup_constraints,
                                      pos_filter = c("QB", "RB", "WR", "TE", "K")) {

  assert_df(rosters, c("pos", "fantasypros_id", "franchise_id", "franchise_name"))
  assert_df(latest_rankings, c("fantasypros_id", "pos", "ecr"))
  assert_df(franchises, c("franchise_id", "franchise_name", "league_id"))
  assert_df(lineup_constraints, c("pos", "min"))

  pos <- franchise_id <- ecr <- fantasypros_id <- player <- n_have <- NULL
  need <- min_req <- rn <- NULL

  r  <- data.table::as.data.table(rosters)
  f  <- unique(data.table::as.data.table(franchises)[
    , c("franchise_id", "franchise_name", "league_id")], by = "franchise_id")
  lc <- data.table::as.data.table(lineup_constraints)[
    pos %in% pos_filter & !is.na(get("min")) & get("min") >= 1]
  if (!nrow(lc)) return(rosters)

  fills <- list()
  for (p in lc$pos) {
    min_req <- lc[pos == p][["min"]][[1]]
    have <- r[pos == p, list(n_have = .N), by = "franchise_id"]
    short <- merge(f[, c("franchise_id", "franchise_name", "league_id")], have,
                   by = "franchise_id", all.x = TRUE)
    short[is.na(n_have), n_have := 0L]
    short <- short[n_have < min_req][, need := min_req - n_have]
    if (!nrow(short)) next

    # best available: ranked at this position and on nobody's roster
    fa <- data.table::as.data.table(latest_rankings)[
      pos == p & !fantasypros_id %in% r$fantasypros_id][order(ecr)]
    if (!nrow(fa)) {
      cli::cli_alert_warning(
        "no unrostered {p} available to fill {nrow(short)} franchise{?s} - slot stays empty")
      next
    }

    # one filler per needed slot, distinct players handed out in ECR order
    slots <- short[rep(seq_len(.N), short$need)][, rn := seq_len(.N)]
    if (nrow(slots) > nrow(fa)) {
      cli::cli_alert_warning(
        "only {nrow(fa)} unrostered {p} for {nrow(slots)} empty slot{?s} - reusing the pool")
    }
    slots[, rn := ((rn - 1L) %% nrow(fa)) + 1L]
    picked <- fa[slots$rn]

    fills[[p]] <- data.table::data.table(
      league_id      = slots$league_id,
      franchise_id   = slots$franchise_id,
      franchise_name = slots$franchise_name,
      player_id      = paste0("FILL_", p, "_", seq_len(nrow(slots))),
      player_name    = paste0("Baseline ", p, " - ", picked$player),
      pos            = p,
      team           = NA_character_,
      age            = NA_real_,
      fantasypros_id = picked$fantasypros_id,
      replacement    = TRUE
    )
    cli::cli_alert_info(
      "filled {nrow(slots)} empty {p} slot{?s} across {length(unique(slots$franchise_id))} franchise{?s}")
  }

  if (!length(fills)) return(rosters)
  data.table::rbindlist(c(list(r), fills), use.names = TRUE, fill = TRUE)
}
