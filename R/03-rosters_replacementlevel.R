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

#' Score profiles for the best players nobody rosters
#'
#' The waiver wire is invisible to a finished simulation. Projections are
#' filtered to rostered players ([ffs_generate_projections()]) and
#' [ffs_score_rosters()] inner-joins, so no unrostered player has weekly scores -
#' which means a trade that sends more bodies than it receives is currently
#' priced as losing that roster spot outright. In a shallow-ish league that is
#' wrong: the spot gets refilled off waivers the same week, so a 2-for-1 is
#' really a 2-for-2.
#'
#' Rather than re-simulate, each free agent borrows the weekly score profile of
#' the *rostered* player nearest him in ECR. Projections are ECR-driven
#' ([ffs_generate_projections()] reads `ecr`/`sd` and nothing else about a
#' player), so an unrostered WR at ECR 71 scores essentially like the rostered WR
#' at ECR 70, and the clone is close to what simulating him would have produced.
#' The payoff is that this runs in seconds against an already-saved simulation,
#' so the n=2000 confirmation path keeps working.
#'
#' Cloning is only safe while the donor is not himself part of the deal being
#' evaluated - "I sent Blake Corum and picked up a player who scores exactly like
#' Blake Corum" would make the send free. `n_donors` alternates per slot are
#' emitted so [.ffs_waiver_pick()] can fall through to the next-nearest donor.
#'
#' @param base_simulation an `ff_simulation` from `ff_simulate(..., return = "all")`
#'   (`rosters` and `latest_rankings` are both required, so a slimmed simulation
#'   will not do)
#' @param n_per_pos how many free agents to carry per position
#' @param pos_filter positions to build proxies for
#' @param n_donors how many donor alternates to emit per free agent
#'
#' @return a data.table of `roster_scores`-shaped rows with `franchise_id = NA`
#'   and globally unique `player_id`s (`WAIVER_<pos>_<k>_<donor_rank>`), plus
#'   `waiver_slot`, `donor_id`, `donor_rank`, `ecr` and `ecr_gap` for auditing
#'
#' @export
ffs_waiver_proxies <- function(base_simulation,
                               n_per_pos = 3L,
                               pos_filter = c("QB", "RB", "WR", "TE"),
                               n_donors = 2L) {
  checkmate::assert_class(base_simulation, "ff_simulation")
  checkmate::assert_int(n_per_pos, lower = 0)
  checkmate::assert_int(n_donors, lower = 1)

  pos <- ecr <- fantasypros_id <- player_id <- player <- donor_id <-
    donor_rank <- ecr_gap <- ecr_donor <- ecr_fa <- waiver_slot <- rn <- NULL

  rosters <- base_simulation$rosters
  lr <- base_simulation$latest_rankings
  if (is.null(rosters) || is.null(lr))
    stop("ffs_waiver_proxies() needs a simulation built with return = \"all\" ",
         "(rosters and latest_rankings are both dropped otherwise)")
  if (n_per_pos < 1L) return(.ffs_empty_waiver_scores(base_simulation))

  r <- data.table::as.data.table(rosters)
  lr <- data.table::as.data.table(lr)[pos %in% pos_filter & !is.na(ecr)]
  rs <- data.table::as.data.table(base_simulation$roster_scores)

  # the free agents: ranked, unrostered, best first at each position
  fa <- lr[!fantasypros_id %in% r$fantasypros_id][order(pos, ecr)]
  fa <- fa[, utils::head(.SD, n_per_pos), by = "pos"]
  if (!nrow(fa)) return(.ffs_empty_waiver_scores(base_simulation))
  fa[, rn := seq_len(.N), by = "pos"]
  fa[, waiver_slot := paste(pos, rn, sep = "_")]

  # candidate donors: rostered, ranked, and actually present in roster_scores
  donors <- merge(r[, c("player_id", "fantasypros_id", "pos")],
                  lr[, list(fantasypros_id, ecr)],
                  by = "fantasypros_id")
  donors <- donors[player_id %in% unique(rs$player_id)]
  if (!nrow(donors)) return(.ffs_empty_waiver_scores(base_simulation))

  # nearest ECR at the same position, n_donors deep
  pairs <- merge(fa[, list(waiver_slot, pos, player, ecr_fa = ecr)],
                 donors[, list(pos, donor_id = player_id, ecr_donor = ecr)],
                 by = "pos", allow.cartesian = TRUE)
  pairs[, ecr_gap := abs(ecr_donor - ecr_fa)]
  data.table::setorderv(pairs, c("waiver_slot", "ecr_gap"))
  pairs <- pairs[, utils::head(.SD, n_donors), by = "waiver_slot"]
  pairs[, donor_rank := seq_len(.N), by = "waiver_slot"]

  out <- lapply(seq_len(nrow(pairs)), function(i) {
    p <- pairs[i]
    rows <- data.table::copy(rs[player_id == p$donor_id])
    if (!nrow(rows)) return(NULL)
    data.table::set(rows, j = "player_id",
                    value = paste0("WAIVER_", p$waiver_slot, "_", p$donor_rank))
    for (cc in intersect(c("franchise_id", "franchise_name"), names(rows)))
      data.table::set(rows, j = cc, value = NA_character_)
    if ("player_name" %in% names(rows))
      data.table::set(rows, j = "player_name", value = p$player)
    data.table::set(rows, j = "waiver_slot", value = p$waiver_slot)
    data.table::set(rows, j = "donor_id", value = p$donor_id)
    data.table::set(rows, j = "donor_rank", value = p$donor_rank)
    data.table::set(rows, j = "ecr_gap", value = p$ecr_gap)
    data.table::set(rows, j = "ecr", value = p$ecr_fa)
    rows
  })

  data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
}

#' An empty waiver-proxy table shaped like roster_scores
#' @keywords internal
.ffs_empty_waiver_scores <- function(base_simulation) {
  rs <- data.table::as.data.table(base_simulation$roster_scores)[0]
  rs[, `:=`(waiver_slot = character(), donor_id = character(),
            donor_rank = integer(), ecr_gap = numeric(), ecr = numeric())]
  rs[]
}

#' Rank waiver proxies by what they are worth to one roster
#'
#' [.ffs_waiver_index()] falls back to ranking free agents by raw mean weekly
#' points, which is position-blind and therefore wrong: points are not comparable
#' across positions, and what a roster actually gains from a signing depends on
#' the holes it has. Measured on Goofball, the best available WR by mean points
#' (7.03) was worth **-0.02 wins** to the roster while a tight end scoring 5.72
#' was worth **+0.05** - the points ordering picks the wrong player.
#'
#' This values each free agent exactly, with the same [ffs_player_value()] call
#' the trade board uses, and writes it back as `value_w` so the pick order and
#' the score agree. One valuation per slot, so it is cheap - but it is specific
#' to `franchise_id`, so re-run it if you switch teams.
#'
#' @param base_simulation an `ff_simulation` carrying `waiver_scores`
#' @param franchise_id the franchise doing the signing
#' @param engine an optional [ffs_trade_engine()], shared with the caller
#'
#' @return `base_simulation$waiver_scores` with a `value_w` column
#' @export
ffs_waiver_rank <- function(base_simulation, franchise_id, engine = NULL) {
  checkmate::assert_class(base_simulation, "ff_simulation")
  waiver_slot <- player_id <- value_w <- NULL

  w <- base_simulation$waiver_scores
  if (is.null(w) || !nrow(w)) return(w)
  w <- data.table::as.data.table(w)

  idx <- .ffs_waiver_index(w)
  prim <- idx[!duplicated(idx$waiver_slot)]
  base_summary <- .ffs_franchise_summary_cached(base_simulation, franchise_id, engine)
  v <- vapply(prim$player_id, function(p)
    ffs_player_value(base_simulation, p, franchise_id,
                     base_summary = base_summary, engine = engine)$h2h_wins,
    numeric(1))
  # the donor alternates within a slot are clones of near-identical-ECR players,
  # so they inherit the slot's value rather than each being re-valued
  w[, value_w := v[match(waiver_slot, prim$waiver_slot)]]
  w[]
}
