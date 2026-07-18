# One-off: append 2026 preseason draft rankings to the bundled
# fp_rankings_history.rds (was stale at 2025). Replicates the season transform
# in inst/update_rankings_data.R::build_draft_rankings for 2026 only, so the
# 2012-2025 rows are preserved byte-for-byte. Run from the package root:
#   Rscript dev/append_2026_redraft.R
suppressMessages({library(magrittr)})
options(ffpros.cache = "filesystem")

pkg_file <- here::here("inst", "pkgdata", "fp_rankings_history.rds")
existing <- readRDS(pkg_file)
stopifnot(!2026 %in% existing$season)  # append, don't duplicate

pages <- c(
  "qb-cheatsheets", "ppr-rb-cheatsheets", "ppr-wr-cheatsheets",
  "ppr-te-cheatsheets", "k-cheatsheets", "dst-cheatsheets",
  "dl-cheatsheets", "lb-cheatsheets", "db-cheatsheets"
)

new_2026 <- tibble::tibble(pages = pages) %>%
  dplyr::mutate(
    rankings = purrr::map(pages, ~ ffpros::fp_rankings(page = .x, year = 2026))
  ) %>%
  tidyr::unnest(rankings) %>%
  dplyr::transmute(
    page_pos = stringr::str_remove_all(pages, "cheatsheets|^ppr|\\-") %>%
      toupper() %>% stringr::str_squish(),
    season = 2026L,
    fantasypros_id = as.character(fantasypros_id),
    player_name = nflreadr::clean_player_names(player_name),
    pos = dplyr::case_when(
      pos %in% c("CB", "S") ~ "DB",
      pos %in% c("OLB", "LB") ~ "LB",
      pos %in% c("DE", "DT", "NT") ~ "DL",
      TRUE ~ pos
    ),
    team, rank, ecr, sd
  ) %>%
  dplyr::filter(page_pos == pos)

stopifnot(nrow(new_2026) > 100)
combined <- dplyr::bind_rows(existing, new_2026)

# sanity: existing rows untouched, only 2026 added
stopifnot(identical(
  as.data.frame(existing),
  as.data.frame(combined[combined$season != 2026, ])
))

saveRDS(combined, pkg_file)
cli::cli_alert_success("Appended {nrow(new_2026)} rows for 2026 -> {pkg_file}")
cat("seasons now:", paste(sort(unique(combined$season)), collapse = ", "), "\n")
sh <- combined[combined$season == 2026 &
                 grepl("Shough", combined$player_name),
               c("player_name", "pos", "rank", "ecr")]
cat("Shough 2026:\n"); print(sh)
