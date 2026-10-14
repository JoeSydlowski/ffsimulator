# Preseason dynasty forecast from the last pre-Week-1 FantasyPros boards (Sep 4,
# 2026): dynasty-overall for where each player sits, the PPR redraft positional
# pages for the redraft gap. Season quality is drawn Q ~ U(0,1) - the efficient-
# redraft assumption the 11-holdout backtest tests - so no season simulation is
# needed. Same transition model the backtest grades.
#   FFS_TALK_GAP=0.5 Rscript dev/presentation/forecast_week1.R   (gap bandwidth, "" = off)
suppressPackageStartupMessages(library(data.table))
devtools::load_all(here::here(), quiet = TRUE)
set.seed(2026)
S <- here::here("dev", "presentation")
K <- 2000
h_gap <- Sys.getenv("FFS_TALK_GAP", "")
if (nzchar(h_gap)) options(ffsimulator.dyn_h_gap = as.numeric(h_gap))
AS_OF <- as.Date(Sys.getenv("FFS_TALK_ASOF", "2026-09-04"))

pan <- as.data.table(arrow::read_parquet(here::here("dev", "data", "ecr_panel.parquet"), mmap = FALSE))
dyn <- pan[page_type == "dynasty-overall" & scrape_date == AS_OF & pos %in% c("QB", "RB", "WR", "TE")]
dyn[, `:=`(rank = frank(ecr, ties.method = "first"))]
dyn[, pos_rank := frank(ecr, ties.method = "first"), by = pos]
red <- pan[scrape_date == AS_OF & page_type %in% c("redraft-qb", "redraft-rb", "redraft-wr", "redraft-te")]
red[, redraft_rank := frank(ecr, ties.method = "first"), by = page_type]
dyn <- merge(dyn, red[, .(fantasypros_id, redraft_rank)], by = "fantasypros_id", all.x = TRUE)
dyn[, redraft_gap := fifelse(!is.na(redraft_rank), log(pos_rank / redraft_rank), NA_real_)]
ids <- as.data.table(ffscrapr::dp_playerids())[!is.na(fantasypros_id) & !is.na(birthdate),
  .(fantasypros_id = as.character(fantasypros_id), birthdate = as.Date(birthdate))]
dyn <- merge(dyn, unique(ids, by = "fantasypros_id"), by = "fantasypros_id", all.x = TRUE)
dyn[, age := as.numeric(as.Date("2026-09-01") - birthdate) / 365.25]
cat("dynasty board", format(AS_OF), ":", nrow(dyn), "players;", sum(!is.na(dyn$redraft_rank)), "with a redraft rank;",
    sum(is.na(dyn$age)), "missing age\n")

scoring <- readRDS("C:/Github/ffsimulator/dev/validate_outputs/scoring_history_2012_2025.rds")
pools <- .ffs_dynasty_transition_pools(scoring, format = "1qb")
idx <- rep(seq_len(nrow(dyn)), each = K)
q <- ifelse(is.na(dyn$redraft_rank[idx]), NA_real_, runif(length(idx)))
dr <- .ffs_draw_transition(pos = dyn$pos[idx], age = dyn$age[idx], dyn_pos_rank = dyn$pos_rank[idx],
                           q = q, transitions = pools$transitions, redraft_gap = dyn$redraft_gap[idx])
draws <- data.table(row = idx, q = q, next_pos_rank = dr[[1]], exited = dr[[2]])
draws[exited == FALSE, next_rank := .ffs_pos_to_overall(dyn$pos[row], next_pos_rank, reference = dyn)]
vc <- function(r) 10000 * exp(-0.023 * r)
draws[, next_value := fifelse(exited, 0, vc(next_rank))]
dyn[, cur_value := vc(rank)]
sm <- draws[, .(next_value_med = median(next_value), next_value_mean = mean(next_value),
                p10 = quantile(next_value, .1), p90 = quantile(next_value, .9),
                p_rise = mean(next_value > dyn$cur_value[row[1]]), p_exit = mean(exited)), by = row]
out <- cbind(dyn[sm$row], sm[, -"row"])
out[, `:=`(win_later = next_value_med, win_now = cur_value - next_value_med)]
tag <- if (nzchar(h_gap)) paste0("gap", sub("\\.", "", h_gap)) else "base"
saveRDS(list(forecast = out, draws = draws[, .(fantasypros_id = dyn$fantasypros_id[row], q, next_pos_rank, exited, next_value)],
             as_of = AS_OF, h_gap = h_gap), file.path(S, paste0("forecast_week1_", tag, ".rds")))
cat("saved forecast_week1_", tag, ".rds\n", sep = "")
print(out[order(rank)][1:25, .(player, pos, age = round(age, 1), rank, pos_rank, redraft_rank, cur = round(cur_value),
                                later = round(win_later), p_rise = round(p_rise, 2), p_exit = round(p_exit, 2))])
