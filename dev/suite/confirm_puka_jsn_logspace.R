# Confirm the log-space transition's real-world effect on the motivating case:
# Puka Nacua (9493) and Jaxon Smith-Njigba (9488) win_now_value, on the actual
# league sim, rank (shipped) vs log (proposed).
suppressPackageStartupMessages({library(data.table); library(arrow)})
devtools::load_all(here::here(), quiet = TRUE)

sim <- readRDS("dev/league_sims/1359546500786434048/2026-07-21/simulation.rds")
fc  <- as.data.table(read_parquet("dev/data/fantasycalc_values.parquet"))
fc[, scraped_date := NULL]                       # match fc_dynasty_values shape
ids <- c("9493" = "Puka Nacua", "9488" = "Jaxon Smith-Njigba")

run <- function(space) {
  if (space == "rank") {
    options(ffsimulator.dyn_move_space = "rank")  # shipped rank constants (defaults)
  } else {
    options(ffsimulator.dyn_move_space = "log",
            ffsimulator.dyn_move_slope = c(QB=.639, RB=.852, TE=.734, WR=.860),
            ffsimulator.dyn_move_bias  = c(QB=.002, RB=-.027, TE=.055, WR=.016),
            ffsimulator.dyn_disp_factor= c(QB=1.20, TE=1.13, WR=1.06, RB=1.03))
  }
  set.seed(42)
  d <- as.data.table(ffs_dynasty_outlook(sim, dynasty_values = fc))
  d[as.character(player_id) %in% names(ids),
    .(space, player_name, pos, age, cur_value = round(cur_value),
      next_mean = round(next_value_mean), next_med = round(next_value_med),
      win_now_value = round(cur_value - next_value_mean),
      mean_med_gap = round(next_value_med - next_value_mean),
      p_rise = round(p_rise, 3),
      p90_vs_cur = round(next_value_p90 - cur_value))][order(player_name)]
}

res <- rbind(run("rank"), run("log"))
cat("\n==== Puka / JSN: rank (shipped) vs log (proposed) ====\n")
print(res[order(player_name, space)])
