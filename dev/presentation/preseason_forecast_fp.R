# Preseason-2026 dynasty outlook on FantasyPros alone (no market scrape), for the talk.
# usage: Rscript dev/presentation/preseason_forecast_fp.R log|rank
suppressPackageStartupMessages(library(data.table))
devtools::load_all(here::here(), quiet = TRUE)
space <- commandArgs(TRUE)[1]
if (is.na(space)) space <- "log"
options(ffsimulator.dyn_move_space = space)
S <- here::here("dev", "presentation")
sim_path <- Sys.getenv("FFS_TALK_SIM", "C:/Github/ffsimulator/dev/league_sims/1326464763936403456/2026-07-19/simulation.rds")
sim <- readRDS(sim_path)
fmt <- ffsimulator:::.ffs_detect_qb_format(sim$lineup_constraints)
cat("format:", fmt, " n_seasons:", sim$simulation_params$n_seasons, " move_space:", space, "\n")
set.seed(2026)
t0 <- Sys.time()
out <- ffs_dynasty_outlook(sim, format = fmt, dynasty_values = "fantasypros")
cat("rows:", nrow(out), " minutes:", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "\n")
saveRDS(list(outlook = out, draws = attr(out, "draws"), fmt = fmt, space = space),
        file.path(S, paste0("forecast_fp_", space, ".rds")))
cat("saved\n")
