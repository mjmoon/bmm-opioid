source("R/functions/00-sim-data.R")
set.seed(2023)
load("data/sim/params.RData") # read parameters
# Simulate without interval
rmat <- sapply(r0, sim_r, n = n, sigma = siggamma) # r1 and r2 (n x 2)
pmat <- sapply(p0, sim_p, n = n, sigma = sigdelta) # p1, p2, p3 (n x 3)
sim_res <- sim_transition(n, rmat, pmat, u) # simulate transitions
surveyed <- sim_survey(sim_res$sims, p = s) # simulate surveyed
### Check ###
message("Check counts...\n")
message("  Number of incidence from rmat: ", sum(rmat), "\n")
message("  Number of simulated from sim_res: ", dim(sim_res$sims)[1], "\n")
message(if_else(sum(rmat) == dim(sim_res$sims)[1], 
                paste0("  ...OK."), paste0("  ...ERR")), "\n")
# Plot
plot_path <- file.path("results", "sim")
merged <- merge_rates(sim_res, surveyed$counts)
gen_simulation_plot(merged, "r1", plot_path)
gen_simulation_plot(merged, "r2", plot_path)
gen_simulation_plot(merged, "w", plot_path)
gen_simulation_plot(merged, "x", plot_path)
gen_simulation_plot(merged, "y", plot_path)
gen_simulation_plot(merged, "p1", plot_path)
gen_simulation_plot(merged, "p2", plot_path)
gen_simulation_plot(merged, "p3", plot_path)
# Save simulated data
saveRDS(sim_res, file.path("data", "sim", "sims.rds"))
saveRDS(surveyed, file.path("data", "sim", "surveyed.rds"))
message("Simulation objects saved to data/sims/00/*.rds\n")
rm(list = ls())
