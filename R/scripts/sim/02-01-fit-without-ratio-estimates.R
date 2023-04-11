library(dplyr)
source("R/functions/01-fit.R")
data_path <- file.path("data", "sim")
load(file.path(data_path, "params.RData")) # read parameters
sim_res <- readRDS(file.path(data_path, "sims.rds"))$counts # read simulated data
control <- list(adapt_delta = 0.99, max_treedepth = 15)
priors <- list(p0 = p0, r0 = r0, 
               s0 = sim_res |> slice(1)|> select(s1, s2) |> unlist())
params <- list(u = u, n = n)
stanfile <- file.path("stan", "00-01-sim-no-ratio-estimate.stan")
# drop points at time 0
X <- sim_res$s4[-1]
Y <- sim_res$s3[-1]
W <- sim_res$w[-1]
stan_prior <- sprintf(
"
siggamma ~ normal(0, %s);
sigdelta ~ normal(0, %s);
", 
  1, 1 # flat priors
)
fit <- fit_opioid_transitions(
  X = X, Y = Y, W = W, S = NULL,
  model_add = stan_prior,
  priors = priors, params = params, 
  stan_file = stanfile, control = control, n_chain = 4
)
# save fit object
saveRDS(fit, file.path("data", "sim", "fit-no-ratio-estimate.rds"))
rm(list = ls())
