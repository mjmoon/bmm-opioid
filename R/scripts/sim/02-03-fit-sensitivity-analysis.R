library(dplyr)
source("R/functions/01-fit.R")
data_path <- file.path("data", "sim")
load(file.path(data_path, "params.RData")) # read parameters
# read simulated data
sim_res <- readRDS(file.path(data_path, "sims.rds"))$counts
surveyed <- readRDS(file.path(data_path, "surveyed.rds"))$counts
control <- list(adapt_delta = 0.99, max_treedepth = 15)
priors <- list(p0 = p0, r0 = r0, 
               s0 = sim_res |> slice(1)|> select(s1, s2) |> unlist())
params <- list(u = u, n = n)
stanfile <- file.path("stan", "00-02-sim-ratio-estimate.stan")
sigma_sigma_gamma <- 1
sigma_sigma_delta <- 1
# drop points at time 0
X <- sim_res$s4[-1]
Y <- sim_res$s3[-1]
W <- sim_res$w[-1]
S <- surveyed |> slice(-c(1, (n + 1))) |> select(s1:tr2)
stan_prior <- sprintf(
  "
siggamma ~ normal(0, %s);
sigdelta ~ normal(0, %s);
phi1 ~ normal(0, 1);
phi2 ~ normal(0, 1);
", 
  sigma_sigma_gamma, sigma_sigma_delta # uninformative priors
)

# fit with biased r0 and p0 priors
fctrs <- c(1/2, 1/1.5, 1.5, 2)

sapply(fctrs, function(x) {
  suffix <- stringr::str_remove(sprintf("_%0.2f", x), "\\.")
  priors$r0 <- r0 * x
  fit <- fit_opioid_transitions(
    X = X, Y = Y, W = W, S = S,
    model_add = stan_prior, model_name = paste0("r0", suffix),
    priors = priors, params = params, 
    stan_file = stanfile, control = control, n_chain = 4
  )
  saveRDS(fit, file.path(data_path, paste0("fit-sa-r0", suffix, ".rds")))
})
priors$r0 <- r0
sapply(fctrs, function(x) {
  suffix <- stringr::str_remove(sprintf("_%0.2f", x), "\\.")
  priors$p0 <- p0 * x
  fit <- fit_opioid_transitions(
    X = X, Y = Y, W = W, S = S,
    model_add = stan_prior, model_name = paste0("p0", suffix),
    priors = priors, params = params, 
    stan_file = stanfile, control = control, n_chain = 4
  )
  saveRDS(fit, file.path(data_path, paste0("fit-sa-p0", suffix, ".rds")))
})
sapply(fctrs, function(x) {
  suffix <- stringr::str_remove(sprintf("_%0.2f", x), "\\.")
  priors$p0 <- p0 * x
  priors$r0 <- r0 * x
  fit <- fit_opioid_transitions(
    X = X, Y = Y, W = W, S = S,
    model_add = stan_prior, model_name = paste0("r0p0", suffix),
    priors = priors, params = params, 
    stan_file = stanfile, control = control, n_chain = 4
  )
  saveRDS(fit, file.path(data_path, paste0("fit-sa-r0p0", suffix, ".rds")))
})
priors$p0 <- p0

# fit with very tight / very flat sigma priors
sapply(fctrs, function(x){
  suffix <- stringr::str_remove(sprintf("_%0.2f", x), "\\.")
  stan_prior <- sprintf(
    "
siggamma ~ normal(0, %s);
sigdelta ~ normal(0, %s);
phi1 ~ normal(0, 1);
phi2 ~ normal(0, 1);
", 
    sigma_sigma_gamma * x, sigma_sigma_delta # uninformative priors
  )
  fit <- fit_opioid_transitions(
    X = X, Y = Y, W = W, S = S,
    model_add = stan_prior, model_name = paste0("nugamma", suffix),
    priors = priors, params = params, 
    stan_file = stanfile, control = control, n_chain = 4
  )
  saveRDS(fit, file.path(data_path, paste0("fit-sa-nugamma", suffix, ".rds")))
})

sapply(fctrs, function(x){
  suffix <- stringr::str_remove(sprintf("_%0.2f", x), "\\.")
  stan_prior <- sprintf(
    "
siggamma ~ normal(0, %s);
sigdelta ~ normal(0, %s);
phi1 ~ normal(0, 1);
phi2 ~ normal(0, 1);
", 
    sigma_sigma_gamma, sigma_sigma_delta * x # uninformative priors
  )
  fit <- fit_opioid_transitions(
    X = X, Y = Y, W = W, S = S,
    model_add = stan_prior, model_name = paste0("nudelta", suffix),
    priors = priors, params = params, 
    stan_file = stanfile, control = control, n_chain = 4
  )
  saveRDS(fit, file.path(data_path, paste0("fit-sa-nudelta", suffix, ".rds")))
})


sapply(fctrs, function(x){
  suffix <- stringr::str_remove(sprintf("_%0.2f", x), "\\.")
  stan_prior <- sprintf(
    "
siggamma ~ normal(0, %s);
sigdelta ~ normal(0, %s);
phi1 ~ normal(0, 1);
phi2 ~ normal(0, 1);
", 
    sigma_sigma_gamma * x, sigma_sigma_delta * x # uninformative priors
  )
  fit <- fit_opioid_transitions(
    X = X, Y = Y, W = W, S = S,
    model_add = stan_prior, model_name = paste0("nugammadelta", suffix),
    priors = priors, params = params, 
    stan_file = stanfile, control = control, n_chain = 4
  )
  saveRDS(fit, file.path(data_path, paste0("fit-sa-nugammadelta", suffix, ".rds")))
})

rm(list = ls())
