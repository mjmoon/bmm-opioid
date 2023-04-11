library(dplyr)
source("R/functions/01-fit.R")
source("R/scripts/us/02-01-set-priors.R")
log_file <- file(
  paste0("logs/fit-us-black.log"), 
  open = "a")
sink(log_file, type = "message")
timestamp <- format(Sys.time(), format = "%Y-%m-%d %H:%M")
message(paste0("----------", timestamp, "----------"))
# load data --------------------------------------------------------------------
mcdus <- readRDS(file.path("data", "us", "mcdus.rds")) |>
  filter(race == "non-hispanic black")
nsduh <- readRDS(file.path("data", "us", "nsduh.rds")) |>
  filter(race == "non-hispanic black")
mort <- readRDS(file.path("data", "us", "mort.rds")) |>
  filter(Race == "Black or African American", 
         `Hispanic Origin` == "Not Hispanic or Latino")
# read stan files --------------------------------------------------------------
stanfile <- "stan/01-fit-full.stan"
# set params -------------------------------------------------------------------
warmup <- 5000
iter <- 10000
control <- list(adapt_delta = 0.999, max_treedepth = 16)
params <- list(
  n = nrow(mcdus),
  u = mort |>
    filter(Year > 2014) |>
    mutate(u = Deaths / Population) |>
    pull(u),
  pop = mort |>
    filter(Year > 2014) |>
    pull(Population),
  pop0 = mort |>
    filter(Year == 2014) |>
    pull(Population)
)
# set priors and initial values ------------------------------------------------
# `stan_prior` sourced from 02-01-set-priors.R
# `priors` sourced from 02-01-set-priors.R
priors$r0 <- c(
  mort$Population[mort$Year == 2014] * r1_prior$mean / 1000,
  mort$Population[mort$Year == 2014] * r2_prior$mean / 1000
)

message(paste0(
  "MCMC parameters ------------------------------------------\n ",
  paste0("warmup = ", warmup , "\n "),
  paste0("iter = ", iter, "\n "),
  paste(names(control), control, collapse = "\n ", sep = " = "),
  "\nPriors ---------------------------------------------------\n ",
  paste(names(priors), priors, collapse = "\n ", sep = " = "),
  paste("\n ", stan_prior)
))
# fit ----------------------------------------
message("Fitting for US black population between 2015-2019. -----")
fit <- fit_opioid_transitions(
  X = mcdus |> pull(n), 
  Y = nsduh |> pull(tr2),
  W = nsduh |> pull(init_ill_opioid),
  S = filter(nsduh, year < 2019) |> select(s1:tr2),
  priors = priors, 
  params = params,
  stan_file = stanfile, 
  model_add = stan_prior,
  model_name = "Non-hispanic black US 2015-2019 with survey",
  control = control, warmup = warmup, iter = iter
)
saveRDS(fit, file.path("data", "us", "fit_black.rds"))
timestamp <- format(Sys.time(), format = "%Y-%m-%d %H:%M")
message(paste0("----------", timestamp, "----------"))
message(paste0("------------------------------------"))
rm(list = ls())
