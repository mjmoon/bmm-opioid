# Functions for fitting stan models. -------------------------------------------
options(mc.cores = max(8, parallel::detectCores()))
rstan::rstan_options(auto_write = TRUE)

#' Fit a model in stan.
#' @param X observed mortality counts.
#' @param Y observed treatment admission counts.
#' @param W observed illicit opioid initiate counts.
#' @param S surveyed counts used for the ratio estimates.
#' @param priors named list of priors.
#' @param params named list of other model parameters.
#' @param stan_file stan script file path.
#' @param model_add (optional) additional model specification string 
#'   to be placed at the location specified by `{}`.
#' @param model_name (optional) model name for logging purposes.
#' @param control (optional) stan controls.
#' @param warmup (optional) the number of warmup iterations per chain.
#' @param iter (optional) the number of iterations for each chain.
#' @param n_chain (optional) number of chains.
#' @return stanfit object.
fit_opioid_transitions <- function(
    X, Y, W, S, priors, params, stan_file, 
    model_add = "",  model_name = "anon_model", 
    control = NULL, warmup = 5000, iter = 10000, n_chain = 8) {
  stopifnot(grepl("\\.stan$", stan_file))
  stan_spec <- readChar(stan_file, file.info(stan_file)$size)
  if (nchar(model_add) > 0) 
    stan_spec <- gsub("\\{\\}", model_add, stan_spec)
  stan_data <- list(
    n_time = params$n, 
    X = X, Y = Y, W = W,
    u = params$u, pop = params$pop, pop0 = params$pop0,
    gamma0 = log(priors$r0),
    delta0 = log(priors$p0/(1 - priors$p0))
  )
  stan_data <- append(stan_data, within(priors, rm(r0, p0)))
  if (!is.null(S)) 
    stan_data <- c(stan_data, list(
      s1 = S$s1, s2 = S$s2, tr1 = S$tr1, tr2 = S$tr2))
  model <- rstan::stan_model(model_code = stan_spec, model_name = model_name)
  stan_fit <- rstan::sampling(
    model, stan_data,
    warmup = warmup, iter = iter, chains = n_chain,
    algorithm = "NUTS", control = control
  )
  return(stan_fit)
}
