source("R/functions/02-plot.R")
sigma_sigma_gamma <- 1
sigma_sigma_delta <- 1
s1p_prior <- list(
  dfun = dnorm,
  mean = 0,
  sigma = .3
)
s2p_prior <- list(
  dfun = dnorm,
  mean = -2,
  sigma = 1
)
r1_prior <- list(
  dfun = d_log_norm,
  mean = 100, # per thousand
  sigma_sigma = sigma_sigma_gamma
)
r2_prior <- list(
  dfun = d_log_norm,
  mean = 0.15, # per thousand
  sigma_sigma = sigma_sigma_gamma
)
p1_prior <- list(
  dfun = d_logit_norm,
  mean = .015,
  sigma_sigma = sigma_sigma_delta
)
p2_prior <- list(
  dfun = d_logit_norm,
  mean = .15,
  sigma_sigma = sigma_sigma_delta
)
p3_prior <- list(
  dfun = d_logit_norm,
  mean = .0005,
  sigma_sigma = sigma_sigma_delta
)

stan_prior <- sprintf(
"
init_s1_logitp ~ normal(%s, %s);
init_s2_logitp ~ normal(%s, %s);
siggamma ~ normal(0, %s);
sigdelta ~ normal(0, %s);
phi1 ~ normal(0, 1);
phi2 ~ normal(0, 1);
", 
  s1p_prior$mean, s1p_prior$sigma,
  s2p_prior$mean, s2p_prior$sigma,
  sigma_sigma_gamma,
  sigma_sigma_delta
)

priors <- list(
  p0 = c(
    p1_prior$mean,
    p2_prior$mean,
    p3_prior$mean
  )
  # r0's based on population at i = 0
)
