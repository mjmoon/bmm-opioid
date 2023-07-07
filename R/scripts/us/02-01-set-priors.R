source("R/functions/02-plot.R")
mcdus <- readRDS(file.path("data", "us", "mcdus.rds")) |>
  # filter(race %in% c("non-hispanic black")) |>
  summarise(n = sum(n), .by = year)
nsduh <- readRDS(file.path("data", "us", "nsduh.rds")) |>
  # filter(race %in% c("non-hispanic black")) |>
  summarise(tr1 = sum(tr1), tr2 = sum(tr2), r2 = sum(r2),
            s1 = sum(s1), s2 = sum(s2),
            r2_raw = sum(r2_raw), n = sum(n),
            .by = year)
mort <- readRDS(file.path("data", "us", "mort.rds")) |>
  # filter(Race %in% c("Black or African American"),
  #        `Hispanic Origin` == "Not Hispanic or Latino") |>
  rename(year = Year) |>
  filter(year > 2014) |>
  summarise(pop = sum(Population), d = sum(Deaths),
            .by = year)
nsduh$r2 / mort$pop
nsduh$tr1 / nsduh$s1
nsduh$tr2 / nsduh$s2
mcdus$n / nsduh$s2

nsduh$s1 / mort$pop
nsduh$s2 / mort$pop

replicate(100000, {
  sig <- abs(rnorm(1))
  rnorm(1, sd = sig) > log(2)
}) |>
  sum()
1 / (1 + exp(0))
1 / (1 + exp(2))
s1p <- 1 / (1 + exp(-rnorm(100000, 0, 0.3)))
s2p <- 1 / (1 + exp(-rnorm(100000, -2, 1)))
mean(s1p < .5)
mean(s1p < .6) - mean(s1p < .4)
mean(s2p < .1)
mean(s2p < .3)

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
