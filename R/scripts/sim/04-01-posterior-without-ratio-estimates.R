source("R/functions/02-plot.R")
# read data
data_path <- file.path("data", "sim")
load(file.path(data_path, "params.RData"))
sigma_sigma_gamma <- 1
sigma_sigma_delta <- 1
obs <- readRDS(file.path(data_path, "sims.rds"))$counts |>
  mutate(time = 0:n) 
# read fitted object
fit <- readRDS(file.path(data_path, "fit-no-ratio-estimate.rds"))
posts <- as.data.frame(fit)
y_posts <- get_post_intervals(fit, "y_hat") |>
  left_join(obs |> select(time, s3), by = "time") |>
  rename(observed = s3, year = time)
x_posts <- get_post_intervals(fit, "x_hat") |>
  left_join(obs |> select(time, s4), by = "time") |>
  rename(observed = s4, year = time)
w_posts <- get_post_intervals(fit, "w_hat") |>
  left_join(obs |> select(time, w), by = "time") |>
  rename(observed = w, year = time)
p1_posts <- get_post_intervals(fit, "p1_hat") |>
  left_join(obs |> select(time, p1), by = "time") |>
  rename(observed = p1, year = time)
p2_posts <- get_post_intervals(fit, "p2_hat") |>
  left_join(obs |> select(time, p2), by = "time") |>
  rename(observed = p2, year = time)
p3_posts <- get_post_intervals(fit, "p3_hat") |>
  left_join(obs |> select(time, p3), by = "time") |>
  rename(observed = p3, year = time)
r1_posts <- get_post_intervals(fit, "r1_hat") |>
  left_join(obs |> select(time, r1), by = "time") |>
  rename(observed = r1, year = time)
r2_posts <- get_post_intervals(fit, "r2_hat") |>
  left_join(obs |> select(time, r2), by = "time") |>
  rename(observed = r2, year = time)
# plot posterior
plot_path <- file.path("results", "sim", "no-ratio-estimate", "00-posterior-single-")
plot_posterior_prior_density(
  posts, "siggamma",
  function(x) ifelse(x > 0, 2 * dnorm(x, sd = sigma_sigma_gamma), 0),
  file = paste0(plot_path, "siggamma.png")
)
plot_posterior_prior_density(
  posts, "sigdelta",
  function(x) ifelse(x > 0, 2 * dnorm(x, sd = sigma_sigma_delta), 0),
  file = paste0(plot_path, "sigdelta.png")
)

# plot intervals over time
plot_path <- file.path("results", "sim", "no-ratio-estimate", "01-posterior-over-time-")
post_theme <- theme_minimal() +
  theme(plot.margin = margin(l = 1, r = 0, t = 0.5, b = 0, unit = "in"))

r1_prior <- list(
  dfun = d_log_norm,
  mean = r0[1],
  sigma_sigma = sigma_sigma_gamma
)
plot_posterior_over_time(r1_posts, prior = r1_prior, both_races = FALSE,
                         obs = TRUE, obs_shape = 23, obs_size = 5,
                         plt_theme = post_theme,
                         ylab = "r1",
                         ytrans = "log10",
                         xlimits = c(1, n), xbreaks = c(1, n),
                         file = paste0(plot_path, "r1.png"))
r2_prior <- list(
  dfun = d_log_norm,
  mean = r0[2],
  sigma_sigma = sigma_sigma_gamma
)
plot_posterior_over_time(r2_posts, prior = r2_prior, both_races = FALSE,
                         obs = TRUE, obs_shape = 23, obs_size = 5,
                         plt_theme = post_theme,
                         ylab = "r2 per thousand",
                         ytrans = "log10",
                         xlimits = c(1, n), xbreaks = c(1, n),
                         file = paste0(plot_path, "r2.png"))
p1_prior <- list(
  dfun = d_logit_norm,
  mean = p0[1],
  sigma_sigma = sigma_sigma_delta
)
plot_posterior_over_time(p1_posts, prior = p1_prior, both_races = FALSE,
                         obs = TRUE, obs_shape = 23, obs_size = 5,
                         plt_theme = post_theme,
                         ylab = "p1",
                         xlimits = c(1, n), xbreaks = c(1, n),
                         file = paste0(plot_path, "p1.png"))
p2_prior <- list(
  dfun = d_logit_norm,
  mean = p0[2],
  sigma_sigma = sigma_sigma_delta
)
plot_posterior_over_time(p2_posts, prior = p2_prior, both_races = FALSE,
                         obs = TRUE, obs_shape = 23, obs_size = 5,
                         plt_theme = post_theme,
                         ylab = "p2",
                         xlimits = c(1, n), xbreaks = c(1, n),
                         file = paste0(plot_path, "p2.png"))
p3_prior <- list(
  dfun = d_logit_norm,
  mean = p0[3],
  sigma_sigma = sigma_sigma_delta
)
plot_posterior_over_time(p3_posts, prior = p3_prior, both_races = FALSE,
                         obs = TRUE, obs_shape = 23, obs_size = 5,
                         plt_theme = post_theme,
                         ylab = "p3",
                         xlimits = c(1, n), xbreaks = c(1, n),
                         file = paste0(plot_path, "p3.png"))
plot_posterior_over_time(x_posts, both_races = FALSE,
                         obs = TRUE, obs_shape = 23, obs_size = 5,
                         plt_theme = post_theme,
                         ylab = "x",
                         xlimits = c(1, n), xbreaks = c(1, n),
                         file = paste0(plot_path, "x.png"))
plot_posterior_over_time(y_posts, both_races = FALSE,
                         obs = TRUE, obs_shape = 23, obs_size = 5,
                         plt_theme = post_theme,
                         ylab = "y",
                         xlimits = c(1, n), xbreaks = c(1, n),
                         file = paste0(plot_path, "y.png"))
plot_posterior_over_time(w_posts, both_races = FALSE,
                         obs = TRUE, obs_shape = 23, obs_size = 5,
                         plt_theme = post_theme,
                         ylab = "w",
                         xlimits = c(1, n), xbreaks = c(1, n),
                         file = paste0(plot_path, "w.png"))
rm(list = ls())
