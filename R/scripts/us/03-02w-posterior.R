source("R/functions/02-plot.R")
source("R/scripts/us/02-01-set-priors.R")
log_file <- file(paste0("logs/plot-us-white.log"), open = "a")
sink(log_file, type = "message")
timestamp <- format(Sys.time(), format = "%Y-%m-%d %H:%M")
message(paste0("----------", timestamp, "----------"))
# read data
data_path <- file.path("data", "us")
obs <- readRDS(file.path(data_path, "mort.rds")) |>
  filter(Race == "White",
         `Hispanic Origin` == "Not Hispanic or Latino",
         Year > 2014) |>
  rename_with(tolower) |>
  mutate(race = "non-hispanic white") |>
  left_join(
    readRDS(file.path(data_path, "nsduh.rds")),
    by = c("year", "race")
  ) |>
  left_join(
    readRDS(file.path(data_path, "mcdus.rds")) |>
      rename(opioid_deaths = n),
    by = c("year", "race")
  ) |>
  mutate(
    x_per_thousand = opioid_deaths / population * 1000,
    y_per_thousand = tr2 / population * 1000,
    w_per_thousand = init_ill_opioid / population * 1000,
    p1_surveyed = tr1 / s1,
    p2_surveyed = tr2 /s2,
    u_per_thousand = deaths / population * 1000
  )
pop0 <- readRDS(file.path(data_path, "mort.rds")) |>
  filter(Race == "White",
         `Hispanic Origin` == "Not Hispanic or Latino",
         Year == 2014) |>
  pull(Population)
# read fitted object
fit <- readRDS(file.path(data_path, "fit_white.rds"))
posts <- as.data.frame(fit)
y_posts <- get_post_intervals(fit, "y_hat_per_thousand",
                              time = 2015:2019) |>
  rename(year = time) |>
  left_join(obs |> select(year, y_per_thousand), by = "year") |>
  rename(observed = y_per_thousand)
x_posts <- get_post_intervals(fit, "x_hat_per_thousand",
                              time = 2015:2019) |>
  rename(year = time) |>
  left_join(obs |> select(year, x_per_thousand), by = "year") |>
  rename(observed = x_per_thousand)
w_posts <- get_post_intervals(fit, "w_hat_per_thousand",
                              time = 2015:2019) |>
  rename(year = time) |>
  left_join(obs |> select(year, w_per_thousand), by = "year") |>
  rename(observed = w_per_thousand)
p1_posts <- get_post_intervals(fit, "p1_hat",
                               time = 2015:2019) |>
  rename(year = time) |>
  left_join(obs |> select(year, p1_surveyed), by = "year") |>
  rename(observed = p1_surveyed)
p2_posts <- get_post_intervals(fit, "p2_hat",
                               time = 2015:2019) |>
  rename(year = time) |>
  left_join(obs |> select(year, p2_surveyed), by = "year") |>
  rename(observed = p2_surveyed)
p3_posts <- get_post_intervals(fit, "p3_hat",
                               time = 2015:2019) |>
  rename(year = time)
r1_posts <- get_post_intervals(fit, "r1_hat_per_thousand",
                               time = 2015:2019) |>
  rename(year = time)
r2_posts <- get_post_intervals(fit, "r2_hat_per_thousand",
                               time = 2015:2019) |>
  rename(year = time)
# plot posterior
plot_path <- file.path("results", "us", "white", "01-posterior-single-")
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
plot_posterior_prior_density(
  posts, "phi1",
  function(x) ifelse(
    abs(x) <= 1, 
    dnorm(x) / (pnorm(1) - pnorm(-1)), 
    0
  ),
  file = paste0(plot_path, "phi1.png")
)
plot_posterior_prior_density(
  posts, "phi2",
  function(x) ifelse(
    abs(x) <= 1, 
    dnorm(x) / (pnorm(1) - pnorm(-1)), 
    0
  ),
  file = paste0(plot_path, "phi2.png")
)
logit_trans <- scales::trans_new(
  "logit",
  transform = function(x) 1 / (1 + exp(-x)),
  inverse = function(x) log(x / (1 - x))
)
plot_posterior_prior_density(
  posts, "init_s1_logitp",
  function(x) s1p_prior$dfun(x, s1p_prior$mean, s1p_prior$sigma),
  file = paste0(plot_path, "init_s1_logitp.png")
)
plot_posterior_prior_density(
  posts, "init_s2_logitp",
  function(x) s1p_prior$dfun(x, s2p_prior$mean, s2p_prior$sigma),
  file = paste0(plot_path, "init_s2_logitp.png")
)

# plot intervals over time
plot_path <- file.path("results", "us", "white", "02-posterior-over-time-")
post_theme <- theme_minimal() +
  theme(plot.margin = margin(l = 1, r = 0, t = 0.5, b = 0, unit = "in"))
plot_posterior_over_time(r1_posts, prior = r1_prior, both_races = FALSE,
                         plt_theme = post_theme,
                         ylab = "r1 per thousand",
                         ytrans = "log10",
                         file = paste0(plot_path, "r1.png"))
plot_posterior_over_time(r2_posts, prior = r2_prior, both_races = FALSE,
                         plt_theme = post_theme,
                         ylab = "r2 per thousand",
                         ytrans = "log10",
                         file = paste0(plot_path, "r2.png"))
plot_posterior_over_time(p1_posts, prior = p1_prior, both_races = FALSE,
                         obs = TRUE, obs_shape = 23, obs_size = 5,
                         plt_theme = post_theme,
                         ylab = "p1",
                         file = paste0(plot_path, "p1.png"))
plot_posterior_over_time(p2_posts, prior = p2_prior, both_races = FALSE,
                         obs = TRUE, obs_shape = 23, obs_size = 5,
                         plt_theme = post_theme,
                         ylab = "p2",
                         file = paste0(plot_path, "p2.png"))
plot_posterior_over_time(p3_posts, prior = p3_prior, both_races = FALSE,
                         plt_theme = post_theme,
                         ylab = "p3",
                         file = paste0(plot_path, "p3.png"))
plot_posterior_over_time(x_posts, both_races = FALSE,
                         obs = TRUE, obs_shape = 3, obs_size = 5,
                         plt_theme = post_theme,
                         ylab = "x per thousand",
                         file = paste0(plot_path, "x.png"))
plot_posterior_over_time(y_posts, both_races = FALSE,
                         obs = TRUE, obs_shape = 3, obs_size = 5,
                         plt_theme = post_theme,
                         ylab = "y per thousand",
                         file = paste0(plot_path, "y.png"))
plot_posterior_over_time(w_posts, both_races = FALSE,
                         obs = TRUE, obs_shape = 3, obs_size = 5,
                         plt_theme = post_theme,
                         ylab = "w per thousand",
                         file = paste0(plot_path, "w.png"))
message(paste0("----------", timestamp, "----------"))
message(paste0("------------------------------------"))
