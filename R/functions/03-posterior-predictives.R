library(dplyr)
library(tidyr)
library(ggdist)
get_posterior_predictive <- function(
    fit_black, fit_white, n_draws = 100, n_time = 5) {
  param_black <- rstan::extract(fit_black)
  param_white <- rstan::extract(fit_white)
  n_sims <- length(param_black$lp__)
  i_sims <- sample(seq(n_sims), min(n_sims, n_draws))
  
  w_rep_black <- matrix(numeric(length(i_sims) * n_time), ncol = n_time)
  x_rep_black <- matrix(numeric(length(i_sims) * n_time), ncol = n_time)
  y_rep_black <- matrix(numeric(length(i_sims) * n_time), ncol = n_time)
  
  w_rep_white <- matrix(numeric(length(i_sims) * n_time), ncol = n_time)
  x_rep_white <- matrix(numeric(length(i_sims) * n_time), ncol = n_time)
  y_rep_white <- matrix(numeric(length(i_sims) * n_time), ncol = n_time)
  
  for (s in seq(length(i_sims))) {
    w_rep_black[s, ] <- sapply(
      param_black$w_hat[i_sims[s], ], rpois, n = 1)
    x_rep_black[s, ] <- sapply(
      param_black$x_hat[i_sims[s], ], rpois, n = 1)
    y_rep_black[s, ] <- sapply(
      param_black$y_hat[i_sims[s], ], rpois, n = 1)
    w_rep_white[s, ] <- sapply(
      param_white$w_hat[i_sims[s], ], rpois, n = 1)
    x_rep_white[s, ] <- sapply(
      param_white$x_hat[i_sims[s], ], rpois, n = 1)
    y_rep_white[s, ] <- sapply(
      param_white$y_hat[i_sims[s], ], rpois, n = 1)
  }
  
  w_reps <- as.data.frame(rbind(w_rep_black, w_rep_white)) |>
    mutate(race = c(rep("Non-Hispanic black", length(i_sims)),
                    rep("Non-Hispanic white", length(i_sims)))) |>
    pivot_longer(
      -race, names_to = "time", values_to = "w") 
  x_reps <- as.data.frame(rbind(x_rep_black, x_rep_white)) |>
    mutate(race = c(rep("Non-Hispanic black", length(i_sims)),
                    rep("Non-Hispanic white", length(i_sims)))) |>
    pivot_longer(
      -race, names_to = "time", values_to = "x")  |>
    select(-c(time, race))
  y_reps <- as.data.frame(rbind(y_rep_black, y_rep_white)) |>
    mutate(race = c(rep("Non-Hispanic black", length(i_sims)),
                    rep("Non-Hispanic white", length(i_sims)))) |>
    pivot_longer(
      -race, names_to = "time", values_to = "y") |>
    select(-c(time, race))
  return(cbind(w_reps, x_reps, y_reps) |>
    mutate(time = as.numeric(stringr::str_extract(time, "\\d"))))
}

plot_posterior_predictive <- function(
    pp, obs, var, 
    obs_size = 3, obs_line_width = 1, obs_line_type = "dotted", obs_shape = 21,
    title = NULL, ylab = "", 
    ylimits = NULL,
    ybreaks = waiver(), xbreaks = c(2015, 2019),
    plt_theme = theme_minimal(),
    file = NULL, save = TRUE, device = "png") {
  y_rep <- pp[[paste0(var, "_per_thousand")]]
  y_obs <- obs[[paste0(var, "_per_thousand")]]
  plt <- pp |>
    ggplot(aes(x = year, y = y_rep)) +
    stat_slab() +
    geom_line(aes(x = year, y = y_obs),
              data = obs, linewidth = obs_line_width, linetype = obs_line_type) +
    geom_point(aes(x = year, y = y_obs), fill = "white",
               data = obs, size = obs_size, shape = obs_shape) +
    labs(x = NULL, y = ylab, title = title) +
    facet_grid(race ~ ., labeller = labeller(
      race = function(s) gsub("\\s", "\n", s)),
      switch = "y") +
    scale_x_continuous(breaks = xbreaks) +
    scale_y_continuous(breaks = ybreaks, limits = ylimits, position = "right") +
    plt_theme
  if (!save) {
    return(plt)
  }
  ggsave(file, plot = plt, device = device, dpi = 450,
         width = 8, height = 6, unit = "in")
}

