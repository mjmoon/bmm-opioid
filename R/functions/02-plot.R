# Functions for plotting results of fitted models. -----------------------------

require(dplyr)
require(tibble)
library(ggplot2)

#' Quantile function for a half-normal distribution.
#' @param p Probability of interest.
#' @param sigma Sigma of the distribution.
#' @return The quantile.
qhnorm <- function(p, sigma) {
  sigma * qnorm((1 + p) / 2)
}

#' Density function of a log-normal distribution.
#' @param x Quantity of interest in exponential scale.
#' @param mean Mean of the distribution in exponential scale.
#' @param sd Standard deviation of the distribution.
#' @return The density at x.
d_log_norm <- function(x, mean, sd) {
  dnorm(log(x), mean = log(mean), sd = sd)
}
#' Density function of a logit-normal distribution.
#' @param x Quantity of interest in logistic scale.
#' @param mean Mean of the distribution in logistic scale.
#' @param sd Standard deviation of the distribution.
#' @return The density at x.
d_logit_norm <- function(x, mean, sd) {
  dnorm(log(x / (1 - x)), mean = log(mean / (1 - mean)), sd = sd)
}

#' Plot posterior vs prior.
#' @param posts `data.frame` object containing posterior samples from rstan.
#' @param name Name of the parameter.
#' @param dprior Density function for the prior distribution.
#' @param xlimits (optional) A vector specifying the x axis limits.
#' @param plt_theme (optional) A `ggplot` theme object.
#' @param file (optional) File path to save the plot.
#' @param save (optional) If `FALSE`, the file is not saved and the function 
#'     returns the constructed `ggplot` object
#' @return A `ggplot` object if `save` is `FALSE`.
plot_posterior_prior_density <- function(
    posts, name, dprior, 
    xtrans = "identity", xlimits = NULL, 
    plt_theme = theme_minimal(), file = NULL, save = TRUE) {
  dpost <- density(posts[[name]])
  plt <- ggplot(mapping = aes(x = dpost$x)) +
    plt_theme +
    theme(legend.position = "top") +
    coord_cartesian(xlim = xlimits) +
    labs(y = "density", x = name, colour = NULL) +
    geom_line(aes(colour = "Posterior", y = dpost$y)) +
    geom_line(aes(colour = "Prior"), y = dprior(dpost$x))
  if (!save) {
    return(plt)
  }
  ggsave(file, plot = plt, device = "png", dpi = 450,
         width = 8, height = 6, unit = "in")
}

#' Get posterior intervals.
#' @param fit A stanfit object.
#' @param name Name of the parameter as it appears in stan.
#' @param probs (optional) Probabilities for credible interval.
#' @param time (optional) Time values for the return data table.
#' @return The quantile values in a tibble.
get_post_intervals <- function(
    fit, name, probs = c(.025, .1, .5, .9, .975), time = NULL) {
  post <- rstan::summary(fit, pars = c(name), probs = probs)$summary |>
    as_tibble() |>
    select(ends_with("%")) |>
    rownames_to_column("value") |>
    mutate(
      time = as.numeric(value),
      var = gsub("_hat", "", name)
    ) |>
    select(-value)
  if (!is.null(time)) {
    post$time <- time
  }
  return(post)
}

#' Plot posterior intervals over time.
#' @param post A tibble of posterior estimate medians with 80% and 95% CIs.
#' @param prior (optional) A list specifying the initial value prior.
#'     The list must contain the following:
#'     + `dfun`: density function.
#'     + `mean`: mean of prior.
#'     + `sigma_sigma`: sigma of the standard deviaiton of the prior.
#' @param both_races (optional) Plot both race if `TRUE`.
#' @param obs (optional) Whether the plot should include observed values.
#' @param obs_shape (optional) Point shape for observed values. required 
#'     when `obs` is `TRUE`.
#' @param obs_size (optional) Point size of the observed values.
#' @param title (optional) Plot title.
#' @param subtitle (optional) Plot stitle.
#' @param ylab (optional) Y axis title.
#' @param ytrans (optional) Transformation for y axis.
#' @param ylimits (optional) A vector specifying the y axis limits.
#' @param ybreaks (optional) A vector specifying the y axis breaks.
#' @param xlimits (optional) A vector specifying the x axis limits.
#' @param xbreaks (optional) A vector specifying the x axis breaks.
#' @param colors (optional) A named vector specifying the colours for markers. 
#'     Required items:
#'     + `non-hispanic black`
#'     + `non-hispanic white`
#'     + `prior`
#' @param plt_theme (optional) A `ggplot` theme object.
#' @param file (optional) File path to save the plot.
#' @param save (optional) If `FALSE`, the file is not saved and the function 
#'     returns the constructed `ggplot` object
#' @param device (optional) Device for saving the plot file.
#' @return A `ggplot` object if `save` is `FALSE`.
plot_posterior_over_time <- function(
    post, prior = NULL, both_races = TRUE,
    obs = FALSE, obs_shape = NULL, obs_size = 1.5, 
    title = NULL, subtitle = NULL, ylab = NULL, 
    ytrans = "identity", ylimits = NULL, ybreaks = waiver(), 
    xlimits = c(2015, 2019), xbreaks = c(2015, 2019),
    colors = c(`non-hispanic black` = "#00A189", 
               `non-hispanic white` = "#F1C500",
               `prior` = "#404040"), 
    plt_theme = theme_minimal(),
    file = NULL, save = TRUE, device = "png") {
  if (both_races) {
    plt <- ggplot(
      post, 
      aes(y = year + ifelse(race == "non-hispanic black", -.1, .1), 
          colour = race)
    ) +
      scale_colour_manual(values = colors, aesthetics = list("color", "fill"))
  } else {
    plt <- ggplot(post, aes(y = year))
  }
  plt <- plt +
    plt_theme +
    labs(title = title, subtitle = subtitle, y = NULL, x = ylab) + 
    scale_y_continuous(breaks = xbreaks, expand = expansion(add = .5)) +
    scale_x_continuous(trans = ytrans, breaks = ybreaks, position = "top",
                       labels = scales::label_number()) +
    coord_flip(ylim = xlimits, xlim = ylimits, clip = 'off')
  
  d_scale <- .5 # scale for density lines
  if (!is.null(prior)) {
    for (q in (5:9) / 10) {
      plt <- plt +
        stat_function(
          aes(colour = NULL), geom = "line", linewidth = 1.2,
          color = colors["prior"], alpha = 1 - q,
          fun = function(x, q) {
            xlimits[1] - .5 - d_scale * prior$dfun(
              x, prior$mean, qhnorm(q, prior$sigma_sigma))
          },
          args = list(q = q), n = 2000
        )
    }
  }
  
  plt <- plt +
    geom_linerange(aes(xmin = `2.5%`, xmax = `97.5%`), 
                   linewidth = 1) +
    geom_linerange(aes(xmin = `10%`, xmax = `90%`), 
                   linewidth = 2) +
    geom_point(aes(x = `50%`), size = 5, shape = 18) +
    geom_path(aes(x = `50%`), linewidth = 1.2, linetype = "dotted")
  
  if (obs) {
    plt <- plt +
      geom_point(aes(x = observed), shape = obs_shape, 
                 size = obs_size, fill = "white")
  }
  if (!save) {
    return(plt)
  }
  ggsave(file, plot = plt, device = device, dpi = 450,
         width = 8, height = 6, unit = "in")
}
