require(dplyr)
require(tidyr)
require(ggplot2)
#' Simulate entry rates.
#' @param n number of time intervals
#' @param r0 initial rate value
#' @param sigma standard deviation for the log error term
#' @return a vector of simulated rates
sim_r <- function(n, r0, sigma) {
  n <- n + 1 
  if(r0 <= 0) stop("r0 must be positive.")
  # simulate in log scale
  loge <- rnorm(n, mean = 0, sd = sigma)
  logr <- cumsum(loge) + log(r0)
  # return in natural scale
  return(round(exp(logr)))
}
#' Simulate transition probabilities.
#' @param n number of time intervals
#' @param p0 initial probability value
#' @param sigma standard deviation for the logit error term
#' @return a vector of simulated probabilities
sim_p <- function(n, p0, sigma) {
  n <- n + 1 
  if(p0 <= 0 | p0 >= 1) stop("p0 must be between 0 and 1.")
  # simulate in logit scale
  logite <- rnorm(n, mean = 0, sd = sigma)
  logitp <- cumsum(logite) + qlogis(p0)
  # return in natural scale
  return(plogis(logitp)) 
}
#' Simulate transitions.
#' @param n number of time intervals
#' @param rmat matrix of entry rates
#' @param pmat matrix of transition probabilities
#' @param u universal mortality probability
#' @return a list with simulated individual states and counts over time.
sim_transition <- function(n, rmat, pmat, u){
  n <- n + 1
  colnames(rmat) <- paste0('r', 1:2)
  colnames(pmat) <- paste0('p', 1:3)
  # a single simulation step 
  # 0 := not in any of the states 
  # 1 := prescription opioid use
  # 2 := opioid misuse
  # 3 := treatment
  # 4 := mortality
  # 99 := in treatment > 1 step
  # -99 := death > 1 step
  # -(#) := death from state (#)
  sim_step <- function(s, p) {
    if (s < 0) return(-99)
    if (s == 0 | s > 4) return(s)
    if (s == 4) return(-4)
    if (s == 3) return(99)
    if (rbinom(1, 1, u)) return(-s)
    i <- 0
    while(i < s) { # for state 1 and 2 only
      if(rbinom(1, 1, p[s + i])) return(s + i + 1)
      i <- i + 1
    }
    return(s)
  }
  # simulate an individual for n time steps
  sim_ind <- function(s, m) {
    sind <- numeric(n)
    sind[m] <- s
    while(m < n) {
      sind[m + 1] <- sim_step(sind[m], pmat[m, ])
      m <- m + 1
    }
    return(sind)
  }
  # simulate entry 1 (at is the entry time)
  sims_1 <- lapply(1:n, function(at) t(replicate(rmat[at, 1], sim_ind(1, at)))) 
  # simulate entry 2 (at is the entry time)
  sims_2 <- lapply(1:n, function(at) t(replicate(rmat[at, 2], sim_ind(2, at))))
  sims <- do.call(rbind, c(sims_1, sims_2))
  counts <- bind_rows(lapply(
    1:n, function(at) 
      as.data.frame(table(sims[ , at])) %>%
      mutate(time = at))) %>%
    rename(state = Var1, count = Freq) %>%
    pivot_wider(
      id_cols = time,
      names_from = state,
      values_from = count,
      values_fill = 0
    ) %>%
    rename_with(.fn = ~ if_else(
      grepl("-", .x), 
      paste0("x", sub("-", "", .x)),
      paste0("s", .x)),
      .cols = matches("-?\\d")) %>%
    bind_cols(as_tibble(rmat), as_tibble(pmat)) %>%
    mutate(
      w = c(0, diff(s2)) + x2 + s3 + s4
    )
  return(list(counts = counts, sims = sims))
}
#' Simulate survey recruits.
#' @param sims matrix of simulated.
#' @param p probability of being recruited. Default value is 0.1.
#' @return a list with simulated individual surveys and counts over time.
sim_survey <- function(sims, p = 0.1) {
  n <- ncol(sims)
  sims[sims == 99] <- 3
  # survey a time step
  survey_step <- function(at) {
    # sample from states 1 to 3
    available <- subset(sims, between(sims[ , at], 1, 3) & sims[ , at - 1] > 0)
    samp <- rbinom(nrow(available), 1, p)
    recruit <- subset(available, samp == 1)
    if (at < n && nrow(recruit) > 0)
      recruit[ , (at + 1):n] <- 0
    if (at > 2)
      recruit[ , 1:(at - 2)] <- 0
    return(recruit)
  }
  surveyed <- do.call(rbind, lapply(2:n, survey_step))
  
  count_prevalence <- function(at) {
    s <- subset(surveyed, surveyed[ , at] > 0)
    if (at > 1)
      s <- subset(s, s[ , at - 1] == 0)
    table(factor(s[ , at], levels = 1:3))
  }
  count_transition <- function(at) {
    data.frame(
      time = at,
      tr1 = sum(surveyed[ , at] == 1 & surveyed[ , at + 1] == 2),
      tr2 = sum(surveyed[ , at] == 2 & surveyed[ , at + 1] == 3),
      w = sum(surveyed[ , at] < 2 & surveyed[ , at + 1] == 2)
    )
  }
  counts_prev <- bind_rows(lapply(
    1:n, count_prevalence)) %>%
    rename_with(~ paste0("s", .x)) %>%
    mutate(
      across(everything(), .fns = as.numeric),
      across(everything(), .fns = function(s) replace_na(s, 0)),
      time = 1:n
    )
  counts_trans <- bind_rows(lapply(
    1:(n - 1), count_transition)) 
  counts <- left_join(counts_prev, counts_trans, by = "time")
  return(list(counts = counts, sims = surveyed))
}
#' Merge simulated transitions.
#' @param sims a list of simulation object with counts 
#'   and individual states over time.
#' @param surveyed a data frame with simulates survey counts.
#' @return a data frame with simulated entry rates and transition probabilities.
merge_rates <- function(sims, surveyed) {
  n <- nrow(surveyed)
  counts <- sims$counts
  states <- sims$sims
  extract_entries <- function(at) {
    if (at == 1)
      return(table(states[states[ , 1] > 0, 1]))
    table(states[(states[ , at - 1] == 0) & (states[ , at] > 0), at])
  }
  extract_transitions <- function(at, step) {
    table(states[states[ , at + 1] - states[ , at] == step, at])
  }
  bind_cols(
    counts %>% 
      select(
        starts_with("r"),
        starts_with("w"),
        starts_with("p"),
        starts_with("s"),
        starts_with("x")
      ),
    surveyed %>% 
      rename_with(~ paste0(.x, "_surveyed"))
  ) %>% mutate(time = 1:n) %>%
    left_join(
      bind_cols(
        bind_rows(lapply(1:(n - 1), extract_transitions, step = 1)) %>%
          select(-`0`) %>%
          rename_with(~ paste0("tr", .x)),
        bind_rows(lapply(1:(n - 1), extract_transitions, step = 2)) %>%
          select(-`0`) %>%
          rename_with(~ "tr3")
      ) %>% mutate(time = 1:(n - 1))
    ) %>%
    mutate(across(everything(), ~ as.numeric(.x))) %>%
    rename(x = s4, y = s3) %>%
    mutate(
      p1_observed = .data$tr1 / (.data$s1 - .data$x1),
      p2_observed = .data$tr2 / (.data$s2 - .data$x2),
      p3_observed = .data$tr3 / (.data$s2 - .data$x2 - .data$tr2),
      p1_surveyed = .data$tr1_surveyed / .data$s1_surveyed,
      p2_surveyed = .data$tr2_surveyed / .data$s2_surveyed
    )
}
#' Plot simulated mean, observed, and surveyed rates together.
#' @param data a merged data frame with all rates.
#' @param param a string specifying the rate of interest.
#' @param dir path tot  directory to save the plot. ignored when plot is TRUE.
#' @param x_lab (optional) x-axis label.
#' @param plot (optional) returns a plot if TRUE.
#' @param suffix (optional) suffix to file name.
#' @return a ggplot object
gen_simulation_plot <- function(
    data, param, dir, x_lab = "Time", plot = FALSE, suffix = "") {
  plt <- ggplot(data, aes(x = time)) +
    theme_minimal() +
    labs(x = x_lab, y = param, colour = "") 
  if (grepl("^p", param)) {
    plt <- plt +
      geom_line(aes(y = .data[[param]], colour = "Mean")) +
      geom_point(
        aes(y = .data[[paste0(param, "_observed")]], colour = "Observed"))
  } else {
    plt <- plt +
      geom_line(aes(y = .data[[param]])) +
      geom_point(aes(y = .data[[param]]))
  }
    
  if (param %in% c("p1", "p2"))
    plt <- plt + 
      geom_point(
        aes_(y = data[[paste0(param, "_surveyed")]], colour = "Surveyed"))
  if (plot)
    return(plt)
  ggsave(file.path(dir, paste0(param, suffix, ".png")), plt)
}
