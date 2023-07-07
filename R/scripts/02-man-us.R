source("R/functions/02-plot.R")
source("R/functions/03-posterior-predictives.R")
# US case study ----------------------------------------------------------------
source("R/scripts/us/02-01-set-priors.R")
data_path <- "data/us"
# read data and merge
obs <- readRDS(file.path(data_path, "mort.rds")) |>
  filter(Race %in% c("White", "Black or African American"),
         `Hispanic Origin` == "Not Hispanic or Latino",
         Year > 2014) |>
  rename_with(tolower) |>
  mutate(
    race = ifelse(race == "White", "non-hispanic white",
                  "non-hispanic black")
  ) |>
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
    w = init_ill_opioid,
    x = opioid_deaths,
    y = tr2,
    p1_surveyed = tr1 / s1,
    p2_surveyed = tr2 /s2,
    u_per_thousand = deaths / population * 1000
  ) |>
  select(
    year, race, w, x, y,
    x_per_thousand, y_per_thousand, w_per_thousand,
    p1_surveyed, p2_surveyed, u_per_thousand,
    opioid_deaths, deaths, population
  ) |>
  mutate(
    race = ifelse(race == "non-hispanic black", 
                  "Non-Hispanic black",
                  "Non-Hispanic white")
  )
white_pop0 <- readRDS(file.path(data_path, "mort.rds")) |>
  filter(Race == "White",
         `Hispanic Origin` == "Not Hispanic or Latino",
         Year == 2014) |>
  pull(Population)
black_pop0 <- readRDS(file.path(data_path, "mort.rds")) |>
  filter(Race == "Black or African American",
         `Hispanic Origin` == "Not Hispanic or Latino",
         Year == 2014) |>
  pull(Population)

# read fitted objects and extract posteriors 
fit_black <- readRDS(file.path(data_path, "fit_black.rds"))
fit_white <- readRDS(file.path(data_path, "fit_white.rds"))
r1_posts <- rbind(
  get_post_intervals(
    fit_black, "r1_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "Non-Hispanic black"),
  get_post_intervals(
    fit_white, "r1_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "Non-Hispanic white")
) |>
  rename(year = time)
r2_posts <- rbind(
  get_post_intervals(
    fit_black, "r2_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "Non-Hispanic black"),
  get_post_intervals(
    fit_white, "r2_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "Non-Hispanic white")
) |>
  rename(year = time)
p1_posts <- rbind(
  get_post_intervals(fit_black, "p1_hat", time = 2015:2019) |>
    mutate(race = "Non-Hispanic black"),
  get_post_intervals(fit_white, "p1_hat", time = 2015:2019) |>
    mutate(race = "Non-Hispanic white")
) |>
  rename(year = time) |>
  left_join(
    obs |> select(year, race, p1_surveyed),
    by = c("year", "race")
  ) |>
  rename(observed = p1_surveyed)
p2_posts <- rbind(
  get_post_intervals(fit_black, "p2_hat", time = 2015:2019) |>
    mutate(race = "Non-Hispanic black"),
  get_post_intervals(fit_white, "p2_hat", time = 2015:2019) |>
    mutate(race = "Non-Hispanic white")
) |>
  rename(year = time) |>
  left_join(
    obs |> select(year, race, p2_surveyed),
    by = c("year", "race")
  ) |>
  rename(observed = p2_surveyed)
p3_posts <- rbind(
  get_post_intervals(fit_black, "p3_hat", time = 2015:2019) |>
    mutate(race = "Non-Hispanic black"),
  get_post_intervals(fit_white, "p3_hat", time = 2015:2019) |>
    mutate(race = "Non-Hispanic white")
) |>
  rename(year = time) 

x_posts <- rbind(
  get_post_intervals(fit_black, "x_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "Non-Hispanic black"),
  get_post_intervals(fit_white, "x_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "Non-Hispanic white")
) |>
  rename(year = time) |>
  left_join(
    obs |> select(year, race, x_per_thousand),
    by = c("year", "race")
  ) |>
  rename(observed = x_per_thousand)

y_posts <- rbind(
  get_post_intervals(fit_black, "y_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "Non-Hispanic black"),
  get_post_intervals(fit_white, "y_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "Non-Hispanic white")
) |>
  rename(year = time) |>
  left_join(
    obs |> select(year, race, y_per_thousand),
    by = c("year", "race")
  ) |>
  rename(observed = y_per_thousand)

w_posts <- rbind(
  get_post_intervals(fit_black, "w_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "Non-Hispanic black"),
  get_post_intervals(fit_white, "w_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "Non-Hispanic white")
) |>
  rename(year = time) |>
  left_join(
    obs |> select(year, race, w_per_thousand),
    by = c("year", "race")
  ) |>
  rename(observed = w_per_thousand)

pp_wxy <- get_posterior_predictive(fit_black, fit_white, 500) |>
  mutate(year = time + 2014) |>
  left_join(obs |> select(year, race, population),
            by = c("year", "race")) |>
  mutate(
    w_per_thousand = w / population * 1000,
    x_per_thousand = x / population * 1000,
    y_per_thousand = y / population * 1000
  )

# plot -------------------------------------------------------------------------
plot_path <- file.path("results", "man")
bg_color <- "#ffffff"
text_color <- "black"
line_color <- "black"
font_family <- "Times New Roman"

base_theme <- theme(
  legend.position = "none",
  rect = element_rect(fill = bg_color, colour = bg_color),
  panel.background = element_rect(fill = bg_color, colour = bg_color),
  panel.grid.major.y = element_line(colour = line_color, linewidth = .5),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  plot.title = element_blank(),
  plot.subtitle = element_blank(),
  axis.text = element_text(
    colour = text_color, size = 32, family = font_family
  ),
  axis.title.x = element_blank(),
  axis.text.y.right = element_text(
    margin = margin(0.1, 0, 0, 0, unit = "in")
  ),
  axis.title.y.right = element_text(
    colour = text_color, size = 32, family = font_family,
    margin = margin(0, 0, 0, .2, unit = "in")
  ),
  axis.line.x = element_line(colour = line_color),
  axis.line.y = element_blank(),
  axis.ticks = element_blank()
)

post_theme <- base_theme +
  theme(
    plot.margin = margin(l = 1, r = 0, t = 0.5, b = 0, unit = "in")
  )

pp_theme <- base_theme +
  theme(
    axis.text.x = element_text(hjust = 0),
    strip.background = element_blank(),
    strip.text = element_text(size = 32, family = font_family,
                              colour = text_color)
  )

colors <- c(`Non-Hispanic black` = "black", 
           `Non-Hispanic white` = "darkgrey",
           `prior` = "#404040") 
shapes <- c(`Non-Hispanic black` = 20, `Non-Hispanic white` = 20)

plot_posterior_over_time(r1_posts, prior = r1_prior,
                         # title = expression(italic(r^(1))),
                         # subtitle = "Prescription opioid initiates.",
                         cis = 95,
                         ylab = "(per 1 000)",
                         ytrans = "log10",
                         ybreaks = c(1, 10, 100, 1000),
                         obs_shape = 21,
                         est_size = 7,
                         colors = colors,
                         shapes = shapes,
                         plt_theme = post_theme,
                         file = file.path(plot_path, "r1.png")) 
plot_posterior_over_time(r2_posts, prior = r2_prior,
                         # title = expression(italic(r^(2))), 
                         # subtitle = "Direct illicit opioid initiates.",
                         cis = 95,
                         ylab = "(per 1 000)",
                         ytrans = "log10", 
                         ybreaks = c(0.01, 0.1, 1),
                         obs_shape = 21,
                         est_size = 7,
                         colors = colors,
                         shapes = shapes,
                         plt_theme = post_theme,
                         file = file.path(plot_path, "r2.png")) 
plot_posterior_over_time(p1_posts, prior = p1_prior,
                         obs = TRUE, obs_size = 4,
                         # title = expression(italic(p^(1))), 
                         # subtitle = "Prescription to illicit opioid use.",
                         cis = 95,
                         obs_shape = 21,
                         est_size = 7,
                         colors = colors,
                         shapes = shapes,
                         plt_theme = post_theme,
                         file = file.path(plot_path, "p1.png")) 
plot_posterior_over_time(p2_posts, prior = p2_prior,
                         obs = TRUE, obs_size = 4,
                         # title = expression(italic(p^(2))), 
                         # subtitle = "Illicit opioid use to treatment.",
                         cis = 95,
                         obs_shape = 21,
                         est_size = 7,
                         colors = colors,
                         shapes = shapes,
                         plt_theme = post_theme,
                         file = file.path(plot_path, "p2.png")) 
plot_posterior_over_time(p3_posts, prior = p3_prior,
                         # title = expression(italic(p^(3))), 
                         # subtitle = "Illicit opioid use to death.",
                         cis = 95,
                         obs_shape = 21,
                         est_size = 7,
                         colors = colors,
                         shapes = shapes,
                         plt_theme = post_theme,
                         file = file.path(plot_path, "p3.png")) 

plot_posterior_predictive(pp_wxy, obs, "w",
                          obs_size = 4,
                          # title = "W", 
                          ylab = "(per 1 000)",
                          ybreaks = c(6, 8),
                          # ylimits = c(5, 10),
                          plt_theme = pp_theme,
                          file = file.path(plot_path, "w.png"))
plot_posterior_predictive(pp_wxy, obs, "x",
                          obs_size = 4,
                          # title = "X", 
                          ylab = "(per 1 000)",
                          ybreaks = c(.05, .08),
                          plt_theme = pp_theme,
                          file = file.path(plot_path, "x.png"))
plot_posterior_predictive(pp_wxy, obs, "y",
                          obs_size = 4,
                          # title = "Y", 
                          ylab = "(per 1 000)",
                          ybreaks = c(5, 15),
                          plt_theme = pp_theme,
                          file = file.path(plot_path, "y.png"))

