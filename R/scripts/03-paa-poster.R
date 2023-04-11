source("R/functions/02-plot.R")
source("R/scripts/us/02-01-set-priors.R")
# data -------------------------------------------------------------------------
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
    p1_surveyed = tr1 / s1,
    p2_surveyed = tr2 /s2,
    u_per_thousand = deaths / population * 1000
  ) |>
  select(
    year, race, 
    x_per_thousand, y_per_thousand, w_per_thousand,
    p1_surveyed, p2_surveyed, u_per_thousand,
    opioid_deaths, deaths, population
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

# read fitted objects and extract poteriors 
fit_black <- readRDS(file.path(data_path, "fit_black.rds"))
fit_white <- readRDS(file.path(data_path, "fit_white.rds"))
r1_posts <- rbind(
  get_post_intervals(
    fit_black, "r1_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "non-hispanic black"),
  get_post_intervals(
    fit_white, "r1_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "non-hispanic white")
) |>
  rename(year = time)
r2_posts <- rbind(
  get_post_intervals(
    fit_black, "r2_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "non-hispanic black"),
  get_post_intervals(
    fit_white, "r2_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "non-hispanic white")
) |>
  rename(year = time)
p1_posts <- rbind(
  get_post_intervals(fit_black, "p1_hat", time = 2015:2019) |>
    mutate(race = "non-hispanic black"),
  get_post_intervals(fit_white, "p1_hat", time = 2015:2019) |>
    mutate(race = "non-hispanic white")
) |>
  rename(year = time) |>
  left_join(
    obs |> select(year, race, p1_surveyed),
    by = c("year", "race")
  ) |>
  rename(observed = p1_surveyed)
p2_posts <- rbind(
  get_post_intervals(fit_black, "p2_hat", time = 2015:2019) |>
    mutate(race = "non-hispanic black"),
  get_post_intervals(fit_white, "p2_hat", time = 2015:2019) |>
    mutate(race = "non-hispanic white")
) |>
  rename(year = time) |>
  left_join(
    obs |> select(year, race, p2_surveyed),
    by = c("year", "race")
  ) |>
  rename(observed = p2_surveyed)
p3_posts <- rbind(
  get_post_intervals(fit_black, "p3_hat", time = 2015:2019) |>
    mutate(race = "non-hispanic black"),
  get_post_intervals(fit_white, "p3_hat", time = 2015:2019) |>
    mutate(race = "non-hispanic white")
) |>
  rename(year = time) 

x_posts <- rbind(
  get_post_intervals(fit_black, "x_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "non-hispanic black"),
  get_post_intervals(fit_white, "x_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "non-hispanic white")
) |>
  rename(year = time) |>
  left_join(
    obs |> select(year, race, x_per_thousand),
    by = c("year", "race")
  ) |>
  rename(observed = x_per_thousand)

y_posts <- rbind(
  get_post_intervals(fit_black, "y_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "non-hispanic black"),
  get_post_intervals(fit_white, "y_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "non-hispanic white")
) |>
  rename(year = time) |>
  left_join(
    obs |> select(year, race, y_per_thousand),
    by = c("year", "race")
  ) |>
  rename(observed = y_per_thousand)

w_posts <- rbind(
  get_post_intervals(fit_black, "w_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "non-hispanic black"),
  get_post_intervals(fit_white, "w_hat_per_thousand", time = 2015:2019) |>
    mutate(race = "non-hispanic white")
) |>
  rename(year = time) |>
  left_join(
    obs |> select(year, race, w_per_thousand),
    by = c("year", "race")
  ) |>
  rename(observed = w_per_thousand)

# plot -------------------------------------------------------------------------
plot_path <- file.path("results", "paa-poster")
bg_color <- "#ffffff"
text_color <- "#404040"
line_color <- "#696969"
font_familiy <- "Montserrat"

post_theme <- theme(
  legend.position = "none",
  # rect = element_rect(fill = bg_color, colour = bg_color),
  # panel.background = element_rect(fill = bg_color, colour = bg_color), 
  panel.grid.major.y = element_line(colour = line_color, linewidth = .5),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  plot.title = element_text(
    colour = text_color, size = 40, hjust = 0,
    family = "DejaVu Serif", face = "italic" # use a math font
  ),
  plot.subtitle = element_text(
    colour = text_color, size = 24, hjust = 0,
    family = font_familiy
  ),
  axis.text = element_text(
    colour = text_color, size = 20, family = font_familiy
  ),
  axis.text.y.right = element_text(
    margin = margin(0.1, 0, 0, 0, unit = "in")
  ),
  axis.title.y.right = element_text(
    colour = text_color, size = 24, family = font_familiy,
    margin = margin(0, 0, 0, .2, unit = "in")
  ),
  axis.line.x = element_line(colour = line_color),
  axis.line.y = element_blank(),
  axis.ticks = element_blank(),
  plot.margin = margin(l = 1, r = 0, t = 0.5, b = 0, unit = "in")
)

plot_posterior_over_time(r1_posts, prior = r1_prior,
                         title = expression(italic(r^(1))),
                         subtitle = "Prescription opioid initiates.",
                         ylab = "(per 1 000)",
                         ytrans = "log10",
                         ybreaks = c(1, 10, 100, 1000),
                         plt_theme = post_theme,
                         file = file.path(plot_path, "r1.svg"),
                         device = "svg") 
plot_posterior_over_time(r2_posts, prior= r2_prior,
                         title = expression(italic(r^(2))), 
                         subtitle = "Direct illicit opioid initiates.",
                         ylab = "(per 1 000)",
                         ytrans = "log10", 
                         ybreaks = c(0.01, 0.1, 1),
                         plt_theme = post_theme,
                         file = file.path(plot_path, "r2.svg"),
                         device = "svg") 
plot_posterior_over_time(p1_posts, prior = p1_prior,
                         obs = TRUE, obs_shape = 23, obs_size = 4,
                         title = expression(italic(p^(1))), 
                         subtitle = "Prescription to illicit opioid use.",
                         plt_theme = post_theme,
                         file = file.path(plot_path, "p1.svg"),
                         device = "svg") 
plot_posterior_over_time(p2_posts, prior = p2_prior,
                         obs = TRUE, obs_shape = 23, obs_size = 4,
                         title = expression(italic(p^(2))), 
                         subtitle = "Illicit opioid use to treatment.",
                         plt_theme = post_theme,
                         file = file.path(plot_path, "p2.svg"),
                         device = "svg") 
plot_posterior_over_time(p3_posts, prior = p3_prior,
                         title = expression(italic(p^(3))), 
                         subtitle = "Illicit opioid use to death.",
                         plt_theme = post_theme,
                         file = file.path(plot_path, "p3.svg"),
                         device = "svg") 

plot_posterior_over_time(x_posts, 
                         obs = TRUE, obs_shape = 1, obs_size = 5,
                         title = "E[X]",
                         subtitle = "Mean opioid-induced deaths.",
                         ylab = "(per 1 000)",
                         ylimits = c(0, .1),
                         ybreaks = c(0, .04, .08),
                         plt_theme = post_theme,
                         file = file.path(plot_path, "x.svg"),
                         device = "svg") 
plot_posterior_over_time(y_posts, 
                         obs = TRUE, obs_shape = 1, obs_size = 5,
                         title = "E[Y]",
                         subtitle = "Mean drug treatment initiates.",
                         ylab = "(per 1 000)",
                         ylimits = c(0, 17),
                         ybreaks = c(0, 5, 10, 15),
                         plt_theme = post_theme,
                         file = file.path(plot_path, "y.svg"),
                         device = "svg") 
plot_posterior_over_time(w_posts,
                         obs = TRUE, obs_shape = 1, obs_size = 5,
                         title = "E[W]",
                         subtitle = "Mean illicit opioid initiates.",
                         ylab = "(per 1 000)",
                         ylimits = c(0, 10),
                         ybreaks = c(0, 4, 8),
                         plt_theme = post_theme,
                         file = file.path(plot_path, "w.svg"),
                         device = "svg") 
