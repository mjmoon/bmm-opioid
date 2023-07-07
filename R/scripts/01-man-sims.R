source("R/functions/02-plot.R")
# simulation study -------------------------------------------------------------
data_path <- file.path("data", "sim")
load(file.path(data_path, "params.RData"))
sigma_sigma_gamma <- 1
sigma_sigma_delta <- 1
sims <- readRDS(file.path(data_path, "sims.rds"))$counts |>
  mutate(time = 0:n, x = s4, y = s3) |>
  select(time, r1, r2, p1, p2, p3, x, y, w) |>
  tidyr::pivot_longer(r1:w, names_to = "var", values_to = "observed") 
surveyed <- readRDS(file.path(data_path, "surveyed.rds"))$counts |>
  slice(-c(1, n + 1)) |>
  mutate(time = 1:(n - 1), p1 = tr1 / s1, p2 = tr2 / s2) |> 
  select(time, p1, p2) |>
  tidyr::pivot_longer(p1:p2, names_to = "var", values_to = "surveyed") |>
  mutate(var = factor(var,
                      levels = c("p1", "p2"),
                      labels = c("p[i]^{(1)}",
                                 "p[i]^{(2)}")),
         ratio = "`With ratio estimates`")
# read fitted objects and extract posteriors 
fit_no_ratio <- readRDS(file.path(data_path, "fit-no-ratio-estimate.rds"))
fit_with_ratio <- readRDS(file.path(data_path, "fit-ratio-estimate.rds"))
posts <- rbind(
  rbind(
    get_post_intervals(fit_no_ratio, "r1_hat"),
    get_post_intervals(fit_no_ratio, "r2_hat"),
    get_post_intervals(fit_no_ratio, "p1_hat"),
    get_post_intervals(fit_no_ratio, "p2_hat"),
    get_post_intervals(fit_no_ratio, "p3_hat"),
    get_post_intervals(fit_no_ratio, "x_hat"),
    get_post_intervals(fit_no_ratio, "y_hat"),
    get_post_intervals(fit_no_ratio, "w_hat")
    ) |> mutate(ratio = "`Without ratio estimates`"),
  rbind(
    get_post_intervals(fit_with_ratio, "r1_hat"),
    get_post_intervals(fit_with_ratio, "r2_hat"),
    get_post_intervals(fit_with_ratio, "p1_hat"),
    get_post_intervals(fit_with_ratio, "p2_hat"),
    get_post_intervals(fit_with_ratio, "p3_hat"),
    get_post_intervals(fit_with_ratio, "x_hat"),
    get_post_intervals(fit_with_ratio, "y_hat"),
    get_post_intervals(fit_with_ratio, "w_hat")
  ) |> mutate(ratio = "`With ratio estimates`")
  ) |>
  left_join(sims, by = c("var", "time")) |>
  mutate(var = factor(var,
                      levels = c("r1", "r2", "p1", "p2", "p3", "x", "y", "w"),
                      labels = c("r[i]^{(1)}", 
                                 "r[i]^{(2)}",
                                 "p[i]^{(1)}",
                                 "p[i]^{(2)}",
                                 "p[i]^{(3)}",
                                 "x[i]", 
                                 "y[i]",
                                 "w[i]")))

# plot -------------------------------------------------------------------------
plot_path <- file.path("results", "man")
bg_color <- "#ffffff"
text_color <- "black"
line_color <- "black"
font_family <- "Times New Roman"
font_math <- "DejaVu Serif Italic"

axis_breaks <- function(x) {
  prec <- if_else(max(x) > 1, .0001, 1000)
  maxx <- floor(max(x) * prec) / prec
  minx <- if_else(max(x) > 1, 0, ceiling(min(x) * prec) / prec)
  return(c(minx, maxx))
}
axis_limits <- function(x) {
  brks <- axis_breaks(x)
  if (max(x) > 1)
    brks[1] <- -.05 * diff(brks)
  return(brks)
}

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
    colour = text_color, size = 20, family = font_family
  ),
  axis.title = element_blank(),
  axis.text.y.right = element_text(
    margin = margin(0.1, 0, 0, 0, unit = "in"), family = font_family
  ),
  axis.title.y.right = element_text(
    colour = text_color, size = 32, family = font_math,
    margin = margin(0, 0, 0, .2, unit = "in")
  ),
  axis.line.x = element_line(colour = line_color),
  axis.line.y = element_blank(),
  axis.ticks = element_blank(),
  panel.spacing.y = unit(1.5, "lines"),
  strip.background = element_blank(),
  strip.placement = "outside",
  strip.text = element_text(colour = text_color, size = 24, family = font_family),
  strip.text.y.left = element_text(
    family = font_math, angle = 0, size = 24)
)
ggplot(posts |> filter(!var %in% c("x[i]", "y[i]", "w[i]")), aes(x = time)) +
  base_theme +
  geom_linerange(aes(ymin = `2.5%`, ymax = `97.5%`)) +
  geom_point(aes(y = observed), shape = 21, fill = "white", size = 3) +
  geom_point(aes(y = `50%`), size = 2) +
  geom_point(aes(y = observed, alpha = `2.5%` > observed | `97.5%` < observed), 
             size = 5, shape = 22, fill = NA, show.legend = FALSE) +
  geom_point(aes(y = surveyed), data = surveyed, shape = 4, size = 3) +
  scale_alpha_manual(values = c(0, 1)) +
  scale_x_continuous(breaks = c(1, 10, 20)) +
  scale_y_continuous(expand = expansion(mult = .1),
                     labels = function(x) format(x, big.mark = " "),
                     breaks = axis_breaks) +
  facet_grid(vars(var), vars(ratio), labeller = label_parsed, 
             scales = "free_y", switch = "y")
# ggsave(file.path(plot_path, "sim-params.png"), device = "png", dpi = 450, 
#        width = 8, height = 6, unit = "in")
# save to png 1440 x 1080 to preserve the fonts

single_theme <- theme(
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
  axis.title = element_blank(),
  axis.line.x = element_blank(),
  axis.line.y = element_blank(),
  axis.ticks = element_blank(),
)
ggplot(posts |> filter(var == "x[i]"), aes(x = time, y = observed)) +
  single_theme +
  geom_line(linetype = "dotted", linewidth = 1.5) +
  geom_point(size = 4, shape = 21, fill = "white") +
  scale_x_continuous(breaks = c(1, 10, 20)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " "))
ggsave(file.path(plot_path, "sim-x.png"), device = "png", dpi = 450,
       width = 8, height = 6, unit = "in")
ggplot(posts |> filter(var == "y[i]"), aes(x = time, y = observed)) +
  single_theme +
  geom_line(linetype = "dotted", linewidth = 1.5) +
  geom_point(size = 4, shape = 21, fill = "white") +
  scale_x_continuous(breaks = c(1, 10, 20)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " "))
ggsave(file.path(plot_path, "sim-y.png"), device = "png", dpi = 450,
       width = 8, height = 6, unit = "in")
ggplot(posts |> filter(var == "w[i]"), aes(x = time, y = observed)) +
  single_theme +
  geom_line(linetype = "dotted", linewidth = 1.5) +
  geom_point(size = 4, shape = 21, fill = "white") +
  scale_x_continuous(breaks = c(1, 10, 20)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " "))
ggsave(file.path(plot_path, "sim-w.png"), device = "png", dpi = 450,
       width = 8, height = 6, unit = "in")

# sensitivity analysis results
axis_breaks_log <- function(x) {
  ndigit <- floor(log10(max(x)))
  if (ndigit > 4) {
    return(10^(3:5))
  } 
  return(10^(1:4))
}
fs <- dir(data_path, pattern = "(050|200).rds$", full.names = TRUE)
fs <- fs[!grepl("(gammadelta|r0p0)", fs)]
posts_sa <- lapply(fs, function(f) {
  fit_sa <- readRDS(f)
  tmp <- rbind(
    get_post_intervals(fit_sa, "r1_hat"),
    get_post_intervals(fit_sa, "r2_hat"),
    get_post_intervals(fit_sa, "p1_hat"),
    get_post_intervals(fit_sa, "p2_hat"),
    get_post_intervals(fit_sa, "p3_hat")
  ) |>
    mutate(
      param = stringr::str_extract(f, "(?<=sa-)(.+)(?=_)"), 
      adj = sub("_",  "", stringr::str_extract(f, "_\\d+"))
    )
}) |> 
  bind_rows() |>
  mutate(var = factor(var,
                      levels = c("r1", "r2", "p1", "p2", "p3"),
                      labels = c("r[i]^{(1)}", 
                                 "r[i]^{(2)}",
                                 "p[i]^{(1)}",
                                 "p[i]^{(2)}",
                                 "p[i]^{(3)}")),
         param = factor(param,
                        levels = c("nugamma", "nudelta", "r0", "p0"),
                        labels = c("(SA1)", "(SA2)", "(SA3)", "(SA4)")))
posts_sa |>
  filter(grepl("^r", var)) |>
  ggplot(aes(x = time, ymin = `2.5%`, ymax = `97.5%`)) +
  base_theme +
  geom_ribbon(fill = "grey90", colour = NA,
              data = posts |> filter(grepl("^r", var), 
                                     ratio == "`With ratio estimates`")) +
  geom_point(aes(y = observed), size = 3, shape = 21, fill = "white",
              data = posts |> filter(grepl("^r", var), 
                                     ratio == "`With ratio estimates`")) +
  geom_ribbon(aes(linetype = adj), fill = NA, colour = "black", linewidth = 1) +
  scale_linetype_manual(values = c(`050` = "dashed", `200` = "dotted")) +
  scale_x_continuous(breaks = c(1, 10, 20)) +
  scale_y_continuous(trans = "log",
                     expand = expansion(mult = .1),
                     labels = function(x) 
                       format(x, big.mark = " ", scientific = FALSE),
                     breaks = axis_breaks_log) +
  facet_grid(vars(var), vars(param), 
             scales = "free_y", switch = "y",
             labeller = label_parsed)
# save to png 1440 x 1080 to preserve the fonts
posts_sa |>
  filter(grepl("^p", var)) |>
  ggplot(aes(x = time, ymin = `2.5%`, ymax = `97.5%`)) +
  base_theme +
  geom_ribbon(fill = "grey90", colour = NA,
              data = posts |> filter(grepl("^p", var), 
                                     ratio == "`With ratio estimates`")) +
  geom_point(aes(y = observed), size = 3, shape = 21, fill = "white",
             data = posts |> filter(grepl("^p", var), 
                                    ratio == "`With ratio estimates`")) +
  geom_ribbon(aes(linetype = adj), fill = NA, colour = "black", linewidth = 1) +
  scale_linetype_manual(values = c(`050` = "dashed", `200` = "dotted")) +
  scale_x_continuous(breaks = c(1, 10, 20)) +
  scale_y_continuous(expand = expansion(mult = .1),
                     labels = function(x) 
                       format(x, big.mark = " ", scientific = FALSE),
                     breaks = axis_breaks) +
  facet_grid(vars(var), vars(param), 
             scales = "free_y", switch = "y",
             labeller = label_parsed)
# save to png 1440 x 1080 to preserve the fonts
