library(bayesplot)
source("R/functions/02-plot.R")
# source("R/scripts/us/02-01-set-priors.R")
log_file <- file(paste0("logs/plot-us-black.log"), open = "a")
sink(log_file, type = "message")
timestamp <- format(Sys.time(), format = "%Y-%m-%d %H:%M")
message(paste0("----------", timestamp, "----------"))
# read fitted object
fit <- readRDS(file.path("data", "us", "fit_black.rds"))
post <- as.array(fit)
np <- nuts_params(fit)
lp <- log_posterior(fit)
# check diagnostics
rstan::check_hmc_diagnostics(fit)
# plot diagnostics
theme_set(theme_minimal())
color_scheme_set("viridisA")
plot_path <- file.path("results", "us", "black", "00-diagnostics-")
mcmc_nuts_divergence(np, lp)
ggsave(paste0(plot_path, "divergenc.png"))
plt_tree <- mcmc_nuts_treedepth(np, lp)
ggsave(paste0(plot_path, "treedepth.png"), plot = plt_tree)
# trace plots
mcmc_trace(post, np = np, regex_pars = "x_hat_per", 
          facet_args = list(nrow = 5))
ggsave(paste0(plot_path, "trace-x.png"))
mcmc_trace(post, np = np, regex_pars = "y_hat_per",
          facet_args = list(nrow = 5))
ggsave(paste0(plot_path, "trace-y.png"))
mcmc_trace(post, np = np, regex_pars = "w_hat_per",
          facet_args = list(nrow = 5))
ggsave(paste0(plot_path, "trace-w.png"))
mcmc_trace(post, np = np, regex_pars = "r1_hat_per",
          transformations = "log",
          facet_args = list(nrow = 5))
ggsave(paste0(plot_path, "trace-r1.png"))
mcmc_trace(post, np = np, regex_pars = "r2_hat_per",
          transformations = "log",
          facet_args = list(nrow = 5))
ggsave(paste0(plot_path, "trace-r2.png"))
mcmc_trace(post, np = np, regex_pars = "p1_hat",
          transformations = function(x) 1 / (1 + exp(-x)),
          facet_args = list(nrow = 5))
ggsave(paste0(plot_path, "trace-p1.png"))
mcmc_trace(post, np = np, regex_pars = "p2_hat",
          transformations = function(x) 1 / (1 + exp(-x)),
          facet_args = list(nrow = 5))
ggsave(paste0(plot_path, "trace-p2.png"))
mcmc_trace(post, np = np, regex_pars = "p3_hat",
          transformations = function(x) 1 / (1 + exp(-x)),
          facet_args = list(nrow = 5))
ggsave(paste0(plot_path, "trace-p3.png"))
mcmc_trace(post, np = np, regex_pars = "sig",
          facet_args = list(nrow = 2))
ggsave(paste0(plot_path, "trace-sigmas.png"))
mcmc_trace(post, np = np, regex_pars = "phi",
          facet_args = list(nrow = 2))
ggsave(paste0(plot_path, "trace-phis.png"))
mcmc_trace(post, np = np, regex_pars = "init_s",
          facet_args = list(nrow = 2))
ggsave(paste0(plot_path, "trace-initial-state-ps.png"))
# pair plots
plt_pair <- mcmc_pairs(post, lp = lp, np = np, max_treedepth = 16,
                      regex_pars = c("sig", "phi"))
ggsave(paste0(plot_path, "pairs-sigmas-phis.png"), plot = plt_pair)
plt_pair <- mcmc_pairs(post, lp = lp, np = np, max_treedepth = 16,
                      transformations = list(
                        `r1_hat_per_thousand[1]` = "log", 
                        `r2_hat_per_thousand[1]` = "log", 
                        `p1_hat[1]` =  function(x) 1 / (1 + exp(-x)),
                        `p2_hat[1]` =  function(x) 1 / (1 + exp(-x)),
                        `p3_hat[1]` =  function(x) 1 / (1 + exp(-x)),
                        init_s1_logitp = "identity", 
                        init_s2_logitp = "identity"
                      ),
                      pars = c(
                        "r1_hat_per_thousand[1]",
                        "r2_hat_per_thousand[1]",
                        "p1_hat[1]",
                        "p2_hat[1]",
                        "p3_hat[1]",
                        "init_s1_logitp",
                        "init_s2_logitp"
                      ))
ggsave(paste0(plot_path, "pairs-initial-values.png"), plot = plt_pair)           
plt_pair <- mcmc_pairs(post, lp = lp, np = np, max_treedepth = 16,
                      transformations = list(
                        siggamma = "identity",
                        `r1_hat_per_thousand[1]` = "log", 
                        `r2_hat_per_thousand[1]` = "log"
                      ),
                      pars = c(
                        "siggamma", 
                        "r1_hat_per_thousand[1]", 
                        "r2_hat_per_thousand[1]"
                      ))
ggsave(paste0(plot_path, "pairs-r-siggamma.png"), plot = plt_pair)
plt_pair <- mcmc_pairs(post, lp = lp, np = np, max_treedepth = 16,
                      transformations = list(
                        sigdelta = "identity",
                        `p1_hat[1]` =  function(x) 1 / (1 + exp(-x)),
                        `p2_hat[1]` =  function(x) 1 / (1 + exp(-x)),
                        `p3_hat[1]` =  function(x) 1 / (1 + exp(-x))
                      ),
                      pars = c(
                        "sigdelta", 
                        "p1_hat[1]",
                        "p2_hat[1]",
                        "p3_hat[1]"
                      ))
ggsave(paste0(plot_path, "pairs-p-sigdelta.png"), plot = plt_pair)
# parallel coordinate plots
mcmc_parcoord(post, np = np, transformations = "log",
             regex_pars = "r1_hat_per")
ggsave(paste0(plot_path, "parcoord-r1.png"))
mcmc_parcoord(post, np = np, transformations = "log",
             regex_pars = "r2_hat_per")
ggsave(paste0(plot_path, "parcoord-r2.png"))
mcmc_parcoord(post, np = np, 
             transformations = function(x) 1 / (1 + exp(-x)),
             regex_pars = "p1_hat")
ggsave(paste0(plot_path, "parcoord-p1.png"))
mcmc_parcoord(post, np = np, 
             transformations = function(x) 1 / (1 + exp(-x)),
             regex_pars = "p2_hat")
ggsave(paste0(plot_path, "parcoord-p2.png"))
mcmc_parcoord(post, np = np, 
             transformations = function(x) 1 / (1 + exp(-x)),
             regex_pars = "p3_hat")
ggsave(paste0(plot_path, "parcoord-p3.png"))
mcmc_parcoord(post, np = np, regex_pars = "x_hat_per")
ggsave(paste0(plot_path, "parcoord-x.png"))
mcmc_parcoord(post, np = np, regex_pars = "y_hat_per")
ggsave(paste0(plot_path, "parcoord-y.png"))
mcmc_parcoord(post, np = np, regex_pars = "w_hat_per")
ggsave(paste0(plot_path, "parcoord-w.png"))
message(paste0("----------", timestamp, "----------"))
message(paste0("------------------------------------"))
sink(NULL, type = "message")
rm(list = ls())
