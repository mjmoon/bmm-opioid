library(bayesplot)
source("R/functions/02-plot.R")
# read fitted object
fit <- readRDS(file.path("data", "sim", "fit-no-ratio-estimate.rds"))
post <- as.array(fit)
np <- nuts_params(fit)
lp <- log_posterior(fit)
# check diagnostics
rstan::check_hmc_diagnostics(fit)
# plot diagnostics
theme_set(theme_minimal())
color_scheme_set("viridisB")
plot_path <- file.path("results", "sim", "no-ratio-estimate", "00-diagnostics-")
mcmc_nuts_divergence(np, lp)
ggsave(paste0(plot_path, "divergenc.png"))
plt_tree <- mcmc_nuts_treedepth(np, lp)
ggsave(paste0(plot_path, "treedepth.png"), plot = plt_tree)
# trace plots
mcmc_trace(post, np = np, regex_pars = "x_hat", facet_args = list(nrow = 5))
ggsave(paste0(plot_path, "trace-x.png"))
mcmc_trace(post, np = np, regex_pars = "y_hat", facet_args = list(nrow = 5))
ggsave(paste0(plot_path, "trace-y.png"))
mcmc_trace(post, np = np, regex_pars = "w_hat", facet_args = list(nrow = 5))
ggsave(paste0(plot_path, "trace-w.png"))
mcmc_trace(post, np = np, regex_pars = "r1_hat",
           transformations = "log", facet_args = list(nrow = 5))
ggsave(paste0(plot_path, "trace-r1.png"))
mcmc_trace(post, np = np, regex_pars = "r2_hat",
           transformations = "log", facet_args = list(nrow = 5))
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
# mcmc_trace(post, np = np, regex_pars = "phi",
#            facet_args = list(nrow = 2))
# ggsave(paste0(plot_path, "trace-phis.png"))
# pair plots
# plt_pair <- mcmc_pairs(post, lp = lp, np = np, max_treedepth = 12,
#                        regex_pars = c("sig", "phi"))
# ggsave(paste0(plot_path, "pairs-sigmas-phis.png"), plot = plt_pair)
plt_pair <- mcmc_pairs(post, lp = lp, np = np, max_treedepth = 12,
                       transformations = list(
                         `r1_hat[1]` = "log", 
                         `r2_hat[1]` = "log", 
                         `p1_hat[1]` =  function(x) 1 / (1 + exp(-x)),
                         `p2_hat[1]` =  function(x) 1 / (1 + exp(-x)),
                         `p3_hat[1]` =  function(x) 1 / (1 + exp(-x))
                       ),
                       pars = c(
                         "r1_hat[1]",
                         "r2_hat[1]",
                         "p1_hat[1]",
                         "p2_hat[1]",
                         "p3_hat[1]"
                       ))
ggsave(paste0(plot_path, "pairs-initial-values.png"), plot = plt_pair)
plt_pair <- mcmc_pairs(post, lp = lp, np = np, max_treedepth = 12,
                       transformations = list(
                         siggamma = "identity",
                         `r1_hat[1]` = "log", 
                         `r2_hat[1]` = "log"
                       ),
                       pars = c(
                         "siggamma", 
                         "r1_hat[1]", 
                         "r2_hat[1]"
                       ))
ggsave(paste0(plot_path, "pairs-r-siggamma.png"), plot = plt_pair)
plt_pair <- mcmc_pairs(post, lp = lp, np = np, max_treedepth = 12,
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
              regex_pars = "r1_hat")
ggsave(paste0(plot_path, "parcoord-r1.png"))
mcmc_parcoord(post, np = np, transformations = "log",
              regex_pars = "r2_hat")
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
mcmc_parcoord(post, np = np, regex_pars = "x_hat")
ggsave(paste0(plot_path, "parcoord-x.png"))
mcmc_parcoord(post, np = np, regex_pars = "y_hat")
ggsave(paste0(plot_path, "parcoord-y.png"))
mcmc_parcoord(post, np = np, regex_pars = "w_hat")
ggsave(paste0(plot_path, "parcoord-w.png"))
rm(list = ls())
