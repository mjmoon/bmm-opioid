################################################################################
#' Set simulation parameters.                                                 '#
#' - default parameters                                                       '#
################################################################################
n <- 20 # number of time periods
r0 <- c(10^4, 10^3) # initial rates
siggamma <- .5 # sigma for log rates
p0 <- c(0.05, 0.1, 0.005) # initial transition probabilities
sigdelta <- 0.05 # sigma for logit transition probabilities
u <- 0.02 # common mortality rates
s <- 0.05 # surveyed rate
save(
  n, r0, siggamma, p0, sigdelta, u, s,
  file = file.path("data", "sim", "params.RData")
)
rm(list = ls())
