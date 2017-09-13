setwd("./stan")

library(RcppEigen)
library(Rcpp)
library(rstan)
lookup(dnorm)

schools_data <- list(
  J = 8,
  y = c(28,  8, -3,  7, -1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
)

fit1 <- stan(
  file = "schools.stan",  # Stan program
  data = schools_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 2,              # number of cores (using 2 just for the vignette)
  refresh = 1000          # show progress every 'refresh' iterations
)

print(fit1, pars=c("theta", "mu", "tau", "lp__"), probs=c(.1,.5,.9))
plot(fit1)
traceplot(fit1, pars = c("mu", "tau"), inc_warmup = TRUE, nrow = 2)
print(fit1, pars = c("mu", "tau"))

# all chains combined
sampler_params <- get_sampler_params(fit1, inc_warmup = TRUE)
summary(do.call(rbind, sampler_params), digits = 2)

# each chain separately
lapply(sampler_params, summary, digits = 2)

pairs(fit1, pars = c("mu", "tau", "lp__"), las = 1)


model_code <-
'
functions {
real standard_normal_rng() {
return normal_rng(0,1);
}
}
model {}
'
expose_stan_functions(stanc(model_code = model_code))
standard_normal_rng(seed = 1)


ocode <- "
  data {
int<lower=1> N;
real y[N];
} 
parameters {
real mu;
} 
model {
target += normal_lpdf(y | mu, 1);
} 
"

sm <- stan_model(model_code = ocode)
y2 <- rnorm(20)
mean(y2)
optimizing(sm, data = list(y = y2, N = length(y2)), hessian = TRUE)


(y <- read.table('https://raw.github.com/wiki/stan-dev/rstan/rats.txt', header = TRUE))
x <- c(8, 15, 22, 29, 36)
(xbar <- mean(x))
(N <- nrow(y))
(T <- ncol(y))
# rats_fit <- stan(file = 'https://raw.githubusercontent.com/stan-dev/example-models/master/bugs_examples/vol1/rats/rats.stan')
rats_fit <- stan(file = 'rats.stan')









