library(rstan)
d <- read.csv('data7a.csv')

data <- list(N=nrow(d), Y=d$y)
fit <- stan(file='chap10model_a.stan',data=data,seed=123)
fit

stan_trace(fit)
stan_dens(fit,separate_chains = T)
stan_plot(fit,show_density=T,point_est='median')

ms <- rstan::extract(fit)
summary(ms$beta)
summary(ms$sigma)        

source('../common.R')
