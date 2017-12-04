library(rstan)
d <- read.csv('d1.csv')
d

plot(d$id, d$y,pch=paste0(d$pot))
lines(x = c(1, 50), y = c(mean(d$y[d$f == 'C']), mean(d$y[d$f == 'C'])),lty=2)
lines(x = c(51, 100), y = c(mean(d$y[d$f == 'T']), mean(d$y[d$f == 'T'])),lty=2)

ggpubr::ggboxplot(d,'pot','y',color = 'f')


N_pot <- length(levels(d$pot))
Pot <- as.numeric(d$pot)
F <- as.numeric(d$f == "T")

data <- list(N_sample=nrow(d), N_pot=N_pot, Y=d$y, F=F, Pot=Pot)
fit <- stan(file = 'chap10model_b.stan', data=data)
fit

stan_plot(fit,show_density=T)
stan_dens(fit, pars=c('sigma[1]','sigma[2]'))
stan_trace(fit,pars=c('beta1','beta2','sigma[1]','sigma[2]'))
