setwd()#set your path

load('d.RData')
fit <- glm(y ~ x, family = poisson, data= d)
fit

library(rstan)
N <- nrow(d)
data <- list(X=d$x, Y=d$y, N = N, mu_X = mean(d$x))
fit_stan <- stan(file = 'chap9model.stan', data=data, seed=123,
                 iter=1600, warmup=100, thin=1, chains=3)
#stanでは自己相関が低いためthin=1でOK(らしい)
fit_stan
summary(fit_stan)

#収束診断の図とか色々
library(ggmcmc)
#ggmacmcでpdfに出力
ggmcmc(ggs(fit_stan), file='chap9result.pdf',
           plot=c('traceplot','density','running','autocorreration'))

#stan_で色々表示
stan_trace(fit_stan,pars=c('beta_1','beta_2'))#トレースプロット
stan_hist(fit_stan)#事後分布のヒストグラム
stan_dens(fit_stan,separate_chains=T)#事後分布の密度推定
stan_plot(fit_stan,#各事後分布の表示
          point_est='mean',#平均の点推定値
          show_density=T,#密度推定の分布表示
          ci_level=0.95)#確信区間
stan_ac(fit_stan)#自己相関
stan_rhat(fit_stan)

#stan_plotにつかった値の代表値の抽出
stan_plot(fit_stan,point_est='mean',show_density=T,ci_level=0.95)$data

#fit_stanから@~でデータを取り出す
fit_stan@stan_args 

#ブラウザ上で収束診断とか色々
shinystan::launch_shinystan(fit_stan)

#図9.6
ms <- rstan::extract(fit_stan)

add.mean <- function(bb1, bb2, lty = 2, lwd = 1, ...) {
  lines(d$x, exp(bb1 + bb2 * (d$x - mean(d$x))), lty = lty, lwd = lwd, ...)
}
plot(d$x, d$y, type = "n", xlab = "x_i", ylab = "y_i")
for (i in 1:length(ms$beta_1)) {
  add.mean(ms$beta_1[i], ms$beta_2[i], lty = 1, col = "#ff000010")
}
points(d$x, d$y)
add.mean(median(ms$beta_1), median(ms$beta_2), lty = 1, lwd = 2)

plot(ms$beta_1,ms$beta_2,lty = 1, col = "#0000ff30",pch = 16, cex = 2)
