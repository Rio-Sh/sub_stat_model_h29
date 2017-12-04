data {
  int N;
  real mu_X;
  real<lower=0> X[N];
  int<lower=0> Y[N];
}

parameters {
  real beta_1;
  real beta_2;
}

transformed parameters {
  real lambda[N];
  for (n in 1:N)
    lambda[n] = exp(beta_1 + beta_2*(X[n]-mu_X));
}

model {
  for (n in 1:N)
    Y[n] ~ poisson(lambda[n]);
}
//poisson(exp(x))に相当するpoisson_log(x)の方が推奨