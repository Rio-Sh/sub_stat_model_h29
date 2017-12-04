data{
  int N;
  int <lower=0> Y[N];
}

parameters{
  real beta;
  real<lower=0> sigma;
  real r[N];
}

transformed parameters{
  real<lower=0> q[N];
  for (i in 1:N)
    q[i] = inv_logit(beta + r[i]);
}

model{
  r ~ normal(0,sigma);
  for (i in 1:N)
    Y[i] ~ binomial(8,q[i]);
}
