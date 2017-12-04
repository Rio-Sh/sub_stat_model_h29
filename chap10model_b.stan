data {
  int<lower=0> N_sample;       // number of observations
  int<lower=0> N_pot;          // number of pots
  int<lower=0> Y[N_sample];    // number of seeds
  int<lower=0> F[N_sample];    // fertilizer
  int<lower=0> Pot[N_sample];  // pot
}
parameters {
  real beta1;
  real beta2;
  real r[N_sample];
  real rp[N_pot];
  real <lower=0> sigma[2];
}
transformed parameters {
  real<lower=0> lambda[N_sample];

  for (i in 1:N_sample) {
    lambda[i] = exp(beta1 + beta2 * F[i] + r[i] + rp[Pot[i]]);
  }
}
model {
  for (i in 1:N_sample) {
    Y[i] ~ poisson(lambda[i]);
  }
  beta1 ~ normal(0, 100);
  beta2 ~ normal(0, 100);
  r ~ normal(0, sigma[1]);
  rp ~ normal(0, sigma[2]);
}