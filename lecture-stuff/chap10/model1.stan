data {
  int<lower=0> N;
  real x1[N];
}

parameters {
  real mu[2];
  real alpha;
  real<lower=0> tau;
  real<lower=0> omega;
}

model {
  // Prior
  tau ~ gamma(10,1);
  alpha ~ normal(0,100);
  omega ~ gamma(10,1);
  mu ~ normal(alpha,sqrt(1/omega));
  
  // Likelihood
  for (i in 1:N){
    x1[i] ~ normal(mu[1],sqrt(1/tau));
  }
}

generated quantities {
  real x2;
  x2 = normal_rng(mu[2],sqrt(1/tau));
}
