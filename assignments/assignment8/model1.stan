data {
  int<lower=0> N;
  int<lower=0> h[N];
}

parameters {
  real<lower=0> lambda;
}

model {
  // Prior
  lambda ~ gamma(2, 2);
  
  // Likelihood
  for (i in 1:N) {
    h[i] ~ poisson(lambda);
  }
}

generated quantities {
  vector[N] log_lik;
  for (i in 1:N) 
    log_lik[i] = poisson_lpmf(h[i] | lambda); 
}
