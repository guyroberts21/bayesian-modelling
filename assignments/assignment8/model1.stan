data {
  int<lower=0> N;
  vector[N] h;
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
