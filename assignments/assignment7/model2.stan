data {
  int<lower=0> N; // Num observations
  int<lower=0> x[N];
}

parameters {
  real<lower=0> lambda;
}

model {
  // Prior
  lambda ~ frechet(2, 4);
  x ~ poisson(lambda);
}

generated quantities {
  int x_prepost[N];
  
  for (n in 1:N) {
    x_prepost[n] = poisson_rng(lambda);
  }
}

