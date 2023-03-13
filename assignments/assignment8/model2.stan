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
    target += binomial_lpmf(h[i] | N, N/lambda);
    //h[i] ~ binomial(100, 100/lambda);
  }
}
