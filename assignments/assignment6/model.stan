data {
  int<lower=0> n; // num observations
  real X[n];     // observed values
}

parameters {
  real <lower=0> mu;
  real <lower=0> a;
  real <lower=0> b;
}

model {
  // Priors
  a ~ exponential(1);
  b ~ exponential(2);
  mu ~ beta(a, b);
  
  // Likelihood
  for (i in 1:n) {
    X[i] ~ normal(mu, sqrt(mu));
  }
}

