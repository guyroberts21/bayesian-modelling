data {
  int<lower=0> n;    // num observations
  real X[n];      // observed values
}
parameters {
  real mu;
}
model {
  // Prior
  mu ~ normal(0, 1);
  
  // Likelihood
  for (i in 1:n) {
    X[i] ~ normal(mu, 1);
  }
}
