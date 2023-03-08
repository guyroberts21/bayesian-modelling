data {
  int<lower=0> n;    // num observations
  real x[n];      // observed values
}
parameters {
  real mu;
}
model {
  // Prior
  mu ~ normal(0, 1);
  
  // Likelihood
  for (i in 1:n) {
      x[i] ~ normal(0, 1) T[mu,];
  }
}
