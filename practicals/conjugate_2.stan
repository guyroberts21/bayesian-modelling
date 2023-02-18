data {
  int<lower=0> n;            // num observations
  int<lower=0> y[n];      // observed values
}
parameters {
  real<lower=0> theta;
}
model {
  // Prior
  theta ~ beta(2, 7);
  
  // Likelihood
  for (i in 1:n) {
      y[i] ~ neg_binomial(5, theta/(1-theta));
  }
}
