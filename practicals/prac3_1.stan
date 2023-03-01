data {
  int<lower=0> n;    // num observations
  real X[n];      // observed values
}
parameters {
  real mu;
}
model {
  // Add log prior density to the target distribution
  target += -0.5 * (mu^2);
  
  // Add log likelihood to the target distribution
  for (i in 1:n) {
    target += -0.5 * (X[i]-mu)^2;
    //X[i] ~ normal(mu, 1);
  }
}
