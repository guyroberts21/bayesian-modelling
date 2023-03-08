data {
  int<lower=0> N; // num obs
  vector[N] x;    // obs explanatory
  vector[N] y;    // obs response
}

parameters {
  real beta;           // gradient
  real<lower=0> sigma; // error std.
}

model {
  // Priors
  target += -2 * log(beta); // prior for beta
  target += inv_gamma_lpdf(sigma | 0.1, 0.1); // prior on sigma^2
  
  // Likelihood
  target += normal_lpdf(y | beta * x, sigma);
}
