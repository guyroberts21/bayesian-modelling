/*data {
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
}*/

data {
  int<lower=0> N; 
  real x[N];
  real y[N];
}

parameters {
  real beta;
  real<lower=0> sigma_2;
  real epsilon;
}

model {
  // Prior
  target += -2 * log(beta);
  target += inv_gamma_lpdf(sigma_2 | 0.1, 0.1);
  
  // Likelihood
  for (i in 1:N) {
    y[i] = beta*x[i] + 
  }
  
}