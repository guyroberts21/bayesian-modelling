data {
  int<lower=0> N;                 // num individuals
  int<lower=1> J;                 // num groups
  int<lower=1,upper=J> group[N];  // group for individual
  int<lower=0> X[N];              // observed random variables
}
parameters {
  real<lower=0> beta;         // hyperparameter
  real<lower=0> lambda[J];    // rate by group
}
model {
  // Prior
  beta ~ gamma(1, 1);
  for (j in 1:J)
      lambda[j] ~ gamma(1, beta);
  
  // Likelihood
  for (n in 1:N)
      X[n] ~ poisson(lambda[group[n]]);
}
