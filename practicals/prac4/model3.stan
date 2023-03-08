data {
  int <lower=0> N; // num observations
  int <lower=0> M; // missing data
  real<lower=0> R[N-M];
}

parameters {
  real alpha;
  real beta;
  real <lower=0> R_miss[M];
}

model {
  // Priors
  alpha ~ gamma(5, 0.5);
  beta ~ uniform(0, 1);
  
  // Likelihood
  for (i in 1:N) {
    R[i] ~ gamma(alpha, beta);
  }
}
