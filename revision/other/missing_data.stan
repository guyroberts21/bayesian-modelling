data {
  int<lower=0> N; // num obs
  int<lower=0> M; // num missing
  real r[N];      // observed values 
}

parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
  // model the missing data as a parameter
  real<lower=260> r_miss[M];
}

model {
  // priors
  alpha ~ gamma(5, 0.5);
  beta ~ uniform(0, 1);
  
  for (i in 1:N) {
    r[i] ~ gamma(alpha, beta);
  }
  for (i in 1:M) {
    r_miss[i] ~ gamma(alpha, beta);
  }
}


