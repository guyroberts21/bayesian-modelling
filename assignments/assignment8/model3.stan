data {
  int<lower=0> N;
  vector[N] h;
}

parameters {
  real<lower=0> lambda;
}

model {
  // Prior
  lambda ~ gamma(2, 2);
  
  // Likelihood
  for (i in 1:N) {
    h[i] ~ normal(lambda, (lambda*(100-lambda))/100);
  }
}

generated quantities {
  vector[N] log_lik;
  for (i in 1:N) 
    log_lik[i] = normal_lpdf(h[i] | lambda, (lambda*(100-lambda))/100); 
}
