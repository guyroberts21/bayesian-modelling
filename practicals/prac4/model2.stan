data {
  int<lower=0> N;
  int<lower=1> x[N, 3];
  int<lower=0, upper=1> y[N];
}
parameters {
  real alpha;
  row_vector[3] beta;
}
model {
  // Need to reformat data
  vector[3] x_;
  
  // Prior
  alpha ~ normal(0, 100);
  for (k in 1:3) 
    beta[k] ~ normal(0, 100);

  // Likelihood
  for (i in 1:N){
      x_ = to_vector(x[i,]); // convert row vector to column
      y[i] ~ bernoulli_logit(alpha + beta * x_);
  }
}
