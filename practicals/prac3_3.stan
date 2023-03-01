data {
  int<lower=0> N;                    // num individuals
  int<lower=1> NK;                   // num groups
  int<lower=1> NJ[NK];               // num of subgroups per group
  int<lower=1,upper=NK> group[N];    // group for individual
  int<lower=1> subgroup[N];          // subgroup for individual
  int<lower=0> X[N];                 // observed random variables
}
transformed data {
  int<lower=1> max_subgroups; // max num of subgroups in a group
  
  max_subgroups = max(NJ);
}
parameters {
  real<lower=0, upper=1> alpha;
  real<lower=0> beta[NK];                        // rate by group
  real<lower=0> lambda[max_subgroups, NK];       // rate by subgroup and group
}
model {
  // Prior
  alpha ~ beta(2, 1);
  for (k in 1:NK)
      beta[k] ~ exponential(alpha);
  for (k in 1:NK) {
    for (j in 1:NJ[k])
      lambda[j,k] ~ exponential(beta[k]);
  }
  // Likelihood
  for (n in 1:N)
      X[n] ~ poisson(lambda[subgroup[n],group[n]]);
}
