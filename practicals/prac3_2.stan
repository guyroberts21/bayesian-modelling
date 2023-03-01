data {
  int<lower=0> n;                   // num observations
  real<lower=0, upper=1> p[n];   // observed values
}
parameters {
  real<lower=1, upper=10> theta;
}
model {
  // Prior
  target += 3*log(theta) - theta + log(sin(theta) + 1.2);
  
  // Likelihood
  for (i in 1:n) {
      p[i] ~ beta(2, theta);
      //target += ;
  }
}
