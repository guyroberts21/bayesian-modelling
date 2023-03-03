data {
  int<lower=0> N; // num obs
  real x[N];       // obs explanatory
  real y[N];      // obs response
}

parameters {
  real epsilon;          // intercept
  real beta;             // gradient
  real<lower=0> sigma_2; // error var.
}

functions {
  real inv_square_pdf(real x) {
    return x^(-2);
  }
}

model {
  // Prior
  epsilon ~ normal(0, 10000);
  beta ~ inv_square_pdf(1);
  sigma_2 ~ inv_gamma(0.1, 0.1);
  
  // Likelihood
  for (i in 1:N)
    y[i] = beta*x[i] + epsilon;
}
