data {
  int<lower=0> N; // num obs
}

parameters {
  real<lower=0> alpha;          // rate param for exp distribution
  real<lower=0> nu;             // df param for chisq distribution
  real<lower=0> beta;           // scale parameter for MB distribution
  real<lower=0,upper=1> p1;     // exp distribution (mixing prop.)
  real<lower=0,upper=1-p1> p2;  // chisq distribution (mixing prop.)
}

model {
  // Priors
  alpha ~ gamma(1, 1);
  nu ~ gamma(0.5*nu, 0.5);
  beta ~ gamma(2, 1);
}

generated quantities {
  real<lower=0> y_sim[N];
  
  for (n in 1:N) {
    if (uniform_rng(0, 1) < p1) {
      // exponential
      y_sim[n] = exponential_rng(alpha);
    } else if (uniform_rng(0, 1) < p1 + p2) {
      // chi-squared
      y_sim[n] = chi_square_rng(nu);
    } else {
      // Maxwell-Boltzmann
      real z = normal_rng(0, 1);
      real y = sqrt(2 / (pi() * beta)) * exp(-z^2 / 2);
      y_sim[n] = y^2; // squared since we are given sqrt(Y) ~ MB
    }
  }
}
