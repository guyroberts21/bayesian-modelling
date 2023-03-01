# PART 1
compound_dist = function(N, alpha) {
  return(beta(alpha+1, N+1) / beta(alpha, 1))
}

a = compound_dist(2, 0.4349)
b = compound_dist(1, 0.4349)
c = compound_dist(0, 0.4349)
a+b+c


# PART 2
library(rstan)
setwd('C:/Users/guyro/Desktop/DU/Maths/Y3/bayesian-modelling/assignments/assignment6')

# Compile model
model = stan_model('model.stan')

# Data
X = c(3.12, 0.75, 5.46, 0.80, 3.42,
      3.11, 1.99, 2.86, 4.33, 2.98)
n = length(X)



# Sample from posterior
posterior <- sampling(model, data = list(X = X, n = n), iter = 10000, chains = 4)

# Extract posterior samples
mu_samples <- extract(posterior)$mu

# Summary statistics
summary(mu_samples)

# Histogram
hist(mu_samples, breaks = 30)

