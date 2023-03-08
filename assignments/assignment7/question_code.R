# Q1.3

# Understanding the pdf...
vals = seq(1, 100, 0.1)
n = length(vals)
out = rep(0, n)
for (i in 1:n) {
  x = vals[i]
  out[i] = x ^ (-2)
}
plot(vals, out, type = 'l') # create line plot of the distribution

# Stan model...
library(rstan)

our_data = list(
  N = 8,
  x = c(2.13, 4.32, 3.60, 0.19, 5.62, 2.86, 4.50, 1.95),
  y = c(4.02, 8.73, 7.33, 0.51, 12.09, 5.99, 8.91, 4.02)
)

setwd('C:/Users/guyro/Desktop/DU/Maths/Y3/bayesian-modelling/assignments/assignment7')

model = stan_model('model1.stan')
fit = sampling(model, our_data, chains=4, iter=10000, warmup=500, thin=1)

print(fit, pars=c("beta", "sigma"))

# Extract the posterior samples for beta and sigma
beta_samples = extract(fit)$beta
sigma_samples = extract(fit)$sigma
sigma2_samples = sigma_samples^2

hist(beta_samples)
hist(sigma2_samples)


# ===========

# Q2 /////

N = 3 # no. data points
x = c(1, 3, 2) # some random count data
our_data = list(N = N, x = x)

model2 = stan_model('model2.stan')
fit = sampling(model2, our_data, iter=10000)
samples = extract(fit, pars=c('x_prepost[1]', 'x_prepost[2]', 'x_prepost[3]'))
plot(density(samples$`x_prepost[1]`))





