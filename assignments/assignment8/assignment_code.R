# ===== Q1 ===== #
library(rstan)
library(loo)
library(ggplot2)
our_data = list(N = 20,
                h = c(1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0))
setwd(
  'C:/Users/guyro/Desktop/DU/Maths/Y3/bayesian-modelling/assignments/assignment8'
)

# Load the Stan models into R
model1 = stan_model('model1.stan')
model2 = stan_model('model2.stan')
model3 = stan_model('model3.stan')

# Sample from the posterior (fit the models)
fit1 = sampling(model1, our_data, iter = 10000)
fit2 = sampling(model2, our_data, iter = 10000)
fit3 = sampling(model3, our_data, iter = 10000)


# Extract the lambdas
lambda1 = extract(fit1)$lambda
lambda2 = extract(fit2)$lambda
lambda3 = extract(fit3)$lambda

# Plot the posterior samples
dat = data.frame(var = factor(rep(c(
  'lambda1', 'lambda2', 'lambda3'
), each=20000)),
values = c(lambda1, lambda2, lambda3))

ggplot(dat, aes(x = values, color=var)) + geom_density(size=1.15)

# Calculate pseudo-BMA weights
loo1 = loo(fit1)
loo2 = loo(fit2)
loo3 = loo(fit3)

loo2
loo3

# https://discourse.mc-stan.org/t/a-quick-note-what-i-infer-from-p-loo-and-pareto-k-values/3446
# NOTE: We get a warning about some Pareto k diagnostic values being slightly high - One value is in the range
# (0.5, 0.7] (ok) : which may indicate that the model is badly mis-specified (see link).

loo_model_weights(list(loo1, loo2, loo3),
                  method = "pseudobma",
                  BB = FALSE)

# => Considering the sample densities of the posterior (shown above), these results do match our expectations. The graph shows
# that models 1 and 2 are very similar, with mode ~ 0.4, whereas the mode for model 3 ~ 0.5 and this model has a lower spread than the
# others, and hence a higher peak. The results also suggest that model 1 is marginally more suitable (given the data) than model 2 - however,
# the dataset used is small, so perhaps more in-depth analysis is required in order to make an appropriate decision. 



