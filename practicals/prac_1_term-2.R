library(rstan)

data <- list(N = 3 + 10,
             J = 2,
             group = c(rep(1,3),
                       rep(2,10)),
             X = c(0,1,0,
                   2,0,0,1,2,0,0,1,1,0))

setwd('C:/Users/guyro/OneDrive/Y3/bayesian-modelling/practicals')
our_model = stan_model('stan_3.stan')

fit = sampling(our_model, data=data)
summary(fit)

samples = extract(fit, pars = c('lambda[1]', 'lambda[2]'))
length(samples$`lambda[1]`) # each parameter has 4000 entries
var(samples$`lambda[2]`)
