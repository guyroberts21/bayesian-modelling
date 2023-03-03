library(rstan)

setwd('C:/Users/guyro/Desktop/DU/Maths/Y3/bayesian-modelling/lecture-stuff/chap10/')
model = stan_model('model1.stan')
fit <- sampling(model,
                data = list(N = 12,
                            x1 = c(5.3,5.1,4.8,4.5,
                                   5.5,5.2,5.0,5.0,
                                   5.1,4.6,4.3,5.3)),
                iter = 10000)
x = extract(fit)$'lp__'
density = extract(fit)$'x1'
plot(samples)
