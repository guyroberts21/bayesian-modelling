library(rstan)
setwd('C:/Users/guyro/OneDrive/Y3/bayesian-modelling/practicals/prac4')


## TRUNCATED MODEL ====
our_model <- stan_model('model1.stan')
our_sample <- sampling(our_model,
                       data = list(n=5, x=c(2.3,1.0,0.4,1.1,0.6)), iter=10000)
  
# print summary
summary(our_sample)$summary
# extract mean from sample
mu_sample = extract(our_sample, pars='mu')$mu

plot(our_sample)
traceplot(our_sample)


## LOGISTIC REGRESSION ====
x <- matrix(c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,
              1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,
              1,2,3,1,2,3,1,2,3,1,2,3,1,2,3),nrow = 15,ncol = 3)
colnames(x) <- c('x_1','x_2','x_3')
y <- c(0,0,0,1,0,0,0,1,1,1,0,1,0,1,1)

# plot the correlation within variables (explanatory)
pairs(x)
# add the response to plot
pairs(cbind(x, y))

# Approximate the posterior mode
our_model = stan_model('model2.stan')
our_sample = sampling(our_model, data = list(N=15, x=x, y=y))

summary(our_sample)
betas = extract(our_sample, pars=c('beta[1]', 'beta[2]', 'beta[3]'))
mean(betas$`beta[1]`)
mean(betas$`beta[2]`)
mean(betas$`beta[3]`)

hist(betas$`beta[1]`)
hist(betas$`beta[2]`)
hist(betas$`beta[3]`)


## CENSORED DATA =====
rain_data <- data.frame(year = sort(rep(2016:2022,3)),
                        sunday_number = rep(1:3,7),
                        rainfall = c( 99.78906,174.15984,144.26242,
                                      156.17152,174.89812,228.20058,
                                      144.66969,201.70483,254.25715,
                                      252.56507,168.98985,156.84697,
                                      169.79339,227.39160,253.90171,
                                      88.87051,200.98870,232.28434,
                                      129.17346,137.15685,204.28012))
our_model = stan_model('model3.stan')
our_sample = sampling(our_model, data=list(N = 21, R = rain_data$rainfall))
summary(our_sample)

# 

