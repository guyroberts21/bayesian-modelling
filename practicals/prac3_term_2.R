library(rstan)
setwd('C:/Users/guyro/OneDrive/Y3/bayesian-modelling/practicals')

# ==== 2.1 WRITING YOUR OWN LOG-DENSITY ==== #
model1 = stan_model('prac3_1.stan')
our_sample = sampling(model1, data = list(n=7, X = c(1.20, 1.21, 3.06, 7.89, 5.67, 6.10, 3.90)))
summary(our_sample)
mu_sample <- extract(our_sample,
                     pars = 'mu')$`mu`
hist(mu_sample)

# ==== 2.2 A BIZARRE PRIOR DENSITY ==== #
our_data <- list(n = 20,
                 p = c(0.25057883,0.16862872,0.11989827,0.28149519,0.20427907,
                       0.16859187,0.40253736,0.09341611,0.14762340,0.14047014,
                       0.29998209,0.19349593,0.21179227,0.24900885,0.32570937,
                       0.12341203,0.20488021,0.33726469,0.08214418,0.41775598))

# Looking at the prior density...
vals = seq(1, 10, 0.1)
n = length(vals)
out = rep(0, n)
for (i in 1:n) {
  x = vals[i]
  out[i] = x^3*exp(-x)*(sin(x)+1.2)
}
plot(vals, out, type='l') # create line plot of the distribution

# Sampling from the posterior using model
model2 = stan_model('prac3_2.stan')
sample = sampling(model2, data=our_data)
theta_sample = extract(sample, pars='theta')$`theta`
hist(theta_sample)

# ==== MULTI-LEVEL HIERARCHICAL MODEL ====

hie_data <- list(N = 40,
                 NK = 2,
                 NJ = c(2,2),
                 group = c(rep(1,30),
                           rep(2,10)),
                 subgroup = c(sort(rep(1:2,15)),
                              sort(rep(1:2,5))),
                 X = c(3,2,2,2,1,
                       2,3,4,0,0,
                       2,5,4,4,1,
                       3,3,3,4,4,
                       2,5,3,4,4,
                       1,1,3,3,5,
                       1,0,1,1,0,
                       1,0,1,2,0))

model3 = stan_model('prac3_3.stan')
sample = sampling(model3, data=hie_data)
lamda_sample = extract(sample, pars=c('group', 'subgroup'))




