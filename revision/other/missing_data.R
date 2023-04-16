library(rstan)
rain_data <- data.frame(year = sort(rep(2016:2022,3)),
                        sunday_number = rep(1:3,7),
                        rainfall = c( 99.78906,174.15984,144.26242,
                                      156.17152,174.89812,228.20058,
                                      144.66969,201.70483,254.25715,
                                      252.56507,168.98985,156.84697,
                                      169.79339,227.39160,253.90171,
                                      88.87051,200.98870,232.28434,
                                      129.17346,137.15685,204.28012))

setwd('C:/Users/guyro/OneDrive/Y3/bayesian-modelling/revision')
model = stan_model('missing_data.stan')

my_data = list(N=21, M=5, r=rain_data$rainfall)
samples = sampling(model, data=my_data, iter=10000)
rain_extract = extract(samples)

alpha = extract(samples)$`alpha`
beta = extract(samples)$`beta`

plot(density(alpha))
plot(density(beta))

# some plotting code...
library(ggplot2)
ggplot(data.frame(alpha = rain_extract$alpha,
                  beta = rain_extract$beta),
       aes(x=alpha, y=beta)) +
  geom_bin2d() +
  xlab(expression(alpha)) + 
  ylab(expression(beta))
