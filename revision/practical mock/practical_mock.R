# =================
# ==== Q1 ////
# =================

mc = function(N) {
  # generate a draw of X
  X = rcauchy(N)
  
  total = 0
  for (i in 1:N) {
    if (X[i] > 2) {
      total = total + 1
    }
  }
  return(total/N)
}

# Test the function with N = 1000
p_hat = mc(1000)

# Check the true value of p
p_true = 1 - pcauchy(2)

mcMany = function(N, n) {
  out = vector(length=n)
  
  # independent realizations of p_hat
  for (i in 1:n) {
    out[i] = mc(N)
  }
  
  return(out)
}

# Estimate the mean and variance
vals = mcMany(500, 1000)
mean(vals)
var(vals)

# Compare to the true mean and variance of p_hat?
1-(mean(p_true) - mean(vals)) # very close!


# We do not need to check whether values of X_i are greater or smaller than a certain
# threshold since we are already computing the expected value of the normal distribution 
# based on its pdf.

imp=function(N)
{
  x <- 2/runif(N)
  return((1/N)*sum(x^2/(2*pi*(1+x^2))))
}

mcMany2 = function(N, n) {
  out = vector(length=n)
  
  for ( i in 1:n) {
    out[i] = imp(N)
  }
  
  return(out)
}

N <- 500
n <- 1000
p_hat_N <- mean(mcMany2(N, n))
var_p_hat_N <- var(mcMany2(N, n))

# compare with true mean and variance of pˆN
p_true_N <- 2/pi
var_p_true_N <- (4 - pi)/pi^2/N

# compute relative errors
rel_err_p <- abs(p_hat_N - p_true_N)/p_true_N
rel_err_var <- abs(var_p_hat_N - var_p_true_N)/var_p_true_N

rel_err_p
rel_err_var

# =================
# ==== Q2 //// (***)
# =================

#gamma(2, b)
# prior : b ~ Gamma(1,1)

# Inverse sampling
# 1. Generate N rand. variables from U(0, 1)
# 2. Calculate the corresponding z values using the inverse cdf of the truncated Gamma 
# distribution 
gammaT = function(N, sstar, beta) {
  u = runif(N)
  z = qgamma(u * (1 - pgamma(sstar, 2, beta)), 2, beta)
  return(z)
}

set.seed(3421)
data <- rgamma(100,2,0.5)
#Truncate anything bigger than 5
data[data>5] <- 5
data <- sort(data) #x=(x^o,x^c)

length(data[data<5]) # 66 precise obs

gibbs = function(N,x,sstar)
{
  a <- 1; b <- 1 #prior hyper-parameters
  n <- length(x)
  m <- length(x[x<5])
  x0 <- x[1:m] #observed survival times
  betaVec <- rep(0,N) #store beta samples here
  zMat <- matrix(0,nrow=N,ncol=(n-m)) #sore z samples here
  
  
  #Initialise (beta prior)
  beta <-  rgamma(1, 1)
  z <- rep(sstar,n-m)
  betaVec[1] <- beta; zMat[1,] <- z
  for(i in 2:N)
  {
    #update beta
    beta <- rgamma(1, a+2*n, b+sum(x[1:m]) + sum(z[1:(n-m)]))
    #update z (truncated distribution)
    z[x[(m+1):n] > sstar] = rgamma(sum(x[(m+1):n] > sstar), 2, beta)
    z[x[(m+1):n] <= sstar ] <- sstar + rgamma(sum(x[(m+1):n] <= sstar), 2, beta)
    #store
    betaVec[i] <- beta; zMat[i,] <- z
  }
  return(list(betaVec,zMat))
}

result = gibbs(N=5000, x=data, sstar=5)
beta_samples = result[[1]]
z_samples = result[[2]]

# Plot trace plots
par(mfrow = c(2, 1))
plot(beta_samples, type = "l", xlab = "Iteration", ylab = expression(beta))
for (i in 1:ncol(z_samples)) {
  plot(z_samples[, i], type = "l", xlab = "Iteration", ylab = expression(z[i]))
}

# Burn-in - remove 1000 iterations to be safe
burnin <- 1000
beta_samples <- beta_samples[(burnin + 1):length(beta_samples)]
z_samples <- z_samples[(burnin + 1):length]

# ??









