# Q1 - Random Walk Metropolis

norm = function(N, a, theta = 0)
{
  vec = rep(0, N)
  vec[1] = theta
  count = 0 # count the num of accepted candidates
  for (i in 2:N)
  {
    innov = runif(1, -a, a)
    can = theta + innov
    laprob = dnorm(can, log = TRUE) - dnorm(theta, log = TRUE)
    u = runif(1)
    if (log(u) < laprob) {
      theta = can
      count = count + 1
    }
    vec[i] = theta
  }
  print(count / (N - 1))
  return(vec)
}

#Run for 10k iters with a=1:

# When the uniform proposal is too broad (eg. between -100000 and 100000) then
# basically no samples will be accepted, so the chain will be "stuck" at 0. Similarly,
# NOTE: a horizontal section in the trace plot of MCMC samples ("stuck") is a common
# indication of poor mixing in the Markov chain - This usually i120, 0.08, legend=c("Targetn, "Proposaldi, col=c("redc, "blueates that the
# Markov chain has reached a region of low probability density or a local mode
# of the distribution so is unable to move away from it due to a low acceptance rate
# (or an inappropriate proposal).
# With good mixing, the trace plot should exhibit a random and oscillatory behavior around
# the true value of the parameter.

# Note: when using alpha << 1, most candidates will be accepted
# In general: high acceptance probabilities mean that the chain is too "cold" and
# low acceptance probabilities mean that the chain is too "hot"

normvec = norm(1000, 3.887, 100)
par(mfrow = c(2, 1))
plot(ts(normvec))
hist(normvec, 30, freq = FALSE)
lines(density(normvec), col = "red")
par(mfrow = c(1, 1))



plot(ts(normvec), ylim = c(-10, 10))
for (i in 1:20)
{
  lines(ts(norm(1000, 4, runif(1, -10, 10))), col = (i + 1))
}


gammaMH = function(N, a, b)
{
  mu = a / b
  sig = sqrt(a / (b * b))
  vec = rep(0, N)
  theta = a / b
  vec[1] = theta
  for (i in 2:N)
  {
    can = rnorm(1, mu, sig)
    laprob1 = dgamma(can, a, b, log = TRUE) - dgamma(theta, a, b, log =
                                                       TRUE)
    laprob2 = dnorm(theta, mu, sig, log = TRUE) - dnorm(can, mu, sig, log =
                                                          TRUE)
    laprob = laprob1 + laprob2
    u = runif(1)
    if (log(u) < laprob) {
      theta = can
    }
    vec[i] = theta
  }
  return(vec)
}

vec = gammaMH(10000, 2.3, 2.7)
par(mfrow = c(2, 1))
plot(ts(vec))
hist(vec, 30)
par(mfrow = c(1, 1))


gammaMH = function(N, a, b)
{
  mu = a / b
  sig = sqrt(a / (b * b))
  mat = matrix(0, nrow = N, ncol = 3) #add extra columns to store \log\pi and \log q
  theta = a / b
  mat[1, ] = c(theta,
               dgamma(theta, a, b, log = TRUE),
               dnorm(theta, mu, sig, log = TRUE))
  for (i in 2:N)
  {
    can = rnorm(1, mu, sig)
    laprob1 = dgamma(can, a, b, log = TRUE) - dgamma(theta, a, b, log =
                                                       TRUE)
    laprob2 = dnorm(theta, mu, sig, log = TRUE) - dnorm(can, mu, sig, log =
                                                          TRUE)
    laprob = laprob1 + laprob2
    u = runif(1)
    if (log(u) < laprob) {
      theta = can
    }
    mat[i, ] = c(theta,
                 dgamma(theta, a, b, log = TRUE),
                 dnorm(theta, mu, sig, log = TRUE))
  }
  return(mat)
}

mat = gammaMH(10000, 0.1, 0.01)
#Trace plots
par(mfrow = c(3, 1))
plot(ts(mat[, 1]), ylab = "theta")
plot(ts(mat[, 2]), ylab = "log pi theta")
plot(ts(mat[, 3]), ylab = "log q theta")
par(mfrow = c(1, 1))

# Plot the proposal against the target
a = 0.1
b = 0.01
vals = seq(0, 150, 0.1)
plot(vals, dnorm(vals, a / b, sqrt(a / (b * b))), type = "l", col = 2)
lines(vals, dgamma(vals, a, b), type = "l")


# Comments
# 1. When the chain gets stuck at values of "theta" around 0, the corresponding pi(theta)
# values are large. This suggests that the q(theta)/pi(theta) part in the acceptance
# probability is very small, which in turn leads to a very small acceptance probability.
# Therefore, we get several (tens of) iterations for which the chain doesn't move
# 2. The chain additionally gets stuck at large values of "theta" due to the small values


lpost = function(psi)
{
  lprior = dnorm(psi, log = TRUE)
  llike = -25 * psi - 0.5 * 0.0305 / exp(2 * psi)
  return(lprior + llike)
}

norm2 = function(N, a, lambda)
{
  vec = rep(0, N)
  vec[1] = theta
  count = 0 # count the num of accepted candidates
  for (i in 2:N)
  {
    innov = runif(1, -a, a)
    can = theta + innov
    laprob = dnorm(can, log = TRUE) - dnorm(theta, log = TRUE)
    u = runif(1)
    if (log(u) < laprob) {
      theta = can
      count = count + 1
    }
    vec[i] = theta
  }
  print(count / (N - 1))
  return(vec)
}

