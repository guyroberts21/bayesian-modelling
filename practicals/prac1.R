# PART 1
norm=function(N,a, init) 
{
  vec = rep(0,N)
  theta = init
  count = 0
  vec[1] = theta
  for (i in 2:N) 
  {
    innov = runif(1,-a,a)
    can = theta + innov
    laprob = dnorm(can,log=TRUE)-dnorm(theta,log=TRUE)
    u = runif(1)
    if (log(u) < laprob){ 
      theta = can
      count = count + 1
    }
    vec[i] = theta
  }
  # overall acceptance rate
  print(count/(N-1))
  return(vec)
}

#Run for 10k iters with a=1:

# normvec=norm(100000,3.8756) # ~ 3.875 to get 40% acceptance rate
#par(mfrow=c(2,1))
#plot(ts(normvec))
#hist(normvec,30)
#par(mfrow=c(1,1))

# histogram overlaying the standard
# par(mfrow=c(2,1))
#hist(normvec, 30, col=rainbow(25), freq=FALSE)
#s = seq(-6, 6, 0.001)
#lines(s, dnorm(s), type="l", pch=12)

# 1000 iterations with 40% acceptance rate, and theta_0 = 100
normvec1 = norm(1000, 3.8756, 100)
normvec2 = norm(1000, 3.8756, 50)
normvec3 = norm(1000, 3.8756, 10)
normvec4 = norm(1000, 3.8756, 10000)

par(mfrow=c(2,2))
plot(ts(normvec1))
plot(ts(normvec2))
plot(ts(normvec3))
plot(ts(normvec4))
par(mfrow=c(1,1))

# PART 2

gammaMH=function(N,a,b) 
{
  mu = a/b; sig = sqrt(a/(b*b))
  vec = rep(0,N)
  theta = a/b
  vec[1] = theta
  for (i in 2:N) 
  {
    can = rnorm(1,mu,sig)
    laprob1 = dgamma(can,a,b,log=TRUE)-dgamma(theta,a,b,log=TRUE)
    laprob2 = dnorm(theta,mu,sig,log=TRUE)-dnorm(can,mu,sig,log=TRUE)
    laprob = laprob1+laprob2
    u = runif(1)
    if (log(u) < laprob){ 
      theta = can
    } 
    vec[i] = theta
  }
  return(vec)
}

vec=gammaMH(10000,0.1,0.01)
par(mfrow=c(2,1))
plot(ts(vec))
hist(vec,30)
par(mfrow=c(1,1))

a=0.1;b=0.01
s = seq(0, 150, 0.1)
plot(s, dnorm(s, a/b, sqrt(a/(b*b))), type="l", col="red")
lines(s, dgamma(s,a,b), type="l", col="blue")






