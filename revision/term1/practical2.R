library(coda)

gibbs=function(N,rho) 
{
  mat = matrix(ncol=2,nrow=N)
  th1 = 0
  th2 = 0
  mat[1, ] = c(th1,th2)
  for (i in 2:N) 
  {
    th1 = rnorm(1,rho*th2,sqrt(1-rho^2))
    th2 = rnorm(1,rho*th1,sqrt(1-rho^2))
    mat[i, ]=c(th1,th2)
  }
  return(mat)
}

p = c(0, 0.7, 0.99)

out=gibbs(1000,0)
par(mfrow=c(3,2))
plot(out,col=1:1000,xlab="theta1",ylab="theta2")
plot(out,type="l",xlab="theta1",ylab="theta2")
plot(ts(out[,1]),xlab="Iteration",ylab="theta1")
plot(ts(out[,2]),xlab="Iteration",ylab="theta2")
hist(out[,1],40,freq=FALSE,main="",xlab="theta1")
hist(out[,2],40,freq=FALSE,main="",xlab="theta2")
par(mfrow=c(1,1))

effectiveSize(out)

y = c(
  4,5,4,1,0,4,3,4,0,6,3,3,4,0,2,6,3,3,5,4,5,3,1,4,4,
  1,5,5,3,4,2,5,2,2,3,4,2,1,3,2,1,1,1,1,1,3,0,0,1,0,
  1,1,0,0,3,1,0,3,2,2,0,1,1,1,0,1,0,1,0,0,0,2,1,0,0,
  0,1,1,0,2,2,3,1,1,2,1,1,1,1,2,4,2,0,0,0,1,4,0,0,0,
  1,0,0,0,0,0,1,0,0,1,0,0)

times = seq(1851, 1962, 1)
plot(
  times,
  y,
  lwd = 2,
  main = "Coal Disasters",
  xlab = "Year",
  ylab = "Number per year"
)

gibbs = function(N, data)
{
  n = length(data)
  theta = 1
  lambda = 1
  k = 112  #initial param values
  a1 = 1
  a2 = 1
  b1 = 1
  b2 = 1 #prior hyper-params
  probVec = rep(0, n)             #condiional pmf for k
  out = matrix(0, nrow = N, ncol = 3)  #store samples here
  
  out[1, ] = c(theta, lambda, k) #store initial sample
  
  for (i in 2:N)
  {
    #Update theta from FCD
    theta = rgamma(1, a1 + sum(data[1:k]), b1 + k)
    
    if (k==n) {
      lambda = rgamma(a2, b2)
    } else {
      #Update lambda from FCD
      lambda = rgamma(1, a2 + sum(data[(k + 1):n]), b2 + (n - k))
    }
    
    #calculate pmf for k
    for (j in 1:n)
    {
      probVec[j] = exp((lambda - theta) * j) * (theta / lambda) ** sum(data[1:j])
    }
    #Update k (Hint: k is discrete...use the sample command)
    k = sample(n, 1, prob=probVec)
      
      out[i, ] = c(theta, lambda, k)
  }
  return(out)
}

out = gibbs(5000, y)

plot(ts(out))












