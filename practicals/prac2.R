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


# Note: with rho small, we have low correlation (0 = none) (0.99 = lots) so there is good mixing and with
# high correlation there is poor mixing
out=gibbs(1000,0)
effectiveSize(out)


par(mfrow=c(3,2))
plot(out,col=1:1000,xlab="theta1",ylab="theta2")
plot(out,type="l",xlab="theta1",ylab="theta2")
plot(ts(out[,1]),xlab="Iteration",ylab="theta1")
plot(ts(out[,2]),xlab="Iteration",ylab="theta2")
hist(out[,1],40,freq=FALSE,main="",xlab="theta1")
hist(out[,2],40,freq=FALSE,main="",xlab="theta2")
par(mfrow=c(1,1))


rbivnorm=function(N=1000,rho=0.99)
{
  mat = matrix(ncol=2,nrow=N)
  mat[,1] = rnorm(N)
  mat[,2] = rnorm(N,rho*mat[,1],sqrt(1-rho^2))
  return(mat)
}



rbivnorm2=function(N=1000,rho=0.99)
{
  V = matrix(c(1,rho,rho,1),nrow=2)
  A = t(chol(V))
  Z = matrix(rnorm(2*N),nrow=2)
  return(t(A%*%Z))
}



#Example usage and output
out = rbivnorm()
par(mfrow=c(1,3))
plot(out,col=1:1000,xlab="theta1",ylab="theta2")
hist(out[,1],40,freq=FALSE,main="",xlab="theta1")
lines(seq(-4,4,0.001),dnorm(seq(-4,4,0.001))
hist(out[,2],40,freq=FALSE,main="",xlab="theta2")
lines(seq(-4,4,0.001),dnorm(seq(-4,4,0.001))
par(mfrow=c(1,1))

