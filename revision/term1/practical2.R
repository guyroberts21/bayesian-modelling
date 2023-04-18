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





