## Example: Gibbs sampler for a bivariate normal

gibbs=function(N,rho) 
{
  mat=matrix(ncol=2,nrow=N)
  th1=0
  th2=0
  mat[1, ]=c(th1,th2)
  for (i in 2:N) 
  {
    th1=rnorm(1,rho*th2,sqrt(1-rho^2))
    th2=rnorm(1,rho*th1,sqrt(1-rho^2))
    mat[i, ]=c(th1,th2)
  }
  return(mat)
}

out=gibbs(1000,0.5)
par(mfrow=c(3,2))
plot(out,col=1:1000,xlab="theta1",ylab="theta2")
plot(out,type="l",xlab="theta1",ylab="theta2")
plot(ts(out[,1]),xlab="Iteration",ylab="theta1")
plot(ts(out[,2]),xlab="Iteration",ylab="theta2")
hist(out[,1],40,freq=FALSE,main="",xlab="theta1")
hist(out[,2],40,freq=FALSE,main="",xlab="theta2")