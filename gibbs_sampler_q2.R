# Gibbs sampling scheme that implements problem from 
# Formative Q2a
gibbs = function(N, rho)
{
  mat = matrix(ncol = 2, nrow = N)
  phi1 = 0
  phi2 = 0
  mat[1,] = c(phi1, phi2)
  for (i in 2:N)
  {
    phi1 = rnorm(1, 0, sqrt(1 + rho))
    phi2 = rnorm(1, 0, sqrt(1 - rho))
    mat[i,] = c(phi1, phi2)
  }
  return(mat)
}

out = gibbs(1000, 0.01)
par(mfrow=c(3,2))
plot(out,col=1:1000,xlab="theta1",ylab="theta2")
plot(out,type="l",xlab="theta1",ylab="theta2")
plot(ts(out[,1]),xlab="Iteration",ylab="theta1")
plot(ts(out[,2]),xlab="Iteration",ylab="theta2")
hist(out[,1],40,freq=FALSE,main="",xlab="theta1")
hist(out[,2],40,freq=FALSE,main="",xlab="theta2")
