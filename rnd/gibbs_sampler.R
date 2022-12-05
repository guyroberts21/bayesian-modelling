### Formative Assignment 3 - Gibbs Sampling Implementation
# gibbs_example: Code from Example 2.3.1
# gibbs_a: Q2a(ii) Gibbs sampler
# gibbs_b: Q2b(ii) Gibbs sampler

gibbs_example=function(N,rho) 
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

gibbs_a=function(N,rho) 
{
  mat=matrix(ncol=2,nrow=N)
  phi1=0
  phi2=0
  mat[1, ]=c(phi1,phi2)
  for (i in 2:N) 
  {
    phi1=rnorm(1,0,sqrt(1+rho))
    phi2=rnorm(1,0,sqrt(1-rho))
    mat[i, ]=c(phi1,phi2)
  }
  return(mat)
}

gibbs_b=function(N,rho) 
{
  mat=matrix(ncol=2,nrow=N)
  phi1=0
  phi2=0
  mat[1, ]=c(phi1,phi2)
  for (i in 2:N) 
  {
    phi1=rnorm(1,0,sqrt(1+rho))
    phi2=rnorm(1,0,sqrt(1-rho))
    # now obtain th1 and th2 from phi1 and phi2
    # using change of variable transformation
    th1 = (1/2)*sqrt(2)*(phi1+phi2)
    th2 = (1/2)*sqrt(2)*(phi1-phi2)
    mat[i, ]=c(th1,th2)
  }
  return(mat)
}


# modify this line to produce different plots 
# for comparison (eg. out = gibbs_a(1000, .99))
out=gibbs_example(1000,.99)

par(mfrow=c(3,2))
plot(out,col=1:1000,xlab="theta1",ylab="theta2")
plot(out,type="l",xlab="theta1",ylab="theta2")
plot(ts(out[,1]),xlab="Iteration",ylab="theta1")
plot(ts(out[,2]),xlab="Iteration",ylab="theta2")
hist(out[,1],40,freq=FALSE,main="",xlab="theta1")
hist(out[,2],40,freq=FALSE,main="",xlab="theta2")