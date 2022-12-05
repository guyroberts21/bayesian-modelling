library(ggplot2)
library(MASS)

y = c(1,3,6,25,73,221,294,257,236,189,125,67,26,10,3)
df = data.frame(day = c(0:14), infectives = c(1,3,6,25,73,221,294,257,236,189,125,67,26,10,3))
p = ggplot(df, aes(x=day, y=infectives)) + geom_line()
p # plot time series of the number of infectives




# param = (B, y)'
# dt = timestep
# x0 = initial value
# endT = max timestep
odesim=function(param,dt,x0,endT)
{
  n = endT/dt
  x = matrix(0,nrow=n+1,ncol=2)
  x[1,] = x0 
  for (i in 2:(n+1)) 
  {
    #right hand side of ODE system
    RHS1 = -param[1]*x[i-1,1]*x[i-1,2]
    RHS2 = param[1]*x[i-1,1]*x[i-1,2]-param[2]*x[i-1,2]
    #time step ODE system
    x[i,] = x[i-1,] + c(RHS1,RHS2)*dt
  }
  return(x)
}

loglike=function(theta,data,dt,x0)
{
  n = length(data)
  incr = 1/dt
  out = odesim(exp(theta),dt,x0,n-1)
  out = out[1+(0:(n-1))*incr,2] #assume obs are infecteds
  return(sum(dnorm(data,out,exp(theta[3]),log=TRUE)))
}

logprior = function(theta) {
  return(sum(dnorm(theta, log=TRUE)))
}

MH=function(N,data,dt,Sigma,init,x0)
{
  theta.mat = matrix(0,nrow=N,ncol=3)
  theta.mat[1,] = init
  curr = init
  count = 0
  for (j in 2:N) 
  {
    can = mvrnorm(1,curr,Sigma) #proposal 
    #Evaluate log acceptance probability
    curr.star = theta.mat[j,]
    a = logprior(curr.star)-loglike(curr.star, data, dt, x0)
    b = logprior(curr)-loglike(curr, data, dt, x0)
    laprob =    
      if (log(runif(1)) < laprob) #Accept with the correct prob.
      {
        curr = can; #chain moves
        count = count + 1 #add one to acceptance counter
      }
    theta.mat[j,] = curr
  }
  print(count/(N-1)) # empirical acceptance prob
  return(theta.mat)
}

out = MH(N=1000, data=df, dt=0.1, Sigma=diag(0.001,3,3), init=c(-6,log(0.5),2.5), x0=c(762,1))
hist(out)







