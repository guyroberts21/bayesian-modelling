library(ggplot2)
library(MASS)

y = c(1,3,6,25,73,221,294,257,236,189,125,67,26,10,3)
plot(ts(y,start=0,deltat=1),main="",xlab="Time (days)",ylab="Yt")


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
    laprob = logprior(can) + loglike(can, data, dt, x0) - logprior(curr) - loglike(curr,data,dt,x0)
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

out = MH(N, start=0, delt1at=0.1=1000, data=y, dt=0.1, Sigma=diag(0.001,3,3), init=c(-6,log(0.5),2.5), x0=c(762,1))
plot(, start=0, deltat=0.1ts(ts)(out))

# Note: out = estimate of Var(theta)

#Rule of thumb:
SigmaOpt=(2.38^2)/3*var(out)
#Re-run
out2=MH(5000,y,0.1,SigmaOpt,c(-6,log(0.5),2.5),c(762,1))
plot(ts(out2))

out3 = exp(out2)
#Kernel density estimates (could also use histograms)
par(mfrow=c(1,3))
plot(density(out3[,1]),xlab="beta")
plot(density(out3[,2]),xlab="gamma")
plot(density(out3[,3]),xlab="sigma")
par(mfrow=c(1,1))

#Kernel density estimate of \pi(\beta,\gamma|\bmy)
dens=kde2d(out3[,1],out3[,2])
contour(dens)

#Summaries e.g. for beta (infection rate)
mean(out3[,1])
sd(out3[,1])
quantile(out3[,1],c(0.025,0.975))

N_pop=763
R0 = out3[,1]/out3[,2]*N_pop
hist(R0)
quantile(R0, c(0.025, 0.975))



N = 5000
endT = 14
dt = 0.1
S.mat = matrix(0,nrow=endT/dt+1,ncol=N)
for(i in 1:N)
{
  S.mat[,i] = odesim(exp(out2[i,]),dt,c(762,1),endT)[,1]
}


S.mean = apply(S.mat,1,mean)
S.lq = apply(S.mat,1,quantile,0.025)
S.uq = apply(S.mat,1,quantile,0.975)


plot(ts(S.mean, start=0, deltat=0.1), ylim=c(0,800))
lines(ts(S.lq, start=0, deltat=0.1), col=2)
lines(ts(S.uq, start=0, deltat=0.1), col=2)
