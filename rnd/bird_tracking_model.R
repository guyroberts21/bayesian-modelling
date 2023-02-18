# Simulate initial condition
x0Sim=function(theta)
{
  phi=theta[1]; sigma=theta[2]; kappa=theta[3]
  return(c(0.5*rnorm(1),rt(1,df=5)*sigma/sqrt(1-phi^2)))
}

## Function to simulate X_t|X_{t-1}=x
xtSim=function(x,theta)
{
  phi=theta[1]; sigma=theta[2]; kappa=theta[3]
  xnew=c(x[1]+x[2],phi*x[2]+sigma*rt(1,df=5))
  return(xnew)
}

## Function to simulate Y_t|X_t=x
ytSim=function(x,theta)
{
  phi=theta[1]; sigma=theta[2]; kappa=theta[3]
  return(x[1]+kappa*rnorm(1))
}

## Function to simulate {X_t} and {Y_t} over [0,T]
modelSim=function(T,theta)
{
  Xmat=matrix(0,nrow=T+1,ncol=2)
  Yvec=rep(0,T+1)
  x=x0Sim(theta); y=ytSim(x,theta)
  Xmat[1,]=x; Yvec[1]=y
  for(i in 2:(T+1))
  {
    x=xtSim(x,theta); y=ytSim(x,theta)
    Xmat[i,]=x; Yvec[i]=y
  }
  return(list(Xmat,Yvec))
}

set.seed(5)
out=modelSim(50,c(0.9,0.5,1))

par(mfrow=c(1,2))
plot(ts(out[[1]][,2]),ylab="Vt",xlab="t")
plot(ts(out[[1]][,1]),ylab="St",xlab="t")
lines(ts(out[[2]]),type="p")



