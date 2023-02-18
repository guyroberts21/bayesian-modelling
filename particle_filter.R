## Bootstrap particle filter for tracking model
## Uses functions x0Sim and xtSim above

## Function to evaluate log p(y_t|x_t)

llike=function(y,x,theta)
{
  phi=theta[1]; sigma=theta[2]; kappa=theta[3]
  return(-0.5*(y-x[1])^2/kappa^2)
}

## Function to implement particle filter to track position
# N = number of samples OR number of particles
# mat = (s0 s1 ... st)
#       (.)
#       (.) 
#       (s0)          

BPF=function(N,theta,ydata)
{
  endT=length(ydata)
  # Store unweighted samples from filtering density for position here:
  mat=matrix(0,nrow=N,ncol=endT) 
  wts=rep(0,N) # Store weights here
  x=matrix(0,nrow=N,ncol=2) # Samples of current state X_t=(S_t,V_t)
  # Initialise (t=0)
  for(j in 1:N)
  {
    x[j,]=x0Sim(theta) #Sample prior
    wts[j]=llike(ydata[1],x[j,1],theta) # Weight
  }
  x=x[sample(1:N,N,TRUE,exp(wts)),] # Resample
  mat[,1]=x[,1] # Store position
  # Loop for times t=1,...,T
  for(i in 2:endT)
  {
    for(j in 1:N)
    {
      x[j,]=xtSim(x[j,],theta)  # Propagate
      wts[j]=llike(ydata[i],x[j,1],theta) # Weight
    }
    x=x[sample(1:N,N,TRUE,exp(wts)),] # Resample
    mat[,i]=x[,1] # Store position
  }
  return(mat)
}