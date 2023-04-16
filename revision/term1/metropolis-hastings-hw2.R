Q2b=function(N=1000,theta0=pi/4)
{
  theta=rep(0,N)  # Store theta values here
  theta[1]=theta0 # Initialise
  count=0 # count acceptances
  for(i in 2:N)
  {
    can=runif(1,0,pi/2)
    aprob=sin(can)*theta[i-1]/(sin(theta[i-1])*can)
    if(runif(1)<aprob)
    {
      theta[i]=can # Store candidate value
      count=count+1
    }else{
      theta[i]=theta[i-1] # Else store current value
    }
  }
  print(count/(N-1))
  return(theta)
} 

out=Q2b()

## 0.7237237 = empirical acceptance rate