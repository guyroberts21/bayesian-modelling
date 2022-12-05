MH = function(N = 1000, sigma = 1,x0 = 0)
{
  x = rep(0, N)
  x[1] = x0
  for (i in 2:N)
  {
    y = rnorm(1, x[i - 1], sigma)
    lalpha = abs(x[i - 1]) - abs(y)
    if (log(runif(1)) < lalpha)
    {
      x[i] = y
    } else{
      x[i] = x[i - 1]
    }
  }
  plot(x,main="values of x visited by the MH algorithm")
  return(x)
} 


target = function(x) {
  if (x < 0 || x > pi/2) {
    return(0)
  } else {
    return(sin(x))
  }
}

MH2 = function(N = 1000,x0 = 0)
{
  x = rep(0, N)
  x[1] = x0
  for (i in 2:N)
  {
    y = (8*x[i-1])/(pi^2) 
    alpha = target(y)/target(x[i-1])
    if (runif(1) < alpha)
    {
      x[i] = y
    } else{
      x[i] = x[i - 1]
    }
  }
  plot(x,main="values of x visited by the MH algorithm")
  return(x)
} 


#MH(N=1000, x0=pi/4)
#MH2(N=1000, x0=pi/4)

Q2a=function(N=1000,theta0=pi/4)
{
  theta=rep(0,N)  # Store theta values here
  theta[1]=theta0 # Initialise
  count=0 # count acceptances
  for(i in 2:N)
  {
    rand=runif(1,0,pi/2)
    can=(8*rand)/(pi^2) 
    aprob=sin(can)/sin(theta[i-1])
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

out=Q2a()


