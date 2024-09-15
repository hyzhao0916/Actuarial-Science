rm(list = ls()) # clear memory
cat("\f")       # clear screen

fprimitive_1a = function(x){
  0.25*(x^4) - 0.5*(x^2) + x
}

library(pracma)
a = 0         # lower integral limit
b = 1         # upper integral limit
N = 16         # number of subintervals
h = (b-a)/N   # width of the subiterval
x = seq(a,b,h)
f = function(x){
  x^3-x+1
}
area_exact  = rep(0,N)
area_approx = rep(0,N)
error       = rep(0,N)
cat(sprintf('                      Area           Area\n'))
cat(sprintf('Interval:             Exact:         Approximation:     Error:\n'))
cat(sprintf('==============================================================\n'))
for(i in 1:N){
  area_exact[i] = fprimitive_1a(a+i*h) - fprimitive_1a(a+(i-1)*h)        # calculate the exact area using primitive_1a
  area_approx[i] = 0.5*(abs(f(a+(i-1)*h)-f(a+i*h))*((a+i*h) - (a+(i-1)*h) )) + (f(0.5)*0.5)     # % calculate the approximate area using the Trapezium 
  error[i] <- area_exact[i]-area_approx[i]
  cat(sprintf('(%d): (%5.2f,%.2f)   %12.8f   %12.8f   %12.8f\n',
              i,a+(i-1)*h,a+i*h,area_exact[i],area_approx[i],error[i]))
}
cat(sprintf('==============================================================\n'))
cat(sprintf('Total:              %12.8f   %12.8f   %12.8f\n',sum(area_exact),sum(area_approx),sum(error)))
no_obs <- length(x)        # total number of x-values = N+1
w <- rep(1, no_obs)
for(i in 1:length(w)){
  if(i != 1 & i != length(w)){
    cat(i, "\n")
    w[i] = 2
  }
}
area_approx2 <- h/2*sum(w*f(x))
cat(sprintf('Check: h/2*sum(w.*f)=%.8f\n',area_approx2))

#1b ----------------------------------------------------------------------------
rm(list = ls()) # clear memory
cat("\f")       # clear screen

fprimitive_1b = function(x){
  return(sqrt(2*pi)*pnorm(x))
}
errorw = function(N){
  library(pracma)
  a = 1         # lower integral limit
  b = 2        # upper integral limit
  #N = 2        # number of subintervals
  h = (b-a)/N   # width of the subiterval
  x = seq(a,b,h)
  g = function(x){
    return(exp(-0.5*(x^2)))
  }
  area_exact  = rep(0,N)
  area_approx = rep(0,N)
  error       = rep(0,N)
  cat(sprintf('                      Area           Area\n'))
  cat(sprintf('Interval:             Exact:         Approximation:     Error:\n'))
  cat(sprintf('==============================================================\n'))
  for(i in 1:N){
    area_exact[i] = fprimitive_1b(a+i*h) - fprimitive_1b(a+(i-1)*h)        # calculate the exact area using primitive_1a
    area_approx[i] = 0.5*(abs(g(a+(i-1)*h)-g(a+i*h))*((a+i*h) - (a+(i-1)*h) )) + (g(a+i*h)*((a+i*h) - (a+(i-1)*h) ))     # % calculate the approximate area using the Trapezium 
    error[i] <- area_exact[i]-area_approx[i]
    cat(sprintf('(%d): (%5.2f,%.2f)   %12.8f   %12.8f   %12.8f\n',
                i,a+(i-1)*h,a+i*h,area_exact[i],area_approx[i],error[i]))
  }
  cat(sprintf('==============================================================\n'))
  cat(sprintf('Total:              %12.8f   %12.8f   %12.8f\n',sum(area_exact),sum(area_approx),sum(error)))
  no_obs <- length(x)        # total number of x-values = N+1
  w <- rep(1, no_obs)
  for(i in 1:length(w)){
    if(i != 1 & i != length(w)){
      cat(i, "\n")
      w[i] = 2
    }
  }
  area_approx2 <- h/2*sum(w*g(x))
  cat(sprintf('Check: h/2*sum(w.*g)=%.8f\n',area_approx2))
  cat("\n")
  cat(sprintf("For N = %d  the error = %4.6f", N, sum(error)))
}
errorw(16)

#2 ---------------------------------------------------------------
