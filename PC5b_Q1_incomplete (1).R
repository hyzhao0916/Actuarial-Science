rm(list = ls()) # clear memory
cat("\f")       # clear screen

# put here the function primitive_1a


library(pracma)
a <- 0         # lower integral limit
b <- 1         # upper integral limit
N <- 2         # number of subintervals
h <- (b-a)/N   # width of the subiterval
x <- seq(a,b,h)
f <- x^3-x+1
area_exact  <- rep(0,N)
area_approx <- rep(0,N)
error       <- rep(0,N)
cat(sprintf('                      Area           Area\n'))
cat(sprintf('Interval:             Exact:         Approximation:     Error:\n'))
cat(sprintf('==============================================================\n'))
for(i in 1:N){
  area_exact[i] <-          # calculate the exact area using primitive_1a
  area_approx[i] <-         # % calculate the approximate area using the Trapezium 
  error[i] <- area_exact[i]-area_approx[i]
  cat(sprintf('(%d): (%5.2f,%.2f)   %12.8f   %12.8f   %12.8f\n',
              i,a+(i-1)*h,a+i*h,area_exact[i],area_approx[i],error[i]))
}
cat(sprintf('==============================================================\n'))
cat(sprintf('Total:              %12.8f   %12.8f   %12.8f\n',sum(area_exact),sum(area_approx),sum(error)))
no_obs <- length(x)        # total number of x-values = N+1
w <- rep(1,no_obs)
                           # set interior weights to 2
area_approx2 <- h/2*sum(w*f)
cat(sprintf('Check: h/2*sum(w*f)=%.8f\n',area_approx2))