###############################################################################
#PC LAB 4A --------------------------------------------------------------------
#1
f = function(x){
  return(x/(1 + x^2))
}

g = function(x){
  return(tan(x))
}

x = seq(-pi/2, pi/2, length.out = 100)

plot(x, f(g(x)), xlim = c(-pi/2, pi/2), ylim = c(-0.5, 0.5), col = 4, type = "l", lty = 2)
lines(x, g(f(x)),  lty = 3, col = 2)
legend("bottomright", legend = c("f(g(x)", "g(f(x))"), col = c(4, 2), lty = c(2, 3))

#2
N = 3
cat(sprintf("x:        %10.8f   %10.8f   %10.8f   %10.8f \n", x[1], x[2], x[3], x[4]))
cat(sprintf("cos(x):   %10.8f   %10.8f   %10.8f   %10.8f \n", cos(x[1]), cos(x[2]),cos(x[3]),cos(x[4])))
y = 0
for(i in 0:N){
  y = y + (-1)^i*x^(2*i)/factorial(2*i)
  #print(y)
  if(i >= 1){
    cat(sprintf("N=%d:    %10.8f   %10.8f   %10.8f   %10.8f \n", i, y[1], y[2], y[3], y[4]))
  }
}

#3
N = 3
x = seq(-4,4,0.05)
y = 0
plot(x, cos(x), col = 1, lty = 1, type = "l", lwd = 2)
for(i in 0:N){
  y = y + (-1)^i*x^(2*i)/factorial(2*i)
  if(i >= 1){
    lines(x,y, lty = i+1, col = i+1, lwd = 2)
  }
}

#4
for(i in 1:4){
  y = 0
  for(j in 1:(i+1)){
    y = y + j^i
  }
  cat(sprintf("Sum for p=%d is %d \n", i, y))
}

#5
y = c()
s = c()
for(i in 15:0){
  s = c(s, 2^i)
}

dec = c()
for(i in 1:15){
  dec = c(dec, 2^(-i))
}
z = function(u){
  x = as.integer(u)
  #cat(x, "\n")
  for(j in 1:16){
    if(x - s[j] >= 0){
      x = x - s[j]
      y[j] = 1
    }
    else{y[j] = 0}
  }
  for(l in 1:16){
    if(y[l] == 1){
      y = c(y[l:length(y)])
      break
    }
  }
  #cat("binary representation of ", u, "is ", y)
  ###############
  d = c()
  r = u - as.integer(u)
  if(r == 0){
    cat("binary representation of ", u, "is ", y)
    break
  }
  for(f in 1:15){
    if(r - dec[f] >= 0){
      r = r - dec[f]
      d[f] = 1
    }
    else{
      d[f] = 0
    }
  }
  for(l in 15:1){
    if(d[l] == 1){
      d = c(d[1: l])
      break
    }
  }
  cat("binary representation of ", u, "is ",y,".", d)
}
z(1024)

###############################################################################
#PC LAB 4B --------------------------------------------------------------------
#1a
dg1x = function(x){
  return(-(1/2 * cos(x)/sqrt(1 - (1/2 * sin(x))^2)))
}

dg2x = function(x){
  return(-(2 * sin(x)/sqrt(1 - (2 * cos(x))^2)))
}

x = seq(-2*pi, 2*pi, length = 1e3)
plot(x, dg1x(x), ylim = c(-1,1), xlim = c(-2*pi, 2*pi), type = "l", lwd = 2, col = 4)
abline(h = 0, col = 7)
plot(x, dg2x(x), ylim = c(-10, 10), xlim = c(-2*pi, 2*pi), col = 3, type = "l", lwd = 2, lty = 2)
abline(h = c(-1, 1), col = 7, lty = 4, lwd = 3)

#1b
x = seq(-2*pi, 2*pi, length = 1e3)
ftn = function(x){
  return(acos(0.5*sin(x)))
}
plot(x, ftn(x), xlim = c(1, 1.2), ylim = c(1, 1.2), col = 1, lty = 1, type = "l", lwd = 2)
abline(a = 0, b = 1, col = 2, lty = 2, lwd = 2)

library(spuRs)
fixed = function (ftn, x0, tol = 1e-09, max.iter = 100){
  for(i in 0:10){
    xold <- x0 + i
    xnew <- ftn(xold)
    iter <- 1
    #cat(sprintf("x0=%2d: Root = %7.5f found in %d iterations \n",x0,  xnew, iter))
    while ((abs(xnew - xold) > tol) && (iter < max.iter)) {
      xold <- xnew
      xnew <- ftn(xold)
      iter <- iter + 1
    }
    cat(sprintf("x0=%2d: Root = %7.5f found in %d iterations \n",x0+i,  xnew, iter))
  }
}

fixed(ftn, x0 = -5, tol = 1e-09, max.iter = 100 )

#2
ftn = function(x){
  return(2*cos(x) - sin(x))
}
x = seq(-2*pi, 2*pi, 0.001)
plot(x, ftn(x))

bisect = function (ftn, x.l, x.r, tol = 1e-09){
  if (x.l >= x.r) {
    cat("error: x.l >= x.r \n")
    return(NULL)
  }
  f.l <- ftn(x.l)
  f.r <- ftn(x.r)
  if (f.l == 0) {
    return(x.l)
  }
  else if (f.r == 0) {
    return(x.r)
  }
  else if (f.l * f.r > 0) {
    cat("error: ftn(x.l) * ftn(x.r) > 0 \n")
    return(NULL)
  }
  n <- 0
  while ((x.r - x.l) > tol) {
    x.m <- (x.l + x.r)/2
    f.m <- ftn(x.m)
    if (f.m == 0) {
      return(x.m)
    }
    else if (f.l * f.m < 0) {
      x.r <- x.m
      f.r <- f.m
    }
    else {
      x.l <- x.m
      f.l <- f.m
    }
    n <- n + 1
    #cat("at iteration", n, "the root lies between", x.l, "and", x.r, "\n")
  }
  return((x.l + x.r)/2)
}

bisect(ftn, x.l = -2*pi, x.r = -4)
bisect(ftn, x.l = -4, x.r = 0)
bisect(ftn, x.l = 0, x.r = 2)
bisect(ftn, x.l = 2, x.r = 6)

#3a
f = function(x){
  return((x^3 - x)*sin(x))
}
plot(x,f(x))

dfx = function(x){
  return((3 * x^2 - 1) * sin(x) + (x^3 - x) * cos(x))
}

ftn = function(x){
  return(c(f(x), dfx(x)))
}
x0 = -3:3
root = rep(NA, length(x0))
NewtRaph = function (ftn, x0, tol = 1e-09, max.iter = 100){
  for(i in 1:7){
    x <- x0[i]
    fx <- ftn(x)
    iter <- 0
    while ((abs(fx[1]) > tol) && (iter < max.iter)) {
      x <- x - fx[1]/fx[2]
      fx <- ftn(x)
      iter <- iter + 1
    }
    cat(sprintf("X0=%2d: root=%8.5f found\n", x0[i], x))
    if (abs(fx[1]) > tol) {
      #cat("Algorithm failed to converge\n")
      #return(NULL)
    }
    else {
      #cat("Algorithm converged\n")
      #return(x)
    }
  }
  
}
NewtRaph(ftn, -3:3)

#b
x = seq(-pi,pi, 0.001)
plot(x, dfx(x), type = "l")
abline(h = c(0), col = 2)

install.packages("rootSolve")
library(rootSolve)
uniroot.all(dfx, c(-4,4))

dfx = function(x){
  return((3 * x^2 - 1) * sin(x) + (x^3 - x) * cos(x))
}
D(expression((3 * x^2 - 1) * sin(x) + (x^3 - x) * cos(x)), "x")

d2fx = function(x){
  return(3*(2*x)*sin(x)+(3*x^2-1)*cos(x)+((3*x^2-1)*cos(x) - (x^3 - x) * sin(x)))
}
ftn = function(x){
  return(c(dfx(x), d2fx(x)))
}

NewtRaph = function (ftn, x0, tol = 1e-09, max.iter = 100){
  x <- x0
  fx <- ftn(x)
  iter <- 0
  while ((abs(fx[1]) > tol) && (iter < max.iter)) {
    x <- x - fx[1]/fx[2]
    fx <- ftn(x)
    iter <- iter + 1
  }
  cat(sprintf("X0=%4.2f: root=%8.5f found\n", x0, x))
  if (abs(fx[1]) > tol) {
    #cat("Algorithm failed to converge\n")
    #return(NULL)
  }
  else {
    #cat("Algorithm converged\n")
    #return(x)
  }
}
NewtRaph(ftn, -0.7)

#4
secant = function (ftn, x0, x1, tol = 1e-09, max.iter = 100){
  x = x0
  y = x1
  #fx(x) <- ftn(x)
  iter <- 0
  while ((abs(ftn(x) > tol) && (iter < max.iter))) {
    x = y - (ftn(y)*(y - x)/(ftn(y)-ftn(x)))
    y = x - (ftn(x)*(x - y)/(ftn(x)-ftn(y)))
    #ftn(x) <- ftn(x)
    iter <- iter + 1
    cat("At iteration", iter, "value of x is:", x, "\n")
  }
  if (abs(ftn(x)) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  }
  else {
    cat("Algorithm converged\n")
    return(x)
  }
}

ftn = function(x){
  return(cos(x) - x)
}

secant(ftn, 0, 1)

x = seq(0, 1, 0.001)
plot(x, ftn(x))
abline(h = c(0))

#5
polyroot(z = c(1,1,1,1))
polyroot(z = c(1,1,-2,-4))

###############################################################################
#PC LAB 5A --------------------------------------------------------------------
#1a
library(pracma)
for(i in 1:4){
  x = 0:3
  y = x^2 + 3*x + 2
  p = polyfit(x[i:(i+1)], y[i:(i+1)],1)
  z = c(0.5,1.5, 2.5)
  w = z^2 + 3*z + 2
  pp = polyval(p, z[i])
  e = w - pp
  cat(sprintf("x=%5.3f y(true)= %6.3f y(approximation) = %5.3f error = %6.3f \n", z[i], w[i], pp, e[i]))
}

#2
x = c(0, pi/2, pi)
y = c(0,1,0)

df= c()
for(i in 1:3){
  df = c(df, y[i+1] - y[i])
}

d2f= c()
for(i in 1:3){
  d2f = c(d2f, df[i+1] - df[i])
} 

cat("    x         f        df        d2f")
cat("===========================================")
for(i in 1:3){
  cat(sprintf("%10.6f %10.6f %10.6f %10.6f \n", x[i], y[i], df[i], d2f[i]))
}

#3
x = c(-pi/2, 0, pi/2, pi)
y = c(-1, 0, 1, 0)
polyfit(x,y, 3)

#4
library(pracma)
?cubicspline
x = c(-pi, -pi/2, 0, pi/2, pi)
y = c(0, -1, 0,1,0)
z = seq(-pi, pi, pi/20)
pp <- cubicspline(x, y, z)

plot(x,y, type = "p", col = "red4")
lines(z, pp, col = 5)

#or
x = c(-pi, -pi/2, 0, pi/2, pi)
y = c(0, -1, 0,1,0)
z = seq(-pi, pi, pi/20)
pp <- cubicspline(x, y)
zz = ppval(pp,z)
plot(x,y, type = "p", col = "red4")
lines(z, zz, col = 5)
?polyval



#5
rm(list = ls())
data = read.table("/Users/hengyizhao/Downloads/YX.txt", header = TRUE)
y = data[,1]
x = data[,2]
SSR = function(coef, y, x){
  Sum = 0
  for(i in 1:11){
    Sum = Sum + (y[i] - (coef[1]*sin(x[i]) + coef[2]*cos(x[i])))^2
  }
  return(Sum)
}

results = optim(c(0,0), SSR, gr= NULL, y, x, method = c("BFGS"))
cat(sprintf("a=%8.6f ba=%8.6f", results$par[1], results$par[2]))

a = results$par[1]
b = results$par[2]

yfit = a*sin(x) + b*cos(x)
plot(x, yfit, type = "l", lwd = 2)
points(x,y, col = 3, lwd = 4)

#6a
x = seq(-pi, pi, 0.5*pi);x
y = c(0,-1,0,1,0)
z = seq(-pi, pi, pi/10)
pp = cubicspline(x,y,z);pp
SSR =  sum((pp-sin(z))^2)

#6b
for(j in 1:4){
  c = c()
  for(i in 0:3){
    c = c(c, pp$coefs[j,1+i])
  }
  cat("coefficients of spline", j," are", c, "\n")
}

###############################################################################
#PC LAB 5B --------------------------------------------------------------------
#1a
rm(list = ls()) # clear memory
cat("\f")       # clear screen

fprimitve_1a = function(x){
  return(0.25*x^4 - 0.5*x^2 + x)
}

library(pracma)
a <- 0         # lower integral limit
b <- 1         # upper integral limit
N <- 2         # number of subintervals
h <- (b-a)/N   # width of the subiterval
x <- seq(a,b,h)
f <- function(x){
  return(x^3-x+1)
}
area_exact  <- rep(0,N)
area_approx <- rep(0,N)
error       <- rep(0,N)
cat(sprintf('                      Area           Area\n'))
cat(sprintf('Interval:             Exact:         Approximation:     Error:\n'))
cat(sprintf('==============================================================\n'))
for(i in 1:N){
  area_exact[i] <- fprimitve_1a(a+i*h)-fprimitve_1a(a+(i-1)*h)    
  onderkant = h*f(0.5)
  if(f(a+(i-1)*h) > f(a+i*h)){hoogste = f(a+(i-1)*h) - f(a+i*h)}
  else{hoogste = f(a+i*h) - f(a+(i-1)*h) }
    area_approx[i] <-   onderkant + (h*0.5*hoogste)      # % calculate the approximate area using the Trapezium 
    error[i] <- area_exact[i]-area_approx[i]
  cat(sprintf('(%d): (%5.2f,%.2f)   %12.8f   %12.8f   %12.8f\n',
              i,a+(i-1)*h,a+i*h,area_exact[i],area_approx[i],error[i]))
}
cat(sprintf('==============================================================\n'))
cat(sprintf('Total:              %12.8f   %12.8f   %12.8f\n',sum(area_exact),sum(area_approx),sum(error)))
no_obs <- length(x)        # total number of x-values = N+1
w <- rep(1,no_obs)
w[(2):(length(w)-1)] = 2
w[1] = 1
w[length(x)] = 1
w

area_approx2 <- h/2*sum(w*f(x))
cat(sprintf('Check: h/2*sum(w*f)=%.8f\n',area_approx2))

#1b
rm(list = ls()) # clear memory
cat("\f")       # clear screen

fprimitve_1b = function(x){
  return(sqrt(2*pi)*pnorm(x))
}

library(pracma)
a <- 1         # lower integral limit
b <- 2        # upper integral limit
N <- 4        # number of subintervals
h <- (b-a)/N   # width of the subiterval
x <- seq(a,b,h)
f <- function(x){
  return(exp(-0.5*x^2))
}
area_exact  <- rep(0,N)
area_approx <- rep(0,N)
error       <- rep(0,N)
cat(sprintf('                      Area           Area\n'))
cat(sprintf('Interval:             Exact:         Approximation:     Error:\n'))
cat(sprintf('==============================================================\n'))
for(i in 1:N){
  area_exact[i] <- fprimitve_1b(a+i*h)-fprimitve_1b(a+(i-1)*h)    
  area_approx[i] <- (h)*f(a+i*h) + 0.5*abs(f(a+i*h)-f(a+(i-1)*h))*h
  error[i] <- area_exact[i]-area_approx[i]
  cat(sprintf('(%d): (%5.2f,%.2f)   %12.8f   %12.8f   %12.8f\n',
              i,a+(i-1)*h,a+i*h,area_exact[i],area_approx[i],error[i]))
}
cat(sprintf('==============================================================\n'))
cat(sprintf('Total:              %12.8f   %12.8f   %12.8f\n',sum(area_exact),sum(area_approx),sum(error)))
no_obs <- length(x)        # total number of x-values = N+1
w <- rep(1,no_obs)
w[(2):(length(w)-1)] = 2
w[1] = 1
w[length(x)] = 1

area_approx2 <- h/2*sum(w*f(x))
cat(sprintf('Check: h/2*sum(w*f)=%.8f\n',area_approx2))

#2a
rm(list = ls()) # clear memory
cat("\f")       # clear screen
library(pracma)

x2 <- seq(0,pi,length=1000)
f2 <- sin(5*x2)
par(cex=1.5)
plot(x2,f2,type="l",lwd=2,col="DarkBlue",xlab="x",ylab="f")
M <- 8
NoPoints_x <- 2*M+1
x <- seq(0,pi,pi/(NoPoints_x-1))
f <- sin(5*x)
Lagr <- rep(1,1000)
LagrIntervalWidth <- 1000/M
for(i in 1:M){
  index_low <- 2*(i-1)+1        # for i=1,...,3: 1,3,6
  index_high <- 2*i+1           # for i=1,...,3: 3,5,8
  coef <- polyfit(x[index_low:index_high],f[index_low:index_high],2)
  index_low_x2 <- floor((i-1)*LagrIntervalWidth)+1
  index_high_x2 <- floor(i*LagrIntervalWidth)
  Lagr[index_low_x2:index_high_x2] <- polyval(coef,x2[index_low_x2:index_high_x2])
}
points(x,f,col="Red",lwd=2)
lines(x2,Lagr,type="l",col="Red",lwd=2)

#2b
x = c(0, pi/6, pi/3)
y = c(0,0.5,-0.5*sqrt(3))
coeff = polyfit(x,y,2)
a = coeff[1]
b = coeff[2]
c = coeff[3]


library(Ryacas)
t = ysym("t")
int.exact = integrate(a*t^2 + b*t + c, t, 0, pi/3)
eval(as_r(int.exact))

f = function(x){
  return(sin(5*x))
}
h = pi/6
simpson = h/3 * (f(x[1]) + 4*f(x[2]) + f(x[3]))

#2c
rm(list = ls()) # clear memorycat("\f")       # clear screen

fprimitve_2c = function(x){
  return(-0.2*cos(5*x))
}
library(pracma)
a <- 0         # lower integral limit
b <- 1         # upper integral limit
N <- 3       # number of subintervals
h <- pi/6   # width of the subiterval
x = seq(0, pi, pi/6)
f = function(x){
  return(sin(5*x))
}
area_exact  <- rep(0,N)
area_approx <- rep(0,N)
error       <- rep(0,N)
cat(sprintf('                      Area           Area\n'))
cat(sprintf('Interval:             Exact:         Approximation:     Error:\n'))
cat(sprintf('==============================================================\n'))
for(i in 1:N){
  index = 2*(i-1) + 1
  area_approx[i] <- h/3 * (f(x[index]) + 4*f(x[index+1]) + f(x[index+2]))
  area_exact[i] <-  fprimitve_2c(x[index+2]) - fprimitve_2c(x[index])
  error[i] <- area_exact[i]-area_approx[i]
  cat(sprintf('(%d): (%5.2f,%.2f)   %12.8f   %12.8f   %12.8f\n',
              i,((i-1)*pi/3),(i*pi/3),area_exact[i],area_approx[i],error[i]))
}
cat(sprintf('==============================================================\n'))
cat(sprintf('Total:              %12.8f   %12.8f   %12.8f\n',sum(area_exact),sum(area_approx),sum(error)))
no_obs <- length(x)        # total number of x-values = N+1
w <- rep(2,N)
w[seq(2,(N-1),2)] = 4 
w[1] = 1
w[N] = 1
area_approx2 <- h/3*sum(w*f(x))
cat(sprintf('Check: h/2*sum(w*f)=%.8f\n',area_approx2))

#3
find.package("spuRs")
# estimate and plot the normal cdf Phi
rm(list = ls()) # clear the workspace
source("/Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/library/spuRs/resources/scripts/simpson_n.r")

phi <- function(x){
  return(exp(-x^2/2)/sqrt(2*pi))
} 

Phi <- function(z) {
  if (z < 0) {
    return(0.5 - simpson_n(phi, z, 0))
  } else {
    return(0.5 + simpson_n(phi, 0, z))
  } 
}

intersectie = function(z){
  c(Phi(z) - 0.99, phi(z))
}
newtonraphson(intersectie, 0)

#4
find.package("spuRs")
# estimate and plot the normal cdf Phi
rm(list = ls()) # clear the workspace
source("/Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/library/spuRs/resources/scripts/simpson_n.r")
source("/Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/library/spuRs/resources/scripts/trapezoid.r")

I = function(x){
  return(5*x^4)
}

k = seq(2, 16, 1)
nt = rep(0, 15)
ns = rep(0, 15)
for(i in 1:length(k)){
  error = 2^(-k[i])
  #cat(error, "\n")
  for(j in 1:500){
    n = j
    trap = abs(trapezoid(I, 0, 1, n) - 1)
    if(trap <= error){
      nt[i] = n
      break
    }
  }
  for(j in 1:100){
    n = j
    simp = abs(simpson_n(I, 0, 1, n) - 1)
    if(simp <= error){
      ns[i] = n
      break
    }
  }
  cat(ns, nt, "\n")
}

e = 2^(-k)
plot(log2(e), log2(ns), type = "l", col = "blue", xlim = c(-16, -2), ylim = c(1,9), 
     xlab = "log_2(errorr", ylab = "log_2(n)" )
lines(log2(e), log2(nt), col = "red")
legend("topright", legend = c("Trapezoid", "Simpson"), col = c("blue", "red"), lty = c(1,1))

###############################################################################
#PC LAB 6A --------------------------------------------------------------------
#1
A = matrix(0, 4,4)
r = 1:4
A[1,] = r
A[4:1,4] = r

#2
A = matrix(1,3,3)
B = 2*matrix(1,3,2)
C = 3*matrix(1,2,3)
cbind(A,B)
cbind(A, t(B))
cbind(A,C)
cbind(A, t(C))
cbind(A,C)
cbind(A,t(B))

#4
A = matrix(c(1,1,2,1,-1,-3,-2,-5,1), 3,3,byrow = TRUE)
b = c(1,0,4)
solve(A,b, fractions = TRUE)

#6
library(Ryacas)
yac("A:={{0,1,s},{s,o,1},{1,s,0}}")
yac("Determinant(A)")

y = c()
z = c()
for(i in 1:4){
  s = (i-1)*pi/4
  z[i] = s
  A = matrix(c(0,1,s,s,0,1,1,s,0), 3, 3, byrow = TRUE)
  y[i] = det(A)
}
polyfit(z,y,3)

#7
A = diag(4) 
A[1,4] = A[4,1] = -1

r = eigen(A)
s = r$values; s
V = r$vectors; V

for(i in 1:4){
  cat(sprintf("A*v_%d   lambda_%d*v_%d: \n", i, i, i))
  cat(sprintf("%8.5f    %8.5f\n", (A%*%V[,i]), (s[i]%*%V[,i])))
}

#8
P = matrix(c(0.9,0.5,0.1,0.5), 2,2,byrow = TRUE)
eigengetallen = eigen(P)
ew = eigengetallen$values
ev = eigengetallen$vectors
D = cbind(c(ew[1],0),c(0,ew[2]))
pp = ev%*%D%*%solve(ev)
PP = pp
for(i in 2:10){
  PP = PP%*%pp
}
PP
ev%*%(D^10)%*%solve(ev)

#b
B = matrix(c(-.1,0.5,1,1), 2,2,byrow = TRUE)
b = c(0,1)
v =solve(B,b, fractions = T)
as.matrix(v)
P%*%v


A <- matrix(c(-0.1,1,0.5,1),2,2)
b <- c(0,1)
v <- solve(A,b)
as.matrix(v)
P %*% v
###############################################################################
#PC LAB 6B --------------------------------------------------------------------
#1

logLik<-function(theta,x) {
  return(-n*theta + sum(x*log(theta)))
}
gr_logLik<-function(theta,x) {
  return(-n + sum(x/theta))
}
x <- scan("/Users/hengyizhao/Downloads/countdata.dat")
n <- length(x)
cat('Estimated parameters\n')
result1 <- optim(4,logLik,gr=NULL,x,method="Brent",
                 lower=0,upper=10,control=list(fnscale=-1))
print(result1$par)
result2 <- optim(4,logLik,x=x,gr_logLik, method="BFGS",
                 control=list(fnscale=-1,maxit=300,trace=TRUE,REPORT=5))
names(result2) # all properties of the object result2 print(result2$par)

#2
library(spuRs)

Rosenbrock <- function(x) {
  g <- (1 - x[1])^2 + 100*(x[2] - x[1]^2)^2
  g1 <- -2*(1 - x[1]) - 400*(x[2] - x[1]^2)*x[1]
  g2 <- 200*(x[2] - x[1]^2)
  g11 <- 2 - 400*x[2] + 1200*x[1]^2
  g12 <- -400*x[1]
  g22 <- 200
  return(list(g, c(g1, g2), matrix(c(g11, g12, g12, g22), 2, 2)))
}

x <- seq(-2, 2, .1)
y <- seq(-2, 5, .1)
z <- matrix(nrow = length(x), ncol = length(y))
xyz <- data.frame(matrix(0, length(x)*length(y), 3))
names(xyz) <- c('x', 'y', 'z')
n <- 0
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    n <- n + 1
    xyz[n,] <- c(x[i], y[j], Rosenbrock(c(x[i], y[j]))[[1]])
  }
}
xyz
library(lattice)
print(wireframe(z ~ x*y, data = xyz, scales = list(arrows = FALSE),
                zlab = 'f(x, y)', drape = T))

contour(x,y,matrix(xyz[,3], nrow=41, ncol=71, byrow=TRUE),
        xlim = range(-2,2), ylim = range(-2, 5), col = 2)

#-------------------------------------------------------------------------------
gsection <- function(ftn, x.l, x.r, x.m, tol = 1e-9) {
  # applies the golden-section algorithm to maximise ftn
  # we assume that ftn is a function of a single variable
  # and that x.l < x.m < x.r and ftn(x.l), ftn(x.r) <= ftn(x.m)
  #
  # the algorithm iteratively refines x.l, x.r, and x.m and terminates
  # when x.r - x.l <= tol, then returns x.m
  
  # golden ratio plus one
  gr1 <- 1 + (1 + sqrt(5))/2
  
  # successively refine x.l, x.r, and x.m
  f.l <- ftn(x.l)
  f.r <- ftn(x.r)
  f.m <- ftn(x.m)
  while ((x.r - x.l) > tol) {
    if ((x.r - x.m) > (x.m - x.l)) {
      y <- x.m + (x.r - x.m)/gr1
      f.y <- ftn(y)
      if (f.y >= f.m) {
        x.l <- x.m
        f.l <- f.m
        x.m <- y
        f.m <- f.y
      } else {
        x.r <- y
        f.r <- f.y
      }
    } else {
      y <- x.m - (x.m - x.l)/gr1
      f.y <- ftn(y)
      if (f.y >= f.m) {
        x.r <- x.m
        f.r <- f.m
        x.m <- y
        f.m <- f.y
      } else {
        x.l <- y
        f.l <- f.y
      }
    }
  }
  return(x.m)
}

line.search <- function(f, x, y, tol = 1e-9, a.max = 2^5) {
  # f is a real function that takes a vector of length d
  # x and y are vectors of length d
  # line.search uses gsection to find a >= 0 such that
  #   g(a) = f(x + a*y) has a local maximum at a,
  #   within a tolerance of tol
  # if no local max is found then we use 0 or a.max for a
  # the value returned is x + a*y
  
  if (sum(abs(y)) == 0) return(x) # g(a) constant
  
  g <- function(a) return(f(x + a*y))
  
  # find a triple a.l < a.m < a.r such that
  # g(a.l) <= g(a.m) and g(a.m) >= g(a.r)
  # a.l
  a.l <- 0
  g.l <- g(a.l)
  # a.m
  a.m <- 1
  g.m <- g(a.m)
  while ((g.m < g.l) & (a.m > tol)) {
    a.m <- a.m/2
    g.m <- g(a.m)
  }
  # if a suitable a.m was not found then use 0 for a
  if ((a.m <= tol) & (g.m < g.l)) return(x)
  # a.r
  a.r <- 2*a.m
  g.r <- g(a.r)
  while ((g.m < g.r) & (a.r < a.max)) {
    a.m <- a.r
    g.m <- g.r
    a.r <- 2*a.m
    g.r <- g(a.r)
  }
  # if a suitable a.r was not found then use a.max for a
  if ((a.r >= a.max) & (g.m < g.r)) return(x + a.max*y)
  
  # apply golden-section algorithm to g to find a
  a <- gsection(g, a.l, a.r, a.m)
  return(x + a*y)
}

g <- function(x) -(1 - x[1])^2 - 100*(x[2] - x[1]^2)^2
gradg <- function(x) c(2*(1 - x[1]) + 400*(x[2] - x[1]^2)*x[1], -200*(x[2] - x[1]^2))

ascent <- function(f, grad.f, x0, tol = 1e-9, n.max = 100) {
  # steepest ascent algorithm
  # find a local max of f starting at x0
  # function grad.f is the gradient of f
  
  x <- x0
  x0 = 0
  points(x[1], x[2], pch=19)
  x.old <- x
  x <- line.search(f, x, grad.f(x))
  cat(x, x[1], x[2])
  n <- 1
  while ((f(x) - f(x.old) > tol) & (n < n.max)) {
    points(x=c(x.old[1], x[1]), y = c(x.old[2], x[2]))
    x.old <- x
    x <- line.search(f, x, grad.f(x))
    n <- n + 1
  }
  return(x)
}

contour(x, y, z, nlevels = 20)
ascent(g, gradg, c(0, 3), n.max=10000)

Rosenbrock <- function(x) {
  g <- (1 - x[1])^2 + 100*(x[2] - x[1]^2)^2
  g1 <- -2*(1 - x[1]) - 400*(x[2] - x[1]^2)*x[1]
  g2 <- 200*(x[2] - x[1]^2)
  g11 <- 2 - 400*x[2] + 1200*x[1]^2
  g12 <- -400*x[1]
  g22 <- 200
  return(list(g, c(g1, g2), matrix(c(g11, g12, g12, g22), 2, 2)))
}

newton <- function(f3, x0, tol = 1e-9, n.max = 100) {
  # Newton's method for optimisation, starting at x0
  # f3 is a function that given x returns the list
  # {f(x), grad f(x), Hessian f(x)}, for some f
  
  x <- x0
  f3.x <- f3(x)
  n <- 0
  ## plot starting point x0
  points(x[1], x[2], col = 3)
  while ((max(abs(f3.x[[2]])) > tol) & (n < n.max)) {
    x.old <- x
    x <- x - solve(f3.x[[3]], f3.x[[2]])
    f3.x <- f3(x)
    n <- n + 1
    ## plot line from x.old to x
    lines(x=c(x.old[1], x[1]), y = c(x.old[2], x[2]))
  }
  if (n == n.max) {
    cat('newton failed to converge\n')
  } else {
    return(x)
  }
}

x <- seq(-2, 2, .1)
y <- seq(-2, 5, .1)
z <- matrix(nrow = length(x), ncol = length(y))
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    z[i, j] <- Rosenbrock(c(x[i], y[j]))[[1]]
  }
}
contour(x, y, z, nlevels = 20)
ascent(g, gradg, c(0, 3), n.max=10000)
newton(Rosenbrock, c(0, 3))

#-------------------------------------------------------------------------------
# Ch 12 - Ex 6


f <- function(x) {
  # bug: if x[2] is too large then exp(x[2]) is Inf and cos(x[2]-exp(x[2])) is NaN
  # just set cos(x[2]-exp(x[2])) to 0 in this case
  s <- sum(x^2)
  y <- -(s-2)*(s-1)*s*(s+1)*(s+2)*(2-sin(x[1]^2-x[2]^2)*cos(x[2]-exp(x[2])))
  if (is.nan(y)) {
    y <- -(s-2)*(s-1)*s*(s+1)*(s+2)*2
  }
  return(y)
}

gradf <- function(x) {
  # bug: if x[2] is too large then exp(x[2]) is Inf and cos(x[2]-exp(x[2])) is NaN
  # just set cos(x[2]-exp(x[2])) and sin(x[2]-exp(x[2])) to 0 in this case
  s <- sum(x^2)
  f1 <- -2*x[1]*((s-1)*s*(s+1)*(s+2) + (s-2)*s*(s+1)*(s+2) + (s-2)*(s-1)*(s+1)*(s+2) +
                   (s-2)*(s-1)*s*(s+2) + (s-2)*(s-1)*s*(s+1))*(2-sin(x[1]^2-x[2]^2)*cos(x[2]-exp(x[2]))) +
    2*x[1]*(s-2)*(s-1)*s*(s+1)*(s+2)*cos(x[1]^2-x[2]^2)*cos(x[2]-exp(x[2]))
  f2 <- -2*x[2]*((s-1)*s*(s+1)*(s+2) + (s-2)*s*(s+1)*(s+2) + (s-2)*(s-1)*(s+1)*(s+2) +
                   (s-2)*(s-1)*s*(s+2) + (s-2)*(s-1)*s*(s+1))*(2-sin(x[1]^2-x[2]^2)*cos(x[2]-exp(x[2]))) -
    2*x[2]*(s-2)*(s-1)*s*(s+1)*(s+2)*cos(x[1]^2-x[2]^2)*cos(x[2]-exp(x[2])) -
    (1 - exp(x[2]))*(s-2)*(s-1)*s*(s+1)*(s+2)*sin(x[1]^2-x[2]^2)*sin(x[2]-exp(x[2]))
  if (is.nan(f1)) {
    f1 <- -2*x[1]*((s-1)*s*(s+1)*(s+2) + (s-2)*s*(s+1)*(s+2) + (s-2)*(s-1)*(s+1)*(s+2) +
                     (s-2)*(s-1)*s*(s+2) + (s-2)*(s-1)*s*(s+1))*2
  }
  if (is.nan(f2)) {
    f2 <- -2*x[2]*((s-1)*s*(s+1)*(s+2) + (s-2)*s*(s+1)*(s+2) + (s-2)*(s-1)*(s+1)*(s+2) +
                     (s-2)*(s-1)*s*(s+2) + (s-2)*(s-1)*s*(s+1))*2
  }
  return(c(f1, f2))
}

# use numerical approx of derivative to check gradf is correct
check.gradf <- function(x, e=1e-8) {
  f1 <- (f(x+c(e,0)) - f(x))/e
  f2 <- (f(x+c(0,e)) - f(x))/e
  return(c(gradf(x) - c(f1, f2)))
}
check.gradf(c(0,0))
check.gradf(c(1,1))
check.gradf(c(1,-1))
x <- seq(-1.5, 1.5, .05)
y <- seq(-1.5, 1.5, .05)
xyz <- data.frame(matrix(0, length(x)*length(y), 3)) names(xyz) <- c('x', 'y', 'z')
n <- 0
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    n <- n + 1
    xyz[n,] <- c(x[i], y[j], max(-3,f(c(x[i], y[j]))))
  }
  5
}
library(lattice)
print(wireframe(z ~ x*y, data = xyz, scales = list(arrows = FALSE),
                zlab = 'f(x, y)', drape = T))

