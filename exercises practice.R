################################################################################
################### -------- HW EXERCISES lec 4B --------- #####################
D(expression(x^(1/3)), "x")
#1
library(pracma)
#a
f = function(x){return(1/(x-2))}

bisection(f, 3,8)
df = function(x){return(-(1/(x - 2)^2))}
ftn = function(x){return(c(f(x), df(x)))}
newtonraphson(ftn, 3)

#b
bisection(f, 1,7)
newtonraphson(ftn, 1)

#c
f = function(x){return(tan(x))}
D(expression(tan(x)), "x")
df = function(x){return(1/cos(x)^2)}
ftn = function(x){return(c(f(x),df(x)))}
bisection(f, 3,4)
newtonraphson(ftn, 3)

#d
bisection(f, 1,3)
newtonraphson(ftn, 1)

#2
f = function(x){return(x^2 - 2)}
df = function(x){return(2*x)}
ftn = function(x){return(c(f(x),df(x)))}
bisection(f, 1.35,1.45)
newtonraphson(ftn, 1.35)

#4
#a
rm(list = ls())
ftn = function(x){return(sqrt(6+x))}
x = seq(0,7,0.001)
plot(x,ftn(x),type ="l", col = "magenta", xlim =c(0,7), ylim = c(0,7), lwd = 2)
lines(x = c(0,7), y = c(0,7), col = 333999, lwd = 2)
x = c()
y = c()
fixed = function (ftn, x0, tol = 1e-09, max.iter = 100){
  x[1] = x0
  y[1] = 0
  x[2] = x0
  y[2] = ftn(x0)
  xold <- x0
  xnew <- ftn(xold)
  iter <- 0
  lines(x,y, col = 1, type = "b")
  index = seq(3, max.iter, 2)
  for(i in index){
    x[i] = y[i-1]
    y[i] = x[i]
    lines(x = c(x[i], x[i-1]), y = c(y[i], y[i-1]), col = 1, lwd = 1, type = "l" )
    x[i+1] = x[i]
    y[i+1] = ftn(x[i+1])
    iter = iter + 1
    cat(x, "\n")
    cat(y, "\n")
    lines(x = c(x[i+1], x[i]), y = c(y[i+1], y[i]), col = 1, lwd = 1, type = "l" )
    if(abs(x[i+1]-x[i-1])<tol | (iter>max.iter)){
      break
    }
  }
  if (abs(x[i+1]-x[i]) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  }
  else {
    cat("Algorithm converged\n")
    cat("At iteration", iter, "value of x is:", xnew, "\n")
    return(xnew)
  }
  return(x,y)
}
fixed(ftn, 7)
#converges

#b
rm(list = ls())
ftn = function(x){return(1 + 2/x)}
x = seq(1,4.5,0.001)
plot(x,ftn(x),type ="l", col = "peachpuff4", xlim =c(1,4.5), ylim = c(1,4.5), lwd = 2)
lines(x = c(1,4.5), y = c(1,4.5), col = "hotpink1", lwd = 2)
x = c()
y = c()
fixed = function (ftn, x0, tol = 1e-09, max.iter = 100){
  x[1] = x0
  y[1] = 0
  x[2] = x0
  y[2] = ftn(x0)
  xold <- x0
  xnew <- ftn(xold)
  iter <- 0
  lines(x,y, col = 1, type = "b")
  index = seq(3, max.iter, 2)
  for(i in index){
    x[i] = y[i-1]
    y[i] = x[i]
    lines(x = c(x[i], x[i-1]), y = c(y[i], y[i-1]), col = i, lwd = 1, type = "l" )
    x[i+1] = x[i]
    y[i+1] = ftn(x[i+1])
    iter = iter + 1
    cat(x, "\n")
    cat(y, "\n")
    lines(x = c(x[i+1], x[i]), y = c(y[i+1], y[i]), col = "orchid", lwd = 1, type = "l" )
    if(abs(x[i+1]-x[i-1])<tol | (iter>max.iter)){
      break
    }
  }
  if (abs(x[i+1]-x[i]) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  }
  else {
    cat("Algorithm converged\n")
    cat("At iteration", iter, "value of x is:", xnew, "\n")
    return(xnew)
  }
  return(x,y)
}
fixed(ftn,4)
#converges

#c
rm(list = ls())
ftn = function(x){return((x^2)/3)}
x = seq(1,6,0.001)
plot(x,ftn(x),type ="l", col = "cyan2", xlim =c(1,6), ylim = c(1,6), lwd = 2)
lines(x = c(1,6), y = c(1,6), col = "limegreen", lwd = 2)
x = c()
y = c()
fixed = function (ftn, x0, tol = 1e-09, max.iter = 100){
  x[1] = x0
  y[1] = 0
  x[2] = x0
  y[2] = ftn(x0)
  xold <- x0
  xnew <- ftn(xold)
  iter <- 0
  lines(x,y, col = 1, type = "b")
  index = seq(3, max.iter, 2)
  for(i in index){
    x[i] = y[i-1]
    y[i] = x[i]
    lines(x = c(x[i], x[i-1]), y = c(y[i], y[i-1]), col = 1, lwd = 1, type = "l" )
    x[i+1] = x[i]
    y[i+1] = ftn(x[i+1])
    iter = iter + 1
    cat(x, "\n")
    cat(y, "\n")
    lines(x = c(x[i+1], x[i]), y = c(y[i+1], y[i]), col = "peru", lwd = 1, type = "l" )
    if(abs(x[i+1]-x[i-1])<tol | (iter>max.iter)){
      cat("Algorithm failed to converge\n")
      break
    }
  }
  if (abs(x[i+1]-x[i]) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  }
  else {
    cat("Algorithm converged\n")
    cat("At iteration", iter, "value of x is:", xnew, "\n")
    return(xnew)
  }
  return(x,y)
}
fixed(ftn,3.5,tol = 1e-09, max.iter = 100)
#diverges

#d
rm(list = ls())
ftn = function(x){return(-x^2 + 2*x + 2)}
x = seq(-1,3,0.001)
plot(x,ftn(x),type ="l", col = "peachpuff", xlim =c(-1,3), ylim = c(-1,3), lwd = 2)
lines(x = c(-1,3), y = c(-1,3), col = "hotpink4", lwd = 2)
x = c()
y = c()
x0 = 2.5
fixed = function (ftn, x0, tol = 1e-09, max.iter = 100){
  x[1] = x0
  y[1] = 0
  x[2] = x0
  y[2] = ftn(x0)
  xold <- x0
  xnew <- ftn(xold)
  iter <- 0
  lines(x,y, col = 1, type = "b")
  index = seq(3, max.iter, 2)
  for(i in index){
    x[i] = y[i-1]
    y[i] = x[i]
    lines(x = c(x[i], x[i-1]), y = c(y[i], y[i-1]), col = i, lwd = "magenta4", type = "l" )
    x[i+1] = x[i]
    y[i+1] = ftn(x[i+1])
    iter = iter + 1
    cat(x, "\n")
    cat(y, "\n")
    lines(x = c(x[i+1], x[i]), y = c(y[i+1], y[i]), col = "cyan4", lwd = 1, type = "l" )
    if(abs(x[i+1]-x[i-1])<tol | (iter>max.iter)){
      break
    }
  }
  if (abs(x[i+1]-x[i]) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  }
  else {
    cat("Algorithm converged\n")
    cat("At iteration", iter, "value of x is:", xnew, "\n")
    return(xnew)
  }
  return(x,y)
}
fixed(ftn,2.5)
#doesn't converge, however noncovergent does not mean divergent in this cas

#5b
rm(list = ls())
library(pracma)
f = function(x){return(x^2-2)}
bisection(f, 1.4, 4)
df = function(x){return(2*x)}
ftn = function(x){return(c(f(x), df(x)))}
newtonraphson(ftn, 1.4)

#6
rm(list = ls())
f = function(x){return(x*exp(-x))}
df = function(x){return(exp(-x) - x * exp(-x))}
ftn = function(x){return(c(f(x),df(x)))}
x = seq(0,8, 0.001)
plot(x,f(x), type ="l", lwd = 2, col = "blue", xlim = c(0,8), ylim = c(0,0.4))
newt = function (ftn, x0, tol = 1e-09, max.iter = 100){
  x <- x0
  fx <- ftn(x)
  iter <- 0
  lines(x=c(x,x), y = c(0,ftn(x)[1]))
  while ((abs(fx[1]) > tol) && (iter < max.iter)) {
    pp = polyfit(x,ftn(x)[2])
    lines(x = c(x,x - fx[1]/fx[2]), y = c(fx[1],0), col= as.integer(x), lty = 3, type = "l", lwd = 2)
    x <- x - fx[1]/fx[2]
    fx <- ftn(x)
    iter <- iter + 1
    #cat("At iteration", iter, "value of x is:", x, "\n")
    lines(x=c(x,x), y = c(0,ftn(x)[1]))
  }
  if (abs(fx[1]) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  }
  else {
    cat("Algorithm converged\n")
    return(x)
  }
}
newt(ftn, 2)

#7b
rm(list = ls())
#f1 = function(x){return(sqrt(x))}
#f2 = function(x){return(-sqrt(-x))}
f = function(x){
  coor = c()
  for(i in 1:length(x)){
    if(x[i] > 0){
      coor[i] = sqrt(x[i])
      }
    else{
      coor[i] = -sqrt(-(x[i]))
      }
  }
  return(coor)
}

#x = seq(0,3,0.001)
#plot(x,f1(x), type = "l", col = "magenta4", xlim = c(-3,3), ylim = c(-2,2))
#x = seq(-3,0,0.001)
#lines(x,f2(x), type = "l", col = "magenta4")

x = seq(-3,3,0.001)
plot(x,f(x), type = "l", col = "hotpink", xlim = c(-3,3), ylim = c(-2,2))
grid()

#df1 = function(x){return(0.5 * x^-0.5)}
#df2 = function(x){return(0.5 * (-x)^-0.5)}

df = function(x){
  dcoor = c()
  for(i in 1:length(x)){
    if(x[i] > 0){
      dcoor[i] = 0.5 * x^-0.5
    }
    else{
      dcoor[i] = 0.5 * (-x)^-0.5
    }
  }
  return(dcoor)
}

ftn = function(x){return(c(f(x),df(x)))}
newt = function (ftn, x0, tol = 1e-09, max.iter = 100){
  x <- x0
  fx <- ftn(x)
  iter <- 0
  lines(x=c(x,x), y = c(0,ftn(x)[1]))
  while ((abs(fx[1]) > tol) && (iter < max.iter)) {
    pp = polyfit(x,ftn(x)[2])
    lines(x = c(x,x - fx[1]/fx[2]), y = c(fx[1],0), col= 2, lty = 3, type = "l", lwd = 2)
    x <- x - fx[1]/fx[2]
    fx <- ftn(x)
    iter <- iter + 1
    #cat("At iteration", iter, "value of x is:", x, "\n")
    lines(x=c(x,x), y = c(0,ftn(x)[1]))
  }
  if (abs(fx[1]) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  }
  else {
    cat("Algorithm converged\n")
    return(x)
  }
}
newt(ftn, 2)

#8
f = function(x){return(x^(1/3))}
df = function(x){return(x^((1/3) - 1) * (1/3))}
ftn = function(x){return(c(f(x),df(x)))}
newtonraphson(ftn,1)
x = seq(-5,5, 0.001)
plot(x,f(x), type = "l")

#9
f = function(x){return(x^3-3*x+2)}
seca = function (fun, a, b, maxiter = 500, tol = 1e-08, ...){
    fun <- match.fun(fun)
    f <- function(x) fun(x, ...)
    x1 <- a
    x2 <- b
    f1 <- f(x1)
    if (abs(f1) <= tol) 
      
      return(x1)
    f2 <- f(x2)
    if (abs(f2) <= tol) 
      return(x1)
    n <- 0
    while (n <= maxiter && abs(x2 - x1) > tol) {
      n <- n + 1
      slope <- (f2 - f1)/(x2 - x1)
      if (slope == 0) 
        return(root = NA, f.root = NA, iter = n, estim.prec = NA)
      x3 <- x2 - f2/slope
      f3 <- f(x3)
      if (abs(f3) <= tol) 
        break
      x1 <- x2
      f1 <- f2
      x2 <- x3
      f2 <- f3
      cat(sprintf("At iteration %d approximation is: %7.5f \n", n, x3))
    }
    if (n > maxiter) {
      warning("Maximum number of iterations 'maxiter' was reached.")
    }
    #return(list(root = x3, f.root = f3, iter = n, estim.prec = 2 * abs(x3 - x2)))
  }
seca(f, -2.6, -2.4)

################################################################################
################### -------- HW EXERCISES lec 5B --------- #####################

library(pracma)
library(spuRs)
ftn = function(x){return(1/sqrt(2)*exp(-0.5*x^2))}
trapezoid(ftn, 0,1.96, n = 4)

f = function(x){return(-x^4-2*x^3-8*x^2-5*x)}
x = c()
y = c()

x[1] = -2
x[2] = (-2+1)/2
x[3] = 1
y[1] = f(x[1])
y[2] = f(x[2])
y[3] = f(x[3])
x[4] = (y[1]*(x[2]^2-x[3]^2) + y[2]*(x[3]^2-x[1]^2) + y[3]*(x[1]^2-x[2]^2))/(2*(y[1]*(x[2]-x[3]) + y[2]*(x[3]-x[1]) + y[3]*(x[1]-x[2])))
y[4] = f(x[4])
z = seq(-2,1,0.001)
pp = polyfit(x,y,2)
ywaarde = polyval(pp,z)
z = seq(-2,1,0.001)
plot(z,f(z),col = "blue", xlim = c(-2,1), ylim = c(-20,1),
     type = "l", lwd = 2, ylab = "f(x)", xlab = "x")
lines(z,ywaarde, col = "red2", type = "l", lwd = 2,lty = "dotted")
points(-0.3472504665, f(-0.3472504665), col = "blue", pch = 16)
points(x[4], y[4], col = "red", pch = 16)


third = 2
c = c(1,2,3)
c[-1]
sum(1:3)

rnorm(10,mean = 5, sd = 1)

a = c(3,1,4,2)
element = a[1]
rest = a[-1]
A = matrix(c(1,2,3,4),2)
?qr(A)
