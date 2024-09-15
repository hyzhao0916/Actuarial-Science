#Exercise 1--------------------------------------------------------------------
#a
x = seq(-2*pi, 2*pi, length=1e3)

dg1x = function(x){
  return(-(2 * sin(x)/sqrt(1 - (2 * cos(x))^2)))
}

dg2x = function(x){
  return(-(1/2 * cos(x)/sqrt(1 - (1/2 * sin(x))^2)))
}

plot(x, dg1x(x), type = "l", ylim=c(-10,10), xlim = c(-2*pi, 2*pi))
lines(x, dg2x(x), col = 3)
abline(h=c(-1,1),col = 4,lty=2,lwd=2)

#b
x = seq(-2*pi, 2*pi, length=1e3)
ftn = function(x){
  return(acos(0.5*sin(x)))
}

plot(x, ftn(x), type = "l", ylim=c(-6,6))
abline(a = 0, b= 1, col = 3)
grid()

x0 = seq(-5, 5)

fixedpoint = function (ftn, x0, tol = 1e-6, max.iter = 100){
  xold <- x0
  xnew <- ftn(xold)
  iter <- 1
  #cat("x0 = -5:Root = ", xnew, "\n")
  while ((abs(xnew - xold) > tol) && (iter < max.iter)) {
    xold <- xnew
    xnew <- ftn(xold)
    iter <- iter + 1
    #cat("x0 = At iteration", iter, "value of x is:", xnew, "\n")
  }
  if (abs(xnew - xold) > tol) {
    #cat("Algorithm failed to converge\n")
    return(NULL)
  }
  else {
    #cat("Algorithm converged\n")
    cat("At iteration", iter, "value of x is:", xnew, "\n")
    paste(c(xnew, iter))
  }
}

for (x00 in x0) {
  a = c(fixedpoint(ftn, x00, tol = 1e-9))
}
cat(a[length(a)-1], a[length(a)])

#Exercise 2--------------------------------------------------------------------
x0 = seq(-2*pi, 2*pi, length=1e3)
ftn = function(x){
  return(2*cos(x) - sin(x))
}
plot(x, ftn(x), type= "b", col = 3)

bisection = function (ftn, x.l, x.r, tol = 1e-09){
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

x.l = (-2*pi)
x.r = -3
r1 = c(bisection(ftn, x.l, x.r, tol = 1e-09))
a = r1[length(r1)]

x.l = (-3)
x.r = 0
r2 = c(bisection(ftn, x.l, x.r, tol = 1e-09))
b = r2[length(r2)]

x.l = 0
x.r = 3
r3 = c(bisection(ftn, x.l, x.r, tol = 1e-09))
c = r3[length(r3)]

x.l = 3
x.r = 2*pi
r4 = c(bisection(ftn, x.l, x.r, tol = 1e-09))
d = r4[length(r4)]

cat("The roots are:", a, b, c, d)

#Exercise 3--------------------------------------------------------------------
#a
x = seq(-pi, pi, length=1e3)
f = function(x){
  return((x^3 - x) * sin(x))
}
plot(x, f(x), col = 5, ylim = c(-5,10), xlim = c(-pi, pi))

df = function(x){
  return((3 * x^2 - 1) * sin(x) + (x^3 - x) * cos(x))
}
lines(x, df(x), col = 6)

fNR = function(x){
  return(c(f(x),df(x)))
}

newtonraphson = function (ftn, x0, tol = 1e-09, max.iter = 100){
  x <- x0
  fx <- ftn(x)
  iter <- 0
  while ((abs(fx[1]) > tol) && (iter < max.iter)) {
    x <- x - fx[1]/fx[2]
    fx <- ftn(x)
    iter <- iter + 1
    #cat("At iteration", iter, "value of x is:", x, "\n")
  }
  if (abs(fx[1]) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  }
  else {
    #cat("Algorithm converged\n")
    return(x)
  }
}


root = rep(NA, length(x01))

x0 = -3:3
for(x01 in x0){
  e = c(newtonraphson(fNR, x01, 1e-6))
  cat(e, " ")
} 

#b









