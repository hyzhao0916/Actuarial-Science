library(spuRs)
#-------------------------------------------------------
ftn = function(x){
  return(1/(x-2))
}
x = -1:7
bisection(ftn, 1, 7, tol = 1e-09)

plot(ftn, x, xlim = c(-1, 7), col = 6, ylim = c(-10, 10))
grid(col = 2)
#--------------------------------------------------------
ftn = function(x){
  return(tan(x))
}
x = 1:4
bisection(ftn, 3,4, tol = 1e-09)
plot(ftn, x, xlim = c(1,8), col = 6, ylim = c(-10, 10))
grid(col = 2)
#--------------------------------------------------------
ftn = function(x){
  return(x-sqrt(2))
}
x = 1.35;1.45
#View(bisection)
bisection(ftn, 1.35,1.45, tol = 1e-09)
plot(ftn, x, xlim = c(1,8), col = 6, ylim = c(-10, 10))
grid(col = 2)
#--------------------------------------------------------
ftn = function(x){
  return(1-0.25*x^2)
}
x = 0:1
plot(ftn, x, xlim = c(0,5), col = 6, ylim = c(0,5))
abline(a = 0, b= 1)
grid(col = 2)
