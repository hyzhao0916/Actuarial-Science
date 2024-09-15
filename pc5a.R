#pc 5a
#a
x = 0:3
library(pracma)
a = c(0.500, 1.500, 2.500)
y = x^2 +3*x + 2
for(i in 1:3){
  p = polyfit(x[i:(i+1)], y[i:(i+1)], 1)
  b = polyval(p, a[i])
  d = a[i]^2 + 3*a[i] + 2
  e = d - b
  cat(sprintf("x =%6.3f y(true) =%6.3f y(approximation) =%6.3f error = %5.3f \n", 
              a[i], d, b, e))
}

#2
x = c(0, 0.5*pi, pi)
y = c(0, 1, 0)
z = c()
for(i in 1:3){
  r = y[i+1] - y[i]
  if(r != "NA"){
  z = c(z, r)
  }
}
z

d2f = c()
for(i in 1:3){
  s = z[i+i] - z[i]
  if(s != "NA"){
  d2f = c(d2f, s)
  }
}
d2f

cat(" x     f     df      d2f \n")
cat("=========================== \n")
for(i in 1:3){
  cat(sprintf("%8.6f  %8.6f  %8.6f  %8.6f \n", x[i], y[i], z[i], d2f[i]))
}

#4
library(pracma)
x = seq(-pi, pi, by=pi/20)
r = c(-pi, -0.5*pi, 0, 0.5*pi, pi)
s = c(0, -1, 0, 1, 0)
plot(r, s, col = 3, xlim = c(-4, 4), ylim = c(-1.5, 1.5))
u = cubicspline(r,s,x)
lines(x, u, col = 1)

x <- c(-pi, -0.5*pi, 0, 0.5*pi, pi)
f <- c(0,-1,0,1,0)
x2 <- seq(-pi,pi,pi/20)
pp = cubicspline(x, f)
pp
u = ppval(pp, x2)
u
plot(x, u, col = 2, xlim = c(-4, 4), ylim = c(-1.5, 1.5))

x=seq(0,3*pi,pi/2)
y=sin(x)
plot(x,y,lwd=2,cex=2,col="darkblue")
x2=seq(0,3*pi,length=100);x2
y2=sin(x2)
lines(x2,y2,type="l",lwd=2,col="darkblue")
#Cubic spline
pp <- cubicspline(x, y)
pp
fit_spline=ppval(pp,x2)
lines(x2,fit_spline,lty=1,lwd=4,col="red")
for(i in 1:6){ abline(v=i*pi/2,lty=2) }


#5
rm(list = ls())
data <- read.table("/Users/hengyizhao/Downloads/YX.txt",header=TRUE)
attach(data)
SSR <- function(coef,y,x){
  fit <- coef[1]*sin(x)+coef[2]*cos(x)
  residuals <- y-fit
  return(sum(residuals^2))
}
results <- optim(c(0,0),SSR,gr=NULL,y,x,method = c("BFGS"))
coef <- results$par
cat(sprintf("a=%10.4f b=%10.4f\n",coef[1],coef[2]))

#6
x = seq(-pi,pi,pi/2)
f = c(0,-1,0,1,0)
pp = cubicspline(x, f)
x2 = seq(-pi,pi,pi/10)
f_spline = ppval(pp,x2)
f_true = sin(x2)
SSR = sum((f_true-f_spline)^2)
cat(sprintf("SSR = %10.4f\n",SSR))

#b
coef = c(pp$coefs[1,], pp$coefs[2,], pp$coefs[3,], pp$coefs[4,])
for(i in 1:16){
  cat(sprintf("%6.4f ",coef[i]))
  if(i %% 4 == 0 ){
    cat("\n")
  }
}

#ppt
library(Ryacas)
f<-function(coord){
  coord[1]^2+coord[2]^2
}
optim(c(1,1),f,gr=NULL)

x <- seq(0,pi,pi/20)
y = sin(1.45*x) + rnorm(21, 0, 0.5)
optim(1, function(a,y,x){sum((y-sin(a*x))^2)},gr = NULL, y, x)
plot(x,y)
?rnorm

