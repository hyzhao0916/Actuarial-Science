rm(list = ls()) # clear memory
cat("\f")       # clear screen
library(pracma)
#2a-----------------------------------------------------------------------------
x2 <- seq(0,pi,length=1000)
f2 <- sin(5*x2)
par(cex=1.5)
plot(x2,f2,type="l",lwd=2,col="DarkBlue",xlab="x",ylab="f")
M = 8
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

#2b-----------------------------------------------------------------------------
x2 <- seq(0,pi/3,length=1000)
f2 = function(x){
   return(sin(5*x2))
}
par(cex=1.5)
plot(x2,f2(x),type="l",lwd=2,col="DarkBlue",xlab="x",ylab="f")
M = 3
NoPoints_x <- 2*M+1
x <- seq(0,pi/3,pi/(NoPoints_x-1))
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
a = coef[1]
b = coef[2]
c = coef[3]
library(Ryacas)
t = ysym("t")
int.exact = integrate(a*t^2 + b*t + c, t, 0, pi/3)
eval(as_r(int.exact))

l = function(x){sin(5*x)}
h = pi/6
o = seq(0, pi/3, length = 3)
(h/3)*(l(o[1]) + 4* l(o[2]) + l(o[3]))

#2c-----------------------------------------------------------------------------



A<-matrix(c(-5,-7,2,4),2,2);A
r<-eigen(A);r
V<-r$vectors; V
V[1,1]^2 + V[2,1]^2
lambda<-r$values; lambda


















