x=c(0,2,4)
y=c(3,21,151)
plot(x,y,col=2,pch=16,cex=2)

# Fitting 2-nd order polynomial by hand
A<-matrix(c( 1,0,  0,
             1,2,2^2,
             1,4,4^2),3,3,byrow=TRUE)
f<-c(3,21,151)
z<-solve(A,f)
z
x.fine <- seq(0,4, by=0.1)
f.fit<-z[1]+z[2]*x.fine+z[3]*x.fine^2
lines(x.fine,f.fit,col=4,lwd=2,lty=2)

# Fitting 2-nd order polynomial by polyfit
# install.packages("pracma")
library(pracma)
p <- polyfit(x,y,n=2)
y.fit <- polyval(p,x)
y.fit

# Lagrange polynomial
# install.packages("Ryacas")
library(Ryacas)
# define symbolic variables
x <- ysym("x")
x1 <- ysym("x1"); x2 <- ysym("x2"); x3 <- ysym("x3")

res <- simplify( 1+(x-x1)/(x1-x2)+(x-x1)*(x-x2)/((x1-x2)*(x1-x3)) )
res
y_fn( res - (x-x2)/(x1-x2)*(x-x3)/(x1-x3) , "Simplify" )

# remove symbolic variables
rm(x,x1,x2,x3); x=c(0,2,4)

# Fitting cubic splines by hand
h<- 2
A<-matrix(c(1,0,  0,    0,0, 0,  0,  0, # s1(x1)=f1
            1,h,h^2,  h^3,0, 0,  0,  0, # s1(x2)=f2
            0,0,  0,    0,1, 0,  0,  0, # s2(x2)=f2
            0,0,  0,    0,1, h,h^2,h^3, # s2(x3)=f3
            0,1,2*h,3*h^2,0,-1,  0,  0, # s1'(x2)=s2'(x2)
            0,0,  2,  6*h,0, 0, -2,  0, # s1''(x2)=s2''(x2)
            0,0,  2,0    ,0, 0,  0,  0, # s1''(x1)=0
            0,0,  0,0    ,0, 0,  2,6*h  # s2''(x3)=0
            ),8,8,byrow=TRUE)
b=c(3,21,21,151,0,0,0,0)
# values for a1,b1,c1,d1 & a2,b2,c2,d2
solve(A,b)

# Fitting cubic splines by cubicspline
pp <- cubicspline(x, y)
pp$coefs
y.fit <- ppval(pp, x.fine)
plot(x,y,col=2,pch=16,cex=2)
lines(x.fine,y.fit,col=4,lwd=2,lty=2)

# Fitting linear trend by hand
A=matrix(c(3,6,6,20),2,2, byrow=TRUE)
b=c(175,646)
coef<-solve(A,b)
coef
fitted<-coef[1]+coef[2]*x
plot(x,y,col=2,pch=16,cex=2,ylim=c(-20,160))
lines(x,fitted,col=4,lwd=2,lty=1)
e<-y-fitted
e
SSR<-sum(e^2)
SSR
# Other estimates chosen on sight
fitted2<- -17+38*x
lines(x,fitted2,col=6,lwd=3,lty=2)
e2<-y-fitted2
SSR2<-sum(e2^2)
SSR2
# Fitting linear trend by optim
optim(c(0,0),function(coef,y,x){sum((y-(coef[1]+coef[2]*x))^2)},gr=NULL,y,x)

