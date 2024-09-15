curve(-5*x^4-3*x^3+4*x^2-1,-1.3,0.8,col=4,lwd=2,main="-5*x^4-3*x^3+4*x^2-1")

cat("\f") # clear screen
# ML: Maximum Likelihood
n=30         # number of observations
set.seed(20)  # set seed for reproducibility
y <- rnorm(n, mean=10, sd=2)  # Note: variance is 2^2=4
hist(y)
points(y,rep(0,n),type="p",col=2,pch=16,cex=2)

cat("\f") # clear screen
logL <- function(theta,y){
  mu <- theta[1]
  sigma2 <- theta[2]
  n <-length(y)
  res <- (y-mu)
  -n/2*log(sigma2)-1/2*sum(res^2)/sigma2
}

pdf <- function(mu,sigma2,y){
  1/sqrt(2*pi*sigma2)*exp(-1/2*(y-mu)^2/sigma2)
}

## UNIVARIATE
# vary mean: mu  (assuming sigma2=2^2)
mu.seq=seq(4,16,length=100)
lines(mu.seq,70*pdf(8,4,mu.seq),col=4,lwd=2)
logL(c(8,4),y)
lines(mu.seq,70*pdf(10,4,mu.seq),col=3,lwd=2)
logL(c(10,4),y)
lines(mu.seq,70*pdf(12,4,mu.seq),col=2,lwd=2)
logL(c(12,4),y)

par(mfrow=c(2,1))   # two plots in a 2x1 matrix
l=sapply(mu.seq,function(mu) {logL(c(mu,4),y)})
plot(mu.seq,l,type="l",col=4,lwd=2)
plot(mu.seq,exp(l),type="l",col=2,lwd=2)
par(mfrow=c(1,1))   # reset plots

cat("\f") # clear screen
# Newton's Method
theta <- 10
# first iteration
gr <- sum(y-theta)/4
H <- -n/4
theta <- theta - gr/H; theta
# second iteration
gr <- sum(y-theta)/4
H <- -n/4
theta <- theta - gr/H; theta

f <- function(x){
  logL(c(x,4),y)
}

par(mfrow=c(3,1))
cat("\f") # clear screen
# First iteration Golden-Section
mu.seq=seq(9,9.8,length=100)
l=sapply(mu.seq,f)
plot(mu.seq,l,type="l",col=4,lwd=2,main="log Likelihood")
a <- 9.2
b <- 9.6
points(a,f(a),type="p",col=2,pch=16,cex=2)
points(b,f(b),type="p",col=2,pch=16,cex=2)
g=(sqrt(5)-1)/2
midpoint=(a+b)/2
abline(v=midpoint)
x1 <- a+(1-g)*(b-a)
points(x1,f(x1),type="p",col=3,pch=16,cex=2)
x2 <- a+g*(b-a)
points(x2,f(x2),type="p",col=6,pch=16,cex=2)
midpoint-x1
x2-midpoint
if(f(x1)>f(x2)){
  b <- x2
} else {
  a <- x1
}

cat("\f") # clear screen
# Second iteration Golden-Section
plot(mu.seq,l,type="l",col=4,lwd=2,main="log Likelihood")
abline(0,0)
points(a,f(a),type="p",col=2,pch=16,cex=2)
points(b,f(b),type="p",col=2,pch=16,cex=2)
midpoint=(a+b)/2
abline(v=midpoint)
x1 <- a+(1-g)*(b-a)
points(x1,f(x1),type="p",col=3,pch=16,cex=2)
x2 <- a+g*(b-a)
points(x2,f(x2),type="p",col=6,pch=16,cex=2)
if(f(x1)>f(x2)){
  b <- x2
} else {
  a <- x1
}

cat("\f") # clear screen
# Third iteration Golden-Section
plot(mu.seq,l,type="l",col=4,lwd=2,main="log Likelihood")
abline(0,0)
points(a,f(a),type="p",col=2,pch=16,cex=2)
points(b,f(b),type="p",col=2,pch=16,cex=2)
midpoint=(a+b)/2
abline(v=midpoint)
x1 <- a+(1-g)*(b-a)
points(x1,f(x1),type="p",col=3,pch=16,cex=2)
x2 <- a+g*(b-a)
points(x2,f(x2),type="p",col=6,pch=16,cex=2)
par(mfrow=c(1,1))

cat("\f") # clear screen
## MULTIVARIATE
# vary variance: sigma2
hist(y)
points(y,rep(0,n),type="p",col=2,pch=16,cex=2)
mu.seq=seq(4,14,length=100)
lines(mu.seq,60*pdf(9.392,3,mu.seq),col=4,lwd=2)
logL(c(9.392,3),y)
lines(mu.seq,60*pdf(9.392,4,mu.seq),col=3,lwd=2)
logL(c(9.392,4),y)
lines(mu.seq,60*pdf(9.392,6,mu.seq),col=2,lwd=2)
logL(c(9.392,6),y)

mu.seq <- seq(9.4-1,9.4+1, length=100);    k.mu <- length(mu.seq) 
sigma2.seq <- seq(2,7, length=100); k.simga2 <- length(sigma2.seq)
l <-  matrix(nrow = k.mu, ncol = k.simga2)
for(i in 1:k.mu){
  for(j in 1:k.simga2){
    l[i,j] <- logL(c(mu.seq[i],sigma2.seq[j]),y)
  }
}

cat("\f") # clear screen
contour(mu.seq, sigma2.seq, l, nlevels = 20)
install.packages("rgl")  # to install package
library(rgl)
open3d()
persp3d(mu.seq, sigma2.seq, l, col="blue")

cat("\f") # clear screen
gradient <- function(theta,y){
  mu <- theta[1]
  sigma2 <- theta[2]
  n <-length(y)
  res <- (y-mu)
  score1 <- sum(res)/sigma2
  score2 <- -n/(2*sigma2)+1/(2*sigma2^2)*sum(res^2)
  return(c(score1,score2))
}

theta0 <- c(10,4)  # starting value
# gradient function evaluated at starting value theta0
gradient(theta0,y)

# let's check this gradient
h <- 0.000001
# numerical partial gradient w.r.t. mu
e1 <- c(1,0)
(logL(theta0+e1*h,y)-logL(theta0,y))/h
# numerical partial gradient w.r.t. sigma2
e2 <- c(0,1)
(logL(theta0+e2*h,y)-logL(theta0,y))/h

#Steepest Ascent
g <- function(alpha,theta,y){
  logL(theta+alpha*gradient(theta,y),y)
}

logL(theta0,y)
g(0,theta0,y)
g(0.1,theta0,y)
g(0.2,theta0,y)
alpha.seq<-seq(-0.1,0.4,length=100)
g.seq<-sapply(alpha.seq,function(alpha) g(alpha,theta0,y))
# Plot g() as function of alpha
plot(alpha.seq,g.seq,type="l",col=4,lwd=2)
abline(v=0)
# Determine optimal alpha
which.max(g.seq)
alpha.seq[which.max(g.seq)]
# Graphical representation
contour(mu.seq, sigma2.seq, l, nlevels = 20, xlab="mu", ylab="sigma2")
points(theta0[1],theta0[2],type="p",col=2,pch=16,cex=2)
# First iteration
theta <- theta0+0.2*gradient(theta0,y)
theta
lines(c(theta0[1], theta[1]), c(theta0[2], theta[2]),col=4,lwd=2)
points(theta[1],theta[2],type="p",col=4,pch=16,cex=2)

cat("\f") # clear screen
# Hessian
H <- function(theta,y){
  mu <- theta[1]
  sigma2 <- theta[2]
  n <-length(y)
  res <- (y-mu)
  H11 <- -n/sigma2
  H12 <- -sum(res)/sigma2^2
  H21 <- H12
  H22 <- n/(2*sigma2^2)-1/sigma2^3*sum(res^2)
  return(matrix(c(H11,H12,H21,H22),2,2,byrow=TRUE))
}

# Hessian evaluated at starting value theta0
H(theta0,y)
# Numerical check
(gradient(theta0+e1*h,y)-gradient(theta0,y))/h
(gradient(theta0+e2*h,y)-gradient(theta0,y))/h

cat("\f") # clear screen
# Newton's method
theta <- theta0
for(i in 1:5){
  theta <- theta - solve(H(theta,y)) %*% gradient(theta,y)
  print(theta)
}

cat("\f") # clear screen
## CURVE FITTING
# Optimise without gradient
optim(theta0,logL,gr=NULL,y,control=list(fnscale=-1))
# Now optimise using the analytical gradient
optim(theta0,logL,gradient,y,method = "BFGS",control=list(fnscale=-1))

Data = c(2.5, 3, "na", 5, NA, 10 )
sum(is.na(Data))
