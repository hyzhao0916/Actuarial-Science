library(pracma)
x <- seq(-2,2,0.2)
coef <- c(1,0,0,-1)
y <- polyval(coef, x)
plot(x, y)

?polyval





rm(list=ls())
x <-seq(0,1,0.005025125628141)
g <- (x^3)+1
h <- x+2
z <- x^2
y <- cos (pi*x)
f <- (z*y)/(g*h);f
f[200]



