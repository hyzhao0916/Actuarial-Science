#1
logLik<-function(theta,x) {
  functie = -n*theta + sum(x)*log(theta)
  return(functie)
}

gr_logLik<-function(theta, x){
  #theta = (1/n)*sum(x)
  #return(theta)
  score = -n + sum(x)/theta
  return(score)
}
x <- scan("/Users/hengyizhao/Downloads/countdata.dat");x
n <- length(x)
cat('Estimated parameters\n')
result1 <- optim(4,logLik,gr=NULL,x,method="Brent",
                 lower=0,upper=10,control=list(fnscale=-1))
print(result1$par)
result2 <- optim(4,logLik,x=x,gr_logLik, method="BFGS",
                 control=list(fnscale=-1,maxit=300,trace=TRUE,REPORT=5))
names(result2) # all properties of the object result2 print(result2$par)
1/n * sum(x)

#2
# program spuRs/resources/scripts/Rosenbrock.r
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
xyz <- data.frame(matrix(0, length(x)*length(y), 3))
names(xyz) <- c('x', 'y', 'z')
n <- 0
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    n <- n + 1
    xyz[n,] <- c(x[i], y[j], Rosenbrock(c(x[i], y[j]))[[1]])
  }
}
library(lattice)
print(wireframe(z ~ x*y, data = xyz, scales = list(arrows = FALSE),
                zlab = 'f(x, y)', drape = T))
contour(x, y, matrix(xyz[,3], nrow=41, ncol=71, byrow=TRUE),
        xlim = range(-2,2), ylim = range(-2, 5), col = 2)

#----------------------------------------------------------------------------
#Code needed at lines marked ##
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
  x.old <- x
  x <- line.search(f, x, grad.f(x))
  n <- 1
  while ((f(x) - f(x.old) > tol) & (n < n.max)) {
    ##
    #cat(x.old[1], x.old[2],x[1], x[2], "\n")
    lines(x = c(x.old[1], x[1]) , y = c(x.old[2], x[2]), 
          type = 'b', xlim = c(-2,2), ylim = c(-2,5), col = "cyan")
    x.old <- x
    x <- line.search(f, x, grad.f(x))
    n <- n + 1
  }
  return(x)
}


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
  x0 = 0
  while ((max(abs(f3.x[[2]])) > tol) & (n < n.max)) {
    x.old = x
    x <- x - solve(f3.x[[3]], f3.x[[2]])
    f3.x <- f3(x)
    n <- n + 1
    ## plot line from x.old to x
    lines(x = c(x.old[1], x[1]) , y = c(x.old[2], x[2]),
          type = 'b', xlim = c(-2,2), ylim = c(-2,5))
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

#3 ---------------------------------------------------------------------------
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

f <- function(x) {
  # x is a vector of length 2
  s <- sum(x^2)
  z <- -(s-2)*(s-1)*s*(s+1)*(s+2)*
    (2-sin(x[1]^2-x[2]^2)*cos(x[2]-exp(x[2])))
  if (is.nan(z)) {
    z <- -(s-2)*(s-1)*s*(s+1)*(s+2)*2
  }
  return(z)
}

x <- seq(-1.5, 1.5, .05)
y <- seq(-1.5, 1.5, .05)
xyz <- data.frame(matrix(0, length(x)*length(y), 3))
names(xyz) <- c('x', 'y', 'z')
n <- 0
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    n <- n + 1
    xyz[n,] <- c(x[i], y[j], max(-3,f(c(x[i], y[j]))))
  }
  5
}

ascent <- function(f, grad.f, x0, tol = 1e-9, n.max = 100) {4
  # steepest ascent algorithm
  # find a local max of f starting at x0
  # function grad.f is the gradient of f
  x <- x0
  x0 = 0
  x.old <- x
  x <- line.search(f, x, grad.f(x))
  n <- 1
  while ((f(x) - f(x.old) > tol) & (n < n.max)) {
    ##
    #cat(x.old[1], x.old[2],x[1], x[2], "\n")
    lines(x = c(x.old[1], x[1]) , y = c(x.old[2], x[2]), 
          type = 'b', col = as.integer(x), xlim = c(-2,2), ylim = c(-2,5))
    x.old <- x
    x <- line.search(f, x, grad.f(x))
    n <- n + 1
  }
  return(x)
}

library(lattice)
print(wireframe(z ~ x*y, data = xyz, scales = list(arrows = FALSE),
                zlab = 'f(x, y)', drape = T ))
contour(x, y, matrix(xyz[,3], nrow=61, ncol=61, byrow=TRUE), nlevels = 20)
for(i in 1:10){
  ascent(f, gradf, runif(2, -1.5, 1.5), n.max=10000)
}



