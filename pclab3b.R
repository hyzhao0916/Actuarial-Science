#exercise 1 ------------------------------------------------------------------
geometric = function(r, n){
  p = c(0:n)
  s = c(sum(r^p))
  cat(s)
}
geometric(1.03, 10)

#exercise 2 ------------------------------------------------------------------
Euler = function(n){
  b = c(1:n)
  s = (sum(b^(-2)))
  g = pi^2/6
  #cat(s)
  cat(sprintf("pi^2/6 is %6.4f \nThe sum of the first %2d terms is %6.4f", g, n, s))
}
Euler(50)

#exercise 3 ------------------------------------------------------------------
?set.seed

set.seed(321)
x = matrix(sample(1:30, 3*5, replace = T), 3, 5); x
maxmat = function(mat){
  r = dim(mat)[1]
  c = dim(mat)[2]
  cmax = 0
  maxcol = c()
  for(j in 1:c){
    cmax = 0
    for(i in 1:r){
      col = x[i,j]
      if(col > cmax ){
        cmax = col
      }
    }
    maxcol = c(maxcol, cmax)
  }
  #
  maxrow = c()
  for(i in 1:r){
    rmax = 0
    for(j in 1:c){
      row = x[i,j]
      if(row > rmax ){
        rmax = row
      }
    }
    maxrow = c(maxrow, rmax)
  }
  #
  xmax = 0
  maxtot = c()
  for(i in 1:r){
    for(j in 1:c){
      coo = x[i,j]
      if(coo > xmax ){
        xmax = coo
      }
    }
  }
  maxtot = c(maxtot, xmax)
  #
  a = list("maxcol" = maxcol, "maxrow" = maxrow, "maxtot" = maxtot)
  return(a)
} 
results = maxmat(x)
results


#exercise 4 -------------------------------------------------------------------
areatri = function(x1, y1, x2, y2, x3, y3){
  dist = function(x1, y1, x2, y2){
    return(sqrt((x1-x2)^2 + (y1-y2)^2))
  }
  a = dist(x1, y1, x2, y2)
  b = dist(x1, y1, x3, y3)
  c = dist(x2, y2, x3, y3)
  s = (a + b + c)/2
  area = sqrt(s*(s-a)*(s-b)*(s-c))
  return(area)
}
areatri(0, 0, 4, 0, 4, 3)

#exercise 5 -------------------------------------------------------------------
Approx = function (x, M){
  a = 0
  x = seq(-0.99, 0.99, by = 0.03)
  for(i in 1: M){
    b = (x^(2*i-1))/(2*i - 1)
    a = a + b
  }
  return(2*a)
}
Approx(-1:1, 4)

werkelijk = function(x){
  x = seq(-0.99, 0.99, by = 0.03)
  b = log((1+x)/(1-x))
  return(b)
}

#b
x = seq(-0.99, 0.99, by = 0.03)
plot(x, Approx(-1:1, 5), col = 2, lty = 3, main = "Function and it Taylor approximation", xlim = c(-0.99, 0.99), 
     xlab = "x", ylab = "f(x) = solid line", ylim = c(-4, 4))
lines(x, werkelijk(-1:1), col = 4)

#c
table = data.frame(x=x[1:6],
                  Taylor = c(werkelijk(x[1:6])),
                  log = c(Approx(x[1:6], 5)))
table

#d
cat('log((1+x)/(1-x))~2(');
for(n in 1:M){
  cat( sprintf('x^%d/%d',(2*n-1),(2*n-1)) )
  if(n<M)
    cat('+')
  else
    cat(')\n')
}