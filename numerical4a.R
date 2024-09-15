#exercise 1 ------------------------------------------------------------------
x = seq(-0.5*pi, 0.5*pi, length.out = 100)

f = function(x){
  x * (1+x^2)^(-1)
}

g = function(x){
  tan(x)
}

plot(x, f(g(x)), xlab = "x", xlim = c(-0.5*pi, 0.5*pi),
     ylim = c(-0.5, 0.5), type = "l", lty = 2, col = "4")
lines(x, g(f(x)), col = "2", lty = 3)
legend("bottomright",
       legend = c("f(g(x))", "g(f(x))"),
       lty = c(2,3),
       col = c("blue","red"),
       )

#exercise 2 ------------------------------------------------------------------
x = c(0, 1, 2 , 3)

Taylor = function(x, N){
  a = 0
  for(i in 0: N){
    a = (-1)^i * (x^(2*i))/(factorial(2*i)) + a
  }
  return(a)
}


cat("x:     ", sprintf("%13.8f", x),
    "\ncos(x):", sprintf("%13.8f", cos(x)),
    "\nN=1:   ", sprintf("%13.8f", Taylor(x, 1)),
    "\nN=2:   ", sprintf("%13.8f", Taylor(x, 2)),
    "\nN=3:   ", sprintf("%13.8f", Taylor(x, 3)))

#exercise 3 ------------------------------------------------------------------
u = seq(-4, 4, by = 0.05)
z = cos(u)

plot(u, z, ylab = "cos(x)", xlab = "x", xlim = c(-4, 4), 
     ylim = c(-1, 1), type = "l", col = 1)
lines(u, Taylor(u, 1), lty = 2,  col = 2)
lines(u, Taylor(u, 2), lty = 3,  col = 3)
lines(u, Taylor(u, 3), lty = 4,  col = 4)
legend(2.7, 1,
          legend=c("n = 1", "n = 2", "n = 3"),
          lty = c(2,3,4),
          col = c(2,3,4))

#exercise 4 ------------------------------------------------------------------
p = 1:4

p4 = function(p){
  b = 0
  for(j in 1:(p+1)){
    b = j^p + b
  }
  return(b)
}
for(l in 1:length(p)){
  xx = p4(l)
  cat(sprintf("Sum for p = %d is %d\n", l, xx))
}

#exercise 5 ------------------------------------------------------------------
z = 10
t = c(2^z)
for(i in (z-1):0){
  t = c(t, 2^i)
}
cat(t)



int = function(x){
  n = trunc(x)
  m = x - n
  #INTEGER 
  #-----------------
  z = floor(log2(n))
  t = c(2^z)
  for(i in (z-1):0){
    t = c(t, 2^i)
  }
  #-----------------
  a = n
  c = c()
  for(j in 1:(z+1)){
    if(a < t[j]){
        c = c(c,0)
    }
    else{
      a = a - t[j]
      c = c(c,1)
    }
  }
  #DECIMALS
  #-----------------
  y = floor(log2(m))
  cat(y, "\n")
  s = 0.5
  for(i in -2:-30){
    s = c(s, 2^i)
  }
  #-----------------
  b = m
  d = c()
  for(i in 1:30){
    if(b < s[i]){
      d = c(d,0)
    }
    else{
      b = b - s[i]
      d = c(d,1)
    }
  }
  cat(c, ".", d)
}

int(625.625)
 


    