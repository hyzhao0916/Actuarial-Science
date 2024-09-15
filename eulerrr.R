#6 euler- werkt godzijdank
n = 1:100
b = sum(n^2)
c = sum(n)^2
a = c - b; a

fac = function(n){
  a = 1
  for(i in 100:1){
    a = i * a
  }
  return(a)
}

#25
kkrfibo = function(n){
  a = 1
  b = 1
  count = 0
  for(i in 1:n){
    count = count + 1
    a = c(a + b)
    count = count + 1
    b = c(a + b)
    
    }
  }
  cat(max, " ", a, " ", " ", b)
kkrfibo(10)

#29 - Euler Werkt godzijdank
g = function(a1, a2, b1, b2){
  c = c()
  for(i in a1:a2){
    for(j in b1:b2){
      c = c(c, i^j)
    }
  }
  d = sort(c)
  #cat("orgineel",d, "\n")
  e = length(d)
  #cat("lengte", e, "\n")
  h = c()
  f = 0
  for(l in 2:e){
    if(d[l] > d[l-1]){
      h = c(h, d[l])
      #cat(h)
    }
  }
  #cat(h)
  m = sort( h)
  n = length(h)
  cat("\n", n)
}
g(2,100,2,100)

#30- kkr shit werkt niet ouwe
kkr = function(n){
  A = 0
  B = 0
  C = 0
  D = 0
  E = 0
  G = 0
  for(i in 1:n){
    A = i^5
    for(j in 1:n){
      B = i^5
      for(k in 1:n){
        C = i^5
        for(l in 1:n){
          D = i^5
          H = as.numeric(unlist(strsplit(as.character(sum(A,B,C,D)), ""))[[1]])
          H[2]
          I = c(A,B,C,D)
          
        }
      }
    }
  }
  if(H[1] == A & H[2] == B & H[3] == C & H[4] == D){
    return(A,B,C,D)
  }
}
kkr(9)

#31

#h7 plot
t = c(seq(0, 2*pi, length.out =10000 ))
x = cos(t) - cos(80*t)*sin(t)
y = 2*sin(t) - sin(80*t)
plot(x, y, main = "Nice Curve", col = (sample(24)), xlim = c(-3,3),  ylim = c(-3,3), type = "l")
x = c(seq(1, 4, 0.01))
g = sin(x)
cat(' x log((1+x)/(1-x)) Taylor\n');
cat('--------------------------------------\n');
for(i in 1:5){
  xx=x[i];
  cat( sprintf('%.2f %14.6f %14.6f\n',x[i],log((1+x[i])/(1-
                                                           x[i])),g[i]) )
}

