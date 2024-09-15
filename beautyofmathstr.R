#1 ----------------------------------------------------------------------------
#%d means integers
#%s means string
#%f means float, deciamsl

xstr = ""
ystr = ""
for (i in 1:9){
  xstr = paste(xstr, i, sep="")
  ystr = paste(ystr, 10-i, sep="")
  cat(sprintf("%s x 8 + %d = %s\n", xstr, i, ystr))
}
sprintf()
paste("12", "3", sep="")

#2 ----------------------------------------------------------------------------
n = 1
exact = exp(-1)
approx = (1 - 1/n)^n

while(abs(exact - approx)>=0.0001){
  n = n + 1
  approx = (1 - (1/n) )^n
}
cat("The build-in value of e^(-1) is", exp(-1))
cat("The approximation is", approx)
cat("The value for n is", n)

#3 ----------------------------------------------------------------------------
install.packages("tictoc")
library(tictoc)
tic()
#algorithm 1
k = 1000
found = FALSE
a = 0
maxa = k
while (a < maxa & found == FALSE){
  a = a + 1
  b = 0
  maxb = k
  while (b < maxb & found == FALSE){
    b = b + 1
    c = 0
    maxc = k
    while (c < maxc & found == FALSE){
      c = c + 1
      if (a + b + c == k & a^2 + b^2 == c^2)
        found = TRUE
    }
  }
}
cat(a,b,c)
toc()


#algorithm 2
tic()
k = 1000
found = FALSE
a = 1
maxa = k/3
while (a < maxa & found == FALSE){
  a = a + 1
  b = a
  maxb = k
  while (b < maxb & found == FALSE){
    b = b + 1
    c = 1000 - a - b
    if (a + b + c == k & a^2 + b^2 == c^2)
        found = TRUE
    }
  }
cat(a,b,c)
toc()


#algorithm 3
tic()
k = 1000
found = FALSE
for (m in 501:1000){
  if( 500000 %% m==0){
    a = 1000 - (500000 %/% m)
    b = 1000 - m
    c = 1000 - a - b
    break
  }
}
cat(a,b,c)
toc()
