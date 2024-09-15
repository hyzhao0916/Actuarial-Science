################################################################################
################### -------- MOCK PC TEST 2023 --------- #######################

f = function(x){
  return((x-1)*(x-1.5)*(x-2))
}

x = c(1.7,1.8,1.9)
y = c(f(x[1]),f(x[2]),f(x[3]))
x1 = x[1]
x2 = x[2]
x3 = x[3]
n = 0
while(abs(x1-x2) > 0.001 & n < 20){
  n = n + 1
  y1 = f(x1)
  y2 = f(x2)
  y3 = f(x3)
  x = c(x1,x2,x3)
  y = c(y1,y2,y3)
  pp = polyfit(x,y,2)
  root = roots(pp)
  differ = c()
  for(i in 1:length(root)){
    differ = c(differ, abs(x1 - root[i]))
  }
  x3 = x2
  x2 = x1
  x1 = x1 - min(differ) #x1 is now equal to the found root
  cat(sprintf("Iteratie i %d: approx. root = %7.5f \n", n, x1))
}

################################################################################
################### -------- MOCK PC TEST 2022 --------- #######################
rm(list = ls())
cat("\f")

#1
D(expression(-(4*x^3+2*(3*x^2)+8*(2*x)+5)), "x")
f = function(x){
  return(-x^4 - 2*x^3 - 8*x^2 - 5*x)
}
df = function(x){
  return(-(4*x^3+2*(3*x^2)+8*(2*x)+5))
}
d2f = function(x){
  return(-(4*(3*x^2)+2*(3*(2*x))+8*2))
}

x = seq(-2,1,0.001)
plot(x,f(x), type = "l", col = "blue", xlim = c(-2,1), ylim = c(-22, 5),
     main = "-x^4 - 2*x^3 - 8*x^2 - 5*x")

x = c(1,0,0,0)
for(i in 1:3){
  points(x[i], f(x[i]), col = i, pch = 16)
  x[i+1] = x[i] - (df(x[i])/d2f(x[i]))
  cat(df(x[i]), d2f(x[i]), (df(x[i])/d2f(x[i])), x, x[i+1], "\n")
}
cat(sprintf("x=%7.5f after three iterations", x[length(x)]))


#2
a = -2
b = 1
iter = 0
while(abs(b-a)>1E-4){
  iter = iter + 1
  g = (sqrt(5)-1)/2
  x1 = a + (1-g)*(b-a)
  x2 = a + g*(b-a)
  if(f(x1)>f(x2)){
    b = x2
  }
  else{
    a = x1
  }
}
cat(sprintf("[a,b] = [%8.5f,%8.5f] after %2d iterations", a,b,iter))

#3
rm(list = ls())
cat("\f")

f = function(x){
  return(-x^4 - 2*x^3 - 8*x^2 - 5*x)
}

library(pracma)
#x = runif(3, -1,1)
x = c(-2,((-2+1)/2),1)
x0 = x[1]
x1 = x[2]
x2 = x[3]
y = c(f(x0),f(x1),f(x2))
x3 = (y[1]*(x1^2-x2^2) + y[2]*(x2^2-x0^2)+y[3]*(x0^2-x1^2))/(2*(y[1]*(x1-x2)+y[2]*(x2-x0)+y[3]*(x0-x1)))
x[4] = x3
y[4] = f(x3)
pp = polyfit(x,y, 2)  

z = seq(-2,1,0.001)
plot(z, f(z), type = "l", col = "blue", xlim = c(-2,1), ylim = c(-22, 5),
     main = "-x^4 - 2*x^3 - 8*x^2 - 5*x")
points(x3, f(x3), pch = 16, col = 2)
points(-0.3472504665, f(-0.3472504665), pch = 16, col = "blue")
lines(z, polyval(pp,z), col = "red", lty = "dotted")
  
################################################################################
################### -------- MOCK PC TEST 2020 --------- #######################
#1
rm(list = ls())
cat("\f")

f = function(x){
  return(0.5*exp(-x/2))
}
a = 0                 #lower bound
b = 4*log(2)+2*log(5) #upper bound

S = 0
NumInt = function(f,a,b,n){
  R = matrix(0,n,n)
  R[1,1] = (b-a)*(f(a)+f(b))*0.5
  for(j in 2:n){
    h = (b-a)/(2^(j-1))
    S = 0
    for(i in 1:(2^(j-2))){
      S = S + f(a+(2*i-1)*h)
    }
    R[j,1] = 0.5*R[j-1,1] + h*S
    for(k in 2:j){
      R[j,k] = ((4^(k-1)*R[j, k-1]) - R[j-1,k-1])/(4^(k-1)-1)
    }
  }
  return(R[n,n])
}

n = 2:6
err = c()
for(i in n){
  a = 0                 
  b = 4*log(2)+2*log(5)  
  Approx = NumInt(f,a,b,i)
  #cat(Approx)
  Exact = 0.95
  Error = Approx - Exact
  err = c(err, Error)
  cat(sprintf("n=%d: True area = %13.10f  Approx =%12.10f  Error =%15.13f \n", i, Exact,Approx,Error  ))
  
}
plot(n, log2(err), xlim = c(2,6), main = "Absolute error", ylab = "log_2(error)",xlab = "n",
     pch = 16, col = "red", type = "p")

n = 6
uniroot(function(b,a,f,n){NumInt(f,a,b,n)-0.99}, interval = c(8,10))

NumInt2 = function(b,a,f,n){
  return(NumInt(f,a,b,n)-0.99)
}
w = uniroot(NumInt2, interval = c(8,10),0,f,6)
b = w$root
cat(sprintf("For n=6 and b=%6.4f:I(0,b)=0.99", b ))
?uniroot











