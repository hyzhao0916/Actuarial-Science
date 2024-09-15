#exercise 1 ------------------------------------------------------------------
stirling = function(n){
  for (i in 1:n){
    m = sqrt(2*pi*i)*(i/exp(1))^i
    s = factorial(i)/m
    cat(sprintf("n=%2d n!/Stirling's formula = %.8f\n", i, s))
  }
  
}
stirling(10)

#exercise 3 ------------------------------------------------------------------

times2 = function(x){
  k = length(x)
  x2 = rep(0,k)
  carry = 0
  i = k
  while(i >= 1){
    tmp = 2*x[i] + carry
    carry = 0
    if (tmp > 9){
      carry = 1
      x2[i] = (tmp - 10)
    }
    else{
      x2[i] = tmp
    }
    i = i - 1  
  }
  if(carry>0){
    x2 = c(1, x2)
  }
  return(x2)
}

times2(2^1000)

#exercise 4 ------------------------------------------------------------------
b <- as.numeric(readline("Enter value for b: "))
s = 0
x = seq(2, 4, 0.1)
approx2 = function(x,b){
  g = exp(2/b)-2*(x-b)/b^2*exp(2/b)+2*(1+b)*(x-b)^2/b^4*exp(2/b)
  return(g)
}
f = exp(2/x)
g = approx2(x,3)
plot(x, g, type = "b", pch = 17, col = 4, main = "approx", xlab = 'function')
points(x,f, type = "b", pch = 16, lty = 5, col = 2)
?plot
approx2(x,1)

MSe = sqrt((1/length(f))*sum(f-g)^2)
f_mean = (1/length(f))*sum(f)
NRMSE = MSe/f_mean
cat("NRMSE for the 2nd order approximation is:", NRMSE)
cat(sprintf("%9.5f",x[1:6]))
cat(sprintf("\n"))
cat(sprintf("%9.5f",g[1:6]))

