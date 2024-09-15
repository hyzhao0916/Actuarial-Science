#1
t = 1:3
s = rep(0, 3)
a = rbind(t, s, s, s)
r = 4:1
cbind(a,r)

#2
A <- matrix(1,3,3);A
B <- 2*matrix(1,3,2);B
C <- 3*matrix(1,2,3);C
cbind(A,B)
#cbind(A,t(B))
#cbind(A,C)
cbind(A,t(C))
rbind(A,C)
rbind(A,t(B))

#3
a = matrix(c(1,2,3,4),2,2, byrow = TRUE)
b = matrix(c(3,4,-1,2),2,2, byrow = TRUE)
a%*%b

a = 2*matrix(c(3,5,6,-2),2,2, byrow = TRUE)
b = -4*matrix(c(-1,0,2,1),2,2, byrow = TRUE)
a%*%b

a = matrix(c(1,3,5),1,3, byrow = TRUE)
b = matrix(c(2,-1,-1,0,7,-2),3,2, byrow = TRUE)
a%*%b

#4
A = matrix(c(1,1,2,1,-1,-3,-2,-5,1), 3,3, byrow = TRUE)
b = c(1,0,4)
solve(A,b, fractions = TRUE)

#5
A = matrix(c(3,2,3,-2), 2,2, byrow = TRUE)
dim(A)
b = c(7,7)
solve(A,b, fractions = TRUE)

A = matrix(c(1,1,1,1,1,-1,1,1,0), 3,3, byrow = TRUE)
b = c(1,0,0)
solve(A,b, fractions = TRUE)

b = rep(1,6)
for(i in 2:6){
  a = rep(1,6)
  a[i] = -1
  b = rbind(b, a)
}
b
c = rep(1,6)
solve(b,c, fractions = TRUE)

#6
library(Ryacas)
#a
A = matrix(c(0,1,s,s,0,1,1,s,0), 3, 3, byrow = TRUE)
det(A)
yac("A:={{0,1,s},{s,0,1},{1,s,0}}")
yac("Determinant(A)")

#b
y = rep(0,4)
z = c()
for(i in 1:4){
  s = (i - 1)*pi/4
  A = matrix(c(0,1,s,s,0,1,1,s,0), 3, 3, byrow = TRUE)
  y[i] = det(A)
  z = c(z,s)
}
polyfit(z,y,3 )

#7
#a
A = diag(4) 
A[1,4] = A[4,1] = -1

r = eigen(A)
s = r$values; s
V = r$vectors; V

#b
for(i in 1:4){
  v = A%*%c(V[,i])
  d = matrix(c(s[i]%*%V[,i]))
  w = cbind(v,d)
  cat(sprintf("A*v_%d \t lambda_%d*v_%d: \n", i, i, i))
  for(i in 1:4){
    for(j in 1:2){
      cat(sprintf("%8.5f  ", w[i,j]))
    }
    cat("\n")
  }
  cat("\n")
}












