#programming and numerical analysis
update.packages()

x <- rep(c(5,12,13)); x
x
rep(x, 4)
rep(x, each = 2, 4)

y <- c(1,2, 3, 4)
y[2:3]
y[-2:-3]

A <- 5.5 : 0.5 ; A
A <- c(5.5 : 0.5) ; A
A - 1:2

B <- matrix(1:9, nrow = 3, ncol = 3);B
B[2:1, c(2,3) ] <- c(65, -65, 88, -88);B

C = rbind(c(2, 5, 2), c(6, 1, 4));C
D = cbind(c(3,-1,1), c(-3,1,5));D
C %*% D #denk aan Linear algebra matrix matrix multiplication
D %*% C
C*D #works only if element wise product
D*C #works only if element wise product, so both matricies needs to be the same size e.g. 2x2 matrix and 2x2 matrix

solve(D)
solve(B)
B%*%solve(B) # = solve(B) %*% B
B*solve(B) # = solve(B) * B
