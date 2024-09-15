# Lecture 1A ---------------------------------------------------------------
# chapter 2 ------------------------------------------------------------
# Exercise 2.1 ------------------------------------------------------------
#a
y <- (6*a+42) * 3^(-(4.2-3.62));y 
a=2.3

#b
(-4)^2 + 2
#-> i.

#c
sqrt(0.5*(0.2*(25.2+15+16.44+15.3+18.6)))

#d
log(x = .3)

#e
exp(.3)

#f
print("−0.00000000423546322")

# Exercise 2.2 ------------------------------------------------------------
#a
x <- (3^2*4^(1/8));x

#b
x <- x/2.33;x

#c
y <--8.2*10^(-13);y

#d
x*y

# Exercise 2.3 ------------------------------------------------------------
#a
v <- seq(5,-11,-0.3);v

#b
v<-sort(v, decreasing = FALSE);v

#c
v <- c(-1,3,-5,7,-9)
c <- rep(v, each=10, times=2, decreasing = TRUE);c

#d
w <-c(6:12, rep(x = 5.3, times = 3),-3, seq(102, to = length(c), length = 9));w

#e
length(w)

# Exercise 2.4 ------------------------------------------------------------
#a
a <- c(seq(3,6, length = 5), rep(c(2,-5.1,-33), times = 2), 7/42+2);a

#b
b <- a[c(1, length(a))];b

#c
c <- a[-c(1, length(a))];c

#d
d <- c(b[1], c, b[2]) ;d

#e
d <- sort(d, decreasing = FALSE);d

#f
e <-c(d[length(d):1]);e
d <- sort(d, decreasing = TRUE);d

#g
f <- c((rep(c[3], times = 3)),(rep(c[6], times = 4)), (rep(c[length(c)], times = 1)));f

#h
z <- e
z[c(1, 5:7, length(e))] <- 99:95;z
#g <- (z[c(1, 5:7, length(e))] <- 99:95);g werkt niet


# Exercise 2.5 ------------------------------------------------------------
#a
c(2,0.5,1,2,0.5,1,2,0.5,1) / c(2,0.5,1)

#b
v <- c(45, 77, 20, 19, 101, 120, 212)
c <- (5/9) * (v-32);c

#c
v <- c(2,4,6);v
g <- rep(v, times = 2)*rep(c(1,2), each = 3);g

#d
g[2:5] <- c(-0.1, -100);g
g

# Lecture 1B ------------------------------------------------------------
# chapter 3 -------------------------------------------------------------
# Exercise 3.1 ------------------------------------------------------------
#a
a <- matrix(data = c(4.3, 3.1, 8.2, 8.2, 3.2, .9, 1.6, 6.5), nrow = 4 , ncol = 2, byrow = TRUE);a

#b
dim(a[-1,])

#c
c <- a
b<-sort(c[,2], decreasing = FALSE);b
c[,2] <- b; c

#d
d<-c[-4,-1]
matrix(d)

#e
e<-matrix(data = c[3:4, 1:2], nrow = 2, ncol = 2) ;e

#f
f<- -0.5* diag(e);f
c[c(4,1), 2:1] <- f
c

# Exercise 3.2 ------------------------------------------------------------
#a
a <- cbind(c(1,2,7), c(2,4,6))
b <- rbind(c(10,20), c(30,40), c(50,60))
c <- 2/7 * (a - b);c

#b
a = cbind(c(1,2,7))
b = cbind(c(3,4,8))
#i
a %*% b #not possible
#II
t(a)%*%b
#III
t(b)%*%(a%*%t(a))
#IV
(a%*%t(a))%*%t(b) #not possoble
#V
solve((b%*%t(b)) + (a%*%t(a)) - 100* diag(x = 3))

#c
c <- diag(x = c(2,3,5,-1));c
solve(c) %*%c - diag(4)

# Exercise 3.3 ------------------------------------------------------------
#a
ar <- array(data = seq(4.8, 0.1, length = 48), dim = c(4,2,6));ar

#b
ar2 <- ar[c(4,1),2,];ar2

#c
ar3 <- array(data = rep(ar2[2,], times = 4), dim = c(2,2,2,3));ar3

#d
ar4<- ar[,,-6];ar4

#e
ar4[c(2,4), 2, c(1,3,5)] <- -99

# chapter 4 ------------------------------------------------------------
# Exercise 4.1 ------------------------------------------------------------
#a
a<-c(6,9,7,3,6,8,9,6,3,6,6,7,1,9,1);a
#I 
a == 6
#II
a >= 6
#III, haakjes hoeven nie perse
a <= (6 + 2)
#IV
a != 6

#b
b<-a[-(1:3)];b
ar <- array(b, dim = c(2,2,3))
#I
ar <= (6/2 + 4)
#II
ar <- ar + 2
ar <= (6/2 + 4)
# or ar + 2 <= (6/2 + 4)

#c
diag(10) == 0

#e
e <- diag(10)
e[-diag(10)]

# Exercise 4.2 ------------------------------------------------------------
#a
foo <-c(7,1,7,10,5,9,10,3,10,8)
(foo >= 5 | foo == 2)

#b
bar <- c(8,8,4,4,5,1,5,6,6,8)
(bar<= 6 & bar != 4)

#c
((foo >= 5 | foo == 2) & (bar<= 6 & bar != 4))

#d
baz <-c(foo + bar);baz
#or baz <-foo + bar ; baz
#I
(baz >=14 & baz != 15 )
#II
baz/foo >= 4 | baz/foo <=2

#e
(foo>5)||(foo==2)
(bar<=6)&&(bar!=4)
((foo>5)||(foo==2))&&((bar<=6)&&(bar!=4))
(baz>=14)&&(baz!=15)
(baz/foo>4)||(baz/foo<=2)

# Exercise 4.3 ------------------------------------------------------------
#a
foo <- c(7,5,6,1,2,10,8,3,8,2)
#i
bar <- foo[foo>=5];bar
#ii
bar <- foo[foo>=5]
bar
foo[-which(x=foo>=5)]

#b
baz <- matrix(data = foo, nrow = 2, ncol = 3, byrow = T);baz
#i
baz[baz== 8] <- baz[1,2]^2
baz
#ii
all(baz<= 25& baz >4)

#c
qux <- array(data = c(10,5,1,4,7,4,3,3,1,3,4,3,1,7,8,3,7,3), dim= c(3,2,3));qux
#i
which(qux == 3 | qux == 4, arr.ind = T)
#ii
qux[qux<=3 | qux >= 7] <- 100
qux

#d
foo[c(F,T)]
foo[c(0,1)]


