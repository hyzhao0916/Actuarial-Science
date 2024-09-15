esieve <- function(n) {
  if (n==1) return(NULL)
  if (n==2) return(n)
  l <- c(2, seq(from=3, to=n, by=2))
  # Start counter
  i <- 1
  # Select p as the first prime number in the list, p=2.
  p <- 2
  while (p^2<=n) {
    # Remove all multiples of p from the l.
    l <- l[l==p | l%%p!=0]
    # set p equal to the next integer in l which has not been removed.
    i <- i+1
    # Repeat steps 3 and 4 until p2 > n, all the remaining numbers in the list are primes
    p <- l[i]
  }
  return(l)
}

#-------------------------------------------------------------------------------
?plot
x = c(1,2,3,4,5,6,0,-1,-2,-3)
sort(x)
if(any(x < 0)){
  cat(x < 0 )
}

mob <- c("Apr","Jan","Dec","Sep","Nov","Jul","Jul","Jun", "joris")
ms <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov",
        "Dec")
mob.fac <- factor(x=mob,levels=ms,ordered=TRUE)
mob.fac

levels(mob.fac)
new.values <- factor(x=c("Oct","Feb","Feb"),levels=levels(mob.fac),
                     ordered=TRUE)
new.values

Y <- c(0.53,5.4,1.5,3.33,0.45,0.01,2,4.2,1.99,1.01)
br <- c(0,2,4,6)
cut(x=Y,breaks=br)
?label

lab <- c("Small","Medium","Large")
cut(x=Y,breaks=br,right=F,include.lowest=T)

?sub
which(Y<1)

foo <- list(matrix(data=1:4,nrow=2,ncol=2),c(T,F,T,T),"hello")
labels(foo) <- c("mymatrix","mylogicals","mystring")
foo



foo <- c(1.1,2,3.5,3.9,4.2)
bar <- c(2,2.2,-1.3,0,0.2)
plot(foo,bar,type="b", col=5)
?plot
