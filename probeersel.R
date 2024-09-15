#2022 pc lab
#exercise 1
circle <- function(r){
  for(i in 0:(2*r)){
    for(j in 0:(2*r)){
      dist = sqrt((i-r)^2+(j-r)^2)
      if(abs(dist-r)<0.5)
        cat("*")
      else
        cat(" ")
    }
    cat("\n")
  }
}
circle(3)
4/2
4%%2
8%/%2

isReversible <- function(n){
  number <- n
  if(n %% 10==0) return(FALSE)
  reversed <- 0
  while(number>0){
    reversed <- 10*reversed+(number %% 10)
    number <- number %/% 10
  }
  sum <- n+reversed
  while(sum>0){
    if((sum %% 10) %% 2==0) return(FALSE)
    sum <- sum %/% 10
  }
  return(TRUE)
}
count<-0
for(n in 1:10^4){
  if(isReversible(n) == TRUE) count<-count+1
}
cat(sprintf("There are %d reversible numbers below 10,000.\n",count))

#exercise 2
t = seq(0, 2*pi, length = 10000)
x = cos(t) - cos(80*t)*sin(t)
y = 2*sin(t) - sin(80*t)
plot(x, y, type = "l", main = "Nice Curve", ylim = c(-3, 3), xlim = c(-3, 3)
     , xlab = " ", ylab = " ")


?plot


#exercise 3
isReversible = function(n){
  number = n
  if(n %% 10 == 0) return (FALSE)
  reversed = 0
  while (number > 0){
    reversed = 10 * reversed + (number %% 10)
    number = number %/% 10
  }
  sum = n + reversed
  while (sum > 0){
    if ((sum %% 10)%% 2 == 0) 
      return(FALSE)
    else 
      sum = sum %/% 10
  }
  return(TRUE)
}
count = 0
for(n in 1:10000){
  if(isReversible(n) == TRUE )
    count = count + 1
}

isReversible(1000)
count
cat(sprintf("There are %d reversible numbers below 10,000.\n",count))       

#3
isReversible = function(n){
  number = n
  if(n %% 10 != 0){
    reversed = 0
    while (number > 0){
      reversed = 10 * reversed + (number %% 10)
      number = number %/% 10
    }
    sum = n + reversed
    while (sum > 0){
      if ((sum %% 10)%% 2 == 0) 
        return(FALSE)
      else 
        sum = sum %/% 10
    }
    return(TRUE)
  }
  return(FALSE)
}
count = 0
for(n in 1:10000){
  if(isReversible(n) == TRUE )
    count = count + 1
}

isReversible(1000)
count
cat(sprintf("There are %d reversible numbers below 10,000.\n",count)) 

