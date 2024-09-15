#vraag 4
# Determine the number of living neighbours in the grid x for
# the cell at position (i,j).
Neighbours <- function(x,i,j) {
  living <- 0
  # Check if there is a top-left neighbour (in the first
  # row and column there isn’t) and add to living
  # if the top-left neighbour is living.
    if( (i-1)>0 & (j-1)>0 ) living<-living+x[i-1,j-1]
    if( (i-1)>0 ) living<-living+x[i-1,j]
    if( (i-1)>0 & (j+1)<6 ) living<-living+x[i-1,j+1]
    if( (j-1)>0) living<-living+x[i,j-1]
    if( (j+1)<6) living<-living+x[i,j+1]
    if( (i+1)<6 & (j-1)>0 ) living<-living+x[i+1,j-1]
    if( (i+1)<6) living<-living+x[i+1,j]
    if( (i+1)<6 & (j+1)<6 ) living<-living+x[i+1,j+1]
    return(living)
}
# Compute the new grid y representing the next day from the
# old grid x.
x=matrix(c(1,0,0,0,1,1,0,1,0,0,0,0,0,1,1,0,1,0,1,0,0,0,1,0,1),
         5,5)
y <- matrix(0,5,5)
for(i in 1:5) {
  for(j in 1:5) {
    if(x[i,j]==0){
      if(Neighbours(x,i,j)==3) 
        y[i,j]<-1
    }
    if(x[i,j]==1){
      count <- Neighbours(x,i,j)
      if((count==2) | (count==3)) 
        y[i,j]<-1
    # DETERMINE IF y[i,j] WILL BE LIVING OR DEAD BY
    # USING Neighbours(x,i,j).
    # YOUR CODE HERE
    # ...
  }
  }
}
y
sum(y)

#vraag 3.d
# initialize, L geeft aan dat het een integer is
m = 1000000L
len = c(1,2,8,3,6, rep(0L, m-5))

for(n in 6:m){
  x = n
  count = 0
  # calculate len[n]
  while (x >= n) {
    count <- count + 1
    if(x %% 2 == 0) {
      x = x %/% 2
    } else {
      x = 3*x + 1
    }
    # test for a cycle
    if (x == n) {
      stop(paste("Starting value",n,"leads to a cycle!",sep="
"), call.=FALSE)
    }
  }
  # populate len[x]
  len[n] <- count+len[x]
}
# finalize: find and dump results
# find the starting value with the highest number of steps
# initialize
steps = 0
n_start = 0
# update steps and n_start
for(n in 1:m){
  if(len[n]>steps){
    steps = len[n]
    n_start = n
  }
}
# print result
cat("Starting value ",n_start," takes ",steps," steps.")




#vraag 3.c
collatz_length <- function(n) {
  count <- 1
  for (n in 2:100){
    count <- count + 1
    if (n%%2 == 0)
      n <- (n/2)
    else{
      n <- (3*n + 1)
    }
  }
  if (count >= max_length)
  
  return(count)
}
collatz_length(13)


next_collatz(13)

#vraag 3.b
next_collatz <- function(n) {
  count <- 1
  while (n>1){
    count <- count + 1
    if (n%%2 == 0)
      n <- (n/2)
    else{
      n <- (3*n + 1)
    }
  }
  return(count)
}

next_collatz(13)