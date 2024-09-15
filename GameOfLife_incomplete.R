# Determine the number of living neighbours in the grid x for 
# the cell at position (i,j).
Neighbours <- function(x,i,j) {
  living <- 0
  # Check if there is a top-left neighbour (in the first 
  # row and column there isn’t) and add to living 
  # if the top-left neighbour is living. 
  if( (i-1)>0 & (j-1)>0 ) {
    living <- living + x[i-1,j-1]
  }
  
  # IMPLEMENT OTHER DIRECTIONS HERE
  # YOUR CODE HERE
  # ...
  
  # Check if there is a bottom-right neighbour (in the last
  # row and column there isn’t) and add to living if the 
  # bottom-right neighbour is living.
  if( (i+1)<6 & (j+1)<6 ) {
    living <- living + x[i+1,j+1]
  }
  return(living)
}

# Compute the new grid y representing the next day from the 
# old grid x.
x=matrix(c(1,0,0,0,1,1,0,1,0,0,0,0,0,1,1,0,1,0,1,0,0,0,1,0,1),5,5)
y <- matrix(0,5,5)
for(i in 1:5) {
  for(j in 1:5) {
    # DETERMINE IF y[i,j] WILL BE LIVING OR DEAD BY
    # USING Neighbours(x,i,j).
    # YOUR CODE HERE
    # ...
  }
}
y
sum(y)
