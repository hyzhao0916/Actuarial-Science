#vraag 3.c
collatx_length <- function(n) {
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

next_collatz(13)#vraag 3.b
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