#2021 toets
# 1a ----------------------------------------------------------------------
Tn <- function(n){
  a = 0
  for (i in 1:n){
    a = c(a + i)
  }
  cat(a)
}
#Tn(5)

# 1b ----------------------------------------------------------------------
Triangular <- function(n){
  for(i in 1:n){
    a = rep(" ", times = (n-i+1))
    b = rep("*", times = i)
    cat(rep(" ",(n-i+1)), rep(" * ", i), "\n")
    #cat(a, b, a, "\n")
  }
}

Triangular(5)

Triangular<-function(n){
  for(i in 1:n){
    for(j in 1:(n-i+1)){
      cat(" ")
    }
    for(j in 1:i){
      cat("* ")
    }
    cat("\n")
  }
}
Triangular(5)

# 1c ----------------------------------------------------------------------
Ln_seq <- function(n){
  
}
  