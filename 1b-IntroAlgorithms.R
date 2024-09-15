## -------------------------------------------------------------------------------------------------
Armstrong <- function(x){
  if(x%%10==0) return(FALSE)
  else {
    tmp <- x
    count <- 0
    while(tmp>0){
      count <- count+1
      tmp <- tmp%/%10
    }
    tmp <- x
    som <- 0
    while(tmp>0){
      som <-  som + (tmp%%10)^count
      tmp <- tmp%/%10
    }
    if(som==x) return(TRUE) else return(FALSE)
  }
}
Armstrong(153)


## -------------------------------------------------------------------------------------------------
Armstrong <- function(x){
  if(x%%10==0) return(FALSE)
  else {
    xstr <- toString(x)
    count <- nchar(xstr)
    som <- 0
    for(i in 1:count){
      som <-  som + as.numeric(substr(xstr,i,i))^count
    }
    if(som==x) return(TRUE) else return(FALSE)
  }
}
Armstrong(153)

