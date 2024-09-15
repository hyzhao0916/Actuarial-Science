DrawBoard <- function(A,Hum,PC){
  for(i in 1:3){
    for(j in 1:3){
      if(A[i,j]==1) cat(" ",Hum," ",sep="")
      else if(A[i,j]==2) cat(" ",PC," ",sep="")
      else cat("   ")
      if(j<3) cat("|") else cat("\n")
    }
    if(i<3) cat("---|---|---\n")
  }
  cat("\n\n")
}

ChooseLetter <- function() {
  letter <- ""
  while(!(letter=="X" | letter=="O")){
    letter <- readline("Do you want to be X or O? ")
  }
  return(letter)
}

isWinner <- function(A,pl){
  pl.won<-FALSE
  tot.cols<-colSums(A==pl)
  tot.rows<-rowSums(A==pl)
  if(any(tot.cols==3)) pl.won<-TRUE
  if(any(tot.rows==3)) pl.won<-TRUE
  if(sum(diag(A==pl))==3) pl.won<-TRUE
  if(sum(diag(A[nrow(A):1,]==pl))==3) pl.won<-TRUE
  return(pl.won)
}

ConvertNumtoCoord <- function(move){
  if(move==7) {i<-1; j<-1}
  if(move==8) {i<-1; j<-2}
  if(move==9) {i<-1; j<-3}
  if(move==4) {i<-2; j<-1}
  if(move==5) {i<-2; j<-2}
  if(move==6) {i<-2; j<-3}
  if(move==1) {i<-3; j<-1}
  if(move==2) {i<-3; j<-2}
  if(move==3) {i<-3; j<-3}
  return(c(i,j))
}

FieldFree <- function(A,move){
  coord<-ConvertNumtoCoord(move)
  i<-coord[1]; j<-coord[2]
  return(A[i,j]==0)
}

PlayerMove <- function(A){
  repeat{
    move<-as.integer(readline("What is your next move? (1-9) "))
    if(move>=1 & move<=9){
      if(FieldFree(A,move)) break
    }
  }
  return(move)
}

ChooseNextMove<-function(A){
  count<-0
  pos.moves<-vector()
  for(k in 1:9){
    if(FieldFree(A,k)){
      count<-count+1
      pos.moves[count]=k
    }
  }
  if(count>0){
    random.id<-sample.int(count,1)
    return(pos.moves[random.id])
  } else {
    return(NA)
  }
}

DetPCmove <- function(A){
  # First look if we can win in the next move
  for(k in 1:9){
    if(FieldFree(A,k)){
      coord<-ConvertNumtoCoord(k)
      B<-A
      B[coord[1],coord[2]]<-2
      if(isWinner(B,2)==TRUE) return(k)
    }
  }
  # Next, if the human can win in the next move, block it!
  for(k in 1:9){
    if(FieldFree(A,k)){
      coord<-ConvertNumtoCoord(k)
      B<-A
      B[coord[1],coord[2]]<-1
      if(isWinner(B,1)==TRUE) return(k)
    }
  }
  # Finally, determine a random move
  return(ChooseNextMove(A))
}

isBoardFull <- function(A){
  return(sum(A>0)==9)
}

doMove <- function(A,move,player){
  coord<-ConvertNumtoCoord(move)
  A[coord[1],coord[2]]<-player
  return(A)
}

TicTacToe<-function(){
  cat("Welcome to Tic-Tac-Toe!\n")
  A <- matrix(0,3,3)
  HumanLetter<-ChooseLetter()
  if(HumanLetter=='X') 
    PCletter<-"O"
  else 
    PCletter<-"X"
  DrawBoard(A,HumanLetter,PCletter)
  while(TRUE){
    move<-PlayerMove(A)
    A<-doMove(A,move,1)
    if(isWinner(A,1)){
      DrawBoard(A,HumanLetter,PCletter)
      cat("Hurray! You won\n")
      break
    } else {
      if(isBoardFull(A)){
        DrawBoard(A,HumanLetter,PCletter)
        cat("It's a draw\n")
        break
      }
    }
    PCmove<-DetPCmove(A)
    A<-doMove(A,PCmove,2)
    DrawBoard(A,HumanLetter,PCletter)
    if(isWinner(A,2)){
      cat("The computer has won! Better luck next time...\n")
      break
    } else {
      if(isBoardFull(A)){
        DrawBoard(A,HumanLetter,PCletter)
        cat("It's a draw\n")
        break
      }
    }
  }
}