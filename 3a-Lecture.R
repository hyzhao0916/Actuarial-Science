###
pyramid <- function(height){
  if(height>0 & height<9){
    for(i in 1:height){
      cat(rep(" ",height-i),rep("#",i),"\n",sep="")
    }
  }
}
pyramid(3)

###
cat(sprintf("  (i): pi=%f\n",pi))
cat(sprintf(" (ii): pi=%.2f\n",pi))
cat(sprintf("(iii): pi=%11.7f\n",pi))


###
1/6
sprintf('%.4f', 1/4)
cat(sprintf("Hi, %s, %s and %s","Tick","Trick","Track"))
cat(sprintf("Hello, %s, pi is 6 decimals equals %.6f\n","Donald",pi))
sprintf("I woke up at %s:%s%s a.m.", 8, 0, 5)


###
sum=1
for(i in 1:10){
  sum=sum+(-1)^i/(2*i+1)
  cat(sprintf("Leibniz approximation after %2d terms: %10.6f\n",i,4*sum))
}


###
library(ggplot2)
library(MASS)
ggplot(data = txhousing, aes(x=volume, y=sales))


ggplot(txhousing, aes(x=volume, y=sales)) +
  geom_point() +
  geom_rug(aes(color=median))


ggplot(txhousing, aes(x=median)) +
  geom_histogram()


ggplot(txhousing, aes(x=factor(year), y=median)) + geom_boxplot()


ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_point() +
  xlim(c(1,3)) # cut ranges from 0 to 5 in the data


ggplot(diamonds, aes(x=carat, y=price)) + geom_point() +
facet_wrap(~cut) # create a ribbon of plots using cut


###

set.seed(1)
runif(1)
runif(1)
set.seed(1)
runif(1)
runif(1)


###

set.seed(2)
runif(1)
runif(1)
set.seed(3)
runif(1)
runif(1)


###
sample.int(6,1)
sample.int(5:100,5, replace = TRUE)


###
sample.int(6,5, replace = TRUE)
hist(sample.int(6,100000, replace = TRUE))


###
runif(1)
runif(1)
runif(5)

###
printIntro <- function(){
  # Prints an introduction to the program
  cat("This program simulates a game of racquetball between two\n")
  cat('players called "A" and "B".  The abilities of each player is\n')
  cat("indicated by a probability (a number between 0 and 1) that\n")
  cat("the player wins the point when serving. Player A always\n")
  cat("has the first serve.\n")
}

getInputs <- function(){
  # RETURNS a vector with probA, probB, number of games to simulate
  a = as.numeric(readline("What is the prob. player A wins a serve? "))
  b = as.numeric(readline("What is the prob. player B wins a serve? "))
  n = as.numeric(readline("How many games to simulate? "))
  return(c(a, b, n))
}

simNGames <- function(n, probA, probB){
  # Simulates n games of racquetball between players A and B
  # RETURNS number of wins for A, number of wins for B
  winsA <- 0
  winsB <- 0
  for(i in 1:n){
    scores <- simOneGame(probA, probB)
    scoreA <- scores[1]; scoreB <- scores[2]
    if(scoreA>scoreB)
      winsA <- winsA + 1
    else
      winsB <- winsB + 1
  }
  return(c(winsA,winsB))
}


simOneGame <- function(probA, probB){
  # Simulates a single game or racquetball between players A and B
  # RETURNS A's final score, B's final score
  serving <- "A"
  scoreA <- 0
  scoreB <- 0
  while( !gameOver(scoreA, scoreB) ){
    if(serving == "A"){
      if( runif(1) < probA )
        scoreA = scoreA + 1
      else
        serving = "B"
    } else {
      if( runif(1) < probB )
        scoreB = scoreB + 1
      else
        serving = "A"
    }
  }
  return(c(scoreA, scoreB))
} 

gameOver  <- function(a,b){
  # a and b are scores for players in a racquetball game
  # RETURNS true if game is over, false otherwise
  return( (a == 15) | (b == 15) )
}

printSummary <- function (winsA, winsB){
  # Prints a summary of wins for each player.
  n <- winsA + winsB
  cat(sprintf("\nGames simulated: %d\n", n))
  cat(sprintf("Wins for A: %d (%.2f%%)\n",winsA, 100*winsA/n))
  cat(sprintf("Wins for B: %d (%.2f%%)\n",winsB, 100*winsB/n))
}

printIntro()
pars <- getInputs()
probA <- pars[1]; probB <- pars[1]; n <- pars[3]
results <- simNGames(n, probA, probB)
winsA <- results[1]; winsB <- results[2];
printSummary(winsA, winsB)


###
gameOver(0,0)
gameOver(5,10)
gameOver(15,10)
gameOver(3,15)


###
simOneGame(.5, .5)
simOneGame(.5, .5)
simOneGame(.4, .9)
simOneGame(.9, .4)
