library(TeachingDemos)
n = 900
for(i in 999:n){
  for(j in 990:n){
    c = digits(i * j)
    if(c[1] == c[6] & c[2] == c[5] & c[3] == c[4]){
      cat("you've found the larges palindrome", c)
    }
  }
}  







