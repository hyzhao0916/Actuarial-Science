a = 0
b = 1
s = 0
v = 0
while(a < 4000000 | b < 4000000){
  a = a + b
  b = a + b
  if(a%%2 == 0 ){
    s = a + s
  }
  if( b %% 2 == 0){
    v = b + v
  }
  
}
cat(s + v)

s = 0
d = rep(x = 1:10, times = 2, each = 4)
if (d%%2 == 0){
  s = a + s
}

d = rep(x = 1:10, times = 2, each = 4)
d
which(d<4)

A <- matrix(c(0.3,4.5,55.3,91,0.1,105.5,-4.2,8.2,27.9),nrow=3,ncol=3)
A
A[c(T,F,F),c(F,T,T)]
?grep
bar <- "How much wood could a woodchuck chuck"
sub(pattern="chuck",replacement="hurl",x=bar)
gsub(pattern="chuck",replacement="hurl",x=bar)
