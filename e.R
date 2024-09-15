#1
v = c()
for(i in 1:999){
  if(i %% 3 == 0 | i %% 5 == 0){
    v = c(v, i)
  }
  
}
cat(sum(v))


#4
for(i in 999:990){
  for(j in 999:900){
    s = i*j
    d = paste(rev(as.numeric(unlist(strsplit(as.character(s), "")))), collapse = "")
    if(s == d){
      break
    }
  }
  if(s == d){
    break
  }
}
i*j

#5
i = 2520
while(sum(i%%(1:20)) != 0){
  i = 1  + 2520
  }
cat(i) 

#3
number = as.integer(readline("type in a number that is bigger than 2: "))
flag = 1
for(i in 2:(number - 1)){
  if((number %% i) == 0){
    flag = 0
    break
  }
}

if(flag == 1){
  is.prime = number
}
514
x = 600851475143
b = c()
flag = 1
for(j in 2: (x-1)){
  if((x %% j) == 0){
    b = c(b, j)
  }
}
b
c = length(b)
c
d= c()
for(l in 1:c){
  for(m in 2:(b[l] - 1)){
    if(b[l] %% m == 0){
      d = c(d, b[l])
    }
  }
}
max(d)


#48
a = 0
for(i in 1:10){
  a = i^i + a
}
a

#7
table = data.frame(x = z[1:6], y = y[2:7])
table
cat(' x log((1+x)/(1-x)) Taylor\n');
cat('--------------------------------------\n');
for(i in 1:5){
  xx=z[i];
  cat( sprintf('%.2f %14.6f %14.6f\n',z,y, z/y))
}

#81
x = c(131, 673, 234, 103, 18, 201, 96, 342, 965, 150, 630, 803, 746, 422, 111 ,537,
      699, 497, 121, 956, 805, 732, 524, 37, 331)
a = matrix(x, nrow = 5, ncol = 5, byrow = TRUE)
a
b = dim(a)[1]
c = dim(a)[2]
a[i,j]
for(i in 1:4){
  for(j in 1:4){
    f = a[1,1]
    b = c(a[i + 1, j+1],  a[i + 1, j], a[i, j + 1])
    a[i,j] = min(b)
    
  }
}

#10
a = as.integer(readline("Enter a number: "))
flag = 0
if( a > 1){
  flag = 1
  for(i in 2:(a-1)){
    if(a%%i == 0){
      flag = 2
      break
    }
  }
}

if(a == 2){
  flag = 1
}
if(flag == 1){
  cat(a, "issa prime number")
}
if(flag == 2){
  cat(a, "aint no prime number")
}

b = c()
for(a in 2:2000000){
  if(a%% 2 != 0){
    flag = 1
    x = a
    if(x %% 3 != 0){
      flag = 1
      z = x
      for(i in 2:(z-1)){
        if(x%%i == 0){
          flag = 2
          x = x + 1
        }
      }
      if(flag == 1){
        b = c(a, b)
      }
      
    }
    c = sum(b)
  }
}
cat(c)

answer <- sum(esieve(2 * 10^6))

#d = 1 tm 50k
d = 280551801 + 15434800 + 24504678 + 41842647 - 15 
d = 362333911

#d = 50k tm 100k


x= 32768
z = as.numeric(unlist(strsplit(as.character(x), "")))
r = sum(z)

cat(' x log((1+x)/(1-x)) Taylor\n');
cat('--------------------------------------\n');
for(i in 1:5){
  xx=x[i];
  cat( sprintf('%.2f %14.6f %14.6f\n',xx,log((1+xx)/(1-
                                                       xx)),g[i]) )
}



#--------------------------------------------------------
hollow = function(b){
  a = (b-1)%/%2
  c = (b+1)%/%2
  e = c()
  for(l in 1:(b-1)){
    if(l%%2 != 0){
      e = c(e, l)
    }
  }
  cat(e, " \n")
  for(i in c:1){
    cat(rep(" ", i))
    cat("*")
    for(j in e){
      cat(rep("", 3:1))
    }
    cat("*")
    cat("\n")
    
  }
  cat(rep("*", b))
}
hollow(7)





