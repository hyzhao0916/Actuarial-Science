setwd("C:/Users/jvophem1/OneDrive - UvA/onderwijs/Introduction Econometrics and Act/Computerpracticum/data")
load("AEX.rdata")
attach(AEX)

##CALCULATE MEAN
n=length(aex)
mean1=0
for (i in 1:n){mean1 = mean1 +aex[i]}
mean1=mean1/n
##CALCULATE VARIANCE
var1=0
for (i in 1:n){var1=var1 + (aex[i] - mean1)^2}
var1=var1/(n-1)
res=c(mean1,var1)
print(res)
detach(AEX)