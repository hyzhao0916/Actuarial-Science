#variance 3 manieren
#1. var(#naam van column = name)
#2. (1/(n-1))*sum((#name^2-(n*mean(name)^2)))
#3. (1/(n-1))*(sum(name^2)-(n*mean(name)^2))

load("~/Downloads/AEX.RData")
View(AEX)

t=seq(1,2870,1)
reg=lm(aex~1+t)
summary(reg)
plot(aex)
abline(reg)

reg1=lm(aex~1+t,subset=t<=1500)
par(col="cyan")
plot(aex)
abline(reg1)
