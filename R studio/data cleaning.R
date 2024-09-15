library(tidyverse)

View(nlsy97)

glimpse(nlsy97)

filter(nlsy97, siblings >= 0, wage >= 0)
nlsy97_metric <- filter(nlsy97, siblings >= 0, wage >= 0, )
view(nlsy97_metric)

attach(nlsy97_metric)


mal = subset(nlsy97_metric, male == 0)

view(mal)

regm = lm(lwage~1 + siblings + hdegree, subset = male ==1)
summary(regm)


