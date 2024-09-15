load("~/AS1/intro to cnmtrcs and as/R studio/ds.rdata")

View(nlsy97)

library(dplyr)

#specifying subset: deleting irrelevant variables for this study
dataset <- nlsy97 %>%
  select(siblings, male, female, wagealt, hgrade2, tenure2017, health2017, hdegmoth, hdegfath)  

View(dataset)


#cleaning process: deleting negative observations from wagealt and number of siblings
clean = subset(dataset, siblings >=0 & wagealt > 0 & hgrade2 >= 0 & tenure2017 >= 0 & health2017 >= 0 & hdegmoth >=0 & hdegfath >= 0 )
View(clean)

#making dummy variable
attach(clean)
health2017_bad <- ifelse(health2017 > 4, 1, 0)

lowdegmoth <- ifelse(hdegmoth >= 0 & hdegmoth < 4, 1, 0 )
middegmoth <- ifelse(hdegmoth >= 4 & hdegmoth < 6, 1, 0 )
hghdegmoth <- ifelse(hdegmoth > 5, 1, 0 )

lowdegfath <- ifelse(hdegfath >= 0 & hdegfath < 4, 1, 0 )
middegfath <- ifelse(hdegfath >= 4 & hdegfath < 6, 1, 0 )
hghdegfath <- ifelse(hdegfath > 5, 1, 0 )


#regresion analyse 1 direct impact

lwagealt = log(wagealt)
reg_dir = lm(lwagealt ~ 1 + siblings + male + tenure2017 + tenure2017^2 + 
               health2017_bad + lowdegmoth + middegmoth + hghdegmoth+ 
               lowdegfath + middegfath + hghdegfath + hgrade2)
summary(reg_dir)


