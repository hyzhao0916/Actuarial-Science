load("~/AS1/period 3/etrics/R studio/ds.rdata")

View(nlsy97)

library(dplyr)


#specifying subset: deleting irrelevant variables for this study
dataset <- nlsy97 %>%
  select(siblings, male, wagealt, hgrade2,
         tenure2017, health2017, hdegmoth, hdegfath)  

View(dataset)

#cleaning process: deleting negative observations from wagealt and number of siblings
clean = subset(dataset, siblings >= 0 & wagealt > 0 & hgrade2 >= 0 & hgrade2 < 90 &
                 tenure2017 >= 0 & health2017 >= 0 & hdegmoth >= 0 & hdegfath >= 0 )
View(clean)

#making dummy variables and adjustments to variables --------------------------------
attach(clean) 

health2017_bad <- ifelse(health2017 > 4, 1, 0)

middegmoth <- ifelse(hdegmoth >= 4 & hdegmoth < 6, 1, 0 )
hghdegmoth <- ifelse(hdegmoth > 5, 1, 0 )

middegfath <- ifelse(hdegfath >= 4 & hdegfath < 6, 1, 0 )
hghdegfath <- ifelse(hdegfath > 5, 1, 0 )

tenure2 = (tenure2017*52)^2
tenure = tenure2017 * 52


#regression analysis -----------------------------------------------------------
#1 direct impact of number of siblings on earnings
lwagealt_sib_on_earn = log(wagealt)

reg_dir_sib_on_earn = lm(lwagealt_sib_on_earn ~ 1 + siblings + male + health2017_bad + 
               hgrade2 + tenure + tenure2 + middegmoth + hghdegmoth + 
               middegfath + hghdegfath)
summary(reg_dir_sib_on_earn)

#2 indirect impact on earnings through a direct effect on education
leducation = hgrade2
reg_indir_sib_on_educ = lm(leducation ~ 1 + siblings + male + health2017_bad + 
                       tenure + tenure2 + middegmoth + hghdegmoth + 
                       middegfath + hghdegfath)
summary(reg_indir_sib_on_educ)

#3 direct effect of education on income
lwagealt_educ_on_earn = log(wagealt)
reg_dir_educ_on_earn = lm(lwagealt_educ_on_earn ~ 1 + male + health2017_bad + hgrade2 +
                            tenure + tenure2 + middegmoth + hghdegmoth +
                            middegfath + hghdegfath)
summary(reg_dir_educ_on_earn)

#Descriptive statistics -------------------------------------------------------
#siblings
avg_rough_sib <- clean %>%
  group_by(siblings) %>%
  summarise(avg_earnings_sib = mean(wagealt),
            sd_earnings_sib = sd(wagealt),
            n())
View(avg_rough_sib)

ggplot(avg_rough_sib, aes(x = siblings,
                          y = avg_earnings_sib)) +
  geom_point() 

#education
avg_rough_educ <- clean %>%
  group_by(hgrade2) %>%
  summarise(avg_earnings_educ = mean(wagealt),
            sd_earnings_educ = sd(wagealt),
            n())
View(avg_rough_educ)

ggplot(avg_rough_educ, aes(x = hgrade2,
                          y = avg_earnings_educ )) + geom_point() 
  

#finding mean and standard deviation values for the variables in our study
summary(clean) #show only mean
sapply(clean, sd) # shows standard deviation
