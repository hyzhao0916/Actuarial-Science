library(tidyverse)

view(nlsy97)

?filter
nlsy97_eff <- filter(nlsy97, siblings >= 0, wage >= 0)
view(nlsy97_eff)

efficient <- nlsy97_eff %>%
  select(-c(marstatus: weight2017, whours:exp, industrycode, jobcode ))
view(efficient)

ggplot(efficient, aes(x = siblings)) +
  geom_histogram()

ggplot(efficient, aes(x = siblings)) +
  geom_freqpoly()

ggplot(efficient, aes(x = siblings,
                      y = wage)) +
  geom_point() +
  geom_smooth(method = 'lm')
