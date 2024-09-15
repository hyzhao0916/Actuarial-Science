data()
View(USArrests)
?USArrests
?mean

glimpse(USArrests)

USArrests_murder <- filter(USArrests, Murder <= 5.0)
view(USArrests_murder)

USArrests_metric <- mutate(USArrests, Murder_metric = 1 + Murder)
glimpse(USArrests_metric)

USArrests_metric <- USArrests %>% 
  mutate(Murder_metric = 1+ Murder)

ggplot(USArrests, aes(x = Murder)) +
  geom_freqpoly() +
  labs(x = "doodgevallen")

ggplot(USArrests, aes(x = Murder)) +
  geom_histogram() +
  labs(x = "doodgevallen")

ggplot(USArrests, aes(x = Murder)) +
  geom_freqpoly() +
  geom_histogram() +
  labs(x = "doodgevallen")

ggplot(USArrests, aes(x = Murder,
                      y = UrbanPop)) +
  geom_point() +
  geom_smooth(method = "lm")
