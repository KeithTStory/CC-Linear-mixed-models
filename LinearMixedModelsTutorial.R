library(tidyverse)

load("dragons.RData")
head(dragons)

hist(dragons$testScore)

#scale the date so the mean is 0. This makes it easier to compare effects sizes.
dragons$bodyLength2 <- scale(dragons$bodyLength)
hist(dragons$bodyLength2)

#Basic linear model
first_lm <- lm(testScore ~ bodyLength2, data = dragons)
summary(first_lm)

#visualizing the linear model. Shows that bigger dragons have better scores, which is odd.
(first_lm_plot <- ggplot(data = dragons, aes(x = bodyLength2, y = testScore)) +
  geom_point() +
  geom_smooth(method = "lm"))

#look at the residuals, specifically the first plot. Residuals show a trend, which is not good (means difference in variance)
plot(first_lm, which = 1)
plot(first_lm, which = 2) #Note that the residuals do not follow a solid diagonal line.

#look at other variables that might explain the bodysize trend. Possible co-variates.
boxplot(testScore ~ mountainRange, data = dragons) #plot shows that there is variation associated with mountain range.

#Look at the scatterplot again, but this time separate the data by mountain ranges
ggplot(data = dragons, aes(y = testScore, x = bodyLength2, colour = mountainRange)) +
  geom_point() 
#The plot shows that there is a nested factor of importance.

#another way to visualize the data is with facets.
ggplot(data = dragons, aes(y = testScore, x = bodyLength2)) +
  geom_point() +
  facet_wrap(~ mountainRange)



