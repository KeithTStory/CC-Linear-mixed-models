library(tidyverse)
library(lme4)
library(stargazer)
library(dotwhisker)
library(broom)

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

#by separating data and performing separate analyses, you decrease the sample size and increase type I error. BAD!
#Alternatively, do a linear mixed model!

#This model is NOT a linear mixed effects model:
#Rather, this model looks at how the mountain ranges differently affect test scores.
#From this, we find that body length was indeed not significant when considering mountain range.
mountain_lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain_lm)


#when working with random effects, it is suggested that they have at least 5 levels. Otherwise use them as fixed variables.

first_lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(first_lmer) #note that the mountain ranges explain ~60% of the variance after accounting for fixed effects.

plot(first_lmer) #look at residual variance. Should be mostly even.
qqnorm(resid(first_lmer)) 
qqline(resid(first_lmer)) #quantiles should follow diagonal line

#from str(dragons), we see that each mountain range has three sites. There may be variability in these sites.
#This is an implicit nesting (the site means nothing unless it is paired with the mountain range).
#To look at the effect of the site, we can explicitly nest it:

dragons <- within(dragons, sample <- factor(mountainRange:site)) #creates a new column "sample" that binds the mountain range and site together.

#the next model is wrong as it treats mountain range and site as if they are crossed (not nested) 
second_lmer_WRONG <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|site), data = dragons)



#in the correct model, the nested factor is used:
second_lmer_CORRECT <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), data =dragons)
summary(second_lmer_CORRECT)
#this model accounts for random effect variation in mountain range as well as the nested site.

#This plot is similar to an earlier plot that faceted the testScore ~ bodyLength2, but has the model added. 
#From the plots we see that body length doesn't have an impact on test scores when we account for location!
(lmer_plot <- ggplot(data = dragons, aes(x = bodyLength2, y = testScore, color = site)) +
  geom_point() +
  facet_wrap(~mountainRange, nrow = 3) +
  geom_line(data = cbind(dragons, pred = predict(second_lmer_CORRECT)), aes(y = pred)) +
  theme(legend.position = "none"))

#Now to present the results!

stargazer(second_lmer_CORRECT, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

#the dotwhisker library allows for visualization of model coefficients. 
#The output shows the relative effect of a certain model effect.
dwplot(second_lmer_CORRECT,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))
#In this case, it is clear that bodyLength2 does not have an observably positive or negative effect.

