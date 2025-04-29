#Getting Started#
rm(list=ls())
library(tidyverse)
library(readr)
library(knitr)
library(dplyr)
library(sjPlot)
setwd("~/Desktop/POLI 300/Problem Set 7")

#Loading the data#
load("~/Desktop/POLI 300/Problem Set 7/city_happiness.RData")
view(city_happy)

#Replece the values of -999 with NA#
city_happy <- city_happy %>%
  mutate_all(~ ifelse(. == -999, NA, .))

#Run a regression model to predict happiness based on how safe and walkable the city is#
reg_1 <- lm(happiness ~ safe_walk, data = city_happy)
summary(reg_1)
tab_model(reg_1, show.ci = FALSE, show.se = TRUE, p.style = "stars")

#Redefine the variable "SQ1" as a dummy variable#
#Define "Man" as 0 and "Woman" as 1#
city_happy <- city_happy %>%
  mutate(gender_dummy = ifelse(SQ1 == "Man", 0, 1)) 

#Run a regression model to predict happiness based on how safe and walkable the city is and gender#
model_1 <- lm(happiness ~ safe_walk + gender_dummy, data = city_happy)
summary(model_1)
tab_model(model_1, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars")

#Run a regression model to predict happiness based on how safe and walkable the city is for men#
ch_man <- city_happy %>%
  filter(gender_dummy == 0)
model_man <- lm(happiness ~ safe_walk, data = ch_man)
summary(model_man)
tab_model(model_man, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars")

ch_woman <- city_happy %>%
  filter(gender_dummy == 1)
model_woman <- lm(happiness ~ safe_walk, data = ch_woman)
summary(model_woman)
tab_model(model_woman, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars")

#Check for correlation between the variables#
cor(city_happy$safe_walk, city_happy$happiness, use = "complete.obs")
cor(city_happy$gender_dummy, city_happy$happiness, use = "complete.obs")
cor(city_happy$safe_walk, city_happy$gender_dummy, use = "complete.obs")
cor(city_happy$happiness, city_happy$children, use = "complete.obs")
cor(city_happy$happiness, city_happy$price_high, use = "complete.obs")
cor(city_happy$happiness, city_happy$children, use = "complete.obs")
cor(city_happy$happiness, city_happy$gov_concerns, use = "complete.obs")
cor(city_happy$happiness, city_happy$gov_transp, use = "complete.obs")
cor(city_happy$happiness, city_happy$health, use = "complete.obs")
cor(city_happy$safe_walk, city_happy$health, use = "complete.obs")
cor(city_happy$happiness, city_happy$institutions, use = "complete.obs")
cor(city_happy$happiness, city_happy$marital, use = "complete.obs")
cor(city_happy$safe_walk, city_happy$marital, use = "complete.obs")

#Re run the regression model controlling for health as well#
model_2 <- lm(happiness ~ safe_walk + gender_dummy + health, data = city_happy)
summary(model_2)
tab_model(model_2, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars")

model_3 <- lm(happiness ~ safe_walk + gender_dummy + health + marital + children, data = city_happy)
summary(model_3)
tab_model(model_3, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars")

#Run the same regression model with the control variables but for the separate genders#
model_man_cntrl <- lm(happiness ~ safe_walk + health + marital + children, data = ch_man)
summary(model_man_cntrl)
tab_model(model_man_cntrl, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars")

model_woman_cntrl <- lm(happiness ~ safe_walk + health + marital + children, data = ch_woman)
summary(model_woman_cntrl)
tab_model(model_woman_cntrl, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars")

#Create a table with all the regression models#
tab_model(model_1, model_man, model_woman,
          show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars",
          title = "Regression Models for Happiness by Safety and Walkability of City by Gender Before Controlling for Other Variables")

tab_model(model_2, model_3, model_man_cntrl, model_woman_cntrl,
          show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars",
          title = "Regression Models for Happiness by Safety and Walkability of City by Gender After Controlling for Possible Confounding Variables")

#Create a scatterplot with x = safe_walk and y = happiness, colored by gender#
ggplot(city_happy, aes(x = safe_walk, y = happiness, color = SQ1)) +
  geom_point(position = position_dodge(width = 0.5) , size = 1) +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("skyblue", "pink")) +
  labs(title = "Happiness by Safe Walkability of City", 
       x = "Safe Walkability of City", 
       y = "Happiness",
       color = "Gender")

#Check the assumptions of the regression model#
sum(model_man_cntrl$residuals)
sum(model_woman_cntrl$residuals)
#Check for constant variance#
ggplot(model_man_cntrl, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Residuals vs. Fitted")
ggplot(model_woman_cntrl, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Residuals vs. Fitted")
#Normality of residuals#
qqnorm(model_man_cntrl$residuals)
qqnorm(model_woman_cntrl$residuals)

# interact gender and safe walk
lm(
  happiness ~ safe_walk * gender_dummy,
  data = city_happy
) %>% 
  tab_model()


