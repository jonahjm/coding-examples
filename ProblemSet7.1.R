#Getting Started#
rm(list=ls())
library(tidyverse)
library(readr)
library(knitr)
library(dplyr)
library(sjPlot)
setwd("~/Desktop/POLI 300/Problem Set 7")

#Question 1####
#Read in the data#
cdc <- read_csv("UtahCDC.csv")

##Question 1a####
#Convert all values of -999 to NA#
cdc <- cdc %>%
  mutate_all(~ ifelse(. == -999, NA, .))

##Question 1b####
#Number of NA values in "pov" variable#
cdc %>%
  count(is.na(pov))
#Number of NA values in "uninsured" variable#
cdc %>%
  count(is.na(uninsured))

#Question 2####
#Mean and standard deviation of the percent living below the poverty rate#
stat_pov <- cdc %>%
  summarise(mean_pov = mean(pov, na.rm = TRUE),
            sd_pov = sd(pov, na.rm = TRUE))
#Mean and standard deviation of the percent uninsured#
stat_uninsured <- cdc %>%
  summarise(mean_uninsured = mean(uninsured, na.rm = TRUE),
            sd_uninsured = sd(uninsured, na.rm = TRUE))

#Mean and standard deviation of the percent living below the poverty rate and percent uninsured#
cdc %>%
  summarise(mean_pov = mean(pov, na.rm = TRUE),
            sd_pov = sd(pov, na.rm = TRUE),
            mean_uninsured = mean(uninsured, na.rm = TRUE),
            sd_uninsured = sd(uninsured, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(),
               names_to = c("stat", "variable"),
               names_sep = "_") %>%
  pivot_wider(names_from = "stat", values_from = "value") %>%
  tab_df()

#Question 3####
#Create a histogram of the percent living below the poverty rate#
ggplot(cdc, aes(x = pov)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(title = "Histogram of the Percent Living Below the Poverty Rate",
       x = "Percent Living Below the Poverty Rate (pov)",
       y = "Frequency")
#Create a histogram of the percent uninsured#
ggplot(cdc, aes(x = uninsured)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(title = "Histogram of the Percent of People without Insurance",
       x = "Percent without Insurance (uninsured)",
       y = "Frequency")

#Question 4####
#Create a scatterplot of the percent living below the poverty rate and percent uninsured#
#Add a line of best fit color red#
ggplot(cdc, aes(x = pov, y = uninsured)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatterplot of the Percent Living Below the Poverty Rate and Percent Uninsured",
       x = "Percent Living Below the Poverty Rate (pov)",
       y = "Percent without Insurance (uninsured)")

#Question 5####
##Question 5b####
#Run a linear regression model with county as the predictor and pov as the outcome#
regression <- lm(pov ~ county, data = cdc)
summary(regression)
tab_model(regression, show.ci = FALSE, show.se = TRUE, p.style = "stars")

##Question 5c####
#Create a new variable "county_utahref" to be the reference level in a linear regression model#
cdc <- cdc %>%
  mutate(county_utahref = fct_relevel(county, "Utah"))
#Run a linear regression model with county as the predictor and pov as the outcome#
regression_U <- lm(pov ~ county_utahref, data = cdc)
summary(regression_U)

#Question 6####
#Run a linear regression model with pov as the outcome and uninsured as the predictor#
reg_m1 <- lm(pov ~ uninsured, data = cdc)
summary(reg_m1)

#Question 7####
tab_model(reg_m1, show.ci = FALSE, show.se = TRUE, p.style = "stars")

#Question 8####
#Run a linear regression model with pov as the outcome and nohs as the predictor#
reg_m2 <- lm(pov ~ nohs, data = cdc)
summary(reg_m2)
tab_model(reg_m2, show.ci = FALSE, show.se = TRUE, p.style = "stars")

#Produce a table with both regression models#
tab_model(reg_m1, reg_m2, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars")

#Question 9####
#Run a linear regression model with pov as the outcome and disability as the predictor#
reg_m3 <- lm(pov ~ disability, data = cdc)
summary(reg_m3)

#Produce a table with all three regression models#
tab_model(reg_m1, reg_m2, reg_m3, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars")

#Question 10####
cor(cdc$nohs, cdc$pov, use = "complete.obs")
cor(cdc$nohs, cdc$uninsured, use = "complete.obs")
cor(cdc$pov, cdc$uninsured, use = "complete.obs")

#Question 11####
#Run a regression model with pov as the outcome and uninsured as the predictor, using noveh and nohs as control variables#
reg_wcontrols <- lm(pov ~ uninsured + noveh + nohs, data = cdc)

#Add this regression model to the table with the other regression models#
tab_model(reg_m1, reg_m2, reg_wcontrols, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars")

#Question 12####
#Check the five regression assumptions for reg_wcontrols#
##Question 12a: Zero Conditional Mean####
sum(reg_wcontrols$residuals)
##Question 12b: Constant Variance####
ggplot(reg_wcontrols, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Residuals vs. Fitted Values",
       subtitle = "pov ~ uninsured + noveh + nohs",
       x = "Fitted Values",
       y = "Residuals")
##Question 12e: Normality of Residuals####
#quantile-quantile plot#
qqnorm(reg_wcontrols$residuals)

