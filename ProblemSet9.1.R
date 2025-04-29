#Getting Started#
rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyverse)
library(sjPlot)

#Loading in the data#
load("country_economic_data.RData")
view(data)

#Question 1----------------------------------------------------------------------------------------------------------------
##Run a regression#
regression <- lm(totalcoup ~ pop + rgdptt + inf + edsec + polity, data = data)
summary(regression)
tab_model(regression, show.ci = FALSE, collapse.se = TRUE, p.style = "stars",
          title = "Regression of Total Coups on Population, Real GDP per Capita, Inflation, Secondary Education, and Polity Score",
          dv.labels = "Total Coups",
          pred.labels = c("Intercept", "Population", "Real GDP per Capita", "Inflation", "Secondary Education", "Polity Score"),
          digits = 3)

#Question 2----------------------------------------------------------------------------------------------------------------
##Create three histograms#
###Population of countries#
ggplot(data, aes(x = pop)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(title = "Population of Countries",
       x = "Population",
       y = "Frequency")
###Real GDP per Capita of countries#
ggplot(data, aes(x = rgdptt)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(title = "Real GDP per Capita of Countries",
       x = "Real GDP per Capita",
       y = "Frequency")
###Inflation rate in the countries#
ggplot(data, aes(x = inf)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(title = "Inflation Rate in the Countries",
       x = "Inflation Rate",
       y = "Frequency")

#Question 3----------------------------------------------------------------------------------------------------------------
##Create three histograms of the log-transformed variables#
###Population of countries#
ggplot(data, aes(x = log(pop))) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(title = "Population of Countries (Log-Transformed)",
       x = "Population",
       y = "Frequency")
###Real GDP per Capita of countries#
ggplot(data, aes(x = log(rgdptt)))+
  geom_histogram(fill = "skyblue", color = "black") +
  labs(title = "Real GDP per Capita of Countries (Log-Transformed)",
       x = "Real GDP per Capita",
       y = "Frequency")
###Inflation rate in the countries#
ggplot(data, aes(x = log(inf))) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(title = "Inflation Rate in the Countries (Log-Transformed)",
       x = "Inflation Rate",
       y = "Frequency")

#Question 5----------------------------------------------------------------------------------------------------------------
##Run a regression with log-transformed variables#
regression_log <- lm(totalcoup ~ log(pop) + log(rgdptt) + log(inf) + 
                       edsec + polity, data = data)
summary(regression_log)
tab_model(regression_log, show.ci = FALSE, collapse.se = TRUE, p.style = "stars",
          title = "Regression of Total Coups on Log-Transformed Population, Real GDP per Capita, Inflation, Secondary Education, and Polity Score",
          dv.labels = "Total Coups",
          pred.labels = c("Intercept", "Log Population", "Log Real GDP per Capita", "Log Inflation", "Secondary Education", "Polity Score"),
          digits = 3)

#Question 6----------------------------------------------------------------------------------------------------------------
##Run a regression with log-transformed variables and a dummy variable for region#
data <- data %>%
  mutate(region = as.factor(region))
reg_log_region <- lm(totalcoup ~ log(pop) + log(rgdptt) + log(inf) + 
                              edsec + polity + region, data = data)
summary(reg_log_region)
tab_model(reg_log_region, show.ci = FALSE, collapse.se = TRUE, p.style = "stars",
          title = "Regression of Total Coups on Log-Transformed Population, Real GDP per Capita, Inflation, Secondary Education, Polity Score, and Region",
          dv.labels = "Total Coups",
          pred.labels = c("Intercept", "Log Population", "Log Real GDP per Capita", "Log Inflation", "Secondary Education", "Polity Score", "Asia", "Central America", "Europe", "North America", "Oceania", "South America"),
          digits = 3)

#Question 7----------------------------------------------------------------------------------------------------------------
##Generate a plot of the residuals vs. the fitted values#
resid_data <- data.frame(
  fitted = fitted(reg_log_region), 
  residuals = residuals(reg_log_region))

ggplot(resid_data, aes(x = fitted(reg_log_region), y = residuals(reg_log_region))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

#Question 9----------------------------------------------------------------------------------------------------------------
##Run a regression with log-transformed variables and a dummy variable for region with robust standard errors#
reg_9 <- lm(totalcoup ~ log(pop) + log(rgdptt) + log(inf) + 
                              edsec + polity + region, data = data)
summary(reg_9)
tab_model(reg_9, show.ci = FALSE, collapse.se = TRUE, p.style = "stars",
          vcov.fun = "HC3",
          title = "Regression of Total Coups on Log-Transformed Population, Real GDP per Capita, Inflation, Secondary Education, Polity Score, and Region with Robust Standard Errors",
          dv.labels = "Total Coups",
          pred.labels = c("Intercept", "Log Population", "Log Real GDP per Capita", "Log Inflation", "Secondary Education", "Polity Score", "Asia", "Central America", "Europe", "North America", "Oceania", "South America"),
          digits = 3)



