#Getting Started#
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(sjPlot)
library(corrplot)

setwd("~/Desktop/POLI 300/Problem Set 8")

#Reading in the data#
cdc <- read.csv("UtahCDC.csv")

#Question 1####
#Change all values of -999 to NA#
cdc[cdc == -999] <- NA

#Question 2####
##Question 2a####
#How manu of each unique observation are there in variable "county"#
counties <- table(cdc$county)
counties <- as.data.frame(counties)
counties %>% 
  tab_df(
    col.header = c("Counties", "No. of Observations"),
    title = "Counties in Utah")

##Question 2b####
#Produce a histogram of the variable "disability"#
ggplot(cdc, aes(disability)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Disability in Utah",
       x = "Disability",
       y = "Frequency")

##Question 3####
#Run a regression model with "pov" as the dependent variable and "uninsured", "nohs", and "noveh" as independent variables#
reg_p3 <- lm(pov ~ uninsured + nohs + noveh, data = cdc)
summary(reg_p3)
tab_model(reg_p3, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars",
          title = "Regression Model of Poverty in Utah",
          pred.labels = c("Intercept","Uninsured", "No High School Diploma (nohs)", "No Access to a Vehicle (noveh)"),
          dv.labels = "Poverty")

##Question 4####
#Calculate predicted values of "pov" based off of the regression model reg_p3#
#4.1: Calculate the predicted value when uninsured is at the 25% quantile value and nohs and noveh are at their median values#
uninsured_q25 <- quantile(cdc$uninsured, 0.25, na.rm = TRUE)
nohs_median <- median(cdc$nohs, na.rm = TRUE)
noveh_median <- median(cdc$noveh, na.rm = TRUE)

pov_pred1 <- predict(reg_p3, 
                     newdata = data.frame(uninsured = uninsured_q25, 
                                          nohs = nohs_median, 
                                          noveh = noveh_median),
                     interval = "confidence")
pov_pred1
#4.2: Calculate the predicted value when uninsured is at the 75% quantile value and nohs and noveh are at their median values#
uninsured_q75 <- quantile(cdc$uninsured, 0.75, na.rm = TRUE)

pov_pred2 <- predict(reg_p3, 
                     newdata = data.frame(uninsured = uninsured_q75, 
                                          nohs = nohs_median, 
                                          noveh = noveh_median),
                     interval = "confidence")
pov_pred2

#Question 5####
#Create a table for reg_p3 with robust standard errors using the CR1 estimator#
tab_model(reg_p3, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars",
          digits = 3,
          vcov.fun = "CR1",
          vcov.args = list(cluster = cdc$county),
          title = "Regression Model of Poverty in Utah",
          pred.labels = c("Intercept","Uninsured", "No High School Diploma (nohs)", "No Access to a Vehicle (noveh)"),
          dv.labels = "Poverty")

#Question 7####
#Run a regression model with "pov" as the dependent variable and "uninsured", "disability" "nohs", and "noveh" as independent variables#
reg_p7 <- lm(pov ~ uninsured + disability + nohs + noveh, data = cdc)
summary(reg_p7)
tab_model(reg_p7, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars",
          title = "Regression Model of Poverty in Utah",
          pred.labels = c("Intercept","Uninsured", "Disability", "No High School Diploma (nohs)", "No Access to a Vehicle (noveh)"),
          dv.labels = "Poverty")
#Represent the results graphically using the plot_model function#
plot_model(reg_p7, vline.color = "red", 
           title = "Regression Model of Poverty in Utah",
           show.values = TRUE,
           axis.title = "Poverty",
           axis.labels = c("No Access to a Vehicle (noveh)", "No High School Diploma (nohs)", "Disability", "Uninsured"))

#Question 9####
#Run the same regression model as in Question 7 but an interaction term between "disability" and "uninsured"#
reg_p9 <- lm(pov ~ uninsured * disability + nohs + noveh, data = cdc)
summary(reg_p9)
tab_model(reg_p9, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, 
          p.style = "stars", p.threshold = c(0.1, 0.05, 0.01),
          title = "Regression Model of Poverty in Utah",
          pred.labels = c("Intercept","Uninsured", "Disability", "No High School Diploma (nohs)", "No Access to a Vehicle (noveh)", "Uninsured * Disability"),
          dv.labels = "Poverty")

#Question 11####
#Represent the results graphically using the plot_model function#
plot_model(reg_p9, type = "pred", terms = c("uninsured", "disability"),
           title = "Regression Model of Poverty in Utah",
           show.values = TRUE,
           axis.title = "Poverty",
           axis.labels = c("Uninsured", "Disability", "Uninsured * Disability", 
                           "No High School Diploma (nohs)", "No Access to a Vehicle (noveh)"))
#Have the plot contain only two lines, one for disability at its minimum and one for disability at its maximum#
#Minimum and maximum values of disability#
disability_min <- min(cdc$disability, na.rm = TRUE)
disability_max <- max(cdc$disability, na.rm = TRUE)

##Final Plot##
plot_model(reg_p9, type = "pred",
           title = "Regression Model of Poverty in Utah",
           show.values = TRUE,
           axis.title = c("Uninsured", "Poverty"),
           legend.title = "Disability",
           terms = c("uninsured", "disability [1.9, 28.7]"))

plot_model(reg_p9, type = "pred",
  title = "Regression Model of Poverty in Utah",
  show.values = TRUE,
  axis.title = "Poverty",
  axis.labels = c("Uninsured", "Disability", "Uninsured * Disability", 
                  "No High School Diploma (nohs)", "No Access to a Vehicle (noveh)"),
  terms = c("uninsured", paste0("disability [", disability_min, ", ", disability_max, "]")))

#Question 13####
#Using the same regression model as in Question 9, calculate the coefficient on uninsured for unique values of disability#
#For disability values of 1.9 and 28.7#
uninsured_q13 <- c(1.9, 28.7)
mycoefs <- coef(reg_p9)
mycoefs %>% 
  tab_df(
    col.header = c("Variable", "Coefficient"),
    title = "Coefficients of Regression Model of Poverty in Utah")

#Coefficient on uninsured for disability value of 1.9#
coef1.9 <- mycoefs["uninsured"] + mycoefs["uninsured:disability"] * 1.9

#Coefficient on uninsured for disability value of 28.7#
coef28.7 <- mycoefs["uninsured"] + mycoefs["uninsured:disability"] * 28.7
coef28.7

#Question 14####
#Create a table for reg_p9 with robust standard errors using the CR1 estimator#
tab_model(reg_p9, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, 
          p.style = "stars", p.threshold = c(0.1, 0.05, 0.01),
          digits = 3,
          vcov.fun = "CR1",
          vcov.args = list(cluster = cdc$county),
          title = "Regression Model of Poverty in Utah",
          pred.labels = c("Intercept","Uninsured", "Disability", "No High School Diploma (nohs)", "No Access to a Vehicle (noveh)", "Uninsured * Disability"),
          dv.labels = "Poverty")


           