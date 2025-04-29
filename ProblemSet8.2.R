#Getting Started#
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(sjPlot)
library(corrplot)

setwd("~/Desktop/POLI 300/Problem Set 8")

#Loading the data#
load("Names.RData")
view(names)
attributes(names)

#Run a regression with "call_back" as the outcome and "black" as the predictor#
reg_1 <- lm(call_back ~ black, data = names)
summary(reg_1)
tab_model(reg_1, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars", 
          title = "Regression of Call Backs on Black",
          pred.labels = c("Intercept", "Black"),
          dv.labels = "Call Backs")

#Run a regression with "call_back" as the outcome and "black" as the predictor, controlling for "yearsexp", "high", "college", "honors")
reg_2 <- lm(call_back ~ black + yearsexp + high + college + honors, data = names)
summary(reg_2)
tab_model(reg_2, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars", 
          title = "Regression of Call Backs on Black, Years of Experience, High School, College, and Honors",
          pred.labels = c("Intercept", "Black", "Years of Experience", "High School", "College", "Honors"),
          dv.labels = "Call Backs")

#Check the correlations of the other variables against "call_back" and "black"#
#Use a for loop to check the correlations of the other variables against "call_back" and "black"#
correlations <- c("ofjobs", "yearsexp", "honors", "volunteer", "military", "empholes", "workinschool", "email", "computerskills", "specialskills", 
                  "eoe", "manager", "supervisor", "secretary", "offsupport", "salesrep", "retailsales", 
                  "req", "expreq", "comreq", "educreq", "compreq", "orgreq", "manuf", "transcom", "bankreal", "trade", "busservice", "othservice", 
                  "missind", "chicago", "high", "female", "college", "rand")

#Create data frame to store the correlations#
correlations_df <- data.frame()

#Use a for loop to check the correlations of the other variables against "call_back" and "black"#
for (i in 1:length(correlations)) {
  correlations_df <- rbind(correlations_df, data.frame(cor_call_back = cor(names$call_back, names[[correlations[i]]]), 
                          cor_black = cor(names$black, names[[correlations[i]]])))
}
view(correlations_df)

#Run a regression with "call_back" as the outcome and "black" as the predictor, controlling for variables listed in correlations#
reg_3 <- lm(call_back ~ black + ofjobs + yearsexp + honors + volunteer + military + empholes + workinschool + email + computerskills + specialskills + 
              eoe + manager + supervisor + secretary + offsupport + salesrep + retailsales + 
              req + expreq + comreq + educreq + compreq + orgreq + manuf + transcom + bankreal + trade + 
              busservice + othservice + missind + chicago + high + female + college + rand, data = names)
summary(reg_3)
tab_model(reg_3, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars", 
          title = "Regression of Call Backs on Black, Years of Experience, High School, College, and Honors",
          dv.labels = "Call Backs")

#Only keep the variables that are significant at the 0.05 level#
reg_4 <- lm(call_back ~ black + honors + empholes + specialskills + 
              req + chicago, data = names)
summary(reg_4)
tab_model(reg_4, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars", 
          title = "Regression of Call Backs on Black",
          dv.labels = "Call Backs")

#Run a regression with "call_back" as the outcome and "black" as the predictor with an interaction term between "black" and "eoe"#
reg_5 <- lm(call_back ~ black * eoe, data = names)
summary(reg_5)
tab_model(reg_5, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars", 
          title = "Regression of Call Backs on Black and EOE",
          dv.labels = "Call Backs")
#Run the same regression but with the control variables from reg_4#
reg_6 <- lm(call_back ~ black * eoe + honors + empholes + specialskills + 
              req + chicago, data = names)
summary(reg_6)
tab_model(reg_6, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars", 
          title = "Regression of Call Backs on Black and EOE",
          dv.labels = "Call Backs")
plot_model(reg_6, type = "pred", 
           terms = c("black", "eoe"), 
           title = "Regression of Call Backs 
           with an Interaction of Black and EOE",
           show.values = TRUE,
           axis.title = c("Black", "Call Back"),
           legend.title = "Equal Opportunity 
           Employer (EOE)")

#Produce a table with the results of the regression models reg_4 and reg_6#
tab_model(reg_4, reg_6, 
          show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars", 
          title = "Regression Model of Call Backs",
          dv.labels = "Call Backs",
          pred.labels = c("Intercept", "Black", "Honors Mentioned on Resume", "Employment Holes", "Special Skills Mentioned", 
                          "Ad Mentions Job Requirement", "Chicago", "Equal Opportunity Employer (EOE)", "Black * EOE"))

#FEMALES####
#Run a regression with "call_back" as the outcome and "female" as the predictor#
reg_7 <- lm(call_back ~ female, data = names)
summary(reg_7)
tab_model(reg_7, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars", 
          title = "Regression of Call Back and Female")

#Run a regression with "call_back" as the outcome and "female" as the predictor, controlling for variables listed in correlations#
reg_8 <- lm(call_back ~ female + ofjobs + yearsexp + honors + volunteer + military + empholes + workinschool + email + computerskills + specialskills + 
              eoe + manager + supervisor + secretary + offsupport + salesrep + retailsales + 
              req + expreq + comreq + educreq + compreq + orgreq + manuf + transcom + bankreal + trade + 
              busservice + othservice + missind + black + chicago + high + college + rand, data = names)
summary(reg_8)
tab_model(reg_8, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars", 
          title = "Regression of Call Back and Female with Control Variables")

#Only keep the variables that are significant at the 0.05 level#
reg_9 <- lm(call_back ~ female + ofjobs + yearsexp + honors + empholes + specialskills + retailsales + 
              req + black + chicago, data = names)
summary(reg_9)
tab_model(reg_9, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars", 
          title = "Regression Call Back and Female with Control Variables")

#Run a regression with "call_back" as the outcome and "female" as the predictor with an interaction term between "female" and "eoe"#
reg_10 <- lm(call_back ~ female*eoe, data = names)
summary(reg_10)
tab_model(reg_10, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars", 
          title = "Regression of Call Back and Female with EOE")

#Now include the control variables from reg_9#
reg_11 <- lm(call_back ~ female*eoe + ofjobs + yearsexp + honors + empholes + specialskills + retailsales + 
              req + black + chicago, data = names)
summary(reg_11)
tab_model(reg_11, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars", 
          title = "Regression of Call Back and Female with EOE and Control Variables")
plot_model(reg_11, type = "pred",
           terms = c("female", "eoe"), 
           title = "Regression of Call Backs 
           with an Interaction of Female and EOE",
           show.values = TRUE,
           axis.title = c("Female", "Call Back"),
           legend.title = "Equal Opportunity 
           Employer (EOE)")
           

#Plot the results of the regression model reg_9 and reg_11#
tab_model(reg_9, reg_11, 
          show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars", 
          title = "Regression Model of Call Backs",
          dv.labels = "Call Backs",
          pred.labels = c("Intercept", "Female", "No. of Jobs on Resume", "Years of Work Experience", "Honors Mentioned on Resume", "Employment Holes", "Special Skills Mentioned", "Retail Sales", 
                          "Ad Mentions Job Requirement", "Black", "Chicago", "Equal Opportunity Employer (EOE)", "Female * EOE"))





#Run a regression with "call_back" and an interaction term with "black" and "female"#
reg_12 <- lm(call_back ~ black*female, data = names)
summary(reg_12)
tab_model(reg_12, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars", 
          title = "Regression of Call Back and Black and Female")
#Now include the control variables from reg_4 and reg_9#
reg_13 <- lm(call_back ~ black*female + honors + empholes + specialskills + 
               req + chicago + ofjobs + yearsexp + 
               retailsales + chicago, data = names)
summary(reg_13)
tab_model(reg_13, show.ci = FALSE, show.se = TRUE, collapse.se = TRUE, p.style = "stars", 
          title = "Regression Model of Call Backs with an Interaction between Black and Female with Control Variables",
          dv.labels = "Call Backs",
          pred.labels = c("Intercept", "Black", "Female", "Honors Mentioned on Resume", "Employment Holes", "Special Skills Mentioned", 
                          "Ad Mentions Job Requirement", "Chicago", "No. of Jobs on Resume", "Years of Work Experience", "Retail Sales", "Black * Female"))
plot_model(reg_13, type = "pred", 
           terms = c("black", "female"), 
           title = "Regression of Call Back on Black and Female",
           show.values = TRUE)






