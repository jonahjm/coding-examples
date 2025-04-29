#Getting Started#
rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyverse)
library(sjPlot)

#Loading in the data#
primaries <- readRDS("~/Desktop/POLI 300/Problem Set 9/primaries.RDS")
view(primaries)

#Question 2----------------------------------------------------------------------------------------------------------------
##Create an object named cutoff#
med <- median(primaries$ideo_dist, na.rm = TRUE)

primaries <- primaries %>%
  mutate(cutoff = ifelse(ideo_dist >= med, 1, 0))

#Question 4----------------------------------------------------------------------------------------------------------------
##Run a regression using voteshare_general as the outcome and the threshold/treatment variable as the predictor#
##Only include data that meets the cutoff#
data_cutoff <- primaries %>%
  filter(cutoff == 1)

m_1 <- lm(voteshare_general ~ exwin_prim + exwin_prim_margin, data = data_cutoff)
summary(m_1)
tab_model(m_1, show.ci = FALSE, collapse.se = TRUE, p.style = "stars", p.threshold = c(0.1, 0.05, 0.01),
          title = "Regression of General Election Vote Share on Primary Election Win and Margin of Victory",
          dv.labels = "General Election Vote Share",
          pred.labels = c("Intercept", "Primary Election Win", "Primary Election Margin of Victory"),
          digits = 3)

#Question 5----------------------------------------------------------------------------------------------------------------
##Question 5a####
d_1 <- cbind(m_1$model, m_1["fitted.values"]) %>% as_tibble()
d_1 <- d_1 %>%
  pivot_longer(cols = c("voteshare_general", "fitted.values")) %>%
  mutate(name = dplyr::recode(name, voteshare_general = "Observed",
                              fitted.values = "Model Predictions"))
view(d_1)
###Create a scatterplot of the observed and predicted values using d_1#
ggplot(d_1, aes(x = exwin_prim_margin, y = value, color = name)) +
  geom_point() +
  labs(title = "Observed and Predicted General Election Vote Share 
       by Primary Election Margin of Victory",
       x = "Primary Election Margin of Victory",
       y = "General Election Vote Share",
       color = "Data Type")

#Question 6----------------------------------------------------------------------------------------------------------------
##Run a regression using voteshare_general as the outcome and the threshold/treatment and running variables as the predictor#
##Include an interaction term between the threshold/treatment and running variables#
m_2 <- lm(voteshare_general ~ exwin_prim + exwin_prim_margin + exwin_prim*exwin_prim_margin, data = data_cutoff)
summary(m_2)
tab_model(m_2, show.ci = FALSE, collapse.se = TRUE, p.style = "stars", p.threshold = c(0.1, 0.05, 0.01),
          title = "Regression of General Election Vote Share on Primary Election Win, Margin of Victory, and Interaction Term",
          dv.labels = "General Election Vote Share",
          pred.labels = c("Intercept", "Primary Election Win", "Primary Election Margin of Victory", "Interaction Term"),
          digits = 3)

#Question 7----------------------------------------------------------------------------------------------------------------
##Question 7a####
d_2 <- cbind(m_2$model, m_2["fitted.values"]) %>% as_tibble()
d_2 <- d_2 %>%
  pivot_longer(cols = c("voteshare_general", "fitted.values")) %>%
  mutate(name = dplyr::recode(name, voteshare_general = "Observed",
                              fitted.values = "Model Predictions"))
view(d_2)

###Create a scatterplot of the observed and predicted values using d_2#
ggplot(d_2, aes(x = exwin_prim_margin, y = value, color = name)) +
  geom_point() +
  labs(title = "Observed and Predicted General Election Vote Share 
       by Primary Election Margin of Victory",
       x = "Primary Election Margin of Victory",
       y = "General Election Vote Share",
       color = "Data Type")

#Question 8----------------------------------------------------------------------------------------------------------------
##Run a regression using voteshare_general as the outcome and the threshold/treatment and running variables as the predictor#
##Keep the interaction term between the threshold/treatment and running variables#
##Have a bandwidth of 0.5#
m_3 <- lm(voteshare_general ~ exwin_prim + exwin_prim_margin + exwin_prim*exwin_prim_margin, 
          data = data_cutoff %>% filter(abs(exwin_prim_margin) <= 0.05))
summary(m_3)
tab_model(m_3, show.ci = FALSE, collapse.se = TRUE, p.style = "stars", p.threshold = c(0.1, 0.05, 0.01),
          title = "Regression of General Election Vote Share on Primary Election Win, Margin of Victory, and Interaction Term with Bandwidth of 0.5",
          dv.labels = "General Election Vote Share",
          pred.labels = c("Intercept", "Primary Election Win", "Primary Election Margin of Victory", "Interaction Term"),
          digits = 3)


#Question 9----------------------------------------------------------------------------------------------------------------
##Question 9a####
d_3 <- cbind(m_3$model, m_3["fitted.values"]) %>% as_tibble()
d_3 <- d_3 %>%
  pivot_longer(cols = c("voteshare_general", "fitted.values")) %>%
  mutate(name = dplyr::recode(name, voteshare_general = "Observed",
                              fitted.values = "Model Predictions"))
view(d_3)

###Create a scatterplot of the observed and predicted values using d_3#
ggplot(d_3, aes(x = exwin_prim_margin, y = value, color = name)) +
  geom_point() +
  labs(title = "Observed and Predicted General Election Vote Share 
       by Primary Election Margin of Victory with Bandwidth of 0.5",
       x = "Primary Election Margin of Victory",
       y = "General Election Vote Share",
       color = "Data Type")

#Question 10----------------------------------------------------------------------------------------------------------------
##Run a regression using voteshare_general as the outcome and the threshold/treatment and running variables as the predictor#
##Remove the interaction term and instead include a quadratic and cubic term for the running variable#
m_4 <- lm(voteshare_general ~ exwin_prim + exwin_prim_margin + I(exwin_prim_margin^2) + I(exwin_prim_margin^3), data = data_cutoff)
summary(m_4)
tab_model(m_4, show.ci = FALSE, collapse.se = TRUE, p.style = "stars", p.threshold = c(0.1, 0.05, 0.01),
          title = "Regression of General Election Vote Share on Primary Election Win, Margin of Victory, Quadratic, and Cubic Terms",
          dv.labels = "General Election Vote Share",
          pred.labels = c("Intercept", "Primary Election Win", "Primary Election Margin of Victory", "Quadratic Term", "Cubic Term"),
          digits = 3)

#Question 11----------------------------------------------------------------------------------------------------------------
##Question 11a####
d_4 <- cbind(m_4$model, m_4["fitted.values"]) %>% as_tibble()
d_4 <- d_4 %>%
  pivot_longer(cols = c("voteshare_general", "fitted.values")) %>%
  mutate(name = dplyr::recode(name, voteshare_general = "Observed",
                              fitted.values = "Model Predictions"))
view(d_4)

###Create a scatterplot of the observed and predicted values using d_4#
ggplot(d_4, aes(x = exwin_prim_margin, y = value, color = name)) +
  geom_point() +
  labs(title = "Observed and Predicted General Election Vote Share 
       by Primary Election Margin of Victory with 
       Quadratic and Cubic Terms",
       x = "Primary Election Margin of Victory",
       y = "General Election Vote Share",
       color = "Data Type")


