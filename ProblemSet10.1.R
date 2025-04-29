#Getting Started#
rm(list=ls())
library(ggplot2)
library(tidyverse)
library(sjPlot)
library(lfe)

load("~/Desktop/POLI 300/Problem Set 10/gd.RData")
gd <- as.data.frame(gd)

#Question 1####
view(gd)

##Question 1a####
#How many municipalities are in the dataset?#
gd %>%
  count(muni)

##Question 1b####
#Mutate the dataset to only show municipalities with a non-zero value in refugee arrivals when election equals "2015-09"#
gd_treatment <- gd %>% 
  filter(election == "2015-09" & refugee_arrivals != 0)
#Add back municipality 47 to this dataset#
gd_treatment <- rbind(gd_treatment, gd[gd$muni == 47 & gd$election == "2015-09", ])
#How many municipalities are in this dataset?#
gd_treatment %>%
  count(muni)

##Question 1c####
#What is the range of refugee arrivals in the dataset gd_treatment?#
range(gd_treatment$refugee_arrivals, na.rm = TRUE)

#Question 2####
#Create a histogram of gdvote#
ggplot(gd, aes(x = gdvote)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Golden Dawn Vote", x = "gdvote", y = "Frequency")
#Create a histogram of logdist#
ggplot(gd, aes(x = logdist)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Logged Distance from 
       the Turkish Coast", x = "logdist", y = "Frequency")

#Question 3####
#Create a new variable "treat" that equals 1 if the municipality is in gd_treatment and 0 otherwise#
gd <- gd %>%
  mutate(treat = ifelse(muni %in% gd_treatment$muni, 1, 0))
#Create a new variable "post" that equals 1 if the election is after September 2015 and 0 otherwise#
gd <- gd %>%
  mutate(post = ifelse(election >= "2015-09", 1, 0))

#Question 4####
##Question 4a####
#Estimate a regression model when post == 1 and treat is the predictor and gdvote is the outcome#
gd_post <- gd %>%
  filter(post == 1)
m_1 <- lm(gdvote ~ treat, data = gd_post)
summary(m_1)
tab_model(m_1, show.ci = FALSE, collapse.se = TRUE, p.style = "stars",
          title = "Regression Model of Golden Dawn Vote Post-Migrant Crisis",
          dv.labels = "Golden Dawn Vote",
          pred.labels = c("Intercept", "Treatment"))

#Question 5####
##Question 5a####
#What is the difference in the vote share of Golden Dawn in treated municipalities before and after the migrant crisis?#
gd %>% 
  filter(post == 1) %>%
  group_by(treat) %>%
  summarise(mean_gdvote = mean(gdvote)) %>%
  mutate(diff = diff(mean_gdvote))

gd %>%
  filter(post == 0) %>%
  group_by(treat) %>%
  summarise(mean_gdvote = mean(gdvote)) %>%
  mutate(diff = diff(mean_gdvote))

##Question 5b####
#Estimate a regression model with an interaction term between post and treat#
m_2 <- lm(gdvote ~ treat * post, data = gd)
summary(m_2)
tab_model(m_2, show.ci = FALSE, collapse.se = TRUE, p.style = "stars",
          title = "Regression Model of Golden Dawn Vote with Interaction Term",
          dv.labels = "Golden Dawn Vote",
          pred.labels = c("Intercept", "Treatment", "Post", "Treatment x Post"))

#Question 6####
##Question 6a####
m_3 <- felm(gdvote ~ treat:post | muni + election | 0 | muni, data = gd)
summary(m_3)
tab_model(m_3, show.ci = FALSE, collapse.se = TRUE, p.style = "stars",
          title = "Fixed Effects Model of Golden Dawn Vote with Interaction Term",
          dv.labels = "Golden Dawn Vote",
          pred.labels = c("Treatment x Post"))

#Question 7####
##Question 7a####
#Produce a plot of parallel trends#
p_dat <- gd %>%
  group_by(treat, election) %>% 
  summarize(mean = mean(gdvote)) %>%
  mutate(election = lubridate::ym(election),
         treat = recode(treat, "0" = "Control", "1" = "Treatment"))

ggplot(p_dat, aes(x = election, y = mean, color = treat, group = treat)) +
  geom_point() +
  geom_line() +
  labs(title = "Parallel Trends Plot of Golden Dawn Vote Share",
       x = "Election", y = "Mean of Golden Dawn Vote Share",
       color = "Muncipality")
