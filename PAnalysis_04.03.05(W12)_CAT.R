#Getting Started#
rm(list = ls())
library(tidyverse)
library(ggplot2)
library(dplyr)
library(sjPlot)
library(psych)
library(nnet)
library(stargazer)
library(texreg)
library(interactions)


setwd("~/Desktop/Hawkins-RA/Environment and Populism Project")

#Importing Data#
polat <- read.csv("POLAT_clean.csv")
polat <- as.data.frame(polat)

#Subset to wave 12#
polat12 <- polat %>%
  filter(wave == 12)

#Create new binary variables to represent voting for populist parties#
polat12 <- polat12 %>%
  mutate(vox_vote = ifelse(vote2019nov == "VOX", 1, 0)) %>%
  mutate(podemos_vote = ifelse(vote2019nov == "Unidas Podemos", 1, 0))

#Create a new binary variable to represent those who identify with either party#
polat12 <- polat12 %>%
  mutate(populist_vote = ifelse(vote2019nov == "VOX" | vote2019nov == "Unidas Podemos", 1, 0))


#Create a new binary variable to represent those who identify with Vox#
polat12 <- polat12 %>%
  mutate(vox_id = ifelse(partyid_str == "Vox" | partyidrepesca_str == "Vox", 1, 0))

#Create a new binary variable to represent those who identify with Podemos#
polat12 <- polat12 %>%
  mutate(podemos_id = ifelse(partyid_str == "Podemos" | partyidrepesca_str == "Podemos", 1, 0))

#Create a new binary variable to represent those who identify with either party
polat12 <- polat12 %>%
  mutate(populist_id = ifelse(partyid_str == "Podemos" | partyidrepesca_str == "Podemos" | partyid_str == "Vox" | partyidrepesca_str == "Vox", 1, 0))

#Recoding variables#
polat12 <- polat12 %>%
  mutate(immigeco_rev = 10 - immigeco) %>%
  mutate(climatechange_rev = 10 - climatechange)

#Make ideology a categorical variable#
polat12$ideo_cat <- cut(polat12$lrself, 
                        breaks = c(-Inf, 4.5, 5.5, Inf), 
                        labels = c("Left-Leaning", "Centrist", "Right-Leaning"))

# CLIMATE CHANGE
model_1 <- lm(climatechange_rev ~ popindex * ideo_cat + populist_vote + 
                  populist_id + lrself +
                  age + sex + edu_high + 
                  riskaversion2 + efintindex, data = polat12)

interact_plot(model_1,
              pred = popindex,
              modx = ideo_cat,
              interval = TRUE,
              x.label = "Populist Attitudes (Index)",
              y.label = "Predicted Probability of Anti-Climate Sentiments 
              (Prioritize economic growth over climate change)",
              main.title = "Marginal Effect of Populist Attitudes 
              Across Ideology (W12 - 2020)",
              modx.label = c("Left-Leaning", "Centrist", "Right-Leaning"),
              legend.main = "Left-Right Ideology")

#ggsave(filename = "climatechange_CAT.png", path = "Interaction Models/")

# CLIMATE THREAT VARIABLES
model_2 <- lm(threatclimhealth ~ popindex * ideo_cat + populist_vote + 
                populist_id + lrself +
                age + sex + edu_high + 
                riskaversion2 + efintindex, data = polat12)

interact_plot(model_2,
              pred = popindex,
              modx = ideo_cat,
              interval = TRUE,
              x.label = "Populist Attitudes (Index)",
              y.label = "Predicted Probability of Pro-Climate Sentiments 
              (May harm my health or the health of someone close to me)",
              main.title = "Marginal Effect of Populist Attitudes 
              Across Ideology (W12 - 2020)",
              modx.label = c("Left-Leaning", "Centrist", "Right-Leaning"),
              legend.main = "Left-Right Ideology")

#ggsave(filename = "threatclimhealth_CAT.png", path = "Interaction Models/")



model_3 <- lm(threatclimecpers ~ popindex * ideo_cat + populist_vote + 
                populist_id + lrself +
                age + sex + edu_high + 
                riskaversion2 + efintindex, data = polat12)

interact_plot(model_3,
              pred = popindex,
              modx = ideo_cat,
              interval = TRUE,
              x.label = "Populist Attitudes (Index)",
              y.label = "Predicted Probability of Pro-Climate Sentiments 
              (May harm my job or economic prospects)",
              main.title = "Marginal Effect of Populist Attitudes 
              Across Ideology (W12 - 2020)",
              modx.label = c("Left-Leaning", "Centrist", "Right-Leaning"),
              legend.main = "Left-Right Ideology")

#ggsave(filename = "threatclimecpers_CAT.png", path = "Interaction Models/")



model_4 <- lm(threatclimeccountry ~ popindex * ideo_cat + populist_vote + 
                populist_id + lrself +
                age + sex + edu_high + 
                riskaversion2 + efintindex, data = polat12)

interact_plot(model_4,
              pred = popindex,
              modx = ideo_cat,
              interval = TRUE,
              x.label = "Populist Attitudes (Index)",
              y.label = "Predicted Probability of Pro-Climate Sentiments 
              (May harm the country's job or economic outlook)",
              main.title = "Marginal Effect of Populist Attitudes 
              Across Ideology (W12 - 2020)",
              modx.label = c("Left-Leaning", "Centrist", "Right-Leaning"),
              legend.main = "Left-Right Ideology")

#ggsave(filename = "threatclimeccountry_CAT.png", path = "Interaction Models/")



model_5 <- lm(threatclimvalpers ~ popindex * ideo_cat + populist_vote + 
                populist_id + lrself +
                age + sex + edu_high + 
                riskaversion2 + efintindex, data = polat12)

interact_plot(model_5,
              pred = popindex,
              modx = ideo_cat,
              interval = TRUE,
              x.label = "Populist Attitudes (Index)",
              y.label = "Predicted Probability of Pro-Climate Sentiments 
              (My way of being and my values)",
              main.title = "Marginal Effect of Populist Attitudes 
              Across Ideology (W12 - 2020)",
              modx.label = c("Left-Leaning", "Centrist", "Right-Leaning"),
              legend.main = "Left-Right Ideology")

#ggsave(filename = "threatclimvalpers_CAT.png", path = "Interaction Models/")



model_6 <- lm(threatclimvalcountry ~ popindex * ideo_cat + populist_vote + 
                populist_id + lrself +
                age + sex + edu_high + 
                riskaversion2 + efintindex, data = polat12)

interact_plot(model_6,
              pred = popindex,
              modx = ideo_cat,
              interval = TRUE,
              x.label = "Populist Attitudes (Index)",
              y.label = "Predicted Probability of Pro-Climate Sentiments 
              (The country's way of being and values)",
              main.title = "Marginal Effect of Populist Attitudes 
              Across Ideology (W12 - 2020)",
              modx.label = c("Left-Leaning", "Centrist", "Right-Leaning"),
              legend.main = "Left-Right Ideology")

#ggsave(filename = "threatclimvalcountry_CAT.png", path = "Interaction Models/")

# IMMIGRATION VARIABLES
model_7 <- lm(immigeco_rev ~ popindex * ideo_cat + populist_vote + 
                populist_id + lrself +
                age + sex + edu_high + 
                riskaversion2 + efintindex, data = polat12)

interact_plot(model_7,
              pred = popindex,
              modx = ideo_cat,
              interval = TRUE,
              x.label = "Populist Attitudes (Index)",
              y.label = "Predicted Probability of Anti-Immigrant Sentiments
              (Immigrants negatively impact the economy)",
              main.title = "Marginal Effect of Populist Attitudes
              Across Ideology (W12 - 2020)",
              modx.label = c("Left-Leaning", "Centrist", "Right-Leaning"),
              legend.main = "Left-Right Ideology")

#ggsave(filename = "IE_PopIndex_CAT.png", path = "Interaction Models/")

model_8 <- lm(immicult ~ popindex * ideo_cat + populist_vote + 
                populist_id + lrself +
                age + sex + edu_high + 
                riskaversion2 + efintindex, data = polat12)

interact_plot(model_8,
              pred = popindex,
              modx = ideo_cat,
              interval = TRUE,
              x.label = "Populist Attitudes (Index)",
              y.label = "Predicted Probability of Anti-Immigrant Sentiments
              (Immigrants negatively impact culture)",
              main.title = "Marginal Effect of Populist Attitudes
              Across Ideology (W12 - 2020)",
              modx.label = c("Left-Leaning", "Centrist", "Right-Leaning"),
              legend.main = "Left-Right Ideology")

#ggsave(filename = "IC_PopIndex_CAT.png", path = "Interaction Models/")



