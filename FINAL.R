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
library(scales)

nba <- read_csv("nba.csv")

#Make observations grouped by player name
nba <- nba %>%
  group_by(name)

str(nba)

#Make PTS a numeric variable
nba <- nba %>%
  mutate(PTS = as.numeric(PTS))


#Make a cumulative sum of points variable
nba <- nba %>%
  arrange(name, year) %>%
  group_by(name) %>%
  mutate(cumulative_points = cumsum(PTS))

#Make a binary variable to represent current players
#If a player has a year observation equal to 2022, set all observations of that name to 1
nba <- nba %>%
  group_by(name) %>%
  mutate(current = if_else(any(year == 2022), 1, 0)) %>%
  ungroup() %>% 
  mutate(current = as.factor(current))

#Make a binary variable to be used for the alpha layer and labels
nba <- nba %>%
  mutate(alf_lab = ifelse(name == "Wilt Chamberlain" | 
                           name == "Michael Jordan" | 
                           name == "Kareem Abdul-Jabbar" | 
                           name == "Karl Malone" |
                            name == "Kobe Bryant" |
                           name == "LeBron James" |
                           name == "James Harden" |
                           name == "Kevin Durant" |
                           name == "Stephen Curry", 1, 0)) %>% 
  mutate(alf_lab = as.factor(alf_lab))

nba <- nba %>% 
  mutate(lebron = ifelse(name == "LeBron James", 1, 0)) %>% 
  mutate(lebron = as.factor(lebron))

end_career <- nba %>%
  filter(alf_lab == 1) %>%
  group_by(name) %>%
  filter(year == max(year)) %>%
  ungroup()

ggplot(data = nba) +
  geom_hline(aes(yintercept = 38390), color = "black", size = 0.75, linetype = "dashed") +
  geom_step(aes(x = year, y = cumulative_points, group = name, 
                color = current, alpha = alf_lab, linewidth = lebron)) +
  geom_segment(aes(x = 1946, xend = 2031, y = 0, yend = 0), 
               color = "grey50", size = 2.25) +
  geom_point(data = end_career, aes(x = year, y = cumulative_points, color = current, 
                                     alpha = alf_lab, size = lebron)) +
  geom_label(data = end_career %>% filter(!name %in% c("LeBron James", "Kareem Abdul-Jabbar", "James Harden", "Kevin Durant")),
            aes(x = year, y = cumulative_points, label = name),
            hjust = - 0.025, vjust = -0.05, size = 5.5, 
            color = "black", fill = "white", label.size = 0) +
  geom_label(data = end_career %>% filter(name %in% c("LeBron James", "Kareem Abdul-Jabbar")),
            aes(x = year, y = cumulative_points, label = name),
            hjust = - 0.025, vjust = -0.8, size = 7, fontface = "bold",
            color = "black", fill = "white", label.size = 0) +
  geom_segment(x = 2022.75, xend = 2024, y = 2500, yend = 2500, 
               color = "blue", size = 3) +
  scale_x_continuous(breaks = seq(1950, 2020, 10), expand = c(0, 0), limits = c(1945, 2031)) +
  scale_y_continuous(breaks = seq(5000, 40000, 5000), expand = c(0, 0), limits = c(0, 44000), 
                     labels = function(x) ifelse(x == 40000, 
                                                 paste0(label_comma()(x), " career points"), 
                                                 label_comma()(x))) +
  scale_color_manual(values = c("0" = "grey50", "1" = "blue")) +
  scale_alpha_manual(values = c("0" = 0.3, "1" = 1)) +
  scale_linewidth_manual(values = c("0" = 0.5, "1" = 2)) +
  scale_size_manual(values = c("0" = 2, "1" = 4)) +
  annotate("text", x = 2022.5, y = 27850, label = "James Harden", size = 5.5, color = "black", hjust = 0) +
  annotate("text", x = 2022.5, y = 26923, label = "Kevin Durant", size = 5.5, color = "black", hjust = 0) +
  annotate("text", x = 2024.5, y = 2500, label = "Current players", size = 5.5, color = "black", hjust = 0) +
  annotate("text", x = 1988.5, y = 39150, label = "38,387 (1989)", size = 5.5, color = "black", hjust = 0) +
  annotate("text", x = 2022.5, y = 39150, label = "38,390 (2023)", size = 5.5, color = "black", hjust = 0) +
  labs(title = "How Lebron James Outscored Kareem Abdul-Jabbar and All the N.B.A Greats",
       caption = "Source: Basketball-reference.com • Note: Data is through Feb. 7 • By The New York Times") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 5), size = 15),
        axis.ticks.x = element_line(size = 1, color = "grey50"),
        axis.ticks.length.x = unit(12, "pt"),
        axis.text.y = element_text(vjust = -0.7, hjust = 0, margin = margin(r = -160), size = 15),
        panel.grid.major = element_line(linetype = "dotted", color = "grey50", linewidth = 0.75),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, size = 15, color = "grey50", margin = margin(t = 15)),
        plot.title = element_text(hjust = 0.0325, size = 25))




  
  
  
  
