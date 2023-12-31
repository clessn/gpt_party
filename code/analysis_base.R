library(tidyverse)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt.rds")

# Graphique nuage de points -------------------------------------- #

data_graph <- data_party %>%
  group_by(Ideology, Ideology_gpt_mean) %>%
  summarise(n = n(), .groups = "drop") 

ggplot(data_graph, aes(x = Ideology, y = Ideology_gpt_mean)) +
  geom_point(aes(size = n, alpha = n)) +
  labs(x = "Party alignment (Global Party Survey)", 
       y = "Party alignment (GPT-4)", 
       title = "Party alignment (GPT-4) vs. party alignment (Global Party Survey)") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dotted") +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

# Graphique ggtiles rounded -------------------------------------- #

data_graph_rounded <- data_party %>%
  mutate(Ideology = round(as.numeric(Ideology))) %>%
  mutate(Ideology_gpt_mean = round(as.numeric(Ideology_gpt_mean))) %>%
  group_by(Ideology, Ideology_gpt_mean) %>%
  summarise(n = n(), .groups = "drop") 

ggplot(data_graph_rounded, aes(x = Ideology, y = Ideology_gpt_mean)) +
  geom_tile(aes(alpha = n)) +
  labs(x = "Party alignment (Global Party Survey)", 
       y = "Party alignment (GPT-4)", 
       title = "Party alignment (GPT-4) vs. party alignment (Global Party Survey)") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dotted") +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

m1 <- lm(Ideology_gpt_mean ~ Ideology, data = data_party)
summary(m1)

# ----------------- Comparaison des distributions ----------------- #

ggplot(data_party, aes(x = Ideology)) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  labs(x = "Party alignment (Global Party Survey)", 
       y = "Count", 
       title = "Distribution of party alignment (Global Party Survey)") +
  geom_histogram(aes(x = Ideology_gpt_mean), binwidth = 1, fill = "red", color = "black", alpha = 0.5) +
  theme_classic()

