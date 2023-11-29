library(tidyverse)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt.rds")

data_party$ideo_dist <- abs(as.numeric(data_party$Ideology_gpt) - as.numeric(data_party$Ideology))

data_graph <- data_party %>%
  # Remove strings longer than 1 character and convert to numeric
  mutate(Ideology_gpt = as.numeric(str_replace_all(Ideology_gpt, "\\D", ""))) %>%
  # Round Ideology to the nearest integer
  mutate(Ideology = round(as.numeric(Ideology))) %>%
  # Now we filter out NA and unwanted values after conversion
  filter(!is.na(Ideology_gpt), nchar(as.character(Ideology_gpt)) <= 2) %>%
  group_by(Ideology, Ideology_gpt) %>%
  summarise(n = n(), .groups = "drop") 


ggplot(data_graph, aes(x = Ideology, y = Ideology_gpt)) +
  geom_tile(aes(alpha = n)) +
  labs(x = "Party alignment (Global Party Survey)", 
       y = "Party alignment (GPT-4)", 
       title = "Party alignment (GPT-4) vs. party alignment (Global Party Survey)") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dotted") +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

cor(data_graph$Ideology, data_graph$Ideology_gpt, method = "kendall")

m1 <- lm(Ideology_gpt ~ Ideology, data = data_graph)
summary(m1)