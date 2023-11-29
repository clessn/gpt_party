library(tidyverse)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt.rds")


# cleaning régions
data_party$Region_name <- NA 
data_party$Region_name[data_party$Region == 6] <- "asia_pacific"
data_party$Region_name[data_party$Region == 5] <- "west"
data_party$Region_name[data_party$Region == 4] <- "africa"
data_party$Region_name[data_party$Region == 3] <- "mena"
data_party$Region_name[data_party$Region == 2] <- "latam"
data_party$Region_name[data_party$Region == 1] <- "eurasia"
table(data_party$Region_name)

anglo_saxon <- c("CAN", "USA", "GBR", "AUS", "NZL", "IRL")

data_west <- data_party %>% 
  filter(Region_name == "west")

data_graph <- data_west %>%
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

m1 <- lm(Ideology_gpt ~ Ideology, data = data_graph)
summary(m1)


data_notwest <- data_party %>% 
  filter(Region_name != "west")

data_graph <- data_notwest %>%
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

m1 <- lm(Ideology_gpt ~ Ideology, data = data_graph)
summary(m1)

data_anglo <- data_party %>% 
  filter(ISO %in% anglo_saxon)

data_graph <- data_anglo %>%
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

m1 <- lm(Ideology_gpt ~ Ideology, data = data_graph)
summary(m1)
