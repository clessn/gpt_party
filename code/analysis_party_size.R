library(dplyr)
library(ggplot2)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/data_party.rds")

# econ

data_party %>%
  group_by(Type_Partysize_seat) %>%
  summarise(mean_distance = mean(econ_distance, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Type_Partysize_seat, y = mean_distance)) +
  geom_bar(stat = "identity") +
  labs(x = "Party Size",
       y = "Mean distance",
       title = "Mean distance between GPT-4 and Global Party Survey by party size") +
  theme_classic()

# sos

data_party %>%
  group_by(Type_Partysize_seat) %>%
  summarise(mean_distance = mean(sos_distance, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Type_Partysize_seat, y = mean_distance)) +
  geom_bar(stat = "identity") +
  labs(x = "Party Size",
       y = "Mean distance",
       title = "Mean distance between GPT-4 and Global Party Survey by party size") +
  theme_classic()

