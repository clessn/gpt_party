library(dplyr)
library(ggplot2)
data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt_final.rds")
data_party$econ_distance <- abs(data_party$V4_Scale - data_party$econ_ideo_gpt_mean)
data_party$sos_distance <- abs(data_party$V6_Scale - data_party$sos_ideo_gpt_mean)
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
ggsave("_SharedFolder_article_spsa2024_gpt_party/graph/econ_party_size.png")
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
ggsave("_SharedFolder_article_spsa2024_gpt_party/graph/sos_party_size.png")