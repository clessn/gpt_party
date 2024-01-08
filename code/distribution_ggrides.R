library(dplyr)
library(tidyr)
library(ggridges)
library(ggplot2)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/data_party.rds")

long_data <- data_party %>%
  select(V4_Scale, econ_ideo_gpt_mean, V6_Scale, sos_ideo_gpt_mean) %>%
  pivot_longer(
    cols = everything(),
    names_to = "dataset",
    values_to = "value"
  ) %>%
  mutate(dataset = case_when(
    dataset == "V4_Scale"           ~ "Global Party Survey - Econ",
    dataset == "econ_ideo_gpt_mean" ~ "GPT Mean - Econ",
    dataset == "V6_Scale"           ~ "Global Party Survey - Sos",
    dataset == "sos_ideo_gpt_mean"  ~ "GPT Mean - Sos"
  ))

# Create a ggridges plot
ggplot(long_data, aes(x = value, y = dataset, fill = dataset)) +
  geom_density_ridges(alpha = 0.7) +
  labs(x = "Party alignment", 
       y = "Dataset", 
       title = "Distribution of Party Alignment (Econ and Sos)") +
  scale_fill_manual(values = c("Global Party Survey - Econ" = "grey", 
                               "GPT Mean - Econ" = "red", 
                               "Global Party Survey - Sos" = "blue", 
                               "GPT Mean - Sos" = "green")) +
  scale_x_continuous(limits = c(0, 10), breaks = 0:10) +
  theme_classic() +
  theme(legend.position = "none")
