library(dplyr)
library(ggplot2)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/data_party.rds")

# econ

data_party %>% 
    group_by(econ_alignment) %>%
    summarise(mean_distance = mean(econ_distance, na.rm = TRUE), .groups = "drop") %>% 
    ggplot(aes(x = econ_alignment, y = mean_distance)) +
    geom_bar(stat = "identity") +
    labs(x = "Party alignment", 
         y = "Mean distance", 
         title = "Mean distance between party alignment (GPT-4) and party alignment (Global Party Survey)") +
    theme_classic()

# sos

data_party %>% 
    group_by(sos_alignment) %>%
    summarise(mean_distance = mean(sos_distance, na.rm = TRUE), .groups = "drop") %>% 
    ggplot(aes(x = sos_alignment, y = mean_distance)) +
    geom_bar(stat = "identity") +
    labs(x = "Party alignment", 
         y = "Mean distance", 
         title = "Mean distance between party alignment (GPT-4) and party alignment (Global Party Survey)") +
    theme_classic()
