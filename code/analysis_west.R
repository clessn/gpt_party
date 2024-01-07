library(tidyverse)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/data_party.rds")

# ------------------ Distance graph ---------------------------- #

# Econ

data_distance_econ <- data_party %>%
    filter(Region_name != "NA") %>%
    group_by(Region_name) %>%
    summarise(mean_distance = mean(econ_distance, na.rm = TRUE), .groups = "drop")
    

ggplot(data_distance_econ, aes(x = Region_name, y = mean_distance)) +
    geom_bar(stat = "identity") +
    labs(x = "Region", 
         y = "Mean distance", 
         title = "Mean distance between party alignment (GPT-4) and party alignment (Global Party Survey)") +
    theme_classic()

# sos

data_distance_sos <- data_party %>%
    filter(Region_name != "NA") %>%
    group_by(Region_name) %>%
    summarise(mean_distance = mean(sos_distance, na.rm = TRUE), .groups = "drop")
    

ggplot(data_distance_sos, aes(x = Region_name, y = mean_distance)) +
    geom_bar(stat = "identity") +
    labs(x = "Region", 
         y = "Mean distance", 
         title = "Mean distance between party alignment (GPT-4) and party alignment (Global Party Survey)") +
    theme_classic()


# ------------------ Distance graph anglo saxon ---------------------------- 

# Econ 

data_distance_anglo_econ <- data_party %>%
    filter(Region_name != "NA") %>%
    group_by(group) %>%
    summarise(mean_distance = mean(econ_distance, na.rm = TRUE), .groups = "drop")
    
ggplot(data_distance_anglo_econ, aes(x = group, y = mean_distance)) +
    geom_bar(stat = "identity") +
    labs(x = "Region", 
         y = "Mean distance", 
         title = "Mean distance between party alignment (GPT-4) and party alignment (Global Party Survey)") +
    theme_classic()

# sos

data_distance_anglo_sos <- data_party %>%
    filter(Region_name != "NA") %>%
    group_by(group) %>%
    summarise(mean_distance = mean(sos_distance, na.rm = TRUE), .groups = "drop")
    
ggplot(data_distance_anglo_sos, aes(x = group, y = mean_distance)) +
    geom_bar(stat = "identity") +
    labs(x = "Region", 
         y = "Mean distance", 
         title = "Mean distance between party alignment (GPT-4) and party alignment (Global Party Survey)") +
    theme_classic()
