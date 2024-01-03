library(tidyverse)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt_final.rds")

data_party$econ_distance <- abs(data_party$V4_Scale - data_party$econ_ideo_gpt_mean)
data_party$sos_distance <- abs(data_party$V6_Scale - data_party$sos_ideo_gpt_mean)

data_party$Region_name <- NA 
data_party$Region_name[data_party$Region == 6] <- "asia_pacific"
data_party$Region_name[data_party$Region == 5] <- "west"
data_party$Region_name[data_party$Region == 4] <- "africa"
data_party$Region_name[data_party$Region == 3] <- "mena"
data_party$Region_name[data_party$Region == 2] <- "latam"
data_party$Region_name[data_party$Region == 1] <- "eurasia"
table(data_party$Region_name)

anglo_saxon <- c("CAN", "USA", "GBR", "AUS", "NZL", "IRL")

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

# ------------------ Distance graph anglo saxon ---------------------------- #

data_party$group <- "rest"
data_party$group[data_party$ISO %in% anglo_saxon] <- "anglo_saxon"

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
