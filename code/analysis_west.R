library(tidyverse)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt.rds")

data_party$distance <- abs(data_party$Ideology - data_party$Ideology_gpt_mean)

data_party$Region_name <- NA 
data_party$Region_name[data_party$Region == 6] <- "asia_pacific"
data_party$Region_name[data_party$Region == 5] <- "west"
data_party$Region_name[data_party$Region == 4] <- "africa"
data_party$Region_name[data_party$Region == 3] <- "mena"
data_party$Region_name[data_party$Region == 2] <- "latam"
data_party$Region_name[data_party$Region == 1] <- "eurasia"
table(data_party$Region_name)

anglo_saxon <- c("CAN", "USA", "GBR", "AUS", "NZL", "IRL")


data_distance <- data_party %>%
    filter(Region_name != "NA") %>%
    group_by(Region_name) %>%
    summarise(mean_distance = mean(distance, na.rm = TRUE), .groups = "drop")
    

ggplot(data_distance, aes(x = Region_name, y = mean_distance)) +
    geom_bar(stat = "identity") +
    labs(x = "Region", 
         y = "Mean distance", 
         title = "Mean distance between party alignment (GPT-4) and party alignment (Global Party Survey)") +
    theme_classic()


data_party$group <- "rest"
data_party$group[data_party$ISO %in% anglo_saxon] <- "anglo_saxon"

# Group by the new 'group' column and summarise
data_distance_anglo <- data_party %>%
    filter(Region_name != "NA") %>%
    group_by(group) %>%
    summarise(mean_distance = mean(distance, na.rm = TRUE), .groups = "drop")
    

ggplot(data_distance_anglo, aes(x = group, y = mean_distance)) +
    geom_bar(stat = "identity") +
    labs(x = "Region", 
         y = "Mean distance", 
         title = "Mean distance between party alignment (GPT-4) and party alignment (Global Party Survey)") +
    theme_classic()
