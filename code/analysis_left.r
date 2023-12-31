library(dplyr)
library(ggplot2)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt.rds")

data_party$distance <- abs(data_party$Ideology - data_party$Ideology_gpt_mean)

data_party$party_alignment <- NA
data_party$party_alignment[data_party$Ideology > 4.5 & data_party$Ideology < 5.5 ] <- "center"
data_party$party_alignment[data_party$Ideology >= 5.5] <- "right"
data_party$party_alignment[data_party$Ideology <= 4.5] <- "left"
table(data_party$party_alignment)

data_party$party_alignment <- factor(data_party$party_alignment, levels = c("left", "center", "right"))


data_party %>% 
    group_by(party_alignment) %>%
    summarise(mean_distance = mean(distance, na.rm = TRUE), .groups = "drop") %>% 
    ggplot(aes(x = party_alignment, y = mean_distance)) +
    geom_bar(stat = "identity") +
    labs(x = "Party alignment", 
         y = "Mean distance", 
         title = "Mean distance between party alignment (GPT-4) and party alignment (Global Party Survey)") +
    theme_classic()

