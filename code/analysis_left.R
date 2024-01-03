library(dplyr)
library(ggplot2)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt_final.rds")

data_party$econ_distance <- abs(data_party$V4_Scale - data_party$econ_ideo_gpt_mean)
data_party$sos_distance <- abs(data_party$V6_Scale - data_party$sos_ideo_gpt_mean)

data_party$econ_alignment <- NA
data_party$econ_alignment[data_party$V4_Scale > 4.5 & 
                           data_party$V4_Scale < 5.5 ] <- "center"
data_party$econ_alignment[data_party$V4_Scale >= 5.5] <- "right"
data_party$econ_alignment[data_party$V4_Scale <= 4.5] <- "left"
table(data_party$econ_alignment)

data_party$sos_alignment <- NA
data_party$sos_alignment[data_party$V6_Scale > 4.5 & 
                           data_party$V6_Scale < 5.5 ] <- "center"
data_party$sos_alignment[data_party$V6_Scale >= 5.5] <- "right"
data_party$sos_alignment[data_party$V6_Scale <= 4.5] <- "left"
table(data_party$sos_alignment)



data_party$econ_alignment <- factor(data_party$econ_alignment, 
                                     levels = c("left", "center", "right"))

data_party$sos_alignment <- factor(data_party$sos_alignment, 
                                     levels = c("left", "center", "right"))

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
