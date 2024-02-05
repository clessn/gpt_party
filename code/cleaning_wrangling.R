library(dplyr)
library(tidyr)
library(ggplot2)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt_final.rds")

data_party$econ_distance <- abs(data_party$V4_Scale - data_party$econ_ideo_gpt_mean)
data_party$sos_distance <- abs(data_party$V6_Scale - data_party$sos_ideo_gpt_mean)

data_party$cat_econ_distance <- abs(data_party$econ_ideo_cat - data_party$econ_ideo_cat_gpt)
data_party$cat_sos_distance <- abs(data_party$sos_ideo_cat - data_party$sos_ideo_cat_gpt)

mean(data_party$V4_Scale - data_party$econ_ideo_gpt_mean, na.rm = T)

data_party$Region_name <- NA 
data_party$Region_name[data_party$Region == 6] <- "asia_pacific"
data_party$Region_name[data_party$Region == 5] <- "west"
data_party$Region_name[data_party$Region == 4] <- "africa"
data_party$Region_name[data_party$Region == 3] <- "mena"
data_party$Region_name[data_party$Region == 2] <- "latam"
data_party$Region_name[data_party$Region == 1] <- "eurasia"
table(data_party$Region_name)

data_party$econ_alignment <- NA
data_party$econ_alignment[data_party$V4_Scale == 5] <- "center"
data_party$econ_alignment[data_party$V4_Scale > 5] <- "right"
data_party$econ_alignment[data_party$V4_Scale < 5] <- "left"
table(data_party$econ_alignment)

data_party$sos_alignment <- NA
data_party$sos_alignment[data_party$V6_Scale == 5 ] <- "center"
data_party$sos_alignment[data_party$V6_Scale > 5] <- "right"
data_party$sos_alignment[data_party$V6_Scale < 5] <- "left"
table(data_party$sos_alignment)

data_party$econ_alignment <- factor(data_party$econ_alignment, 
                                     levels = c("left", "center", "right"))

data_party$sos_alignment <- factor(data_party$sos_alignment, 
                                     levels = c("left", "center", "right"))

anglo_saxon <- c("CAN", "USA", "GBR", "AUS", "NZL", "IRL")

data_party$group <- "rest"
data_party$group[data_party$ISO %in% anglo_saxon] <- "anglo_saxon"

data_party$econ_ideo_cat <- NA
data_party$econ_ideo_cat[data_party$V4_Scale < 2] <- 1
data_party$econ_ideo_cat[data_party$V4_Scale >= 2 & data_party$V4_Scale < 4] <- 2
data_party$econ_ideo_cat[data_party$V4_Scale >= 4 & data_party$V4_Scale < 5] <- 3
data_party$econ_ideo_cat[data_party$V4_Scale == 5] <- 4
data_party$econ_ideo_cat[data_party$V4_Scale > 5 & data_party$V4_Scale <= 6] <- 5
data_party$econ_ideo_cat[data_party$V4_Scale > 6 & data_party$V4_Scale <= 8] <- 6
data_party$econ_ideo_cat[data_party$V4_Scale > 8] <- 7

data_party$econ_ideo_cat_gpt <- NA
data_party$econ_ideo_cat_gpt[data_party$econ_ideo_gpt_mean < 2] <- 1
data_party$econ_ideo_cat_gpt[data_party$econ_ideo_gpt_mean >= 2 & data_party$econ_ideo_gpt_mean < 4] <- 2
data_party$econ_ideo_cat_gpt[data_party$econ_ideo_gpt_mean >= 4 & data_party$econ_ideo_gpt_mean < 5] <- 3
data_party$econ_ideo_cat_gpt[data_party$econ_ideo_gpt_mean == 5] <- 4
data_party$econ_ideo_cat_gpt[data_party$econ_ideo_gpt_mean > 5 & data_party$econ_ideo_gpt_mean <= 6] <- 5
data_party$econ_ideo_cat_gpt[data_party$econ_ideo_gpt_mean > 6 & data_party$econ_ideo_gpt_mean <= 8] <- 6
data_party$econ_ideo_cat_gpt[data_party$econ_ideo_gpt_mean > 8] <- 7

data_party$sos_ideo_cat <- NA
data_party$sos_ideo_cat[data_party$V6_Scale < 2] <- 1
data_party$sos_ideo_cat[data_party$V6_Scale >= 2 & data_party$V6_Scale < 4] <- 2
data_party$sos_ideo_cat[data_party$V6_Scale >= 4 & data_party$V6_Scale < 5] <- 3
data_party$sos_ideo_cat[data_party$V6_Scale == 5] <- 4
data_party$sos_ideo_cat[data_party$V6_Scale > 5 & data_party$V6_Scale <= 6] <- 5
data_party$sos_ideo_cat[data_party$V6_Scale > 6 & data_party$V6_Scale <= 8] <- 6
data_party$sos_ideo_cat[data_party$V6_Scale > 8] <- 7

data_party$sos_ideo_cat_gpt <- NA
data_party$sos_ideo_cat_gpt[data_party$sos_ideo_gpt_mean < 2] <- 1
data_party$sos_ideo_cat_gpt[data_party$sos_ideo_gpt_mean >= 2 & data_party$sos_ideo_gpt_mean < 4] <- 2
data_party$sos_ideo_cat_gpt[data_party$sos_ideo_gpt_mean >= 4 & data_party$sos_ideo_gpt_mean < 5] <- 3
data_party$sos_ideo_cat_gpt[data_party$sos_ideo_gpt_mean == 5] <- 4
data_party$sos_ideo_cat_gpt[data_party$sos_ideo_gpt_mean > 5 & data_party$sos_ideo_gpt_mean <= 6] <- 5
data_party$sos_ideo_cat_gpt[data_party$sos_ideo_gpt_mean > 6 & data_party$sos_ideo_gpt_mean <= 8] <- 6
data_party$sos_ideo_cat_gpt[data_party$sos_ideo_gpt_mean > 8] <- 7

write.csv(data_party, "_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/data_party.csv")

saveRDS(data_party, "_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/data_party.rds")
