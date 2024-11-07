library(dplyr)

df <- readRDS("data/ches/tmp/06_ches_data.rds") %>%
  rename(lrecon_mean = lrdecon_mean)

df$lrecon_distance <- abs(df$lrecon - df$lrecon_mean)
df$galtan_distance <- abs(df$galtan - df$galtan_mean)

df$lrecon_alignment <- NA
df$lrecon_alignment[df$lrecon == 5] <- "center"
df$lrecon_alignment[df$lrecon > 5] <- "right"
df$lrecon_alignment[df$lrecon < 5] <- "left"
df$lrecon_alignment <- factor(df$lrecon_alignment, levels = c("left", "center", "right"))
table(df$lrecon_alignment)

df$galtan_alignment <- NA
df$galtan_alignment[df$galtan == 5] <- "center"
df$galtan_alignment[df$galtan > 5] <- "right"
df$galtan_alignment[df$galtan < 5] <- "left"
df$galtan_alignment <- factor(df$galtan_alignment, levels = c("left", "center", "right"))
table(df$galtan_alignment)

df$lrecon_category <- NA
df$lrecon_category[df$lrecon < 2] <- 1
df$lrecon_category[df$lrecon >= 2 & df$lrecon < 4] <- 2
df$lrecon_category[df$lrecon >= 4 & df$lrecon < 5] <- 3
df$lrecon_category[df$lrecon == 5] <- 4
df$lrecon_category[df$lrecon > 5 & df$lrecon <= 6] <- 5
df$lrecon_category[df$lrecon > 6 & df$lrecon <= 8] <- 6
df$lrecon_category[df$lrecon > 8] <- 7

df$galtan_category <- NA
df$galtan_category[df$galtan < 2] <- 1
df$galtan_category[df$galtan >= 2 & df$galtan < 4] <- 2
df$galtan_category[df$galtan >= 4 & df$galtan < 5] <- 3
df$galtan_category[df$galtan == 5] <- 4
df$galtan_category[df$galtan > 5 & df$galtan <= 6] <- 5
df$galtan_category[df$galtan > 6 & df$galtan <= 8] <- 6
df$galtan_category[df$galtan > 8] <- 7

saveRDS(df, "data/ches/tmp/07_ches_data.rds")
