library(dplyr)

df <- readRDS("data/ches/tmp/04_ches_data_id_with_galtan.rds")

df_mean <- df %>%
  mutate(galtan_mean = rowMeans(select(., starts_with("galtan_")), na.rm = TRUE)) %>%
  mutate(lrdecon_mean = rowMeans(select(., starts_with("lrecon_")), na.rm = TRUE)) %>%
  select(-ends_with("_1"), -ends_with("_2"), -ends_with("_3"), -lrgen, -party_id)

saveRDS(df_mean, "data/ches/tmp/05_ches_df.rds")
