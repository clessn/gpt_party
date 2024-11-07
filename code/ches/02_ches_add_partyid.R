library(dplyr)

df <- readRDS("data/ches/tmp/ches_data.rds")

df_id <- df %>%
  group_by(country) %>%
  mutate(party_id = paste0(country_id, "_", seq_len(n()))) %>%
  ungroup()

saveRDS(df_id, "data/ches/tmp/ches_data_id.rds")
