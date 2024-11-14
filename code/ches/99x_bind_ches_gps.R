library(dplyr)

df_id <- readRDS("data/ches/tmp/01_ches_data.rds") %>%
  select(party, party_abb, country)

df <- readRDS("data/ches/tmp/07_ches_data.rds")

# Assuming both dataframes have a country variable
merged_df <- df %>%
  left_join(df_id, by = c("party", "country")) %>%
  relocate(party_abb, .before = galtan)

write.csv(merged_df, "data/ches/input_party_size.csv", row.names = FALSE)
