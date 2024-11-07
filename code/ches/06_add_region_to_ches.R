library(dplyr)

source("code/ches/00_ches_naming.R")

df_ches <- readRDS("data/ches/tmp/05_ches_df.rds")
df_eu <- haven::read_dta("https://www.chesdata.eu/s/CHES2019V3.dta")
df_la <- haven::read_dta("https://www.chesdata.eu/s/ches_la_2020_aggregate_level_v01.dta")
df_isr <- haven::read_dta("https://www.chesdata.eu/s/CHES_ISRAEL_means_2021_2022.dta")

# Create a named vector with reversed mapping (party name -> ID)
party_to_id <- names(party_map)
names(party_to_id) <- party_map

# Now you can create the ches_id variable in your dataframe using match
# Assuming your dataframe is called 'df' and the party column is called 'party':
df_ches$ches_id <- party_to_id[df_ches$party]

# If you want to ensure numeric IDs, you can convert:
df_ches$ches_id <- as.numeric(df_ches$ches_id)

country_to_id <- names(country_map)
names(country_to_id) <- country_map

df_ches$country_num <- country_to_id[df_ches$country]

df_ches$region <- NA
df_ches$region_tmp <- NA
df_ches$region_tmp[df_ches$country_num %in% df_eu$country] <- df_eu$eastwest
df_ches$region[df_ches$region_tmp == 1] <- "western_europe"
df_ches$region[df_ches$region_tmp == 0] <- "eastern_europe"
df_ches$region[df_ches$country_id == "CH"] <- "western_europe"
df_ches$region[df_ches$country_id == "NO"] <- "western_europe"
df_ches$region[df_ches$country_id == "IS"] <- "western_europe"
df_ches$region[df_ches$country_id == "TR"] <- "middle_east"
df_ches$region[df_ches$country_id == "IL"] <- "middle_east"
df_ches$region[df_ches$country_id %in% df_la$country_abb] <- "latin_america"
table(df_ches$region, useNA = "ifany")

df_ches <- df_ches %>%
  select(-region_tmp)

saveRDS(df_ches, "data/ches/tmp/06_ches_data.rds")
