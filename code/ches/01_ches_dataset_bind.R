library(dplyr)

source("code/ches/00_ches_naming.R")

df_eu <- haven::read_dta("https://www.chesdata.eu/s/CHES2019V3.dta")
df_la <- haven::read_dta("https://www.chesdata.eu/s/ches_la_2020_aggregate_level_v01.dta")
df_isr <- haven::read_dta("https://www.chesdata.eu/s/CHES_ISRAEL_means_2021_2022.dta")

df_isr_modified <- df_isr %>%
  filter(year == 2021) %>%
  select(party_name, galtan, lrecon, lrgen) %>%
  mutate(
    country = "israel", country_id = "IL",
    party = party_name
  ) %>%
  select(-party_name)

df_eu_modified <- df_eu %>%
  select(country, party_id, galtan, lrecon, lrgen) %>%
  mutate(
    country = country_map[as.character(country)],
    country_id = countrycode::countrycode(country, origin = "country.name", destination = "iso2c"),
    party = party_map[as.character(party_id)]
  ) %>%
  select(-party_id)

df_la_modified <- df_la %>%
  select(country_en, country_abb, party_en, galtan, lrecon, lrgen) %>%
  mutate(
    country = country_en,
    country_id = country_abb,
    party = party_en
  ) %>%
  select(-country_en, -country_abb, -party_en)

df_combined <- bind_rows(df_eu_modified, df_la_modified, df_isr_modified)

df_combined_reordered <- df_combined %>%
  select(country, country_id, party, galtan, lrecon, lrgen)

saveRDS(df_combined_reordered, "data/ches/tmp/ches_data.rds")
