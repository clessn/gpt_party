library(tidyverse)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt.rds")

data_two_parties <- data_party %>%
  filter(Partyname != "independent") %>% 
  group_by(Country) %>%  # Ensure the column name here matches your original data
  summarise(number_of_parties = n_distinct(Partyname)) %>%
  filter(number_of_parties == 2) %>%
  inner_join(data_party, by = "Country")  # Ensure correct column name is used in the join condition

data_two_parties_not <- data_party %>%
  filter(Partyname != "independent") %>% 
  group_by(Country) %>%  # Ensure the column name here matches your original data
  summarise(number_of_parties = n_distinct(Partyname)) %>%
  filter(number_of_parties != 2) %>%
  inner_join(data_party, by = "Country")  # Ensure correct column name is used in the join condition

ggplot(data_two_parties, aes(x = Ideology)) +
    geom_histogram(bins = 10)
