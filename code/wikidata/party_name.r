library(tidyverse)
library(tidywikidatar)

tw_enable_cache()
tw_set_cache_folder(path = fs::path(fs::path_home_r(), "R", "tw_data"))
tw_set_language(language = "en")
tw_create_cache_folder(ask = FALSE)

data_path <- "_SharedFolder_article_wikipedia-chatgpt/data/"

data_party_country <- readRDS(paste0(data_path, "party_country.rds"))
data_party_country$party_name <- NA

for (i in seq_along(data_party_country$country_qid)) {
  data_party_country$party_name[i] <- tw_get_label(id = data_party_country$qid[i])
  print(data_party_country$party_name[i])
}

data_party_named <- data_party_country[!duplicated(
                                               data_party_country$party_name), ]

saveRDS(data_party_named, paste0(data_path, "final_db.rds"))
