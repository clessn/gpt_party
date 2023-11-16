# This script queries Wikidata for a list of all political parties in the 
# database

library(tidyverse)
library(tidywikidatar)
library(httr)
library(jsonlite)

tw_enable_cache()
tw_set_cache_folder(path = fs::path(fs::path_home_r(), "R", "tw_data"))
tw_set_language(language = "en")
tw_create_cache_folder(ask = FALSE)

data_path <- "_SharedFolder_article_wikipedia-chatgpt/data/"

endpoint <- "https://query.wikidata.org/sparql"

query <- paste(
  "SELECT ?party ?partyLabel WHERE {",
  "?party wdt:P31 wd:Q7278.",
  "SERVICE wikibase:label { bd:serviceParam wikibase:language \"[AUTO_LANGUAGE],en\". }",
  "}"
)

response <- GET(endpoint, query = list(query = query, format = "json"))

# Parse the JSON response
data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
# Extract the results and convert to a dataframe
data_parties <- data.frame(party = data$results$bindings$partyLabel$value)
data_parties$qid <- data$results$bindings$party$value
    
data_parties$qid <- sub("http://www.wikidata.org/entity/", "", data_parties$qid)

saveRDS(data_parties, paste0(data_path, "wikidata_parties.rds"))


