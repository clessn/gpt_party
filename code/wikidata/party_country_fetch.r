# This script binds the country QID to the party QID

library(tidyverse)
library(tidywikidatar)
library(httr)
library(jsonlite)

data_path <- "_SharedFolder_article_wikipedia-chatgpt/data/"

data_party <- readRDS(paste0(data_path, "party_alignment.rds"))
data_country <- readRDS(paste0(data_path, 
                               "un_country_population_qid.rds")) %>%
                               rename(country_qid = qid)

alignments <- unique(data_party$value)

alignments_study <- c("radical left", 
                      "far-left", 
                      "left-wing", 
                      "centre-left", 
                      "centrism", 
                      "centre-right", 
                      "right-wing", 
                      "far-right", 
                      "right-wing extremism")

data_party <- data_party %>% 
  filter(value %in% alignments_study)

endpoint <- "https://query.wikidata.org/sparql"

data_final <- data.frame()
data_noqid <- data.frame()

for(i in seq_along(data_party$qid)) {

  query <- paste0(
    "SELECT ?country
    WHERE {
      wd:", data_party$qid[i], " wdt:P17 ?country.
    }"
  )

  response <- GET(endpoint, query = list(query = query, format = "json"))
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  data_label <- (data.frame(data$results$bindings$country))

  if(length(data$results$bindings$country) == 0) {
    cat(paste0("No results for qid: ", data_party$qid[i], "\n"))
    data_noqid_temp <- data.frame(qid = data_party$qid[i])
    data_noqid <- rbind(data_noqid, data_noqid_temp)
    next
  }

  if(!startsWith(data_label$value[1], "http")) {
    cat(paste0("No results for qid: ", data_parties$qid[i], "\n"))
    data_noqid_temp <- data.frame(qid = data_parties$qid[i])
    data_noqid <- rbind(data_noqid, data_noqid_temp)
    next
  }

  data_label$qid <- data_party$qid[i]

  # Check if data_label has fewer columns than data_final
  while(ncol(data_label) < ncol(data_final)) {
    data_label[paste0("missing_col_", ncol(data_label) + 1)] <- NA
  }

  # Diagnostics
  cat(paste0("Iteration: ", i, "\n"))

  cat(paste0("data_label shape: ", dim(data_label)[1], " x ", 
      dim(data_label)[2], "\n"))

  cat(paste0("data_final shape: ", dim(data_final)[1], " x ", 
      dim(data_final)[2], "\n"))

  data_final <- rbind(data_final, data_label)
  print(paste0(data_label$value))
}

data_final$country_qid <- sub("http://www.wikidata.org/entity/", "", 
                              data_final$value)

saveRDS(data_final, paste0(data_path, "partyqid_countryqid.rds"))
data_final <- readRDS(paste0(data_path, "partyqid_countryqid.rds"))

data_final <- data_final[!duplicated(data_final$qid), ]

data_merge_alignment_party <- data_final %>% 
  merge(data_party, by = "qid")

data_merge_country_party <- data_merge_alignment_party %>% 
  merge(data_country, by = "country_qid")

saveRDS(data_merge_country_party, paste0(data_path, "party_country.rds"))