# This script queries Wikidata for the political alignment of each party
# It also saves a dataframe of all the parties that don't have an alignment

library(tidyverse)
library(tidywikidatar)
library(httr)
library(jsonlite)

data_path <- "_SharedFolder_article_wikipedia-chatgpt/data/"
data_parties <- readRDS(paste0(data_path, "wikidata_parties.rds"))

endpoint <- "https://query.wikidata.org/sparql"

data_final <- data.frame()
data_noqid <- data.frame()


for(i in seq_along(data_parties$qid)) {

  query <- paste0(
    "SELECT ?alignmentLabel
    WHERE {
      wd:", data_parties$qid[i], " wdt:P1387 ?alignment.
      SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
    }"
  )
  
  response <- GET(endpoint, query = list(query = query, format = "json"))
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  data_label <- (data.frame(data$results$bindings$alignmentLabel))

  if(length(data$results$bindings) == 0) {
    cat(paste0("No results for qid: ", data_parties$qid[i], "\n"))
    data_noqid_temp <- data.frame(qid = data_parties$qid[i])
    data_noqid <- rbind(data_noqid, data_noqid_temp)
    next
  }

  if(startsWith(data_label$value[1], "http")) {
    cat(paste0("No results for qid: ", data_parties$qid[i], "\n"))
    data_noqid_temp <- data.frame(qid = data_parties$qid[i])
    data_noqid <- rbind(data_noqid, data_noqid_temp)
    next
  }

  data_label$qid <- data_parties$qid[i]

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

saveRDS(data_final, paste0(data_path, "party_alignment.rds"))
saveRDS(data_noqid, paste0(data_path, "alignment_noqid.rds"))
