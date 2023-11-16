# The goal here is to create a dataframe with the QID of each country in a
# variable to be used in the next step of the analysis.

library(tidyverse)
library(tidywikidatar)

tw_enable_cache()
tw_set_cache_folder(path = fs::path(fs::path_home_r(), "R", "tw_data"))
tw_set_language(language = "en")
tw_create_cache_folder(ask = FALSE)

# ---------------------- Functions ---------------------------------------------
get_matching_id <- function(search_term) {
  loop_results <- tw_search(search = search_term)
  for (j in seq_along(loop_results$id)) {
    data_loop <- tw_get_qualifiers(id = loop_results$id[j], p = "P463")
    if ("Q1065" %in% data_loop$qualifier_id | 
        "Q458" %in% data_loop$qualifier_id) {
      return(loop_results$id[j])
    }
  }
  return(NA)
}

data_path <- "_SharedFolder_article_wikipedia-chatgpt/data/"

# Read MP dataset from a CSV file
# Filter to keep only countries
# Filter to keep only data from 2000
# Filter to keep only countries with a population > 1000000

data_country_list <- read.csv(paste0(data_path, "un_country_population.csv"), 
                              sep = ",") %>%
    filter(LocTypeName == "Country/Area", # Filter to keep only countries
           Time == "2000", # Filter to keep only data from 2000
           TPopulation1Jan > 1000) %>% # Filter to keep only countries with a 
                                       #population > 1000000
    select(ISO3_code,  
           ISO2_code, 
           Location, 
           TPopulation1Jan, 
           TPopulationMale1July, 
           TPopulationFemale1July, 
           PopDensity, 
           PopSexRatio, 
           MedianAgePop, 
           PopGrowthRate, 
           LEx, 
           LExMale, 
           LExFemale, 
           NetMigrations)

missing_countries <- c()

for (i in seq_along(data_country_list$ISO2_code)) {
  print(data_country_list$Location[i])
  
  # First, try using ISO3_code
  qid_result <- get_matching_id(data_country_list$ISO3_code[i])
  
  # If no match found with ISO3_code, try using ISO2_code
  if (is.na(qid_result)) {
    qid_result <- get_matching_id(data_country_list$ISO2_code[i])
  }
  
  # If no match found with both ISO codes, try using Location
  if (is.na(qid_result)) {
    qid_result <- get_matching_id(data_country_list$Location[i])
  }
  
  data_country_list$qid[i] <- qid_result
  
  # If no match is found after all attempts, add to missing_countries
  if (is.na(qid_result)) {
    missing_countries <- c(missing_countries, data_country_list$Location[i])
  }
}

data_country_list <- data_country_list %>%
  filter (!is.na(qid))

unique(data_country_list$qid)
duplicated(data_country_list$qid)

saveRDS(data_country_list, 
        paste0(data_path, "un_country_population_qid.rds"))

