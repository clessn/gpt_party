library(tidyverse)
library(tidywikidatar)
library(httr)
library(jsonlite)

data_path <- "_SharedFolder_article_wikipedia-chatgpt/data/"

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

data_party_grouped <- data_party %>%
    mutate(value = factor(value, levels = alignments_study)) %>%
    group_by(value) %>%
    summarise(n = n())

ggplot(data_party_grouped, aes(x = value, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Party alignment", 
       y = "Number of parties", 
       title = "Number of parties by party alignment") +
  theme_classic()
