library(tidyverse)
library(openai)

data_party <- read.csv("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/dataverse_files/Global Party Survey by Party SPSS V2_1_Apr_2020-2.csv")

#Ideology 
#Partyname
#Country

data_party$Ideology100 <- NA



for (i in seq_along(data_party$Country)) {
  chat_prompt <- create_chat_completion(
    model = "gpt-4",
    messages = list(
      list(
        "role" = "system",
        "content" = "You are a helpful assistant trained to assist in categorizing political parties according to their political alignment."
      ),
      list(
        "role" = "user",
        "content" = paste0("Categorize the political alignment of the following party using a 10-point scale on which the general political views of the party can be arranged from LEFT to RIGHT where 0 means very left and 10 means very right. Party: ", paste0(data_party$Partyname[i]), ". Country: ", paste0(data_party$country[i]), ". Provide only a number between 0 and 10 as your response.")
      
    )
  )
  print(i)
  data$gpt_alignment[i] <- chat_prompt$choices$message.content
  
}

hist(data_party$Ideology)



