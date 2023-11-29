library(tidyverse)
library(openai)
library(stringr)


data_party <- read.csv("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/dataverse_files/Global Party Survey by Party SPSS V2_1_Apr_2020-2.csv")

#Ideology 
#Partyname
#Country

i <- 211

data_party$Ideology_gpt <- NA
delay_duration <- 2

for (i in 1:nrow(data_party)) {
  chat_prompt <- create_chat_completion(
    model = "gpt-4",
    messages = list(
      list(
        "role" = "system",
        "content" = "You are a helpful assistant trained to assist in categorizing political parties according to their political alignment."
      ),
      list(
        "role" = "user",
        "content" = paste0("Categorize the political alignment of the following party using a 10-point scale on which the general political views of the party can be arranged from LEFT to RIGHT where 0 means very left and 10 means very right. Party: ", paste0(data_party$Partyname[i]), ". Country: ", paste0(data_party$Country[i]), ". Provide only a number between 0 and 10 as your response. Please answer to the best of your knowledge.")
      
    ))
  )

    # Check if the chat_prompt was successful before proceeding
  if (!is.null(chat_prompt$choices)) {
    print(paste("Party: ", data_party$Partyname[i], ". Country: ", data_party$Country[i], ". Ideology: ", data_party$Ideology[i], ". Gpt: ", chat_prompt$choices$message.content))
    data_party$Ideology_gpt[i] <- chat_prompt$choices$message.content
  } else {
    print(paste("Error with API for Party: ", data_party$Partyname[i], ". Country: ", data_party$Country[i]))
  }
  
  # Introduce a delay to avoid hitting rate limits
  Sys.sleep(delay_duration)
  
}

write.csv(data_party, "_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt.csv")

saveRDS(data_party, "_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt.rds")


