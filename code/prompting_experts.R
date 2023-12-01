library(tidyverse)
library(openai)
library(stringr)


data_party <- read.csv("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/dataverse_files/Global Party Survey by Party SPSS V2_1_Apr_2020-2.csv")

#Ideology 
#Partyname
#Country

data_party$Ideology_gpt_run1 <- NA
data_party$Ideology_gpt_run2 <- NA
data_party$Ideology_gpt_run3 <- NA

run_errors <- data.frame(
    "run" = NA,
    "party" = NA,
    "country" = NA
)

# Outer loop for three runs
for (run in 1:3) {
    # Inner loop for processing each party
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
                )
            )
        )

        # Check if the chat_prompt was successful
        if (!is.null(chat_prompt$choices)) {
            print(paste("Run: ", run, "Party: ", data_party$Partyname[i], ". Country: ", data_party$Country[i], ". Ideology: ", data_party$Ideology[i], ". Gpt: ", chat_prompt$choices$message.content))
            
            # Assign the result to the correct column based on the run
            if (run == 1) {
                data_party$Ideology_gpt_run1[i] <- chat_prompt$choices$message.content
            } else if (run == 2) {
                data_party$Ideology_gpt_run2[i] <- chat_prompt$choices$message.content
            } else if (run == 3) {
                data_party$Ideology_gpt_run3[i] <- chat_prompt$choices$message.content
            }
        } else {
            print(paste("Error with API for Run: ", run, "Party: ", data_party$Partyname[i], ". Country: ", data_party$Country[i]))
        }
  
        # Delay to avoid hitting rate limits
        Sys.sleep(3)
    }
}

data_party$Ideology_gpt_run1 <- as.numeric(data_party$Ideology_gpt_run1)
data_party$Ideology_gpt_run2 <- as.numeric(data_party$Ideology_gpt_run2)
data_party$Ideology_gpt_run3 <- as.numeric(data_party$Ideology_gpt_run3)

data_party$Ideology_gpt_mean <- (data_party$Ideology_gpt_run1 + data_party$Ideology_gpt_run2 + data_party$Ideology_gpt_run3) / 3

write.csv(data_party, "_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt.csv")

saveRDS(data_party, "_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt.rds")



