library(tidyverse)
library(openai)
library(stringr)


data_party <- read.csv("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/dataverse_files/Global Party Survey by Party SPSS V2_1_Apr_2020-2.csv")

#Ideology 
#Partyname
#Country

# ------------------- PROMPTING V4 ECONOMIC LEFT VS RIGHT ----------------------

data_party$econ_ideo_gpt_run1 <- NA
data_party$econ_ideo_gpt_run2 <- NA
data_party$econ_ideo_gpt_run3 <- NA

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
                    "content" = "You are a helpful assistant"
                ),
                list(
                    "role" = "user",
                    "content" = paste0("Parties can be classified by their current stance on ECONOMIC ISSUES such as privatization, taxes, regulation, government spending, and the welfare state. Those on the economic LEFT want government to play an active role in the economy. Those on the economic RIGHT favor a reduced role for government. Where would you place the following party from the following country on a scale from 0 to 10 where 0 means Extreme economic left and 10 means Extreme economic right. Party: ", paste0(data_party$Partyname[i]), ". Country: ", paste0(data_party$Country[i]), ". Provide only a number between 0 and 10 as your response. Please answer to the best of your knowledge. Make sure to output nothing else than a number between 0 and 10. take a deep breath and work on this problem step by step.")
                )
            )
        )

        # Check if the chat_prompt was successful
        if (!is.null(chat_prompt$choices)) {
            print(paste("Run: ", run, "i: ", i, "Party: ", data_party$Partyname[i], ". Country: ", data_party$Country[i], ". Ideology: ", data_party$V4_Scale[i], ". Gpt: ", chat_prompt$choices$message.content))
            
            # Assign the result to the correct column based on the run
            if (run == 1) {
                data_party$econ_ideo_gpt_run1[i] <- chat_prompt$choices$message.content
            } else if (run == 2) {
                data_party$econ_ideo_gpt_run2[i] <- chat_prompt$choices$message.content
            } else if (run == 3) {
                data_party$econ_ideo_gpt_run3[i] <- chat_prompt$choices$message.content
            }
        } else {
            print(paste("Error with API for Run: ", run, "Party: ", data_party$Partyname[i], ". Country: ", data_party$Country[i]))
        }
  
        # Delay to avoid hitting rate limits
        Sys.sleep(2)
    }
}

write.csv(data_party, "_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt_uncleaned.csv")

saveRDS(data_party, "_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt_uncleaned.rds")

data_party$econ_ideo_gpt_1 <- ifelse(nchar(data_party$econ_ideo_gpt_run1) > 10, NA, data_party$econ_ideo_gpt_run1)
data_party$econ_ideo_gpt_2 <- ifelse(nchar(data_party$econ_ideo_gpt_run2) > 10, NA, data_party$econ_ideo_gpt_run2)
data_party$econ_ideo_gpt_3 <- ifelse(nchar(data_party$econ_ideo_gpt_run3) > 10, NA, data_party$econ_ideo_gpt_run3)

data_party$econ_ideo_gpt_1 <- as.numeric(data_party$econ_ideo_gpt_1)
data_party$econ_ideo_gpt_2 <- as.numeric(data_party$econ_ideo_gpt_2)
data_party$econ_ideo_gpt_3 <- as.numeric(data_party$econ_ideo_gpt_3)

data_party$econ_ideo_gpt_mean <- rowMeans(data_party[, c("econ_ideo_gpt_1", "econ_ideo_gpt_2", "econ_ideo_gpt_3")], na.rm = TRUE)

write.csv(data_party, "_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt.csv")

saveRDS(data_party, "_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt.rds")

# ------------------- PROMPTING V6 LIBERALISM VS CONSERVATISM ------------------

data_party$sos_ideo_gpt_run1 <- NA
data_party$sos_ideo_gpt_run2 <- NA
data_party$sos_ideo_gpt_run3 <- NA

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
                    "content" = "You are a helpful assistant"
                ),
                list(
                    "role" = "user",
                    "content" = paste0("Parties can be classified by their current social values. Those with LIBERAL values favor expanded personal freedoms, for example, on abortion rights, same-sex marriage, and democratic participation. Those with CONSERVATIVE values reject these ideas in favor of order, tradition and stability, believing that government should be a firm moral authority on social and cultural issues. Where would you place the following party from the following country on a scale from 0 to 10 where 0 means Very liberal and 10 means Very conservative. Party: ", paste0(data_party$Partyname[i]), ". Country: ", paste0(data_party$Country[i]), ". Provide only a number between 0 and 10 as your response. Please answer to the best of your knowledge. Make sure to output nothing else than a number between 0 and 10. take a deep breath and work on this problem step by step.")
                )
            )
        )

        # Check if the chat_prompt was successful
        if (!is.null(chat_prompt$choices)) {
            print(paste("Run: ", run, "i: ", i, "Party: ", data_party$Partyname[i], ". Country: ", data_party$Country[i], ". Ideology: ", data_party$V6_Scale[i], ". Gpt: ", chat_prompt$choices$message.content))
            
            # Assign the result to the correct column based on the run
            if (run == 1) {
                data_party$sos_ideo_gpt_run1[i] <- chat_prompt$choices$message.content
            } else if (run == 2) {
                data_party$sos_ideo_gpt_run2[i] <- chat_prompt$choices$message.content
            } else if (run == 3) {
                data_party$sos_ideo_gpt_run3[i] <- chat_prompt$choices$message.content
            }
        } else {
            print(paste("Error with API for Run: ", run, "Party: ", data_party$Partyname[i], ". Country: ", data_party$Country[i]))
        }
  
        # Delay to avoid hitting rate limits
        Sys.sleep(2)
    }
}

write.csv(data_party, "_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt_uncleaned_sos.csv")

saveRDS(data_party, "_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt_uncleaned_sos.rds")

data_party$sos_ideo_gpt_1 <- ifelse(nchar(data_party$sos_ideo_gpt_run1) > 10, NA, data_party$sos_ideo_gpt_run1)
data_party$sos_ideo_gpt_2 <- ifelse(nchar(data_party$sos_ideo_gpt_run2) > 10, NA, data_party$sos_ideo_gpt_run2)
data_party$sos_ideo_gpt_3 <- ifelse(nchar(data_party$sos_ideo_gpt_run3) > 10, NA, data_party$sos_ideo_gpt_run3)

data_party$sos_ideo_gpt_1 <- as.numeric(data_party$sos_ideo_gpt_1)
data_party$sos_ideo_gpt_2 <- as.numeric(data_party$sos_ideo_gpt_2)
data_party$sos_ideo_gpt_3 <- as.numeric(data_party$sos_ideo_gpt_3)

data_party$sos_ideo_gpt_mean <- rowMeans(data_party[, c("sos_ideo_gpt_1", "sos_ideo_gpt_2", "sos_ideo_gpt_3")], na.rm = TRUE)

write.csv(data_party, "_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt_final.csv")

saveRDS(data_party, "_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt_final.rds")

