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

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/gps_gpt.rds")

data_party$ideo_dist <- abs(as.numeric(data_party$Ideology_gpt) - as.numeric(data_party$Ideology))

data_graph <- data_party %>%
  # Remove strings longer than 1 character and convert to numeric
  mutate(Ideology_gpt = as.numeric(str_replace_all(Ideology_gpt, "\\D", ""))) %>%
  # Round Ideology to the nearest integer
  mutate(Ideology = round(as.numeric(Ideology))) %>%
  # Now we filter out NA and unwanted values after conversion
  filter(!is.na(Ideology_gpt), nchar(as.character(Ideology_gpt)) <= 2) %>%
  group_by(Ideology, Ideology_gpt) %>%
  summarise(n = n(), .groups = "drop") 


ggplot(data_graph, aes(x = Ideology, y = Ideology_gpt)) +
  geom_tile(aes(alpha = n)) +
  labs(x = "Party alignment (Global Party Survey)", 
       y = "Party alignment (GPT-4)", 
       title = "Party alignment (GPT-4) vs. party alignment (Global Party Survey)") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dotted") +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

cor(data_graph$Ideology, data_graph$Ideology_gpt, method = "kendall")

m1 <- lm(Ideology_gpt ~ Ideology, data = data_graph)
summary(m1)
