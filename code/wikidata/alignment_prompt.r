library(tidyverse)
library(openai)

data_path <- "_SharedFolder_article_wikipedia-chatgpt/data/"

data <- readRDS(paste0(data_path, "final_db.rds"))

data <- data  |­> 
    select(-type.y, -xml.lang, -value.x, -type.x)  |­> 
    rename(alignment = value.y, 
           country = Location,)

alignments_study <- c("radical left", 
                      "far-left", 
                      "left-wing", 
                      "centre-left", 
                      "centrism", 
                      "centre-right", 
                      "right-wing", 
                      "far-right", 
                      "right-wing extremism")

data <- data %>% 
  filter(alignment %in% alignments_study)

data_grouped <- data %>%
    mutate(alignment = factor(alignment, levels = alignments_study)) %>%
    group_by(alignment) %>%
    summarise(n = n())

ggplot(data_grouped, aes(x = alignment, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Party alignment", 
       y = "Number of parties", 
       title = "Number of parties by party alignment") +
  theme_classic()

data$alignment_digital <- NA

data$alignment_digital[data$alignment == "radical left"] <- 0
data$alignment_digital[data$alignment == "far-left"] <- 1 / 8 
data$alignment_digital[data$alignment == "left-wing"] <- 2 / 8
data$alignment_digital[data$alignment == "centre-left"] <- 3 / 8
data$alignment_digital[data$alignment == "centrism"] <- 4 / 8
data$alignment_digital[data$alignment == "centre-right"] <- 5 / 8
data$alignment_digital[data$alignment == "right-wing"] <- 6 / 8
data$alignment_digital[data$alignment == "far-right"] <- 7 / 8
data$alignment_digital[data$alignment == "right-wing extremism"] <- 8 / 8

data_grouped_digital <- data %>%
    mutate(alignment_digital) %>%
    group_by(alignment_digital) %>%
    summarise(n = n())
    
ggplot(data_grouped_digital, aes(x = alignment_digital, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Party alignment", 
       y = "Number of parties", 
       title = "Number of parties by party alignment") +
  theme_classic()

data$gpt_alignment <- NA
price_4 <- c()
price_35 <- c()

for (i in 2010:2095) {
    chat_prompt <- create_chat_completion(
    model = "gpt-3.5-turbo",
    messages = list(
        list(
            "role" = "system",
            "content" = "You are a helpful assistant trained to assist in categorizing political parties according to their political alignment. Use only the options provided to categorize the party."
        ),
        list(
            "role" = "user",
            "content" = paste0("Categorize the political alignment of the following party using only one of these options: ", paste0(alignments_study, collapse = ", "), ". Party: ", print(data$party_name[i]), ". Country: ", print(data$country[i]), ". Provide only the alignment as your response.")
        )
    )
)
    print(i)
    data$gpt_alignment[i] <- chat_prompt$choices$message.content
    
    price_4[i] <- as.numeric((chat_prompt$usage$prompt_tokens / 1000) * 0.03) + ((chat_prompt$usage$completion_tokens / 1000) * 0.06)
    price_35[i] <- as.numeric((chat_prompt$usage$prompt_tokens / 1000) * 0.0015) + ((chat_prompt$usage$completion_tokens / 1000) * 0.002)
    
}
print(sum(price_35))
saveRDS(data, paste0(data_path, "temp_final_db_run1.rds"))

data$alignment_gpt_digital <- NA

data$alignment_gpt_digital[data$gpt_alignment == "radical left"] <- 0
data$alignment_gpt_digital[data$gpt_alignment == "far-left"] <- 1 / 8 
data$alignment_gpt_digital[data$gpt_alignment == "left-wing"] <- 2 / 8
data$alignment_gpt_digital[data$gpt_alignment == "centre-left"] <- 3 / 8
data$alignment_gpt_digital[data$gpt_alignment == "centrism"] <- 4 / 8
data$alignment_gpt_digital[data$gpt_alignment == "centre-right"] <- 5 / 8
data$alignment_gpt_digital[data$gpt_alignment == "right-wing"] <- 6 / 8
data$alignment_gpt_digital[data$gpt_alignment == "far-right"] <- 7 / 8
data$alignment_gpt_digital[data$gpt_alignment == "right-wing extremism"] <- 8 / 8

ggplot(data, aes(x = alignment_digital, y = alignment_gpt_digital)) +
  geom_point() +
  labs(x = "Party alignment", 
       y = "Party alignment (GPT-4)", 
       title = "Party alignment (GPT-4) vs. party alignment (human)") +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dotted") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 1 / 8)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 1 / 8)) +
  theme_classic()

saveRDS(data, paste0(data_path, "temp_final_db.rds"))
data <- readRDS(paste0(data_path, "temp_final_db.rds"))

data <- data %>%
  group_by(alignment_digital, alignment_gpt_digital) %>%
  summarise(n = n()) %>%
  filter(!is.na(alignment_gpt_digital))

ggplot(data, aes(x = alignment_digital, y = alignment_gpt_digital)) +
  geom_tile(aes(alpha = n)) +
  labs(x = "Party alignment (Wikidata)", 
       y = "Party alignment (GPT-4)", 
       title = "Party alignment (GPT-4) vs. party alignment (Wikidata)") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dotted") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 1 / 8)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 1 / 8)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

