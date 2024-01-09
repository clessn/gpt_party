library(dplyr)
library(tidyr)
library(ggplot2)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/data_party.rds")

long_data <- data_party %>%
  select(Type_Partysize_seat, econ_distance, sos_distance) %>%
  pivot_longer(
    cols = c(econ_distance, sos_distance),
    names_to = "distance_type",
    values_to = "distance"
  ) %>%
  mutate(distance_type = if_else(distance_type == "econ_distance", "Econ", "Sos"))

# Calculate mean, standard error, and confidence interval
long_data2 <- long_data %>%
  group_by(Type_Partysize_seat, distance_type) %>%
  summarise(
    mean_distance = mean(distance, na.rm = TRUE),
    se = sd(distance, na.rm = TRUE) / sqrt(n()),
    lower_ci = mean_distance - qt(0.975, df = n() - 1) * se,
    upper_ci = mean_distance + qt(0.975, df = n() - 1) * se,
    .groups = "drop"
  ) %>% 
  mutate(partysize = case_when(
    Type_Partysize_seat == 1 ~ "Small",
    Type_Partysize_seat == 2 ~ "Medium",
    Type_Partysize_seat == 3 ~ "Large"
  ),
  partysize = factor(partysize, levels = c("Small", "Medium", "Large")),
  distance_type = ifelse(distance_type == "Econ", "Economic", "Social")) %>% 
  tidyr::drop_na(partysize)

# Plot with error bars
ggplot(long_data2, aes(x = partysize, y = mean_distance, fill = distance_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9),
           alpha = 0.7) +
  geom_linerange(aes(ymin = lower_ci,
                     ymax = upper_ci,
                     color = distance_type), linewidth = 1,
                 position = position_dodge(width = 0.9)) +
  labs(x = "\nParty Size (GPS categorization)\n", 
       y = "\nMean Distance\n") +
  clessnverse::theme_clean_light() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 20), # Bolder and larger axis title X
        axis.title.y = element_text(hjust = 0.5, size = 20), # Bolder and larger axis title Y
        axis.text.x = element_text(size = 20), # Bolder and larger axis text X
        axis.text.y = element_text(size = 20),
        plot.caption = element_text(size = 20), # Increased size
        legend.text = element_text(size = 20), # Increase legend text size
        legend.key.size = unit(1.5, "lines")) # Bolder and larger plot caption

ggsave("_SharedFolder_article_spsa2024_gpt_party/graphs/paper/h4_barplot.png",
       width = 8, height = 6)
  

ggplot(long_data2, aes(x = alignment, y = mean_distance, fill = alignment_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9),
           alpha = 0.7) +
  geom_linerange(aes(ymin = lower_ci,
                     ymax = upper_ci,
                     color = alignment_type), linewidth = 1,
                 position = position_dodge(width = 0.9)) +
  labs(x = "\nParty Alignment (GPS)\n", 
       y = "\nMean Absolute Distance\n", 
       title = "Mean Distance Between Party Alignement from\nGPT-4 and GPS by Party Alignment",
       caption = "Parties labeled as 'Left' have a party alignment of less than 5, those labeled as 'Right' have a party alignment greater than 5\nand 'Center' parties are assigned the value of 5.") +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  clessnverse::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

ggsave("_SharedFolder_article_spsa2024_gpt_party/graphs/paper/h3_barplot.png",
       width = 10, height = 6)

