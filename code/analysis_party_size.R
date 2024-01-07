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
long_data <- long_data %>%
  group_by(Type_Partysize_seat, distance_type) %>%
  summarise(
    mean_distance = mean(distance, na.rm = TRUE),
    se = sd(distance, na.rm = TRUE) / sqrt(n()),
    lower_ci = mean_distance - qt(0.975, df = n() - 1) * se,
    upper_ci = mean_distance + qt(0.975, df = n() - 1) * se,
    .groups = "drop"
  )

# Plot with error bars
ggplot(long_data, aes(x = Type_Partysize_seat, y = mean_distance, fill = distance_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                position = position_dodge(width = 0.8), width = 0.25) +
  labs(x = "Party Size", 
       y = "Mean Distance", 
       title = "Mean Distance by Party Size (Econ and Sos) with Error Margins") +
  theme_classic() +
  scale_fill_brewer(palette = "Set1")
  
