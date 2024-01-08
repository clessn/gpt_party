library(tidyr)
library(dplyr)
library(ggplot2)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/data_party.rds")

# ------------------ Distance graph ---------------------------- #

long_data <- data_party %>%
  filter(Region_name != "NA") %>%
  select(Region_name, econ_distance, sos_distance) %>%
  pivot_longer(
    cols = c(econ_distance, sos_distance),
    names_to = "distance_type",
    values_to = "distance"
  ) %>%
  mutate(distance_type = if_else(distance_type == "econ_distance", "Econ", "Sos")) %>%
  group_by(Region_name, distance_type) %>%
  summarise(
    mean_distance = mean(distance, na.rm = TRUE),
    se = sd(distance, na.rm = TRUE) / sqrt(n()),
    lower_ci = mean_distance - qt(0.975, df = n() - 1) * se,
    upper_ci = mean_distance + qt(0.975, df = n() - 1) * se,
    .groups = "drop"
  )

# Plot with error bars
ggplot(long_data, aes(x = reorder(Region_name, -mean_distance), y = mean_distance, fill = distance_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                position = position_dodge(width = 0.8), width = 0.25) +
  labs(x = "Region", 
       y = "Mean Distance", 
       title = "Mean Distance by Region (Econ and Sos) - Sorted with Confidence Intervals") +
  theme_classic() +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
