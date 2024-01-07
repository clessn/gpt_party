library(tidyr)
library(dplyr)
library(ggplot2)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/data_party.rds")

long_data <- data_party %>%
  group_by(group) %>%
  summarise(
    mean_econ_distance = mean(econ_distance, na.rm = TRUE),
    mean_sos_distance = mean(sos_distance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("mean_"),
    names_to = "distance_type",
    values_to = "mean_distance"
  ) %>%
  mutate(distance_type = sub("mean_", "", distance_type))

# Sort the groups by their mean distances
# Assuming we want to sort by the mean of combined econ and sos distances
long_data <- long_data %>%
  arrange(desc(mean_distance))

# Plot
ggplot(long_data, aes(x = reorder(group, -mean_distance), y = mean_distance, fill = distance_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(x = "Group", 
       y = "Mean Distance", 
       title = "Mean Distance in Group Alignments (Econ and Sos) - Sorted") +
  theme_classic() +
  scale_fill_brewer(palette = "Set1")
