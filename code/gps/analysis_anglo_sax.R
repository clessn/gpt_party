library(tidyr)
library(dplyr)
library(ggplot2)

data_party <- readRDS("data/gps/data_party.rds")

data_econ <- data_party %>%
  group_by(group) %>%
  summarise(
    mean_distance = mean(econ_distance, na.rm = TRUE),
    se = sd(econ_distance, na.rm = TRUE) / sqrt(n()),
    lower_ci = mean_distance - qt(0.975, df = n() - 1) * se,
    upper_ci = mean_distance + qt(0.975, df = n() - 1) * se,
    distance_type = "Econ",
    .groups = "drop"
  )

# Calculate mean, standard error, and confidence intervals for sos_distance
data_sos <- data_party %>%
  group_by(group) %>%
  summarise(
    mean_distance = mean(sos_distance, na.rm = TRUE),
    se = sd(sos_distance, na.rm = TRUE) / sqrt(n()),
    lower_ci = mean_distance - qt(0.975, df = n() - 1) * se,
    upper_ci = mean_distance + qt(0.975, df = n() - 1) * se,
    distance_type = "Sos",
    .groups = "drop"
  )

# Combine the datasets
long_data <- bind_rows(data_econ, data_sos)

# Sort the groups by their mean distances
long_data <- long_data %>%
  arrange(desc(mean_distance))

# Plot with error bars
ggplot(long_data, aes(x = reorder(group, -mean_distance), y = mean_distance, fill = distance_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.8), width = 0.25
  ) +
  labs(
    x = "Group",
    y = "Mean Distance",
    title = "Mean Distance in Group Alignments (Econ and Sos) - Sorted with Error Margins"
  ) +
  theme_classic() +
  scale_fill_brewer(palette = "Dark2")
