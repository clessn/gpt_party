library(dplyr)
library(tidyr)
library(ggplot2)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/data_party.rds")

# Reshape and filter data as before
long_data <- data_party %>%
  select(econ_alignment, econ_distance, sos_alignment, sos_distance) %>%
  pivot_longer(
    cols = c(econ_alignment, sos_alignment),
    names_to = "alignment_type",
    values_to = "alignment"
  ) %>%
  pivot_longer(
    cols = c(econ_distance, sos_distance),
    names_to = "distance_type",
    values_to = "distance"
  ) %>%
  filter(if_else(alignment_type == "econ_alignment", distance_type == "econ_distance", distance_type == "sos_distance")) %>%
  mutate(alignment_type = if_else(alignment_type == "econ_alignment", "Econ", "Sos")) %>% 
  filter(!is.na(alignment))

# Calculate mean, standard error, and confidence intervals
long_data <- long_data %>%
  group_by(alignment_type, alignment) %>%
  summarise(
    mean_distance = mean(distance, na.rm = TRUE),
    se = sd(distance, na.rm = TRUE) / sqrt(n()),
    lower_ci = mean_distance - qt(0.975, df = n() - 1) * se,
    upper_ci = mean_distance + qt(0.975, df = n() - 1) * se,
    .groups = "drop"
  )

# Plot with error bars
ggplot(long_data, aes(x = alignment, y = mean_distance, fill = alignment_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                position = position_dodge(width = 0.8), width = 0.25) +
  labs(x = "Party Alignment", 
       y = "Mean Distance", 
       title = "Mean Distance in Party Alignments (Econ and Sos) with Error Margins") +
  theme_classic() +
  scale_fill_brewer(palette = "Set1")

# 

