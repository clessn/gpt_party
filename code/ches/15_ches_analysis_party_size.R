library(dplyr)
library(tidyr)
library(ggplot2)

df <- read.csv("_SharedFolder_article_spsa2024_gpt_party/data/input_party_size.csv")

df_size <- df %>%
  group_by(country) %>%
  filter(!is.na(vote)) %>%
  mutate(size = case_when(
    vote <= quantile(vote, 1 / 3, na.rm = TRUE) ~ "Small",
    vote <= quantile(vote, 2 / 3, na.rm = TRUE) ~ "Medium",
    TRUE ~ "Large"
  )) %>%
  ungroup()

long_data <- df_size %>%
  select(size, galtan_distance, lrecon_distance) %>%
  pivot_longer(
    cols = c(lrecon_distance, galtan_distance),
    names_to = "distance_type",
    values_to = "distance"
  ) %>%
  mutate(distance_type = if_else(distance_type == "lrecon_distance", "econ", "social"))

# Calculate mean, standard error, and confidence interval
long_data2 <- long_data %>%
  group_by(size, distance_type) %>%
  summarise(
    mean_distance = mean(distance, na.rm = TRUE),
    se = sd(distance, na.rm = TRUE) / sqrt(n()),
    lower_ci = mean_distance - qt(0.975, df = n() - 1) * se,
    upper_ci = mean_distance + qt(0.975, df = n() - 1) * se,
    .groups = "drop"
  ) %>%
  mutate(
    partysize = factor(size, levels = c("Small", "Medium", "Large")),
    distance_type = ifelse(distance_type == "econ", "Economic", "Social")
  )

# Plot with error bars
ggplot(long_data2, aes(x = partysize, y = mean_distance, fill = distance_type)) +
  geom_bar(
    stat = "identity", position = position_dodge(width = 0.9),
    alpha = 0.7
  ) +
  geom_linerange(
    aes(
      ymin = lower_ci,
      ymax = upper_ci,
      color = distance_type
    ),
    linewidth = 1,
    position = position_dodge(width = 0.9)
  ) +
  labs(
    x = "\nParty Size\n",
    y = "\nMean Distance\n"
  ) +
  clessnize::theme_clean_light() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(
    axis.title.x = element_text(hjust = 0.5, size = 20), # Bolder and larger axis title X
    axis.title.y = element_text(hjust = 0.5, size = 20), # Bolder and larger axis title Y
    axis.text.x = element_text(size = 20), # Bolder and larger axis text X
    axis.text.y = element_text(size = 20),
    plot.caption = element_text(size = 20), # Increased size
    legend.text = element_text(size = 20), # Increase legend text size
    legend.key.size = unit(1.5, "lines")
  ) # Bolder and larger plot caption

ggsave("data/graphs/h4_barplot.png",
  width = 8, height = 6
)
