library(dplyr)
library(tidyr)
library(ggplot2)

df <- readRDS("data/ches/tmp/07_ches_data.rds")

## h3 for paper

h3 <- df %>% 
  select(party,
         scale_econ = lrecon,
         scale_social = galtan,
         gpt_econ = lrecon_mean,
         gpt_social = galtan_mean) %>% 
  pivot_longer(., cols = starts_with("scale"),
               names_to = "scale",
               names_prefix = "scale_",
               values_to = "ches") %>% 
  pivot_longer(., cols = starts_with("gpt"),
               names_to = "gptscale",
               names_prefix = "gpt_",
               values_to = "gpt") %>% 
  filter(scale == gptscale) %>% 
  mutate(scale = ifelse(scale == "econ", "Economic", "Social")) %>% 
  pivot_longer(., cols = c("gpt", "ches"),
               names_to = "type",
               values_to = "position") %>% 
  mutate(type = factor(type, levels = c("gpt", "ches")))

ggplot(h3, aes(x = position, y = type)) +
  ggridges::geom_density_ridges(aes(fill = type),
                                scale = 2,
                                alpha = 0.6, color = "white",
                                quantile_lines = TRUE,
                                bandwidth = 0.3,
                                show.legend = FALSE) +
  facet_wrap(~scale) +
  xlab("Party Alignment") + ylab("") +
  scale_y_discrete(labels = c("ches" = "CHES",
                              "gpt" = "GPT-4o")) +
  scale_x_continuous(breaks = c(1, 9),
                     labels = c("Left", "Right")) +
  scale_fill_brewer(palette = "Set2") +
  clessnize::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5, vjust = 15, size = 20), # Bold and increased size
        axis.title.y = element_text(hjust = 0.5, size = 20), # Bold and increased size
        axis.text.x = element_text(vjust = 25, size = 20), # Bold and increased size
        axis.text.y = element_text(size = 20), # Bold and increased size
        strip.text = element_text(size = 20)) # Bold and increased size for facet labels

ggsave("data/graphs/h3_distggridges.png",
       width = 10, height = 6)

# Reshape and filter data as before
long_data <- df %>%
  select(lrecon_category, lrecon_distance, galtan_category, galtan_distance) %>%
  pivot_longer(
    cols = c(lrecon_category, galtan_category),
    names_to = "category_type",
    values_to = "category"
  ) %>%
  pivot_longer(
    cols = c(lrecon_distance, galtan_distance),
    names_to = "distance_type", 
    values_to = "distance"
  ) %>%
  filter(if_else(category_type == "lrecon_category", distance_type == "lrecon_distance", distance_type == "galtan_distance")) %>%
  mutate(category_type = if_else(category_type == "lrecon_category", "Econ", "Social")) %>% 
  filter(!is.na(category))

# Calculate mean, standard error, and confidence intervals
long_data2 <- long_data %>%
  group_by(category_type, category) %>%
  summarise(
    mean_distance = mean(distance, na.rm = TRUE),
    se = sd(distance, na.rm = TRUE) / sqrt(n()),
    n = n(),
    lower_ci = mean_distance - 1.96 * se,
    upper_ci = mean_distance + 1.96 * se,
    .groups = "drop"
  ) %>% 
  mutate(category = case_when(
    category == "extreme_left" ~ "Extreme Left",
    category == "left" ~ "Left",
    category == "center" ~ "Center",
    category == "right" ~ "Right",
    category == "extreme_right" ~ "Extreme Right"
  ),
  alignment = factor(category, levels = c("Extreme Left", "Left", "Center", "Right", "Extreme Right")),
  alignment_type = ifelse(category_type == "Econ", "Economic", "Social"))

# Create the plot with sample size annotations
ggplot(long_data2, aes(x = alignment, y = mean_distance, fill = alignment_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9),
           alpha = 0.7) +
  geom_linerange(aes(ymin = lower_ci,
                     ymax = upper_ci,
                     color = alignment_type), linewidth = 1,
                position = position_dodge(width = 0.9)) +
  # Add text annotations for sample sizes
  geom_text(aes(y = -0.1, # Adjust this value to position the text
                label = paste0("n=", n)),
            position = position_dodge(width = 0.9),
            vjust = 1, size = 5) +
  # Expand the y-axis limits to make room for the annotations
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.05))) +
  labs(x = "\nParty Alignment (CHES)\n", 
       y = "\nMean Absolute Distance\n", 
       caption = "Parties labeled as 'Extreme left' have a CHES party alignment of less than 2.\nParties labeled as 'Left' have a party alignment between 2 inclusively and 4.\nParties labeled as 'Center' have a party alignment between 4 inclusively and 6 inclusively.\nParties labeled as 'Right' have a party alignment between 6 and 8 inclusively.\nParties labeled as 'Extreme right' have a CHES party alignment of more than 8.") +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  clessnize::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5, size = 16),
        axis.title.y = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.caption = element_text(size = 12, hjust = 0),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, "lines"),
        plot.caption.position = "plot")

ggsave("data/graphs/h3_barplot.png",
       width = 10, height = 6)


