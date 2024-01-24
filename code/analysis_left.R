library(dplyr)
library(tidyr)
library(ggplot2)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/data_party.rds")

## h3 for paper

h3 <- data_party %>% 
  select(ID_GPS,
         scale_econ = V4_Scale,
         scale_social = V6_Scale,
         gpt_econ = econ_ideo_gpt_mean,
         gpt_social = sos_ideo_gpt_mean) %>% 
  pivot_longer(., cols = starts_with("scale"),
               names_to = "scale",
               names_prefix = "scale_",
               values_to = "gps") %>% 
  pivot_longer(., cols = starts_with("gpt"),
               names_to = "gptscale",
               names_prefix = "gpt_",
               values_to = "gpt") %>% 
  filter(scale == gptscale) %>% 
  mutate(scale = ifelse(scale == "econ", "Economic", "Social")) %>% 
  pivot_longer(., cols = c("gpt", "gps"),
               names_to = "type",
               values_to = "position") %>% 
  mutate(type = factor(type, levels = c("gpt", "gps")))

ggplot(h3, aes(x = position, y = type)) +
  ggridges::geom_density_ridges(aes(fill = type),
                                scale = 2,
                                alpha = 0.6, color = "white",
                                quantile_lines = TRUE,
                                bandwidth = 0.3,
                                show.legend = FALSE) +
  facet_wrap(~scale) +
  xlab("Party Alignment") + ylab("") +
  scale_y_discrete(labels = c("gps" = "GPS",
                              "gpt" = "GPT-4")) +
  scale_x_continuous(breaks = c(1, 9),
                     labels = c("Left", "Right")) +
  scale_fill_brewer(palette = "Set2") +
  clessnverse::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5, vjust = 15, size = 20), # Bold and increased size
        axis.title.y = element_text(hjust = 0.5, size = 20), # Bold and increased size
        axis.text.x = element_text(vjust = 25, size = 20), # Bold and increased size
        axis.text.y = element_text(size = 20), # Bold and increased size
        strip.text = element_text(size = 20)) # Bold and increased size for facet labels

ggsave("_SharedFolder_article_spsa2024_gpt_party/graphs/paper/h3_distggridges.png",
       width = 10, height = 6)

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
long_data2 <- long_data %>%
  group_by(alignment_type, alignment) %>%
  summarise(
    mean_distance = mean(distance, na.rm = TRUE),
    se = sd(distance, na.rm = TRUE) / sqrt(n()),
    lower_ci = mean_distance - 1.96 * se,
      upper_ci = mean_distance + 1.96 * se,
    .groups = "drop"
  ) %>% 
  mutate(alignment = case_when(
    alignment == "left" ~ "Left",
    alignment == "center" ~ "Center",
    alignment == "right" ~ "Right"
  ),
  alignment = factor(alignment, levels = c("Left", "Center", "Right")),
  alignment_type = ifelse(alignment_type == "Econ", "Economic", "Social"))

ggplot(long_data2, aes(x = alignment, y = mean_distance, fill = alignment_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9),
           alpha = 0.7) +
  geom_linerange(aes(ymin = lower_ci,
                     ymax = upper_ci,
                     color = alignment_type), linewidth = 1,
                position = position_dodge(width = 0.9)) +
  labs(x = "\nParty Alignment (GPS)\n", 
       y = "\nMean Absolute Distance\n", 
       caption = "Parties labeled as 'Left' have a GPS party alignment of less than 5.\nParties labeled as 'Right' have a party alignment greater than 5.\n'Center' parties are assigned the value of 5.") +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  clessnverse::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5, size = 20), # Bolder and larger axis title X
        axis.title.y = element_text(hjust = 0.5, size = 20), # Bolder and larger axis title Y
        axis.text.x = element_text(size = 15), # Bolder and larger axis text X
        axis.text.y = element_text(size = 15),
        plot.caption = element_text(size = 20, hjust = 0), # Increased size
        legend.text = element_text(size = 20), # Increase legend text size
        legend.key.size = unit(1.5, "lines"),  # Bolder and larger plot caption
        plot.caption.position = "plot") # Positioning the caption relative to the plot

ggsave("_SharedFolder_article_spsa2024_gpt_party/graphs/paper/h3_barplot.png",
       width = 10, height = 6)


