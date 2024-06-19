library(tidyr)
library(dplyr)
library(ggplot2)

data_party <- readRDS("data/expert_survey/data_party.rds")

# ------------------ Distance graph ---------------------------- #

long_data <- data_party %>%
  filter(Region_name != "NA") %>%
  select(Region_name, econ_distance, sos_distance) %>%
  pivot_longer(
    cols = c(econ_distance, sos_distance),
    names_to = "distance_type",
    values_to = "distance"
  ) %>%
  mutate(distance_type = if_else(distance_type == "econ_distance", "Econ", "Sos"))

# Calculate mean distance for each region and distance type
long_data2 <- long_data %>%
  group_by(Region_name, distance_type) %>%
  summarise(mean_distance = mean(distance, na.rm = TRUE),
            sd = sd(distance, na.rm = TRUE),
            n = n(),
            .groups = "drop") %>% 
  mutate(l_ci = mean_distance - (1.96 * sd / sqrt(n)),
         u_ci = mean_distance + (1.96 * sd / sqrt(n)),
         Region_name = case_when(
    Region_name == "mena" ~ "Middle East\nNorth Africa",
    Region_name == "africa" ~ "Africa",
    Region_name == "eurasia" ~ "Eurasia",
    Region_name == "asia_pacific" ~ "Asia Pacific",
    Region_name == "latam" ~ "Latin America",
    Region_name == "west" ~ "West"
  ),
  distance_type = ifelse(distance_type == "Econ", "Economic", "Social"))


ggplot(long_data2, aes(x = reorder(Region_name, -mean_distance),
                      y = mean_distance, fill = distance_type,
                      color = distance_type)) +
  geom_bar(stat = "identity",
           alpha = 0.7, color = NA,
           position = position_dodge(width = 0.9)) +
  geom_linerange(aes(ymin = l_ci, ymax = u_ci),
                 linewidth = 1,
                position = position_dodge(width = 0.9)) +
  labs(x = "", 
       y = "\nMean Absolute Distance\n") +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  clessnverse::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5, size = 20), # Increased size
        axis.title.y = element_text(hjust = 0.5, size = 20), # Increased size
        axis.text.x = element_text(angle = 45, hjust = 1, size = 20), # Increased size
        axis.text.y = element_text(size = 20)) # Increased size

ggplot(long_data2, aes(x = reorder(Region_name, -mean_distance),
                      y = mean_distance, fill = distance_type,
                      color = distance_type)) +
  geom_bar(stat = "identity",
           alpha = 0.7, color = NA,
           position = position_dodge(width = 0.9)) +
  geom_linerange(aes(ymin = l_ci, ymax = u_ci),
                 linewidth = 1,
                 position = position_dodge(width = 0.9)) +
  labs(x = "", 
       y = "\nMean Absolute Distance\n") +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  clessnverse::theme_clean_light() +
  _SharedFolder_article_spsa2024_gpt_party/graphs/paper/h3_distggridges.png


ggsave("data/graphs/h2_barplot.png",
       width = 8, height = 6)
