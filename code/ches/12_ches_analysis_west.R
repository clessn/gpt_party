library(tidyr)
library(dplyr)
library(ggplot2)

df <- readRDS("data/ches/tmp/07_ches_data.rds")

# ------------------ Distance graph ---------------------------- #

long_data <- df %>%
  filter(Region_name != "NA") %>%
  select(Region_name, econ_distance, sos_distance) %>%
  pivot_longer(
    cols = c(econ_distance, sos_distance),
    names_to = "distance_type",
    values_to = "distance"
  ) %>%
  mutate(distance_type = if_else(distance_type == "econ_distance", "Econ", "Sos"))

df_long <- df %>%
  select(region, lrecon_distance, galtan_distance) %>%
  pivot_longer(
    cols = c(lrecon_distance, galtan_distance),
    names_to = "distance_type",
    values_to = "distance"
  ) %>%
  mutate(distance_type = if_else(distance_type == "lrecon_distance", "Economic", "Social"))

df_long_2 <- df_long %>%
  group_by(region, distance_type) %>%
  summarise(mean_distance = mean(distance, na.rm = TRUE),
            sd = sd(distance, na.rm = TRUE),
            n = n(),
            .groups = "drop") %>% 
  mutate(l_ci = mean_distance - (1.96 * sd / sqrt(n)),
         u_ci = mean_distance + (1.96 * sd / sqrt(n)),
         region = case_when(
    region == "middle_east" ~ "Middle East",
    region == "latin_america" ~ "Latin America",
    region == "western_europe" ~ "Western Europe",
    region == "eastern_europe" ~ "Eastern Europe"
  ),
  distance_type = ifelse(distance_type == "Economic", "Economic", "Social"))


ggplot(df_long_2, aes(x = reorder(region, -mean_distance),
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
  clessnize::theme_clean_light() +
  theme(axis.title.x = element_text(hjust = 0.5, size = 20), # Increased size
        axis.title.y = element_text(hjust = 0.5, size = 20), # Increased size
        axis.text.x = element_text(angle = 45, hjust = 1, size = 20), # Increased size
        axis.text.y = element_text(size = 20)) # Increased size

ggsave("data/graphs/h2_barplot.png",
       width = 8, height = 6)
