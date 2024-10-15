library(dplyr)
library(tidyr)
library(ggridges)
library(ggplot2)

data_party <- readRDS("data/expert_survey/data_party.rds")

# Graphique nuage de points -------------------------------------- #

# Econ

ggplot(data_party, aes(x = V4_Scale, y = econ_ideo_gpt_mean)) +
  geom_point(aes(size = 2, alpha = 0.5)) +
  labs(x = "Party alignment (Global Party Survey)", 
       y = "Party alignment (GPT-4)", 
       title = "Party alignment (GPT-4) vs. party alignment (Global Party Survey)") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dotted") +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  theme_classic()

ggplot(h1, aes(x = gps, y = gpt)) +
  facet_wrap(~scale) +
  geom_jitter(alpha = 0.6, shape = 19,
              width = 0.2, height = 0.2) +
  geom_smooth(method = "lm",
              color = "black",
              alpha = 0.2) +
  clessnverse::theme_clean_light() +
  scale_x_continuous(breaks = c(1, 9), labels = c("Left", "Right")) +
  scale_y_continuous(breaks = c(1, 9), labels = c("Left", "Right")) +
  labs(x = "\nParty Alignment (GPS)\n",
       y = "\nParty Alignment (GPT-4)\n") +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        axis.text.y = element_text(angle = 90))

# Sos

ggplot(data_party, aes(x = V6_Scale, y = sos_ideo_gpt_mean)) +
  geom_point(aes(size = 2, alpha = 0.5)) +
  labs(x = "Party alignment (Global Party Survey)", 
       y = "Party alignment (GPT-4)", 
       title = "Party alignment (GPT-4) vs. party alignment (Global Party Survey)") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dotted") +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  theme_classic()

# Graphique econ ---------------------------------------------------------------

data_graph_econ <- data_party %>%
  group_by(V4_Scale, econ_ideo_gpt_mean) %>%
  summarise(n = n(), .groups = "drop") 

ggplot(data_graph_econ, aes(x = V4_Scale, y = econ_ideo_gpt_mean)) +
  geom_point(aes(size = n, alpha = n)) +
  labs(x = "Party alignment (Global Party Survey)", 
       y = "Party alignment (GPT-4)", 
       title = "Party alignment (GPT-4) vs. party alignment (Global Party Survey)") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dotted") +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

# Graphique sos ----------------------------------------------------------------

data_graph_sos <- data_party %>%
  group_by(V6_Scale, sos_ideo_gpt_mean) %>%
  summarise(n = n(), .groups = "drop") 

ggplot(data_graph_sos, aes(x = V6_Scale, y = sos_ideo_gpt_mean)) +
  geom_point(aes(size = n, alpha = n)) +
  labs(x = "Party alignment (Global Party Survey)", 
       y = "Party alignment (GPT-4)", 
       title = "Party alignment (GPT-4) vs. party alignment (Global Party Survey)") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dotted") +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

# Graphique ggtiles rounded -------------------------------------- #

# Rounded econ

data_graph_rounded <- data_party %>%
  mutate(V4_Scale = round(as.numeric(V4_Scale))) %>%
  mutate(econ_ideo_gpt_mean = round(as.numeric(econ_ideo_gpt_mean))) %>%
  group_by(V4_Scale, econ_ideo_gpt_mean) %>%
  summarise(n = n(), .groups = "drop") 

ggplot(data_graph_rounded, aes(x = V4_Scale, y = econ_ideo_gpt_mean)) +
  geom_tile(aes(alpha = n)) +
  labs(x = "Party alignment (Global Party Survey)", 
       y = "Party alignment (GPT-4)", 
       title = "Party alignment (GPT-4) vs. party alignment (Global Party Survey)") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dotted") +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

# Rounded sos

data_graph_rounded_sos <- data_party %>%
  mutate(V6_Scale = round(as.numeric(V6_Scale))) %>%
  mutate(sos_ideo_gpt_mean = round(as.numeric(sos_ideo_gpt_mean))) %>%
  group_by(V6_Scale, sos_ideo_gpt_mean) %>%
  summarise(n = n(), .groups = "drop") 

ggplot(data_graph_rounded_sos, aes(x = V6_Scale, y = sos_ideo_gpt_mean)) +
  geom_tile(aes(alpha = n)) +
  labs(x = "Party alignment (Global Party Survey)", 
       y = "Party alignment (GPT-4)", 
       title = "Party alignment (GPT-4) vs. party alignment (Global Party Survey)") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dotted") +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

# -------------------- By country --------------------------------- #

# Econ

data_distance_countries_econ <- data_party %>%
  group_by(ISO, Region_name) %>%
  summarise(mean_distance = mean(econ_distance, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_distance)  # Sort in ascending order
  
data_distance_countries_econ$ISO <- factor(data_distance_countries_econ$ISO, levels = data_distance_countries_econ$ISO)

ggplot(data_distance_countries_econ, aes(x = ISO, y = mean_distance, fill = Region_name)) +
    geom_bar(stat = "identity") +
    labs(x = "Countries ISO Code", 
         y = "Mean Distance", 
         title = "Mean distance between party alignment (GPT-4) and party alignment (Global Party Survey)") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Sos

data_distance_countries_sos <- data_party %>%
  group_by(ISO, Region_name) %>%
  summarise(mean_distance = mean(sos_distance, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_distance)  # Sort in ascending order
  
data_distance_countries_sos$ISO <- factor(data_distance_countries_sos$ISO, levels = data_distance_countries_sos$ISO)

ggplot(data_distance_countries_sos, aes(x = ISO, y = mean_distance, fill = Region_name)) +
    geom_bar(stat = "identity") +
    labs(x = "Countries ISO Code", 
         y = "Mean Distance", 
         title = "Mean distance between party alignment (GPT-4) and party alignment (Global Party Survey)") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---------------------- plot --------------------------------------------------

ggplot(data_party, aes(x = V4_Scale, y = econ_ideo_gpt_mean)) +
  geom_point(aes(size = 2, alpha = 0.5)) +
  labs(x = "Party alignment (Global Party Survey)", 
       y = "Party alignment (GPT-4)", 
       title = "Party alignment (GPT-4) vs. party alignment (Global Party Survey)") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dotted") +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  theme_classic() +
  facet_wrap(~ Region_name, nrow = 2)


# For paper ---------------------------------------------------------------

## T-test -------------------------------------------------------------------

ttest_ecn <- t.test(data_party$V6)

h1 <- data_party %>% 
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
  mutate(scale = ifelse(scale == "econ", "Economic", "Social"))

ggplot(h1, aes(x = gps, y = gpt)) +
  facet_wrap(~scale) +
  geom_jitter(alpha = 0.6, shape = 19,
              width = 0.2, height = 0.2) +
  geom_smooth(method = "lm",
              color = "black",
              alpha = 0.2) +
  clessnverse::theme_clean_light() +
  scale_x_continuous(breaks = c(1, 9), labels = c("Left", "Right")) +
  scale_y_continuous(breaks = c(1, 9), labels = c("Left", "Right")) +
  labs(x = "\nParty Alignment (GPS)\n",
       y = "\nParty Alignment (GPT-4)\n") +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        axis.text.y = element_text(angle = 90))

ggsave("data/graphs/h1_scatterplot.png",
       width = 8, height = 6)

