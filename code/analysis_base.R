library(dplyr)
library(ggplot2)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/data_party.rds")

# Graphique nuage de points -------------------------------------- #

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


# ----------------- Comparaison des distributions ----------------- #
# Econ

ggplot(data_party, aes(x = V4_Scale)) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  labs(x = "Party alignment (Global Party Survey)", 
       y = "Count", 
       title = "Distribution of party alignment (Global Party Survey)") +
  geom_histogram(aes(x = econ_ideo_gpt_mean), binwidth = 1, fill = "red", color = "black", alpha = 0.5) +
  theme_classic()

# Sos

ggplot(data_party, aes(x = V6_Scale)) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  labs(x = "Party alignment (Global Party Survey)", 
       y = "Count", 
       title = "Distribution of party alignment (Global Party Survey)") +
  geom_histogram(aes(x = sos_ideo_gpt_mean), binwidth = 1, fill = "red", color = "black", alpha = 0.5) +
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

# ---------------------- T Test ------------------------------------------------

# Econ

t.test(data_party$V4_Scale, data_party$econ_ideo_gpt_mean, paired = TRUE)

# Sos

t.test(data_party$V6_Scale, data_party$sos_ideo_gpt_mean, paired = TRUE)

# ---------------------- Correlation ------------------------------------------

# Econ

cor.test(data_party$V4_Scale, data_party$econ_ideo_gpt_mean, method = "pearson")

# Sos

cor.test(data_party$V6_Scale, data_party$sos_ideo_gpt_mean, method = "pearson")

# ---------------------- Regression -------------------------------------------

# econ

m1 <- lm(econ_ideo_gpt_mean ~ V4_Scale, data = data_party)
summary(m1)

# sos

m2 <- lm(sos_ideo_gpt_mean ~ V6_Scale, data = data_party)
summary(m2)

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
