library(dplyr)
library(tidyr)
library(ggplot2)

data_party <- readRDS("data/gps/data_party.rds")

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


# Perform the t-tests
econ_test <- t.test(data_party$V4_Scale, data_party$econ_ideo_gpt_mean, paired = TRUE)
sos_test <- t.test(data_party$V6_Scale, data_party$sos_ideo_gpt_mean, paired = TRUE)

econ_t_value <- econ_test$statistic
sos_t_value <- sos_test$statistic

# Extract Mean Differences and Confidence Intervals
results <- data.frame(
  Category = c("Econ", "Sos"),
  MeanDifference = c(econ_test$estimate, sos_test$estimate),
  sd = c(econ_test$stderr[1], sos_test$stderr[1]),
  LowerCI95 = c(econ_test$conf.int[1], sos_test$conf.int[1]),
  UpperCI95 = c(econ_test$conf.int[2], sos_test$conf.int[2])
) %>% 
  mutate(LowerCI99 = MeanDifference - (sd * 2.807),
         UpperCI99 = MeanDifference + (sd * 2.807))

ggplot(results, aes(x = MeanDifference, y = Category)) +
  geom_point(size = 3) +
  geom_linerange(aes(xmin = LowerCI99, xmax = UpperCI99), linewidth = 0.7) +
  geom_linerange(aes(xmin = LowerCI95, xmax = UpperCI95), linewidth = 2) +
  clessnize::theme_clean_light() +
  labs(x = "\nMean Difference\n",
       y = "\nIdeological Scale\n",
       caption = "Thicker lines denote a 95% confidence interval.\nThinner lines indicate a 99% confidence interval.") +
  scale_x_continuous(limits = c(-0.75, 0)) +
  scale_y_discrete(labels = c("Sos" = "Social", "Econ" = "Economic")) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_text(aes(x = -0.34, y = "Econ", label = paste("t = ", round(econ_t_value, 2), sep = "")),
            hjust = 0, size = 5) +
  geom_text(aes(x = -0.1, y = "Sos", label = paste("t = ", round(sos_t_value, 2), sep = "")),
            hjust = 0, size = 5) +
  theme(plot.caption.position = "plot",
        axis.title.x = element_text(hjust = 0.5, size = 20),
        axis.title.y = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.caption = element_text(size = 20, hjust = 0))

ggsave("data/graphs/h1_ttest.png",
       width = 8, height = 6)

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

g2 <- ggplot(h1, aes(x = gps, y = gpt)) +
  facet_wrap(~scale) +
  geom_jitter(alpha = 0.6, shape = 19,
              width = 0.2, height = 0.2) +
  geom_smooth(method = "lm",
              color = "black",
              alpha = 0.2) +
  clessnize::theme_clean_light() +
  scale_x_continuous(breaks = c(1, 9), labels = c("Left", "Right")) +
  scale_y_continuous(breaks = c(1, 9), labels = c("Left", "Right")) +
  labs(x = "\nParty Alignment (GPS)\n",
       y = "\nParty Alignment (GPT-4)\n") +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        axis.text.y = element_text(angle = 90))



cat_econ_test <- t.test(data_party$econ_ideo_cat, data_party$econ_ideo_cat_gpt, paired = TRUE)
cat_sos_test <- t.test(data_party$sos_ideo_cat, data_party$sos_ideo_cat_gpt, paired = TRUE)

econ_t_value <- cat_econ_test$statistic
sos_t_value <- cat_sos_test$statistic

# Extract Mean Differences and Confidence Intervals
results <- data.frame(
  Category = c("Econ", "Sos"),
  MeanDifference = c(econ_test$estimate, sos_test$estimate),
  sd = c(econ_test$stderr[1], sos_test$stderr[1]),
  LowerCI95 = c(econ_test$conf.int[1], sos_test$conf.int[1]),
  UpperCI95 = c(econ_test$conf.int[2], sos_test$conf.int[2])
) %>% 
  mutate(LowerCI99 = MeanDifference - (sd * 2.807),
         UpperCI99 = MeanDifference + (sd * 2.807))

ggplot(results, aes(x = MeanDifference, y = Category)) +
  geom_point(size = 3) +
  geom_linerange(aes(xmin = LowerCI99, xmax = UpperCI99), linewidth = 0.7) +
  geom_linerange(aes(xmin = LowerCI95, xmax = UpperCI95), linewidth = 2) +
  clessnize::theme_clean_light() +
  labs(x = "\nMean Difference\n",
       y = "\nIdeological Scale\n",
       caption = "Thicker lines denote a 95% confidence interval.\nThinner lines indicate a 99% confidence interval.") +
  scale_x_continuous(limits = c(-0.75, 0)) +
  scale_y_discrete(labels = c("Sos" = "Social", "Econ" = "Economic")) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_text(aes(x = -0.34, y = "Econ", label = paste("t = ", round(econ_t_value, 2), sep = "")),
            hjust = 0, size = 5) +
  geom_text(aes(x = -0.1, y = "Sos", label = paste("t = ", round(sos_t_value, 2), sep = "")),
            hjust = 0, size = 5) +
  theme(plot.caption.position = "plot",
        axis.title.x = element_text(hjust = 0.5, size = 20),
        axis.title.y = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.caption = element_text(size = 20, hjust = 0))

ggsave("data/graphs/h1_ttest.png",
       width = 8, height = 6)

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

g2 <- ggplot(h1, aes(x = gps, y = gpt)) +
  facet_wrap(~scale) +
  geom_jitter(alpha = 0.6, shape = 19,
              width = 0.2, height = 0.2) +
  geom_smooth(method = "lm",
              color = "black",
              alpha = 0.2) +
  clessnize::theme_clean_light() +
  scale_x_continuous(breaks = c(1, 9), labels = c("Left", "Right")) +
  scale_y_continuous(breaks = c(1, 9), labels = c("Left", "Right")) +
  labs(x = "\nParty Alignment (GPS)\n",
       y = "\nParty Alignment (GPT-4)\n") +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        axis.text.y = element_text(angle = 90))
