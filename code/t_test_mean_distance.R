library(dplyr)
library(ggplot2)

data_party <- readRDS("_SharedFolder_article_spsa2024_gpt_party/data/expert_survey/data_party.rds")

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
  clessnverse::theme_clean_light() +
  labs(title = "Mean Differences from Paired T-Tests",
       x = "\nMean Difference\n",
       y = "\nIdeological Scale\n",
       caption = "Thicker lines denote a 95% confidence interval while thinner lines indicate a 99% confidence interval.") +
  scale_x_continuous(limits = c(-0.75, 0)) +
  scale_y_discrete(labels = c("Sos" = "Social", "Econ" = "Economic")) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

ggsave("_SharedFolder_article_spsa2024_gpt_party/graphs/paper/h1_ttest.png",
       width = 8, height = 6)

results <- data.frame(
  Category = c("Econ", "Sos"),
  MeanDifference = c(econ_test$estimate, sos_test$estimate),
  LowerCI = c(econ_test$conf.int[1], sos_test$conf.int[1]),
  UpperCI = c(econ_test$conf.int[2], sos_test$conf.int[2]),
  Tvalue = c(econ_test$statistic, sos_test$statistic),
  Pvalue = c(econ_test$p.value, sos_test$p.value)
)

# Create the ggplot
ggplot(results, aes(x = Category, y = MeanDifference)) +
  geom_point() +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2) +
  geom_text(aes(label = sprintf("t = %.2f\np = %.2e", Tvalue, Pvalue)), 
            vjust = -1.5, size = 3) +
  theme_minimal() +
  labs(title = "Mean Differences from Paired T-Tests",
       x = "Category",
       y = "Mean Difference") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(results, aes(y = Category, x = MeanDifference)) +
  geom_point() +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Vertical line at zero
  geom_text(aes(label = sprintf("t = %.2f\np = %.2e", Tvalue, Pvalue)), 
            hjust = -0.1, size = 3) +
  theme_minimal() +
  labs(title = "Mean Differences from Paired T-Tests",
       y = "Category",
       x = "Mean Difference") +
  theme(plot.title = element_text(hjust = 0.5))
