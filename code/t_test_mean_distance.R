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
  LowerCI = c(econ_test$conf.int[1], sos_test$conf.int[1]),
  UpperCI = c(econ_test$conf.int[2], sos_test$conf.int[2])
)

ggplot(results, aes(x = Category, y = MeanDifference)) +
  geom_point() +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2) +
  theme_minimal() +
  labs(title = "Mean Differences from Paired T-Tests",
       x = "Category",
       y = "Mean Difference")


