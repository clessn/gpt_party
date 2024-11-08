library(dplyr)
library(tidyr)
library(ggplot2)

df <- readRDS("data/ches/tmp/07_ches_data.rds")

# ---------------------- T Test ------------------------------------------------

# Econ

t.test(df$lrecon, df$lrecon_mean, paired = TRUE)

# Sos

t.test(df$galtan, df$galtan_mean, paired = TRUE)

# ---------------------- Correlation ------------------------------------------

# Econ

cor.test(df$lrecon, df$lrecon_mean, method = "pearson")

# Sos

cor.test(df$galtan, df$galtan_mean, method = "pearson")

# ---------------------- Regression -------------------------------------------

# econ

m1 <- lm(lrecon_mean ~ lrecon, data = df)
summary(m1)

# sos

m2 <- lm(galtan_mean ~ galtan, data = df)
summary(m2)


# Perform the t-tests
lrecon_test <- t.test(df$lrecon, df$lrecon_mean, paired = TRUE)
galtan_test <- t.test(df$galtan, df$galtan_mean, paired = TRUE)

lrecon_t_value <- lrecon_test$statistic
galtan_t_value <- galtan_test$statistic

# Extract Mean Differences and Confidence Intervals
results <- data.frame(
  Category = c("lrecon", "galtan"),
  MeanDifference = c(lrecon_test$estimate, galtan_test$estimate),
  sd = c(lrecon_test$stderr[1], galtan_test$stderr[1]),
  LowerCI95 = c(lrecon_test$conf.int[1], galtan_test$conf.int[1]),
  UpperCI95 = c(lrecon_test$conf.int[2], galtan_test$conf.int[2])
) %>% 
  mutate(LowerCI99 = MeanDifference - (sd * 2.807),
         UpperCI99 = MeanDifference + (sd * 2.807))

plot_h1 <- ggplot(results, aes(x = MeanDifference, y = Category)) +
  geom_point(size = 3) +
  geom_linerange(aes(xmin = LowerCI99, xmax = UpperCI99), linewidth = 0.7) +
  geom_linerange(aes(xmin = LowerCI95, xmax = UpperCI95), linewidth = 2) +
  clessnize::theme_clean_light() +
  labs(x = "\nMean Difference\n",
       y = "\nIdeological Scale\n",
       caption = "Thicker lines denote a 95% confidence interval.\nThinner lines indicate a 99% confidence interval.") +
  scale_x_continuous(limits = c(0, 0.5)) +
  scale_y_discrete(labels = c("galtan" = "Social", "lrecon" = "Economic")) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_text(aes(x = 0.4, y = "lrecon", label = paste("t = ", round(lrecon_t_value, 2), sep = "")),
            hjust = 0, size = 5) +
  geom_text(aes(x = 0.4, y = "galtan", label = paste("t = ", round(galtan_t_value, 2), sep = "")),
            hjust = 0, size = 5) +
  theme(plot.caption.position = "plot",
        axis.title.x = element_text(hjust = 0.5, size = 20),
        axis.title.y = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.caption = element_text(size = 20, hjust = 0))

ggsave("data/graphs/h1_ttest.png",
       width = 8, height = 6)
