library(dplyr)
library(tidyr)
library(ggridges)
library(ggplot2)

df <- readRDS("data/ches/tmp/07_ches_data.rds")

h1 <- df %>% 
  select(country,
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
  mutate(scale = ifelse(scale == "econ", "Economic", "Social"))

ggplot(h1, aes(x = ches, y = gpt)) +
  facet_wrap(~scale) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dotted") +
  geom_jitter(alpha = 0.6, shape = 19,
              width = 0.2, height = 0.2) +
  geom_smooth(method = "lm",
              color = "black",
              alpha = 0.2) +
  clessnize::theme_clean_light() +
  scale_x_continuous(breaks = c(1, 9), labels = c("Left", "Right")) +
  scale_y_continuous(breaks = c(1, 9), labels = c("Left", "Right")) +
  labs(x = "\nParty Alignment (CHES)\n",
       y = "\nParty Alignment (GPT-4o)\n") +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        axis.text.y = element_text(angle = 90))

ggsave("data/graphs/h1_scatterplot.png",
       width = 8, height = 6)

