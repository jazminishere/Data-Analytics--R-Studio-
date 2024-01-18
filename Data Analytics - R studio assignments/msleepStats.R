suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lm.beta))

Q1 <- cor.test(msleep$sleep_total, msleep$bodywt)

msleepcor <- msleep %>%
  select(sleep_total, sleep_rem, brainwt, bodywt)

Q2 <- round(cor(msleepcor, use = "complete.obs"), 2)

Q3 <- lm(bodywt ~ vore, msleep)

Q4mod <- lm(bodywt ~ vore + sleep_rem, msleep)
QQ3 <-  AIC(Q3)
 QQ4 <- AIC(Q4mod)
 Q4 <- QQ4

filter1 <- msleep %>%
  filter(vore != "omni" & vore != "insecti")
mutatevore <- filter1 %>% mutate(vorebin = ifelse(vore == 'carni', 0, 1))
Q5 <- glm(vorebin ~ sleep_total, data = mutatevore, family = binomial)
