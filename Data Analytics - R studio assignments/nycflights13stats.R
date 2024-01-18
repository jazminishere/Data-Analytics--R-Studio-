#Jazmin Aguilar

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(nycflights13))
suppressPackageStartupMessages(library(lm.beta))

dep_delay_upper <- quantile(flights$dep_delay, 0.9985, na.rm = TRUE)
dep_delay_lower <- quantile(flights$dep_delay, 0.0015, na.rm = TRUE)
dep_delay_out <- which(flights$dep_delay > dep_delay_upper | flights$dep_delay < dep_delay_lower)
dep_delay_noout <- flights[-dep_delay_out, ]
 Q1 <- round((nrow(flights) - length(dep_delay_out))/nrow(flights)*100, 2)

Q2 <- cor.test(dep_delay_noout$dep_delay, dep_delay_noout$distance)

Q3 <- summary(lm(dep_delay ~ distance, dep_delay_noout))


Q4 <- lm.beta(lm(dep_delay ~ distance, dep_delay_noout))

Q5 <- summary(lm(dep_delay ~ distance + carrier, data = dep_delay_noout))