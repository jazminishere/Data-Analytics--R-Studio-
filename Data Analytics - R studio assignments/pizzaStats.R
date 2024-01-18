#Jazmin Aguilar
suppressPackageStartupMessages(library(tidyverse)) 
suppressPackageStartupMessages(library(lm.beta))

pizza <- read_csv('pizza.csv')

pizza1 <- pizza %>%
  select(temperature, bill, pizzas, got_wine)
Q1 <- round(cor(pizza1), 2)

pizza2 <- pizza %>%
  filter(operator == "Laura") %>%
  filter(branch == "East") %>%
  select(time, temperature, bill, pizzas)
Q2 <- round(cor(pizza2), 2)


Q3 <- lm(got_wine ~ temperature + bill + pizzas, pizza)


Q4 <- lm.beta(lm(bill ~ temperature + pizzas + got_wine, pizza))

mod1 <- lm(bill ~ temperature + pizzas + got_wine, pizza)
mod2 <- lm(bill ~ temperature + pizzas + got_wine + operator, pizza)
Q5 <- min(AIC(mod1, k = 2), AIC(mod2, k = 2))