#Jazmin Aguilar
suppressPackageStartupMessages(library(tidyverse))

pizza <- read_csv('pizza.csv')

Q1 <- pizza %>%
  filter(free_wine == 1, discount_customer == 1, pizzas > 4) %>%
  select(driver)
 
Q2 <- pizza %>%
  mutate(ratio = bill/pizzas) %>%
  summarise(mean_ratio = round(mean(ratio),2)) %>%
  as.data.frame()

Q3 <- pizza %>%
  group_by(day) %>%
  summarise(var_pizzas = round(var(pizzas),2)) %>%
  as.data.frame()

Q4 <- pizza %>%
  group_by(operator) %>%
  summarise(mean_bill = mean(bill)) %>%
  arrange(desc(mean_bill)) %>%
  select(operator) %>%
  head(1)


Q5 <- pizza %>%
  select(day, driver, free_wine) %>%
  group_by(day, driver) %>%
  summarise(n = as.integer(sum(free_wine))) %>%
  arrange(desc(n)) %>%
  head(1)
 