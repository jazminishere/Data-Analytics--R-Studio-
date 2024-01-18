
#Jazmin Aguilar
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(openintro))

Q1 <- fastfood %>%
  filter(restaurant == "Burger King" | restaurant == "Chick Fil-A") %>%
  group_by(restaurant) %>%
  mutate(max_cal = max(calories)) %>%
  filter(calories == max_cal) %>%
  select(item)

 Q2 <- fastfood %>%
   filter(restaurant == "Subway") %>%
   group_by(restaurant) %>%
   summarise(mean = round(mean(sugar), 2)) %>%
   pull(mean)
 
 Q3 <-fastfood %>%
   filter(restaurant == "Taco Bell") %>%
   group_by(restaurant) %>%
   summarise(mean = round(mean(calories),2)) %>%
   pull(mean)
 
 Q4 <- fastfood %>%
   mutate(fatXsugar = total_fat*sugar) %>%
   arrange(desc(fatXsugar)) %>%
   select(restaurant, item, fatXsugar) %>%
   head(3)
 
 Q5 <- fastfood %>%
   group_by(restaurant) %>%
   summarise(mean = mean(sat_fat)) %>%
   filter(mean > 10) %>%
   count()