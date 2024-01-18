#Jazmin Aguilar

suppressPackageStartupMessages(library(tidyverse))


Q1 <- msleep %>%
  filter(conservation == 'lc', vore == "carni") %>%
  summarise(var = round(var(sleep_total),2)) %>% 
  as.data.frame()
  

 Q2 <- msleep %>%
  filter(order == "Rodentia") %>%
  mutate(ratio = sleep_total/sleep_rem) %>%
  arrange(desc(ratio)) %>%
  head(1) %>%
  select(name)
 
   
  Q3 <- msleep %>%
   filter(order == "Primates") %>%
   transmute(ratio = bodywt/brainwt) %>%
   filter(ratio > 100) %>%
   count()
 
 Q4 <- msleep %>%
   group_by(conservation) %>%
   summarise(mean_sleep = round(mean(sleep_total),2), var_sleep = round(var(sleep_total),2)) %>%
   head(6) %>%
   as.data.frame()
 
 Q5 <- msleep %>%
   filter(vore == "herbi", conservation == "domesticated") %>%
   filter(sleep_total > 12) %>%
   select(name)