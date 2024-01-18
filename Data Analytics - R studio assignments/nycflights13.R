

#Jazmin Aguilar

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(nycflights13))

Q1 <- flights %>%
  select(carrier, distance) %>%
  group_by(carrier) %>%
  summarise(mean = round(mean(distance),2)) %>%
  filter(carrier == "AA" | carrier == "EV" | carrier == "FL") %>%
  as.data.frame()


Q2 <- flights %>%
  group_by(month) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(1)

Q3 <- flights %>%
  group_by(origin, dest) %>%
  summarise(min_dist = min(distance)) %>%
  arrange(min_dist) %>%
  head(5)

Q4 <- flights %>%
  filter(origin == "JFK") %>%
  group_by(month, day) %>%
  summarise(mean_distance = round(mean(distance),2)) %>%
  arrange(desc(mean_distance)) %>%
  as.data.frame() %>%
  head (5)

Q5 <- flights %>%
  filter(dest == "BOS" | dest == "ATL") %>%
  group_by(dest) %>%
  summarise(max_arr_delay = max(arr_delay, na.rm = TRUE))



