library(tidyverse)

input <- as_tibble((readLines("./inputs/day1.txt")))

## Part 1
input %>% 
  separate(col = value, into = c("direction", "distance"), sep = 1, convert = T) %>% 
  mutate(turn = if_else(direction =="L", -distance, distance)) %>% 
  mutate(location = (50 + cumsum(turn)) %% 100) %>% 
  filter(location == 0) %>% 
  nrow()

