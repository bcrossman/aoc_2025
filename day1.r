library(tidyverse)

input <- as_tibble((readLines("./inputs/day1.txt")))

## Part 1

turn_data <- 
  input %>% 
  separate(col = value, into = c("direction", "distance"), sep = 1, convert = T) %>% 
  mutate(turn = if_else(direction =="L", -distance, distance)) %>% 
  mutate(location = (50 + cumsum(turn)) %% 100)

part1 <- 
  turn_data %>% 
  filter(location == 0) %>% 
  nrow()

part1

## Part 2
part2 <- 
  turn_data %>% 
  mutate(zero_activated = (50 + cumsum(turn)) %/% 100) %>% 
  mutate(count_zero_chg = abs(zero_activated-lag(zero_activated, 1, default = 0))) %>% 
  mutate(extra_zero_chg = if_else(turn<0&location==0,1, 0)) %>% 
  mutate(bad_extra_zero = if_else(turn < 0 & lag(location, default = 50) == 0, -1, 0)) %>%
  mutate(total_count = count_zero_chg+extra_zero_chg+bad_extra_zero) %>% 
  pull(total_count) %>% 
  sum()

part2
