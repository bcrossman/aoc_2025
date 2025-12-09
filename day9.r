library(igraph)
library(tidyverse)

input <- read_csv("./inputs/day9.txt", col_names = c("X1", "Y1")) %>%   
  mutate(id1 = row_number())

input %>% 
  crossing(
    input %>% 
      rename_all(~ gsub("1", "2", .))
  ) %>% 
  mutate(area = (abs(X2-X1)+1)*(abs(Y2-Y1)+1)) %>%
  slice_max(area,with_ties = F) %>% 
  pull(area) 

