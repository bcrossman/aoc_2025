library(igraph)
library(tidyverse)

input <- read_csv("./inputs/day8.txt", col_names = c("X", "Y", "Z"))

df1 <- input %>% 
  mutate(point = pmap(list(X, Y, Z), c)) %>% 
  select(point)

pairs <- df1 %>% 
  mutate(id1 = row_number()) %>% 
  rename(p1 = point) %>% 
  crossing(
    df1 %>% 
      mutate(id2 = row_number()) %>% 
      rename(p2 = point)
  )

pairs_with_dist <- pairs %>% 
  mutate(
    dist = map2_dbl(p1, p2, ~ {
      v1 <- unlist(.x)
      v2 <- unlist(.y)
      sqrt(sum((v1 - v2)^2))
    })
  )

pairs_unique <- pairs_with_dist %>% 
  filter(id1 != id2) %>% 
  arrange(dist)

## Part 1
strings <- 1000

edges <- 
  pairs_unique %>% 
  filter(id1 < id2) %>% 
  arrange(dist) %>% 
  slice(1:strings) %>% 
  select(id1, id2)

g <- graph_from_data_frame(edges, directed = FALSE)
comp <- components(g)

comp$csize %>% sort(decreasing = T) %>% .[1:3] %>% prod()

## Part 2
while(max(comp$csize)<nrow(df1)){
  print(max(comp$csize))
  strings = strings+1
  print(strings)
  
  edges <- 
    pairs_unique %>% 
    filter(id1 < id2) %>% 
    arrange(dist) %>% 
    slice(1:strings) %>% 
    select(id1, id2)
  
  g <- graph_from_data_frame(edges, directed = FALSE)
  comp <- components(g)
}
answer <- 
  pairs_unique %>% 
  filter(id1 < id2) %>% 
  arrange(dist) %>% 
  slice(strings)

answer$p1[[1]][1]*answer$p2[[1]][1]

