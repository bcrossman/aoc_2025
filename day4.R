library(tidyverse)

input <- as_tibble((readLines("./inputs/day4.txt")))

create_grid <- function(df){
  
  df %>% 
    rowid_to_column() %>%
    separate_rows(value, sep="", convert = T) %>% 
    drop_na(value) %>% 
    filter(value != "") %>% 
    mutate(rowid = rowid) %>% 
    group_by(rowid) %>% 
    mutate(colid = row_number()) %>% 
    ungroup() %>% 
    mutate(key = paste(colid, rowid, sep=", ")) %>% 
    select(value, rowid, colid, key)
}

data <- create_grid(input) 

## Part 1

data %>% 
  rowwise() %>% 
  mutate(
    df2 = list(
      expand_grid(
        rowid2 = (rowid - 1):(rowid + 1),
        colid2 = (colid - 1):(colid + 1)
      )
    )
  ) %>% 
  ungroup() %>% 
  unnest(df2) %>% 
  mutate(key_2 = paste(colid2, rowid2, sep = ", ")) %>% 
  left_join(data %>% select(key, value), by=c("key_2"="key")) %>% 
  drop_na(value.y) %>% 
  filter(value.x=="@", value.y=="@")%>% 
  count(rowid, colid, key) %>% 
  filter(n<=4) %>% 
  nrow()

## Part 2
total_removed <- 0
removed <- 1

while(removed>0){

removed_list <-   
  data %>% 
    rowwise() %>% 
    mutate(
      df2 = list(
        expand_grid(
          rowid2 = (rowid - 1):(rowid + 1),
          colid2 = (colid - 1):(colid + 1)
        )
      )
    ) %>% 
    ungroup() %>% 
    unnest(df2) %>% 
    mutate(key_2 = paste(colid2, rowid2, sep = ", ")) %>% 
    left_join(data %>% select(key, value), by=c("key_2"="key")) %>% 
    drop_na(value.y) %>% 
    filter(value.x=="@", value.y=="@")%>% 
    count(rowid, colid, key) %>% 
    filter(n<=4) %>% 
    pull(key)

removed <- length(removed_list)
total_removed <- total_removed+removed
data$value[data$key %in% removed_list] <- "."
print(removed)
  
}
total_removed