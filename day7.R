library(tidyverse)

input <- as_tibble((readLines("./inputs/day7.txt")))

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

current_beams <- data %>% filter(value=="S")
beam_runs <- list()
beam_runs[[as.character(1)]] <- current_beams 
i <- 1
while((current_beams %>% drop_na(value) %>% nrow())>0){
  print(i)
  print(current_beams %>% nrow())
  print(current_beams$rowid %>% min())
  print(current_beams$rowid %>% max())
  i <- i+1
  current_beams <- 
    current_beams %>% 
    rowwise() %>% 
    mutate(
      df2 = if_else(value == "."|value=="S", list(
        expand_grid(
          rowid2 = (rowid):(rowid + 1),
          colid2 = colid
        )),
        list(
          expand_grid(
            rowid2 = rowid,
            colid2 = (colid - 1):(colid + 1)
          ))
      )) %>% 
    ungroup() %>% 
    unnest(df2) %>% 
    mutate(key_2 = paste(colid2, rowid2, sep = ", ")) %>% 
    filter(key != key_2) %>% 
    left_join(data %>% select(key, value), by=c("key_2"="key")) %>% 
    drop_na(value.y) %>% 
    transmute(value = value.y, rowid = rowid2, colid = colid2, key = key_2,) %>% 
    distinct_all()
  beam_runs[[as.character(i)]] <- current_beams 
}

total_runs <- bind_rows(beam_runs) %>% filter(value == ".")

total_runs$value <- "|"

data %>% 
  left_join(total_runs, by="key") %>%
  mutate(value.x = if_else(is.na(value.y), value.x, value.y)) %>% 
  group_by(colid.x) %>% 
  arrange(colid.x, rowid.x) %>% 
  filter(value.x == "^", lag(value.x,1)=="|") %>% 
  arrange(rowid.x) %>% 
  nrow()

## Part 2

current_beams <- data %>% filter(value=="S") %>% mutate(n = 1)
beam_runs <- list()
beam_runs[[as.character(1)]] <- current_beams 
i <- 1

while((current_beams %>% drop_na(value) %>% nrow()) > 0){
  i <- i + 1
  print(i)
  print(current_beams %>% nrow())
  print(current_beams$rowid %>% min())
  print(current_beams$rowid %>% max())
  
  current_beams <- 
    current_beams %>% 
    rowwise() %>% 
    mutate(
      df2 = if_else(value == "." | value == "S", 
                    list(expand_grid(
                      rowid2 = (rowid):(rowid + 1),
                      colid2 = colid
                    )),
                    list(expand_grid(
                      rowid2 = rowid,
                      colid2 = (colid - 1):(colid + 1)
                    ))
      )) %>% 
    ungroup() %>% 
    unnest(df2) %>% 
    mutate(key_2 = paste(colid2, rowid2, sep = ", ")) %>% 
    filter(key != key_2) %>% 
    left_join(data %>% select(key, value), by=c("key_2"="key")) %>% 
    drop_na(value.y) %>% 
    transmute(value = value.y, rowid = rowid2, colid = colid2, key = key_2, n = n) %>% 
    group_by(value, rowid, colid, key) %>% 
    summarize(n = sum(n))
  
  beam_runs[[as.character(i)]] <- current_beams 
}

total_runs <- bind_rows(beam_runs) 
max_row <- max(data$rowid)

total_runs %>% 
  filter(rowid == max_row, value != "^") %>% 
  pull(n) %>% 
  sum() %>% as.character()
