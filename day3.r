library(tidyverse)

input <- as_tibble((readLines("./inputs/day3.txt")))

## Part 1
input %>% 
  mutate(id = row_number(), 
         str_len = nchar(value), 
         orig_string = value) %>% 
  separate_rows(value, sep = "") %>% 
  filter(value != "") %>% 
  group_by(id) %>% 
  mutate(loc = row_number()) %>% 
  ungroup() %>% 
  mutate(remaining_string = str_sub(orig_string, loc + 1, str_len)
         ) %>% 
  separate_rows(remaining_string, sep = "") %>% 
  filter(remaining_string != "") %>% 
  mutate(joltage = as.numeric(paste0(value,remaining_string))) %>% 
  group_by(id) %>% 
  summarise(max_jolt = max(joltage)) %>% 
  pull(max_jolt) %>% 
  sum()

## Part 2
results <- tibble()

input_current <- input %>% 
  mutate(id = row_number()) %>% 
  select(id, value)

for(i in 12:1){
  # if(i == 11){adad}
  df <-   
    input_current %>% 
    mutate(str_len = nchar(value), 
           orig_string = value) %>% 
    separate_rows(value, sep = "") %>% 
    filter(value != "") %>% 
    group_by(id) %>% 
    mutate(loc = row_number()) %>% 
    ungroup() %>% 
    mutate(remaining_string = str_sub(orig_string, loc + 1, str_len)
    ) %>% 
    group_by(id) %>% 
    filter(loc <= (max(str_len) - i + 1)) %>% 
    mutate(re_str_len = nchar(remaining_string)) %>% 
    arrange(id, desc(as.numeric(value)), desc(re_str_len)) %>% 
    slice(1) %>% 
    ungroup()
  
  results <- bind_rows(
    results,
    df %>% transmute(id, i = i, value)
  )
  
  input_current <- df %>% 
    transmute(id, value = remaining_string)
}

results %>% 
  arrange(id, desc(i)) %>% 
  group_by(id) %>% 
  summarise(value_str = paste0(value, collapse = "")) %>% 
  mutate(value_num = as.numeric(value_str),
         str_len = nchar(value_str)) %>% 
  pull(value_num) %>% 
  sum() %>% as.character()
