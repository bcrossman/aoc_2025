library(tidyverse)

input <- readLines("./inputs/day6.txt")                        # Part 2 uses this
input_table <- read_table("./inputs/day6.txt", col_names = FALSE)  # Part 1 uses this

## Part 1

input_table %>%
  pivot_longer(everything(), names_to = "id", values_to = "value") %>%
  mutate(type = if_else(!is.na((as.numeric(value))), "number", "operation"))  %>%
  arrange(id) %>% 
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(number = map(number, as.numeric)) %>%
  unnest(operation) %>%
  mutate(result = map2_dbl(number, operation, ~ { reduce(.x, match.fun(.y)) })) %>%  #I tried to get eval parse to work and couldn't
  pull(result) %>% 
  sum() %>% as.character()

## Part 2

as_tibble(input) %>%
  mutate(row = row_number()) %>%
  mutate(value = str_replace_all(value, " ", "S")) %>%
  separate_rows(value, sep = "") %>%
  filter(value != "") %>% 
  group_by(row) %>%
  mutate(col = row_number()) %>%
  group_by(col) %>% 
  mutate(is_break = all(value=="S")) %>% 
  ungroup() %>% 
  arrange(col, row) %>% 
  mutate(id = cumsum(is_break)+1) %>% 
  filter(!is_break) %>% 
  arrange(id, row, col) %>%
  group_by(id) %>% 
  mutate(col = max(col) - col + 1 ) %>% 
  ungroup() %>% 
  filter(value != "S") %>% 
  mutate(type = if_else(!is.na((as.numeric(value))), "number", "operation"))  %>%
  arrange(id,desc(col),row) %>% 
  group_by(id, type, col) %>% 
  summarise(value = paste0(value, collapse = "")) %>% 
  select(-col) %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  mutate(number = map(number, ~ as.numeric(trimws(.x)))) %>%
  unnest(operation) %>% 
  mutate(result = map2_dbl(number, operation, ~ { reduce(.x, match.fun(.y)) })) %>%  
  pull(result) %>% 
  sum() %>% as.character()
