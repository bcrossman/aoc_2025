library(tidyverse)

input <- readLines("./inputs/day5.txt")

# Part 1
split_index <- which(input == "")
fresh_range <- input[1:(split_index - 1)]
ids <- as.numeric(input[(split_index + 1):length(input)])

ranges <- as_tibble(fresh_range) %>% 
  separate(value, into = c("low", "high"), sep = "-", convert = TRUE)

ranges %>% 
  crossing(tibble(id=ids)) %>% 
  mutate(fresh = (id>=low)&(id<=high)) %>% 
  group_by(id) %>% 
  summarise(fresh = any(fresh)) %>% 
  filter(fresh) %>% 
  nrow()

#Part 2

##Create non-overlapping intervals that cover the same area (like drawing a plot)
ranges <- as_tibble(fresh_range) %>% 
  separate(value, into = c("low", "high"), sep = "-", convert = TRUE) %>% 
  arrange(low) 

merged_ranges <- list()
curr_low <- ranges$low[1]
curr_high <- ranges$high[1]

for (i in 2:nrow(ranges)) {
  next_low <- ranges$low[i]
  next_high <- ranges$high[i]
  if (next_low <= curr_high + 1) {
    curr_high <- max(curr_high, next_high)
  } else {
    merged_ranges[[length(merged_ranges) + 1]] <- c(curr_low, curr_high)
    curr_low <- next_low
    curr_high <- next_high
  }
}

merged_ranges[[length(merged_ranges) + 1]] <- c(curr_low, curr_high)

do.call(rbind, merged_ranges) %>% 
  as.data.frame() %>% 
  setNames(c("low", "high")) %>% 
  mutate(coverage_size = high - low + 1) %>% 
  pull(coverage_size) %>% 
  sum() %>% as.character()
