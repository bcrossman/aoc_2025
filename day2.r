library(tidyverse)

input <- as_tibble((readLines("./inputs/day2.txt")))

## Part 1

ranges <- input %>%
  separate_rows(value, sep = ",") %>%
  separate(
    col = value,
    into = c("min", "max"),
    sep = "-",
    convert = FALSE
  )

ranges2 <- ranges %>%
  mutate(
    length_max = nchar(max),
    length_min = nchar(min),
    min_test = if_else(
      (length_min) %% 2 == 0,
      as.numeric(substr(min, 1, (length_min+1) / 2)),
      10^(floor(length_min / 2)) 
    ),
    max_test = if_else(
      length_max %% 2 == 0,
      as.numeric(substr(max, 1, length_max / 2)),
      10^(ceiling(length_max / 2)) - 1
    ),
  )

# ranges2

checks <- ranges2 %>%
  rowwise() %>%
  mutate(
    check = list({
      start <- min_test
      end <- max_test
      if (end < start) {
        -1
      } else {
        nums <- seq(start, end)
        as.numeric(paste0(nums, nums))
      }
    })
  ) %>%
  ungroup() %>%
  unnest(check)

checks %>% filter(check>=as.numeric(min) & check<=as.numeric(max)) %>% pull(check) %>% sum()


## Part 2
checks_part2 <- ranges %>%
  rowwise() %>%
  mutate(
    invalid_ids = list({
      min_val <- as.numeric(min)
      max_val <- as.numeric(max)
      len_range <- nchar(min):nchar(max)
      
      candidates <- numeric(0)
      
      for (L in len_range) {
        group_sizes <- seq_len(floor(L/2))
        group_sizes <- group_sizes[L %% group_sizes == 0]
        
        for (size in group_sizes) {
          reps <- L / size
          base_start <- 10^(size - 1)
          base_end   <- 10^size - 1
          nums <- seq(base_start, base_end)
          generated <- as.numeric(strrep(nums, reps)) #extends the paste num num
          valid_generated <- generated[generated >= min_val & generated <= max_val]
          candidates <- c(candidates, valid_generated)
        }
      }
      candidates
    })
  ) %>%
  ungroup() %>%
  unnest(invalid_ids)

checks_part2 %>% 
  pull(invalid_ids) %>% 
  unique() %>% 
  sum()
