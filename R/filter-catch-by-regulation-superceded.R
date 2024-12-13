
# example of filtering catch data by regulation
library(tidyverse)

# set mock regulation data
regulations <- tribble(
  ~regulation_type, ~start_date, ~end_date,
  'MSF', as.Date('2023-07-01'), as.Date('2023-09-30'),
  'NS', as.Date('2023-11-01'), as.Date('2023-12-31')
)

# echo it to console
regulations

# 'explode' regulations to daily form 
daily_regulations <- regulations |>
  rowwise() |>
  mutate(
    date = list(seq(start_date, end_date, by='day'))
  ) |>
  unnest(date) |>
  select(-start_date, -end_date)

# mock catch data
catch_data <- tibble(
  date = seq(as.Date('2023-07-01'), as.Date('2023-12-31'), by='day')) |>
  rowwise() |>
  mutate(
    catch = abs(rnorm(n=1, mean = 10, 15))
  )

# filtering joins will contextualize catch data
filtered_catch <- catch_data |>
  inner_join(daily_regulations, by = 'date')

# october ommitted per regulations
catch_data |>
  filter(lubridate::month(date) == 10)

filtered_catch |>
  filter(lubridate::month(date) == 10)

# futher filtering catch by regulation
filtered_catch |>
  filter(regulation_type == 'MSF')

# summarization
filtered_catch |>
  group_by(regulation_type) |>
  summarize(across(catch, sum))
