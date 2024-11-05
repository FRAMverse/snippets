
# example of filtering catch data by regulation
# more modern version

library(tidyverse)

# set mock regulation data
regulations <- tribble(
  ~regulation_type, ~start_date, ~end_date,
  'MSF', as.Date('2023-07-01'), as.Date('2023-09-30'),
  'NS', as.Date('2023-11-01'), as.Date('2023-12-31')
)

# mock catch data
catch_data <- tibble(
  date = seq(as.Date('2023-07-01'), as.Date('2023-12-31'), by='day')) |>
  rowwise() |>
  mutate(
    catch = abs(rnorm(n=1, mean = 10, 15))
  )

# join_by() exposes columns in both dataframes
# and allows functions
catch_data |>
  left_join(regulations, join_by(between(date, start_date, end_date))) 


