library(tidyverse)

## example use case of nest/unnest, from Ty's R school demonstration 10/15/24
## Using pssp to pull in chinook data for area 5 and area 6, July 2024.
## What are our counts of legal / sublegal x AD / UM? And how do those counts depend on the size limit?

tf <- pssp::tf_pull(c('05', '06'), '07/01/2024', '07/31/2024')

## Basic idea:
##  tf_lm_summary() is designed to summarize a dataframe, providing the breakdown of fish counts by category.
##  we use nest to break our total tf dataframe into separate dataframes per area
##  We then use expand_grid to create all combinations of our sub-dataframes with a set of size limits.
##  Then pmap() allows us to use multiple columns as inputs to whatever function
##  In this case, we're creating a simple anonymous function to convert the list of inputs into
##  function arguments.
##  Then we use unnest to split up our output lists.
tf |>
  group_by(catch_area_code) |>
  nest() |>
  expand_grid(size_lim = c(16, 18, 20, 22, 24, 26)) |> 
  mutate(
    lm = pmap(list(data, size_lim), \(x, y) pssp::tf_lm_summary(x, size_limit = y))
  ) |>
  unnest(lm)