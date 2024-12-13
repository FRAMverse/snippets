## tidyverse approach to identifying rows of dataframe / tibble with duplicates
## across multiple columns
dat = data.frame(x = c("rat", "dog", "dog", "rat", "cow"),
                 y = c("A", "B", "B", "B", "C"),
                 z = rnorm(5))
## if we should have only one unique combination of x and y (but zs can be duplicated),
## we can identify rows with duplicated x and
dat |>
  group_by(x, y) |>
  mutate(num_dups = n(),
         dup_id = row_number()) |>
  ungroup() |>
  filter(num_dups>1)
