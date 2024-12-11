#####################################
## This code is an example of      ##
## creating a visualization of a   ##
## correlation matrix with vanilla ##
## tidyverse packages.             ##
## Ty Garber 1/4/2024              ##
#####################################

library(tidyverse)

cor(mtcars) |> # Without method argument default is Pearson's
  as.data.frame() |>
  rownames_to_column() |>
  pivot_longer(-rowname) |>
  ggplot(aes(rowname, name, fill=value, label=round(value,2))) +
  labs(x=NULL, y=NULL) +
  geom_tile() +
  geom_text() +
  scale_fill_gradient2(mid="#f6f7fa",
                       low="#3b6cc4",
                       high="#af4545")
