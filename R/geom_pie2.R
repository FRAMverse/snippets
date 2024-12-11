## Tweaks to geom_scatterpie from the scatterpie package 
## 4 things:
##  (a) make geom_scatterpie consistent with other ggplot functions in expecting
##      only long-form data. Perviously this was an option, but the default
##      was wide-format data, and accomodating both made it harder to implement 
##      (b) below.
##  (b) allow user to provide the name of the column that will be used for amounts, rather
##      than requiring that a column of the dataframe be labeled "value"
##  (c) filter out pies with all-zero amounts rather than erroring out with the 
##      difficult-to-interpret error message of 
##        "Computation failed in `stat_pie()` Caused by error in
##        `seq.default()`: ! `from` must be a finite number
##      Note that my implementation may not be the most efficient, but it works. 
##  (d) changing argument names so that they use periods instead of underscores,
##      making them more consistent with my (and others?) convention of underscores
##      for functions only.
geom_pie2 <- function(mapping=NULL, data, 
                      cols, pie.scale = 1, 
                      sorted.by.radius = FALSE, 
                      legend.name = "type", ...) {
  
  if (is.null(mapping))
    mapping <- aes_(x = ~x, y = ~y, amount = ~value)
  mapping <- modifyList(mapping,
                        aes_(r0 = 0,
                             fill = as.formula(paste0("~", legend.name)))
  )
  amountvar = ggfun::get_aes_var(mapping, "amount")
  xvar <- ggfun::get_aes_var(mapping, "x")
  yvar <- ggfun::get_aes_var(mapping, "y")
  ## stop if some values are negative.
  stopifnot(all(data[[amountvar]] >= 0))
  
  ## removing 0-amount piecharts to avoid errors
  ## (there may be a more efficientw ay to do this, but this works)
  dat.test = data |>
    dplyr::group_by(.data[[xvar]], .data[[yvar]]) |>
    dplyr::summarise(amount.tot = sum(.data[[amountvar]])) |>
    filter(amount.tot > 0) |>
    mutate(loc.test = paste0(.data[[xvar]], "x", .data[[yvar]]))
  test.col = paste0(data[[xvar]], "x", data [[yvar]])
  data = data[test.col %in% dat.test$loc.test,]
  
  if (!'r' %in% names(mapping)) {
    size <- diff(range(data[, xvar]))/ 50 * pie.scale
    data$r <- size
    mapping <- modifyList(mapping, aes_(r=size))
  }
  
  names(mapping)[match(c("x", "y"), names(mapping))] <- c("x0", "y0")
  df <- data
    names(df)[which(names(df) == cols)] = legend.name
    cols2 <- rlang::enquo(cols)
  if (!sorted.by.radius) {
    return(ggforce::geom_arc_bar(mapping, data=df, stat='pie', inherit.aes=FALSE, ...))
  }
  
  lapply(split(df, df$r)[as.character(sort(unique(df$r), decreasing=TRUE))], function(d) {
    ggforce::geom_arc_bar(mapping, data=d, stat='pie', inherit.aes=FALSE, ...)
  })
}

library(tidyverse)
d <- data.frame(x=rnorm(5), y=rnorm(5))
d$A <- abs(rnorm(5, sd=1))
d$B <- abs(rnorm(5, sd=2))
d$C <- abs(rnorm(5, sd=3))
df <- tidyr::pivot_longer(d, cols = !(x:y), names_to = "letters", values_to = "val")

ggplot() + geom_pie2(aes(x=x, y=y, amount = val), data=df, cols="letters") + coord_fixed()

d = d |>
  add_row(x = 0, y = 0, A = 0, B=0, C = 0)
df <- tidyr::pivot_longer(d, cols = !(x:y), names_to = "letters", values_to = "val")
ggplot() + geom_pie2(aes(x=x, y=y, amount = val), data=df, cols="letters") + coord_fixed()
## to compare:
library(scatterpie)
ggplot() + 
  geom_scatterpie(aes(x=x, y=y, amount = val), data=df, cols="letters", long_format = TRUE) +
  coord_fixed()
## oop, must have "value" column for scatterpie, can't specify column name.
df2 = df %>% 
  rename(value = val)
ggplot() + 
  geom_scatterpie(aes(x=x, y=y, amount = val), data=df2, cols="letters", long_format = TRUE) +
  coord_fixed()
 ## there's the error because we have one pie chart that includes only 0s in the amount column.