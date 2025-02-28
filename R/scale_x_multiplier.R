library(tidyverse)

## ggplot function to apply "multiplier" scale:
## log2 scaling, labels in intuitive terms
## Acts as custom scale_x_continuous function and can be included as a ggplot layer.
## Takes any additional arguments scale_x_continuous does.
scale_x_multiplier <- function(.data, ...) {
  scale_x_continuous(transform = "log2",
                     labels = \(x) 
                     case_when(
                       x >= 1 ~ paste0("x ", x),
                       x < 1 ~ paste0("x 1/", 1/x)),
                     ...
  )
}

## example: histogram --------------
## Generate simulated data of changes in abundance, where final pop between 0.1 and 3 times the initial population size.
dat = data.frame(x = runif(1000, min = 0.1, max = 3))

## basic plot
ggplot(dat, aes(x = x))+
  geom_histogram()+
  scale_x_multiplier()

## example using additional arguments, showing use in a full chain of ggplot layers
ggplot(dat, aes(x = x))+
  geom_histogram()+
  scale_x_multiplier(name = "Abundance change")+
  theme_bw()
