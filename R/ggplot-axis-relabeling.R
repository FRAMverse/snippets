## Functions to streamline relabeling ggplot axes using functions.
## Developed in particular for taking proportion data and presenting as percents
## without having to change the underlying data -- see scale_y_perc and scale_x_perc
## scale_._continous_fn is a more generic version of these, and make_perc is a helper function.

library(tidyverse)
## functions to simplify relabeling axis using generic functions
scale_x_continuous_fn = function(breaks, labelfn, ...){
  labels = labelfn(breaks)
  scale_x_continuous(breaks = breaks, labels = labels, ...)
}
scale_y_continuous_fn = function(breaks, labelfn, ...){
  labels = labelfn(breaks)
  scale_y_continuous(breaks = breaks, labels = labels, ...)
}
make_perc = function(x, r = 999){paste0(round(x*100, r), "%")} 
## specifically streamline percent-making. r can be defined to round, if not dealing with 
## pretty % values
scale_x_perc = function(breaks, r = 9999, ...){
  labels = make_perc(breaks, r = r)
  scale_x_continuous(breaks = breaks, labels = labels, ...)
}
scale_y_perc = function(breaks, r = 9999, ...){
  labels = make_perc(breaks, r = r)
  scale_y_continuous(breaks = breaks, labels = labels, ...)
}

## Example use case: --------------

dat = data.frame(year = 2000:2020,
                 er = runif(21))

ggplot(dat, aes(x = year, y = er))+
  geom_point()+
  ylim(c(0, NA))+
  ylab("exploitation rate")+
  theme_light(base_size = 18)+
  ggtitle("Motivation: We want ER in percents")

ggplot(dat, aes(x = year, y = er))+
  geom_point()+
  ylab("exploitation rate")+
  scale_y_perc(breaks = seq(0, 1, length = 5), ## just identify the break points desired in the original scale. 
               ##  Can do this programmatically if desired, e.g.:
               ##   seq(round(min(dat$er),2), round(max(dat$er), 2), length = 4)
               limits = c(0,NA))+ #can feed in y limits just like with scale_y_continuous
  theme_light(base_size = 18)+
  ggtitle("Problem solved")
