library(tidyverse)
theme.fram = theme_light(base_size = 18)+
  theme(strip.text = element_text(size = rel(1.1)))

##########################################
## example useage:
## make up some data
dat = data.frame(x = rnorm(10),
                 y = rnorm(10))
## make plot
ggplot(dat, aes(x = x, y = y))+
  geom_point()+
  ggtitle("Title is here")+
  theme.fram