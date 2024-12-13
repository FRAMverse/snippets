library(tidyverse)
theme.larger =   theme(axis.title = element_text(size = rel(1.8)),
                       axis.text = element_text(size = rel(1.8)),
                       strip.text = element_text(size = rel(1.8)),
                       plot.title = element_text(size = rel(1.8)),
                       legend.text = element_text(size = rel(1.8)),
                       legend.title = element_text(size = rel(1.8))
)

##########################################
## example useage:
## make up some data
dat = data.frame(x = rnorm(10),
                 y = rnorm(10))
## make plot
ggplot(dat, aes(x = x, y = y))+
   geom_point()+
   ggtitle("Title is here")+
   theme.larger