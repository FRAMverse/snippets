##################################################
## Collin Edwards
## Thu Sep  4 15:12:24 2025
## Example of how to use ggridges to plot smooths across point data
##################################################


library(ggplot2)
library(ggridges)

## simulate data
n = 4
data = data.frame(doy = rep(1:10, n),
                  year = rep(1:n, each = 10))
data$count = rpois(n = nrow(data), lambda = exp(3-abs(data$doy - 5 + dat$year)))

data |> 
  ggplot(aes(x = doy, y = year, height = after_stat(density),
             weight = count, group = year))+
  geom_density_ridges(stat = "density",
                      adjust = 0.6) ## use `adjust` to change bandwidth scale
