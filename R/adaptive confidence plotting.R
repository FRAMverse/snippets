

library(tidyverse)

## generate some example data of a timeseries with gaps in it, with a primary value (`est`) and upper and lower
## confidence limits (`upper` and `lower`). In our actual use-cases, we would generate these values externally and want to plot them
dat = data.frame(year = c(1990:1992, 1995, 1997:2000, 2002:2003))
dat$est = rnorm(nrow(dat))
dat$upper = dat$est+runif(nrow(dat))
dat$lower = dat$est-runif(nrow(dat))

dat |> 
  complete(year = full_seq(year, period = 1))

## Here's what our current plots tend to look like:
ggplot(dat)+
  geom_ribbon(aes(x = year, ymin = lower, ymax = upper), alpha = 0.2)+
  geom_point(aes(x = year, y = est), size = 3)+
  scale_x_continuous(breaks = seq(1990, 2002, by = 2))+
  ggtitle("Original version")




##The problem here is that it looks like we have estimates of confidence in 1993, 1994, and 1996, 
##which we do not.
##We really want the envelop to be missing in there. And since 1995 is not adjacent to other years with 
##estimates, our modified envelope approach won't work, and so instead of an evelope we want to use a bar to represent confidence. to an envelope,
##we want to use a var to represent confidence instead of an envelope.

dat = dat |> 
  arrange(year) |> 
  ## At this point, if we're using facet_wrap or grouping factors in the plot,
  ## we need to include a group_by() with those terms before moving on to complete() and the mutates()!
  complete(year = full_seq(year, period = 1)) |> 
  ## here the lag() should be looking at a column that is guaranteed to NOT be an NA 
  mutate(ribbon.test = is.na(lag(est, n = 1)) + is.na(lead(est, n=1))) |>
  ## make ribbon version of entries
  mutate(upper.ribbon = if_else(ribbon.test!=2, upper, NA),
         lower.ribbon = if_else(ribbon.test!=2, lower, NA)) |> 
  mutate(upper.segment = if_else(ribbon.test==2, upper, NA),
         lower.segment = if_else(ribbon.test==2, lower, NA) ) |> 
  select(-ribbon.test)

## updated version
## Here we use "upper.segment and "lower.segment" to plot confidence bars, 
## and "upper.ribbon" and "lower.ribbon" to plot confidence envelopes.
ggplot(dat)+
  geom_ribbon(aes(x = year, ymin = lower.ribbon, ymax = upper.ribbon), alpha = 0.2)+
  geom_segment(aes(x = year, y = lower.segment, yend = upper.segment), linewidth = 0.8, alpha = 0.5)+
  geom_point(aes(x = year, y = est), size = 3)+
  scale_x_continuous(breaks = seq(1990, 2002, by = 2))+
  ggtitle("Fixed version")

