## Function to take start and end date(s) and return the months spanned by the period(s), the days within 
## each month that are spanned, and the proportion of months that are represented.
## Useful for taking fishing seasons and identifying proportions of months for scaling non-retention
## or other calculations
## 
## `start` and `end` arguments can be individual dates or vectors of dates. If vectors of dates,
## function presumes that this represents a series of non-overlapping periods, and calculates
## months spanned by the union of those periods
library(tidyverse)
library(lubridate)
months_spanned = function(start, 
                          end,
                          return.empties = FALSE #if TRUE, also returns months within
                          #the given years that were not spanned, with zeroes for days and proportion
                          ) {
  stopifnot("start and end must be dates" = all(is.Date(c(start, end))))
  days.used = do.call(c, map2(start, end, ~ seq(.x, .y, by = "day")))
  years.used = range(year(days.used))
  days.skel = seq(as.Date(paste0(years.used[1],"-01-01")),
             as.Date(paste0(years.used[1],"-12-31")),
             by = "day")
  df.used = data.frame(date = days.used) |> 
    mutate(year = year(date),
           month.num = month(date),
           month.name = month(date, label = TRUE, abbr = FALSE)
           ) |> 
    group_by(year, month.num, month.name) |> 
    summarize(days = n())
  df.skel = data.frame(date = days.skel) |> 
    mutate(year = year(date),
           month.num = month(date),
           month.name = month(date, label = TRUE, abbr = FALSE)
    ) |> 
    group_by(year, month.num, month.name) |> 
    summarize(days.base = n())
  df.used = full_join(df.used, df.skel)
  df.used$days[is.na(df.used$days)] = 0
  df.used = df.used |> 
    mutate(prop = days/days.base) |> 
    select(-days.base)
  if(!return.empties){df.used = df.used |> filter(days != 0)}
  return(df.used)
}

# ## examples ------------------------
# ## Basic example
# months_spanned(start = as.Date("2024-3-12"),
#                end = as.Date("2024-5-20"))
# ## More realistic example based on selection from fishing regs:
# regs.df = data.frame(start = as.Date(c("5/1/2022", "7/5/2022", "8/16/2022"), format = c("%m/%d/%Y")),
#                      end = as.Date(c("6/30/2022", "7/20/2022", "9/28/2022"), format = c("%m/%d/%Y")))
# months_spanned(start = regs.df$start, 
#                end = regs.df$end, 
#                return.empties = TRUE)

