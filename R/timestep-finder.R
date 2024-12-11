library(lubridate)
## Function to calculate the Chinook timestep from a date or vector of dates
get_ts = function(x){
  stopifnot("x must be a date or vector of dates" = is.Date(x))
  ## get month
  mo = month(x, label = TRUE)
  ## prepopulate with -999 for checkability
  ts = rep(-999, length(x))
  ts[mo %in% c("May", "Jun")] = 2
  ts[mo %in% c("Jul", "Aug", "Sep")] = 3
  ts[mo %in% c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")] = 4
  return(ts)
}

## breakpoints for Chinook timesteps, for the sake of plotting. 
## Note (a) leap years will have one extra day in timestep 1/4, and
## (2) I am skipping over trying to distinguish timestep 1 vs 4, as they overlap.
ts.breaks = yday(c("2024-05-01", "2024-07-01", "2024-10-01"))
