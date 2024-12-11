## function to take dataframe of with row per period (e.g. regulatory period) and columns
## identifying start and end of period, and expand to one row per *day*.
## 
## df : data frame with a minimum of two columns defining the start and end of the period. All
##   other columns will be copied appropriately in the returned data frame
## period.names : character vector with the names of columns with the start and end dates,
##   in any order
## dividees : optional argument to identify any columns that should be scaled by the number of days.
##   Example use-case: `fish.caught` is the number of fish caught in each period, and in the expanded version
##   we want to divide this by the number of days in each period to give fish per day.
##   This argument should be NULL or a charactor vector with column name(s).
library(lubridate)

period_2daily = function(df, 
                         period.names = c("reg.start", "reg.end"),
                         dividees = NULL){
  stopifnot("period.names must have exactly two items (the start and end date columns)" = length(period.names) == 2)
  stopifnot("columns defining period must be dates" = 
              all(lapply(df[,period.names], class) == "Date"))
  stopifnot("dividees must be NULL or columns within data frame" = is.null(dividees) |
              all(dividees %in% names(df)))
  # rownames(df) = NULL
  
  daily.ls = list()
  for(i in 1:nrow(df)){
    ## taking advantage of the fact that `seq` can create sequences of DAYS between two dates
    df.date = data.frame(date = seq(ymd(min(do.call(c, df[i, period.names]))),
                                    ymd(max(do.call(c, df[i, period.names]))), by = 'day'))
    df.date$doy = yday(df.date$date)
    daily.ls[[i]] = cbind(df.date, 
                          df[i,-which(names(df) %in% period.names), drop = FALSE],
                          row.names = NULL)
    if(!is.null(dividees)){
      daily.ls[[i]][,dividees] = daily.ls[[i]][,dividees]/nrow(df.date)
    }
  }
  ## turn list into dataframe
  daily.df = do.call(rbind, daily.ls)
  return(daily.df)
}

## example --------------------------------------
library(dplyr)

## generate example data frame. Added "num.caught" as example for a divisee
regs.df = data.frame(areacode = c("05", "05", "05", "05", "06", "06"), 
                     area = c("05", "05", "05", "05", "06", "06"), 
                     subarea = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_), 
                     fishery.id = c(42, 42, 42, 42, 54, 54), 
                     id.num = c("42:Ar 5 Sport",  "42:Ar 5 Sport", "42:Ar 5 Sport", "42:Ar 5 Sport", "54:Ar 6 Sport", "54:Ar 6 Sport"), 
                     run.year = c(2021, 2021, 2021, 2021, 2021, 2021), 
                     status = c("MSF", "NR", "MSF", "NR", "MSF", "NR"), 
                     reg.start = structure(c(18809, 18855, 19052, 19091, 18809, 18842), class = "Date"), 
                     reg.end = structure(c(18854,18900, 19090, 19112, 18841, 18900), class = "Date"),
                     num.caught = c(150, 20, 160, 240, 11, 5))


period_2daily(regs.df |> select(area, status, reg.start, reg.end, num.caught), 
              period.names = c("reg.start", "reg.end")) |> 
  head()

period_2daily(regs.df |> select(area, status, fishery.id, reg.start, reg.end, num.caught), 
              period.names = c("reg.start", "reg.end"),
              dividees = c("num.caught")) |> 
  head()
