doy_2md=function(i){
  ymd=as.Date(i-1, origin="2019-01-01")
  return(format(ymd, "%b %d"))
}
doy_2m=function(i){
  ymd=as.Date(i-1, origin="2019-01-01")
  return(format(ymd, "%b"))
}
## also: two vectors of the doy of the first day of each month:

months.doy = cumsum(c(1, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30))
months.doy.leapyear = cumsum(c(1, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30))

## example usage:
# #generate 30 observations from day of year 40 to 150
# generate gaussian counts with noise
doy=sample(40:150,30)
count=exp(-(doy-100)^2/100)+(runif(30)-.5)*.1
# #Make sequence of days to label. Here, 5 days from day 40 to 150
at=round(seq(40,150, length=5))
## for ggplot:
library(ggplot2)
df = data.frame(doy, count)
ggplot(data = df, aes(x = doy, y = count))+
  geom_point()+
  scale_x_continuous(labels = doy_2md(at),
                     breaks = at)

## Example use of the months.doy:
## First, span a bit more of the year
doy=sample(20:350,100)
count=exp(-(doy-100)^2/100)+(runif(30)-.5)*.1
## for ggplot:
library(ggplot2)
df = data.frame(doy, count)
ggplot(data = df, aes(x = doy, y = count))+
  geom_point()+
  scale_x_continuous(labels = doy_2m(months.doy),
                     breaks = months.doy)

