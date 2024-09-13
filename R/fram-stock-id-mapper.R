library(tidyverse)
## function to map between the stock id used in the bkFRAM tablehttp://127.0.0.1:30661/graphics/3da6d2c1-7105-4ca0-85b7-fd5e211e1c0b.png and the stock ID used literally everywhere else. 
## NumStk defines the maximum normal stock ID in the resulting dataframe
stock_id_mapper = function(NumStk = 80){
  ## note: after the initial 8 stock id, we settle down to a simple pattern.
  ## Going to apply the pattern, then replace the special cases
  raw = data.frame(stock.id.bk = 1 : (NumStk *2)) |> 
    mutate(use = if_else(stock.id.bk %% 3 == 0,
                         0, 1))
  raw$use[1:8] = c(0,1,1, 0, 1,1,1,1)
  res = raw |> 
    filter(use != 0) |> 
    mutate(stock.id.fram = row_number()) |> 
    filter(stock.id.fram <= NumStk) |> 
    select(-use)
  return(res)
}
stock_id_mapper()

