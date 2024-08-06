library(tidyverse)
library(framrsquared)

## function to map between the stock id used in the bkFRAM table and the stock ID used literally everywhere else. 


stock_id_mapper = function(filename, run.id){
  con = connect_fram_db(filename)
  tab = fetch_table(con, "BackwardsFRAM") |> 
    filter(run_id== run.id) |> 
    filter(target_flag != 0) |> 
    arrange(stock_id) |> 
    select(stock_id_bkfram = stock_id) |> 
    mutate(stock_id_normal = row_number())
  disconnect_fram_db(con) 
  return(tab)
}

stock_id_mapper = function(NumStk = 100){
  raw = data.frame(stock_id_bk = 1 : (NumStk *2))
  raw$
  data.frame()
  
}

out = stock_id_mapper(
  filename = "C:/Users/edwc1477/OneDrive - Washington State Executive Branch Agencies/Documents/WDFW FRAM team work/FRAM dev stuff/splitting stock/green river/Valid2022_Round_7.1.1_11142023 - green river split.mdb",
  run.id = 39
)
