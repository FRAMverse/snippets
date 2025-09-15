library(tidyverse)
library(framrsquared)

compare_across_dbs = function(db1, db2, run_id){
  
  mort_table1 = db1 |> 
    fetch_table("Mortality") |> 
    filter(run_id == run_id)
  mort_table2 = db2 |> 
    fetch_table("Mortality") |> 
    filter(run_id == run_id)
  
  ## combine the data and plot them
  rbind(cbind(mort_table1, db = "db1"),
        cbind(mort_table2, db = "db2")
  ) |> 
    pivot_longer(cols = landed_catch:msf_encounter,
                 values_to = "fish") |> 
    mutate(fish = round(fish, 1)) |>
    pivot_wider(names_from = db, values_from = fish) |> 
    select(name, db1, db2) |>  
    distinct() |> 
    ggplot(aes(x = db1, y = db2))+
    geom_abline(intercept = 0, slope = 1)+
    geom_point()+
    facet_wrap(. ~ name, scales = "free")
  
}

framdb = connect_fram_db("fram multirun testing/Valid2024_Zero.mdb")
framdb_ref = connect_fram_db("fram multirun testing/Valid2024_Zero - LOCAL REFERENCE.mdb")

compare_across_dbs(framdb, framdb_ref, run_id = 65)

disconnect_fram_db(framdb)
disconnect_fram_db(fram)