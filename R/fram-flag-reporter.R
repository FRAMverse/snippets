library(here)
library(tidyverse)
library(framrsquared)
library(openxlsx)

fram_flag_reporter = function(fram.db, ##path to database to check
                              summary.file, #path to save file. Must end in .xlsx
                              runids = NULL){ #optional: list of runids to analyze (useful if databae is full and want to check one or a few runs)
  NR.flags = data.frame(non_retention_flag = c(1, 2, 3, 4),
                        flag.description = c("Computed CNR",
                                             "Ratio of CNR Days",
                                             "Legal/Sublegal Encounters",
                                             "Total Encounters"))
  scalers.flags = data.frame(fishery_flag = c(1, 2, 7, 8, 17, 18, 27, 28),
                             flag.description = c("Fishery Scaler", 
                                                  "Fishery Quota",
                                                  "MSF Scaler",
                                                  "MSF Quota",
                                                  "Scaler + MSF Scaler",
                                                  "Scaler + MSF Quota",
                                                  "Quota + MSF Scaler",
                                                  "Quota + MSF Quota"
                             )
  )
  
  fram.db = here("FRAM/NewModelRunTransfer_Chin1924-2124.mdb")
  fr.con = connect_fram_db(fram.db)
  run.list = fetch_table(fr.con, "RunID")
  fishery.scalers = fetch_table(fr.con, "FisheryScalers")
  NR = fetch_table(fr.con, "NonRetention")
  disconnect_fram_db(fr.con)
  
  if(!is.null(runids)){
    fishery.scalers = fishery.scalers |> 
      filter(run_id %in% runids)
    NR = NR |> 
      filter(run_id %in% runids)
  }
  fishery.scalers = left_join(fishery.scalers, run.list |> select(run_id, run_name))
  fishery.scalers = left_join(fishery.scalers, scalers.flags)
  fish.flag.sum = fishery.scalers |> 
    group_by(run_name, fishery_flag, flag.description) |> 
    summarize(fisheries = paste(sort(unique(fishery_id)), collapse = ", "))
  
  fish.flag.collapse = fish.flag.sum |> 
    group_by(fishery_flag, flag.description, fisheries) |> 
    summarize(runs = paste(sort(unique(run_name)), collapse = ", ")) |> 
    relocate(runs)
  
  
  NR = left_join(NR, run.list |> select(run_id, run_name))
  NR = left_join(NR, NR.flags)
  NR.flag.sum = NR |> 
    group_by(run_name, non_retention_flag, flag.description) |> 
    summarize(fisheries = paste(sort(unique(fishery_id)), collapse = ", "))
  NR.flag.collapse = NR.flag.sum |> 
    group_by(non_retention_flag, flag.description, fisheries) |> 
    summarize(runs = paste(sort(unique(run_name)), collapse = ", ")) |> 
    relocate(runs)
  
  wb = createWorkbook()
  addWorksheet(wb, sheetName = "fishery scalers", gridLines = FALSE)
  writeDataTable(wb, sheet = "fishery scalers", x = fish.flag.sum,
                 colNames = TRUE,
                 tableStyle = "TableStyleLight9")
  addWorksheet(wb, sheetName = "fishery scalers simplified", gridLines = FALSE)
  writeDataTable(wb, sheet = "fishery scalers simplified", x = fish.flag.collapse,
                 colNames = TRUE,
                 tableStyle = "TableStyleLight9")
  addWorksheet(wb, sheetName = "NR simplified", gridLines = FALSE)
  writeDataTable(wb, sheet = "NR simplified", x = NR.flag.collapse,
                 colNames = TRUE,
                 tableStyle = "TableStyleLight9")
  saveWorkbook(wb, summary.file, overwrite = TRUE) 
}

## example usage:
# fram_flag_reporter(fram.db = here("FRAM/NewModelRunTransfer_Chin1924-2124.mdb"),
#                    here("flag-check-test.xlsx"))


