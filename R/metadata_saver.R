## Approach to saving a results csv with metadata info.
## 
## Workflow:
##   do whatever coding is needed to get the actual results
##   run make_meta_overvew() and paste into the script. Add appropriate info
##   run make_meta_columns(), giving it the dataframe/tibble you want to save. Add appropriate column details
##   run save_with_meta(), giving it the dataframe/tibble you want to save, 
##     the two metadata objects you just created, and a filename.


save_with_meta = function(data, ## dataframe or tibble of data to save
                          meta.overview, #vector of character strings providing context for this file
                          ## see make_meta_overview() for help
                          meta.columns, #dataframe or tibble with one column of column names and one of
                          ## column descriptions. See make_meta_columns() for help
                          filename ##filename to save as, including `.xlsx`
                          ){
  
  if(gsub(".*[.]", "", filename) != ".xlsx"){
    cli::cli_alert("`filename` did not end in .xlsx. Rectifying this.")    
    filename = paste0(filename, ".xlsx")
  }
  
  openxlsx2::wb_workbook() |> 
    openxlsx2::wb_add_worksheet(sheet = "data") |> 
    openxlsx2::wb_add_data(sheet = "data", x = data) |> 
    openxlsx2::wb_add_worksheet(sheet = "metadata") |> 
    openxlsx2::wb_add_data(sheet = "metadata", x = data.frame(meta.overview),
                col_names = FALSE) |> 
    openxlsx2::wb_add_data(sheet = "metadata", x = meta.cols,
                start_row = length(meta.overview)+2,
                col_names = TRUE) |> 
    openxlsx2::wb_save(file = filename)
  
}


## function to create skeleton of `meta.overview` argument for save_with_meta
## saves to system clipboard for copy-pasting into code
make_meta_overview = function(){
    res = c("meta.overview = c(",
    "  \"[Overview statement]\",",
    "  \"[Details (split into multiple lines as desired)]\",",
    "  paste(\"Created\", date())",
    ")")
    clipr::write_clip(res)
    cli::cli_alert("Meta overview code written to clipboard")
}


## function to create skeleton of `meta.columns` argument for save_with_meta
## saves to system clipboard for copy-pasting into code
make_meta_columns = function(data){
  res = c("meta.cols = tribble(~column, ~description,",
          paste0("\"",names(data),"\", \"[fill in]\","),
          ")")
  clipr::write_clip(res)
  cli::cli_alert("Meta overview code written to clipboard")
}


data = mtcars

# make_meta_overview()
meta.overview = c(
  "[Overview statement]",
  "[Details (split into multiple lines as desired)]",
  paste("Created", date())
)

# make_meta_columns(data)
meta.cols = tribble(~column, ~description,
                    "mpg", "[fill in]",
                    "cyl", "[fill in]",
                    "disp", "[fill in]",
                    "hp", "[fill in]",
                    "drat", "[fill in]",
                    "wt", "[fill in]",
                    "qsec", "[fill in]",
                    "vs", "[fill in]",
                    "am", "[fill in]",
                    "gear", "[fill in]",
                    "carb", "[fill in]",
)

save_with_meta(data = data, 
               meta.overview = meta.overview, 
               meta.columns = meta.cols,
               filename = "test-results")
