## Note: being added to framrsquared as add_stock_check(). The version here works, but has become archival.


## examples:
addstock_check("C:/Repos/fram_dev_material/FRAM QA QC/comparing/2024 Pre-Season Chinook DB - first test.mdb",
               138)
addstock_check()


addstock_check <- function(file.name = NULL, run.id = NULL, old.stockcount = 78){
  ## file.name: filepath to database. If left null, provide summary of process instead
  ## run.id: RunID of interest. If left null, provide summary of process instead
  ## old.stockcount: the number of stocks to treat as the "baseline" -- several checking steps will focus solely on newly added stocks. Defaults to 78.
  if(is.null(file.name) | is.null(run.id)){ #Print instructions

    tab.list = c("*" ="BaseID: increase NumStk argument.",
                 "*" = "Stock: add info for new stock.",
                 "*" = "StockRecruit: add info for new stock x age, including `RecruitScale`. `RecruitCoho` will get updated automatically.",
                 "*" = "BaseCohort: add new entries for stock x age.",
                 "*" = "AEQ: I think this might get calculated for us?",
                 "*" = "BackwardsFRAM: complicated. See below.",
                 "*" = "BaseExploitationRate: add new entries for stock x age x fisheryies. Easiest to manipulate in Excel.",
                 "*" = "Growth: add new entries for stock.",
                 "*" = "MaturationRate: add new entries for stock x age x timestep. Easiest to manipulate in Excel.")
    cli::cli_alert("`file.name` or `run.id` not detected. Providing overview of steps to add stock instead.")
    cli::cli_alert("When adding new Chinook stock, the following tables must be updated with the new stock:")
    cli::cli_ol(tab.list)
    cli::cli_alert("\nRemember that we make two versions of each stock: unmarked (odd stockID) and marked (even stockID).")
    cli::cli_alert("When updating BackwardsFRAM table, need to add THREE rows for every unmarked+marked stock pair:")
    ulid = cli::cli_ul()
    cli::cli_li("The first row is the combined entry, with the each of the `TargetEsc*` columns containing the sum of column values for the unmarked and marked stock. The TargetFlag for this row should be 0.")
    cli::cli_li("Then the next two rows are the unmarked and marked stock, with TargetFlag = 3.")
    cli::cli_end(ulid)

  }else{

    error.count = 0

    cli::cli_alert("Checking for additional stock (beyond stockID = 78)...")

    con = connect_fram_db(file.name)
    run.info = fetch_table(con, "RunID")
    if(! run.id %in% run.info$run_id){
      cli::cli_abort(paste0("`run.id` value must be present in database. Available run ids: ",paste0(run.info$run_id, collapse = ", "), "."))
    }
    ## get the base period ID.
    bp.id = run.info |> dplyr::filter(run_id == run.id) |> dplyr::pull(base_period_id)

    ## BaseID -- get NumStk
    cli::cli_text()
    cli::cli_alert("Checking BaseID table...")
    baseid.df = fetch_table(con, "BaseID") |>
      dplyr::filter(base_period_id == bp.id)
    NumStk = baseid.df |> dplyr::pull(num_stocks)
    cli::cli_alert(paste0("  NumStk = ", NumStk))
    if(NumStk %% 2 != 0){
      cli::cli_alert_danger("NumStk ({NumStk}) should be an even number, as we add stocks as a pair of unmarked and marked stock. FRAM relies on this pattern.")
      error.count = error.count+1
    }

    ## Stock table
    cli::cli_text()
    cli::cli_alert("Checking Stock table...")
    stock.df = fetch_table(con, "Stock") |>
      dplyr::arrange(desc(stock_id)) #|>
    #dplyr::mutate(stock_id = dplyr::if_else(stock_id == 80, 81, stock_id)) # for testing the error messages.
    ## dimension check
    ## check for presence of stock, stock numbering
    error.count = error.count + stock_check_helper("Stock", NumStk = NumStk, stock.vec = stock.df$stock_id, uniques.only = TRUE)

    if(!(all(grep("^M-", stock.df$stock_name) %% 2 == 1) &
         all(grep("^U-", stock.df$stock_name) %% 2 == 0))){ #marked and unmarked don't follow the right pattern
      cli::cli_alert_danger("Entries of Stock table should be Unmarked (odd number) and marked (even number), and `StockName` column should correspondingly start with \"U-\" and \"M-\".")
      error.count = error.count+1
    }else{
      cli::cli_alert_success("  Stock naming convention followed.")
    }

    ## StockRecruit table
    cli::cli_text()
    cli::cli_alert("Checking StockRecruit table...")
    stock.recruit.df = fetch_table(con, "StockRecruit") |>
      dplyr::filter(run_id == run.id)
    ## check for presence of stock, stock numbering

    error.count = error.count + stock_id_comp("StockRecruit", stock.recruit.df)
    ## checking stock-age combinations
    error.count = error.count + stock_age_checker("StockRecruit", NumStk = NumStk, old.stockcount = old.stockcount,
                                                  df = stock.recruit.df)

    ## BaseCohort
    cli::cli_text()
    cli::cli_alert("Checking BaseCohort table...")
    base.cohort.df = fetch_table(con, "BaseCohort") |>
      dplyr::filter(base_period_id == bp.id)
    ## check that stock IDs make sense
    error.count = error.count + stock_id_comp("BaseCohort", base.cohort.df)
    ## checking stock-age combinations
    error.count = error.count + stock_age_checker("BaseCohort", NumStk = NumStk, old.stockcount = old.stockcount,
                                                  df = base.cohort.df)

    ## AEQ
    cli::cli_text()
    cli::cli_alert("Checking AEQ table...")
    cli::cli_alert_warning("(Not yet implemented. I think we don't need to?)")

    ## BackwardsFRAM
    cli::cli_text()
    cli::cli_alert("Checking BackwardsFRAM table...")
    cli::cli_alert_info("  Remember, StockID here is different from everywhere else, unfortunately.")
    bkfram.df = fetch_table(con, "BackwardsFRAM") |>
      dplyr::filter(stock_id > old.stockcount*3/2-1) |>  ## filter to recently added stock. Formula is annoying, but so is bkfram stock_id
      dplyr::mutate(stock.pop.id = floor(stock_id/3))
    vals.expect = (old.stockcount*3/2):(NumStk*3/2-1)
    if(all(vals.expect %in% bkfram.df$stock_id) &
       all(bkfram.df$stock_id %in% vals.expect)){
      cli::cli_alert_success("  Appropriate `StockID` values present in BackwardsFRAM table.")
    }else{
      cli::cli_alert_danger("  Expected StockID {sort(vals.expect)} in BackwardsFRAM table, but had StockID {sort(bkfram.df$stock_id)}")
      error.count = error.count + 1
    }
    ## check that our flags are right
    if(all(bkfram.df$target_flag[bkfram.df$stock_id %% 3 == 0] == 0)){
      cli::cli_alert_success("  TargetFlag set to 0 when appropriate")
    } else {
      cli::cli_alert_danger("  The first row associated with each new stock-pair must be the sum of marked and unmarked, and must have TargetFlag value of 3.")
      error.count = error.count + 1
    }
    if(all(bkfram.df$target_flag[bkfram.df$stock_id %% 3 != 0] == 3)){
      cli::cli_alert_success("  TargetFlag set to 3 when appropriate")
    } else {
      cli::cli_alert_danger("  The second and third row associated with each new stock-pair must be the marked and unmarked targets, and must have TargetFlag values of 3.")
      error.count = error.count + 1
    }
    ## check summation
    bk.sum = bkfram.df |>
      dplyr::filter(target_flag == 3) |>
      dplyr::group_by(stock.pop.id) |>
      dplyr::summarize(target_esc_age3 = sum(target_esc_age3),
                       target_esc_age4 = sum(target_esc_age4),
                       target_esc_age5 = sum(target_esc_age5)
      )
    sum.dif = bk.sum - (bkfram.df |>
                          dplyr::filter(target_flag == 0) |>
                          dplyr::select(stock.pop.id, target_esc_age3, target_esc_age4, target_esc_age5))
    sum.dif$stock.pop.id = (bkfram.df |>
                              dplyr::filter(target_flag == 0) |> dplyr::pull(stock.pop.id))
    if(sum(sum.dif |> dplyr::select(-stock.pop.id)) == 0){
      cli::cli_alert_success("  Unmarked and marked stock targets are summing to the combined target.")
    }else{
      cli::cli_alert_danger("  Unmarked and marked stock targets are not summing to the combined target.")
      cli::cli_alert_danger("  The following shows differences between expected sum and actual sum by stock (non-zeroes are problems)")
      print(sum.dif)
      error.count = error.count + 1
    }



    ## BaseExploitationRate
    cli::cli_text()
    cli::cli_alert("Checking BaseExploitationRate table...")
    bper.df = fetch_table(con, "BaseExploitationRate") |>
      dplyr::filter(base_period_id == bp.id)
    error.count = error.count + stock_id_comp("BaseExploitationRate", bper.df)
    cli::cli_alert("  Cannot easily check that no stock-age-fishery-timestep values are missing.")
    bper.sum = bper.df |>
      dplyr::filter(stock_id > old.stockcount) |>
      dplyr::group_by(stock_id) |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::mutate(lab = paste0("Stock ", stock_id, ": ", n, " records"))
    cli::cli_text("  Most common number of records across stock: {tail(table(bper.df$stock_id),1)[[1]]}")
    cli::cli_text("  Records for new stock:")
    cli::cli_bullets(setNames(bper.sum$lab, rep("*", nrow(bper.sum))))


    ## Growth
    cli::cli_text()
    cli::cli_alert("Checking Growth table...")
    growth.df = fetch_table(con, "Growth")
    error.count = error.count + stock_check_helper("Growth", NumStk = NumStk, stock.vec = growth.df$stock_id,
                                                   uniques.only = TRUE)
    error.count = error.count + stock_id_comp("Growth", growth.df)

    ## MaturationRate
    cli::cli_text()
    cli::cli_alert("Checking MaturationRate table...")
    mat.df = fetch_table(con, "MaturationRate")
    stock_id_comp("MaturationRate", mat.df)
    mat.df = mat.df |> dplyr::filter(stock_id > old.stockcount)
    ## Checking for stock-age-timestep combinations
    ts.df = fetch_table(con, "TimeStep")
    mat.df$comp = paste0("StockID: ", mat.df$stock_id, " Age: ", mat.df$age, " TimeStep: ", mat.df$time_step)
    comp.df = tidyr::expand_grid(stock_id = (old.stockcount+1):NumStk,
                                 age = baseid.df$min_age:baseid.df$max_age,
                                 time_step = ts.df$time_step_id)
    comp.df$comp = paste0("StockID: ", comp.df$stock_id, " Age: ", comp.df$age, " TimeStep: ", comp.df$time_step)
    if(all(mat.df$comp %in% comp.df$comp) &
       all(comp.df$comp %in% mat.df$comp)){
      cli::cli_alert_success("  All appropriate stock-age-timestep combinations represented in MaturationRate table.")
    }else{
      cli::cli_alert_danger(c("  Missing stock-age-timestep combinations:",
                              setNames(setdiff(comp.df$comp, mat.df$comp), rep("*", length(setdiff(comp.df$comp, mat.df$comp)))),
                              "Unexpected stock-age combinations:",
                              ifelse(length(setdiff(mat.df$comp, comp.df$comp))>0,
                                     setNames(setdiff(mat.df$comp, comp.df$comp), rep("*", length(setdiff(mat.df$comp, comp.df$comp)))),
                                     " (none)") ))
      error.count = error.count + 1
    }
    disconnect_fram_db(con)
    cli::cli_text()
    cli::cli_rule()
    cli::cli_text()
    cli::cli_div(theme = list(span.strong = list(color = "blue")))
    cli::cli_text("{.strong Evaluation complete:}")
    if(error.count == 0){
      cli::cli_text()
      cli::cli_alert_success("No issues detected! Great work!")
      cli::cli_text()
    } else {
      cli::cli_alert_danger("{error.count} issues detected")
    }
  }
}

stock_id_comp = function(table.name, df, stock.ref = stock.df$stock_id){
  #helper function to check that stock id exist in the Stock database
  if(all(unique(df$stock_id) %in% stock.ref)){
    cli::cli_alert_success("  Stock IDs match those of the Stock table.")
    return(0)
  }else{
    cli::cli_alert_danger("  Unexpected StockID entry(s) in {table.name} table: {setd(unique(df$stock_id), stock.ref)}. This ID is not present in the Stock table.")
    return(1)
  }
}

stock_age_checker = function(table.name, NumStk, old.stockcount, df, min.age = baseid.df$min_age,
                             max.age = baseid.df$max_age){
  #helper function to check that each relevant stock-age combination is present in the newly added stock.
  ## internal use only. Note that it generates the complete combo of stock x age combinatoins based on the min_age and max_age from the base_period table.
  df = df |>
    dplyr::filter(stock_id > old.stockcount) |>
    tidyr::unite("stock.age", stock_id:age, sep = " age ")
  df.comp = tidyr::expand_grid(stock_id = (old.stockcount+1) : NumStk,
                               age = min.age : max.age) |>
    tidyr::unite("stock.age", stock_id:age, sep = " age ")
  if(all(df$stock.age %in% df.comp$stock.age) &
     all(df.comp$stock.age %in% df$stock.age)){ ## not missing anything
    cli::cli_alert_success("  All appropriate stock-age combinations represented in {table.name} table.")
    return(0)
  }else{
    cli::cli_alert_danger("  Missing stock-age combinations {setd(df.comp$stock.age, df$stock.age)}. Unexpected stock-age combinations: {setd(df$stock.age, df.comp$stock.age)}")
    return(1)
  }
}

stock_check_helper = function(table.name, NumStk, stock.vec, uniques.only = FALSE){
  ## internal use only. Streamlines code above. if 'uniques.only = TRUE', warns about duplicate stock ID entries (useful for Stock and Growth tables.)
  cur.err = 0
  if(uniques.only){
    if(length(stock.vec) == NumStk){
      cli::cli_alert_success("  One entry per stock.")
    } else {
      cli::cli_alert_danger("  Duplicate StockID detected! Look for duplicate StockID(s) {paste0(unique(stock.vec[duplicated(stock.vec)]), collapse = ', ')}")
      cur.err = cur.err + 1
    }
  }

  if(length(unique(stock.vec)) == NumStk){
    cli::cli_alert_success("  Number of stock in {table.name} table matches NumStk ({NumStk})")
  }else{
    cli::cli_alert_danger("  Number of stock in {table.name} table ({length(unique(stock.vec))}) must match NumStk ({NumStk}). Did you update the BaseID table `NumStock` column? Did you add the new stock to the {table.name} table?")
    cur.err = cur.err + 1
  }
  ## checking that stock numbers are sequential
  if(all(unique(stock.vec) %in% 1:NumStk)){
    cli::cli_alert_success("  StockID appears to be sequential (1:{NumStk})")
  }else{
    cli::cli_alert_danger("  StockID in {table.name} table is not sequential -- missing StockID {paste0(setdiff(1:NumStk, unique(stock.vec)), collapse = ', ')}, have unexpected StockID {paste0(setdiff(unique(stock.vec), 1:NumStk), collapse = ', ')}")
    cur.err = cur.err + 1
  }
  return(cur.err)
}

setd = function(a, b){
  ## minor helper function, largely superfluous. Turns out cli:: functions that support glue
  ## will do the collapsing automatically. The above could be refactored to avoid this function.
  paste0(setdiff(unique(a), b), collapse = ', ')
}
