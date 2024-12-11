## tools to quickly check for common style errors in framrsquared

frs_stylecheck_assignment = function(filepath, n = Inf){
  cli::cli_text(cli::col_blue(paste("Checking", gsub(".*[/]", "", filepath), "for accidental uses of `=` for assignment")))
  cli::cli_text(cli::col_grey("Note that this is not perfect -- multi-line function calls which (correctly) use `=` for arguments
  will show up here, as will SQL calls and other edge cases."))
  df = read_lines(filepath) |> 
    as_tibble() |> 
    rename(line.entry = value)
  df$linenum = 1:nrow(df)
  df |> 
    mutate(before.parens = gsub("[(].*", "", line.entry)) |> 
    filter(str_detect(before.parens, "[^=]=[^=]")) |> 
    select(-before.parens) |> 
    print(n = n)
}

frs_stylecheck_assignment("C:/repo/framrsquared/R/copy.R")


frs_stylecheck_snakecase = function(filepath, n = Inf){
  cli::cli_text(cli::col_blue(paste("Checking", gsub(".*[/]", "", filepath), "for variables that are not named using snake_case.")))
  cli::cli_text(cli::col_grey("Note that this will also list single-word variables, which should be fine."))
  df = read_lines(filepath) |> 
    as_tibble() |> 
    rename(line.entry = value)
  df$linenum = 1:nrow(df)
  df |> 
    filter(str_detect(line.entry, "<-")) |> 
    mutate(variable.name = str_squish(gsub("<-.*", "", line.entry)),
           .before = line.entry) |> 
    filter(!str_detect(line.entry, "_")) |> 
    print(n = n)
}
  
frs_stylecheck_snakecase("C:/repo/framrsquared/R/copy.R")
  
  
  