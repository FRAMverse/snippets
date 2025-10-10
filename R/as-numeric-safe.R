as_numeric_safe = function(x, quiet = TRUE){
  attempt_numeric = as.numeric(x)
  ## did we add NAs by converting? If not, it's safe
  if(sum(is.na(attempt_numeric)) == sum(is.na(x))){
    return(attempt_numeric)
  }else{
    if(!quiet){
      new_nas = is.na(attempt_numeric) & !is.na(x)
      cli::cli_alert("Non-numeric information! Indice(s) {which(new_nas)} contain string(s) '{x[new_nas]}'")
    }
    return(x)
  }
}

library(tidyverse)
## Use case: we read an excel sheet in, and while many columns 
## effectively just contain numbers, the sheet had some rows
## of "---"  or "=" to visually separate chunks of numerics.
## We've already removed those rows, so our columns *should*
## have numbers in them. But there's the potential for important
## character notes in entries, and we don't want those to be converted
## to NAs as would happen if we used use.numeric

## set up hypothetical data
dat = mtcars |> 
  dplyr::mutate(across(disp:carb,
                ~ as.character(.x)))
dat$gear[3] = "see notes"
glimpse(dat)

for(cur_col in names(dat)){
  dat[[cur_col]] = as_numeric_safe(dat[[cur_col]], quiet = FALSE)
}
glimpse(dat)
## non-lossy conversion to numerics; warning message and character column make clear there is character-only information in column "gear"
