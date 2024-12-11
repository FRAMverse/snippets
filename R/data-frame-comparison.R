## Ty Garber's Tidyverse code to identify mismatches between 
## two data frames, especially useful for QA/QC


dat <- data.frame(
  SSN = c(23,434,565,878,231),  
  Name=c("hamburgers","butter","cheeze","coffee","teacher"),  
  Age = c(7,8,6,43,56),  
  Gender = c(0,1,0,1,0)
)

dat2 <- data.frame(
  SSN = c(210,345,456,745,245),  
  Name=c("fruits","cupcakes","mangoes","toffee","student"),  
  Number= c(3,5,5,6,77),  
  Different = c(0,0,1,1,0)
)

# order matters here, if that didn't matter join
# by something besides the row id
# if you're checking where data is present in one dataframe but not other 
# (rather than looking for differing values),
# use left join then filter on nulls
joined_tables <- dat |>
  rowid_to_column() |>
  left_join(
    dat2 |> rowid_to_column(),
    by = 'rowid'
  ) |> 
  relocate(starts_with(names(dat)), .after = rowid) ## if not using rowid, drop the `.after` argument

confl = unique(gsub("[.][xy]", "", grep("[.][xy]", names(joined_tables), value = TRUE)))
cat("Mismatches in:\n  ", paste0(confl, collapse = ", "))

# different SSN etc...
joined_tables |>
  filter(SSN.x != SSN.y)

joined_tables |>
  filter(Name.x != Name.y)

