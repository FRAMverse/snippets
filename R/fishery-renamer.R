## Function to relabel inconsistent fishery names in our databases
## with argument sep = TRUE, will return data frame with separate
## columns for the area and the the "class" (Net, Troll, Sport)
fishery_renamer = function(x, sep = FALSE){
  x = gsub("^A ", "Area ", x)
  x = gsub("^Ar ", "Area ", x)
  x = gsub(" Spt$", " Sport", x)
  x = gsub(" Sprt$", " Sport", x)
  x = gsub(" Trl$", " Troll", x)
  x = gsub("BCOutSport", "BCOut Sport", x)
  if(sep){
    x.class = gsub("^.* ", "", x)
    x.ident = gsub(pattern = " [^ ]*$", "", x)
    return(data.frame(full = x, 
                      area = x.ident,
                      class = x.class))
  }else{
    return(x)
  }
}