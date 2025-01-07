## Functions building on the `descriptions()` function of the `fmtr` package.
## Intended to streamline adding column metadata to dataframes; anticipated use in 
## future framrsquared / framrosetta updates.

library(fmtr)
library(tidyverse)

## updated version of "descriptions": if x is an individual vector, now appropriately returns descriptions attribute. 
descriptions <- function(x) {
  
  ret <- list()
  
  if(is.data.frame(x)){
    for (nm in names(x)) {
      if (!is.null(attr(x[[nm]], "description", exact = TRUE))) {
        ret[[nm]] <- attr(x[[nm]], "description", exact = TRUE)
      }
    }
  } else {
    if (!is.null(attr(x, "description", exact = TRUE))) {
      ret[["vector.description"]] <- attr(x, "description", exact = TRUE)
    }
  }
  return(ret)
  
}

## alternative version of `descriptions<-` that provides feedback on variables that are left undescribed,
## descriptions that are being overwritten.
`descriptions_verbose<-` <- function(x, value){
  if(any(duplicated(names(value)))){
    cli::cli_abort("`value` names must be unique!")
  }
  vars.overdescribed <- setdiff(names(value), names(x))
  
  if(length(vars.overdescribed) > 0){
    cli::cli_alert("The following variables are defined in descriptions list and not in dataframe: {vars.overdescribed}")
  }
  
  
  cur.descriptions = fmtr::descriptions(x)
  description.collisions = intersect(names(cur.descriptions), names(value))
  if(length(description.collisions) > 0){
    description.overwrites = data.frame(variable = description.collisions,
                                        original = do.call(c, cur.descriptions[description.collisions]),
                                        new = do.call(c, value[description.collisions]))
    description.overwrites = description.overwrites[description.overwrites$original != description.overwrites$new,]
    if(nrow(description.overwrites) > 0){
      updates.formatted = paste0("{.strong ", description.overwrites$variable, "}:  ", description.overwrites$original, " {.emph ->} ", description.overwrites$new)
      names(updates.formatted) = rep("*", length(updates.formatted))
      cli::cli_alert("The following descriptions are being updated")
      cli::cli_div(theme = list(span.emph = list(color = "cornflowerblue")))
      cli::cli_bullets(updates.formatted)
    }
  }
  fmtr::descriptions(x) <- value
  vars.undefined = setdiff(names(x), names(fmtr::descriptions(x)))
  if(length(vars.undefined)>0){
    cli::cli_alert_warning("The following variables are still undescribed: {vars.undefined}")
  } else {
    cli::cli_alert_success("All variables described")
  }
  return(x)
}

## ----------------------------------

dat = mtcars

## preliminary metadata copy-pasted from ?mtcars. Some mistakes present (see below)
mtcars.metadata <-  c(mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      hp = "Gross horsepower",
                      wt = "Rear axle ratio",
                      qseq = "1/4 mile time",
                      vs = "Engine (0 = V-shaped, 1 = straight)",
                      am = "Transmission (0 = automatic, 1 = manual)",
                      gear = "Number of forward gears",
                      carb = "Number of carburetors")

descriptions_verbose(dat) <- as.list(mtcars.metadata)
descriptions(dat)

## oops! We missed some descriptions. The feedback from descriptions_verbose helps us see what happened:
## missed "drat" and mistyped "qsec" as "qsec". Looks like we put the metadata for "drat" into the 
## "wt" column, so we need to overwrite that.
descriptions_verbose(dat) <- list(wt = "Weight (1000 lbs)",
                                  drat = "Rear axle ratio",
                                  qsec = "1/4 mile time"
)

descriptions(dat)

## Individual vectors of `dat` have the descriptions attribute; our updated `descriptions()`
## function can work for those vectors. 
descriptions(dat$mpg)