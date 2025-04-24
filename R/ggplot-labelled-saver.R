library(labelled)
library(tidyverse)
library(ggplot2)
library(here)

######################################
## FUNCTIONS
######################################

## function to make tibble describing the variables used in a layer
## (helper function for get_ggplot_vars_df)
layer_to_df <- function(layer){
  df <- data.frame(
    aesthetic = names(layer$mapping),
    mapping = sapply(layer$mapping, rlang::expr_text),
    stringsAsFactors = FALSE
  ) 
  if(nrow(df)>0){
    df <- df |> 
      dplyr::mutate(variable = gsub("~", "", .data$mapping),
                    variable = gsub(".*[(]", "", .data$variable),
                    variable = gsub(")", "", .data$variable))
    label_vec = unlist(labelled::get_variable_labels(layer$data))
    if(is.null(label_vec)){
      label_vec = rep(NA_character_, ncol(layer$data))
      names(label_vec) = names(layer$data)
    }
    if(is.null(layer$constructor)){
      constructor = "plot defaults"
    } else{
      constructor = gsub(",.*", "", toString(layer$constructor))
    }
    df <-  df |> 
      dplyr::mutate(label = label_vec[.data$variable]) |> 
      dplyr::mutate(layer = constructor)
  }
  return(df)
}

## function to make tibble describes the variables used in all layers 
## (and the global layer) of a ggplot object
get_ggplot_vars_df <- function(p) {
  ## find the global variables provided in ggplot() call
  global_vars <- layer_to_df(p)

  ## find variables defined specifically in any of the layers
  layer_vars <- lapply(p$layers, layer_to_df)
  
  ## combine, select, rearrange
  res <- dplyr::bind_rows(global_vars, layer_vars)
  if(nrow(res)>0){
    res <- res |> 
    dplyr::select("layer", "aesthetic", "variable", "label")
  rownames(res) <- NULL
  }
  return(res)
}

## print an individual row. Helper function for ggsave_descriptions
row_printer = function(x, file){
  cat("\nAesthetic ", x[1], " --> data variable ", x[2], "\n", file = file, append = TRUE)
  cat("Description: ", x[3], "\n", file = file, append = TRUE)
}

## Key function: alternative to ggsave that also saves metadata
ggsave_descriptions <-  function(filename, #standard ggsave behavior
                                 plot = last_plot(), ## standard ggsave behavior
                                 caption = "", ## optional caption to save in metadata file
                                 verbose = TRUE, ## print warnings for aesthetics lacking descriptions?
                                 ...){
  ggplot2::ggsave(filename, plot, ...)
  gg_meta = get_ggplot_vars_df(plot)
  meta_name = paste0(gsub("[.].*", "", filename), "_META.txt")
  
  if(any(is.na(gg_meta$label)) & verbose){
    meta_missing = gg_meta |> 
      dplyr::filter(is.na(.data$label))
    cli::cli_alert_warning("The following variables do not have descriptions in their corresponding data:")
    ul <- cli::cli_ul()
    for(i in 1:nrow(meta_missing)){
      cli::cli_li("In {meta_missing$layer[i]}: aesthetic {meta_missing$aesthetic[i]} which comes from data variable {meta_missing$variable[i]}")
    }
    cli::cli_end(ul)
  }
  
  ## handling saving
  gg_nest <- gg_meta |> 
    nest(.by = layer)
  cat(as.character(Sys.time()), file = meta_name, append = FALSE)
  cat("\n\n", "CAPTION", "\n================\n", caption, file = meta_name, append = TRUE)
  cat("\n\n\n", "PLOT COMPONENT DESCRIPTIONS", "\n================", file = meta_name, append = TRUE)
  
  for(i.row in 1:nrow(gg_nest)){
    cat("\n\n", gg_nest$layer[i.row], "\n--------------", file = meta_name, append = TRUE)
    apply(as.data.frame(gg_nest$data[i.row]), 1, row_printer, file = meta_name)
  }
}

###################################################
## EXAMPLES
###################################################

##make example lookup table
lut = tibble(variable = c("x", "y", "alpha"),
             variable_label = c("range (miles)", "reach (mm^2)",  "Important nuance when handling this variable! This is in inverse units compared to what is expected! Check the FRAM documentation for details"))

## convert to named vector
dat_labels <- deframe(lut)

## create dataset
dat1 = data.frame(x = rnorm(100),
                  y = rnorm(100)*.5) 
## filter named vector, add as labels
labels_use = dat_labels[names(dat_labels) %in% names(dat1)] 
dat1 <- dat1 |> 
  set_variable_labels(!!!labels_use)

## create second dataset
dat2 = data.frame(z = 1:10,
                  alpha = 10:1)
## filter named vector, add as labels
labels_use = dat_labels[names(dat_labels) %in% names(dat2)] 
dat2 <- dat2 |> 
  set_variable_labels(!!!labels_use)

## make a ggplot object using both layers
gp = ggplot(dat1, aes(x = x , y = y))+
  geom_point()+
  geom_path(data = dat2, aes(x = z, y = alpha))

ggsave_descriptions(filename = here("test.png"), 
                    caption = "A test with two plot layers using different data. Most variables used have labels.",
                    plot = gp)

## make a simple ggplot object with no descriptions to ensure code doesn't break
gp2 = ggplot(mtcars, aes(x = disp, y = mpg))+
  geom_point()

ggsave_descriptions(filename = here("test2.png"), 
                    caption = "A test with no descriptions",
                    plot = gp2)
