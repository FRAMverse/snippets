## prototyping describing data with the labelled package, and carrying those over to 

## helper function: get variable descriptions from ggplot layers (or primary ggplot)
get_local_descriptions = function(gobj, #ggobject. Either the whole plot or a layer of the plot
                                  bigdat){ ## the primary data, in object$data. Used when there is no locally provided data for this layer){ ## either "default" or  class(object$layer$geom)[1]
  if("constructor" %in% names(gobj)){
    layer_name = as.character(gobj$constructor)[1]
  } else {
    layer_name = "default"
  }
  
  if(is.null(gobj$mapping) & layer_name != "default"){
    res = data.frame(layer = layer_name,
                     plot_variable = "<uses defaults>",
                     data_name = "-", 
                     description = "-")
  }  else {
    res = NULL
    for(cur.name in names(gobj$mapping)){
      cur.var = as.character(rlang::quo_get_expr(gobj$mapping[[cur.name]]))
      if("waiver" %in% class(gobj$data)){
        cur.label = labelled::var_labels(bigdat[[cur.var]])
      }else{
        cur.label = labelled::var_label(gobj$data[[cur.var]])
      }
      if(is.null(cur.label)){cur.label = NA_character_}
      res = rbind(res, data.frame(layer = layer_name, 
                                  plot_variable = cur.name,
                                  data_name = cur.var,
                                  description = cur.label)
      )
    }
  }
  return(res)
}

## get all variable descriptions from ggplot object.
## Not designed to behave well with joint plots (e.g. {patchwork} objects)
get_gg_descriptions = function(ggplot_obj){
  res = get_local_descriptions(ggplot_obj,
                               bigdat = ggplot_obj$data)
  for(i in 1:length(ggplot_obj$layers)){
    res.cur = get_local_descriptions(ggplot_obj$layers[[i]],
                                       bigdat = ggplot_obj$data)
    res.cur$layer = paste0(res.cur$layer, "[", i, "]")
    res = rbind(res, res.cur)
  }
  return(res)
}

## Example useage:
library(labelled)
library(tidyverse)

## Start with baseline example: point plot of mtcars
data <-  mtcars |> 
  select(mpg, cyl)
## add labels, based on ?mtcars. using {labelled} functions
data <- data |> 
  set_variable_labels(mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders")

## We can get a description table with look_for()
data |> look_for()

## We can create a plot using this data, and descriptions will be included:
gp = ggplot(data, aes(x = cyl, y = mpg))+
  geom_point()
gp
## we can extract descriptions with get_gg_descriptions()
get_gg_descriptions(gp)

## the "layer" column identifies which layer we are referring to. "default" is the primary ggplot layer (ie, the variables defined in the ggplot() call itself). Each additional layer is labeled with the function call and the layer number (number is necessary if there are multiple calls of the same function, e.g. superimposing points from two data datasets).
## "plot_variable" column lists the ggplot variable name (e.g., x, y, col, fill, etc). Remember that if a layer needs a plot variable and it isn't defined in that layer, the default is used.
## "data_name" is the name in the data object itself. This is mostly useful for debugging. 
## "description" is the actual description defined for the data objects using the {labelled} package.
## If a layer does not define *any* variables in an aes() call, it is still added as a row to make 

## We can also track descriptions for more ggplot objects with multiple datasets and layers.
## For example, let's make a more complex plot that includes another layer that uses a different data object. We'll calculate the median mpg for each cylinder, and make a geom_path going through the medians.
dat_meds = data |> 
  group_by(cyl) |> 
  summarize(mpg_med = median(mpg))
## the description of "cyl" was inherited, but the new variable is undescribed:
dat_meds |> look_for()
dat_meds <- dat_meds |> 
  set_variable_labels(mpg_med = "Median mpg for that number of cylinders")

gp = ggplot(data, aes(x = cyl, y = mpg))+
  geom_point()+
  geom_path(data = dat_meds, aes(y = mpg_med))
gp

## get descriptions
get_gg_descriptions(gp)

## We can now use versions of ggsave(gp) and write_csv(get_gg_descirption(gp)) to save the plot and the variable metadata
