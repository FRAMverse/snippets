library(gratia)
library(tidyverse)
## Function to replace gratia::draw() when plotting
## smooths across doy term. Provides more sane x limits,
##   and gives histogram of data coverage rather than nothing or super time-consuming and very limited rugplots
#   Most of the work is done by `augment_drawn_doy_panel()` (below)
## Could be generalized to handle variable X axis terms, 
##   although would need to toggle between geom_col and geom_histogram() if we wanted to consider both discrete and continuous cases.

draw_augmented_doy = function(object, 
                              maintain_limits = FALSE,
                              ...){ ## additional gratia terms. Note that rug is replaced here
  orig = gratia::draw(object, rug = FALSE, ...)
  for(i in 1:length(orig)){
    orig[[i]] = augment_drawn_doy_panel(orig[[i]], dat.orig = object$model, maintain_limits = maintain_limits)
  } 
  return(orig)
}

augment_drawn_doy_panel = function(cur.panel, #panel from draw.gam, accessed with [[1]], etc
                                   dat.orig, # original data used in fitting model
                                   maintain_limits = FALSE){
  ## function to (a) give more reasonable limits of the day of year based on
  ## the existing data, and
  ## (b) add a histogram of data coverage under the primary plot, aligned and 
  ## sharing axis information
  ## 
  ## Dev notes:
  ## This is tailored specifically to nisqually creel bias investigation. Some of 
  ## this code would need to be revised to make it more generalizable. Notably, 
  ## the histogram only makes sense when x values of are on a discrete basis,
  ## and hard-codoing "doy" but the x axis might be any sort of term. 
  ## Also, may not handle multiple by dimensions
  facet_by = cur.panel$data$.by[1]
  
  dat.lims = dat.orig |> 
    group_by(.data[[facet_by]]) |> 
    summarize(lower_doy = quantile(doy, 0.01),
              upper_doy = quantile(doy, 0.99)) |> 
    ungroup()
  ## 
  lims.original = layer_scales(cur.panel)$x$range$range
  
  
  
  cur.lims = dat.lims |> filter(.data[[facet_by]] == cur.panel$data$mark_stage[1])
  ## update 
  cur.panel$data = cur.panel$data |> filter(doy >= cur.lims$lower_doy, doy<= cur.lims$upper_doy)
  
  top_panel = cur.panel + 
    labs(subtitle = paste0(cur.panel$labels$subtitle, "    |    ", cur.panel$labels$caption[1])) +
    labs(caption = "")+
    theme(plot.margin = margin(b = 0, unit = "pt"))
  if(maintain_limits){top_panel = top_panel + xlim(lims.original)}
  
  ## lower_panel
  dat.rugs = dat.orig |> 
    group_by(.data[[facet_by]], doy) |> 
    summarize(n = n(), .groups = "drop") |> 
    ungroup()
  
  hist1 = dat.rugs |> 
    filter(.data[[facet_by]] == "AD X Adult") |> 
    ggplot(aes(x = doy, y = n))+
    scale_y_continuous(n.breaks = 2)+
    xlim(layer_scales(cur.panel)$x$range$range)+
    geom_col(na.rm = TRUE)
  if(maintain_limits){hist1 = hist1 + xlim(lims.original)}
  
  lower_panel = hist1 + theme(plot.margin = margin(t = -5, unit = "pt"))
  
  
  top_panel / 
    lower_panel + 
    plot_layout(heights = c(10,1), axis_title = "collect", axes = "collect_x")
}

## Example use --------------------------
library(mgcv)
library(gratia)
## model:
## This is based on creel data. Key aspects: doy is the day of year from each sample, mark_stage is a fish classification predictor (factor), and yearfac is year of survey (as a factor).
out = gam(fish_count ~ mark_stage +
            s(doy, by = mark_stage, k = 20) + 
            yearfac,
          method = "REML",
          family = "nb",
          data = dat.fit)

## default plotting:
gratia::draw(out)
## if we have large datasets, the rugplots will eat up minutes of plotting time. 
gratia::draw(out, rug = FALSE)
## improved version:
draw_augmented_doy(out)
