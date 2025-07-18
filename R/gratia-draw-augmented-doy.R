library(gratia)
library(tidyverse)
library(patchwork)
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
  
  
  
  cur.lims = dat.lims |> filter(.data[[facet_by]] == cur.panel$data[[facet_by]][1])
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
    filter(.data[[facet_by]] == cur.panel$data[[facet_by]][1]) |> 
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

## Reworking Tensor Product smooth plots:
draw_gratia_tensor_doy <- function(object, ##
                                   variable_greater, ## primary facet variable
                                   variable_lesser = "yearfac", ## secondary facet variable OR color variable (if cis = FALSE)
                                   cis = TRUE){ #if FALSE, make plots with multiple curves on same panel (colored by variable_lesser, faceted by variable_greater)
  dat.temp = gratia::draw(object)[[1]]$data
  
  ## identify limits
  dat.lims = out$model |> 
    summarize(lower_doy = quantile(doy, 0.01),
              upper_doy = quantile(doy, 0.99)) |> 
    ungroup()
  
  ## make sure variables are present
  var_names = grep("^[.]", names(dat.temp), value = TRUE, invert = TRUE)
  var_names = grep("[(]", var_names, value = TRUE, invert = TRUE)
  if(!((variable_lesser %in% var_names) & 
       (variable_greater %in% var_names))){
    cli::cli_abort("`variable_lesser` and/or `variable_greater` not present in fitted model. Available terms include {var_names}.")
  }
  
  if(cis == TRUE){
    res = dat.temp |> 
      filter(doy >= dat.lims$lower_doy,
             doy <= dat.lims$upper_doy) |> 
      ggplot(aes(x = doy, y = .estimate))+
      geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
                  fill = "gray",alpha = 0.4)+
      geom_path(linewidth = .8)+
      facet_wrap(reformulate(variable_lesser, variable_greater),
                 nrow = length(unique(dat.temp[[variable_greater]])))+
      theme_bw(base_size = 13)
  } else {
    res = dat.temp |> 
      filter(doy >= dat.lims$lower_doy,
             doy <= dat.lims$upper_doy) |> 
      ggplot(aes(x = doy, y = .estimate, col = .data[[variable_lesser]]))+
      geom_path(linewidth = .8)+
      facet_wrap(.~.data[[variable_greater]])+
      theme_bw(base_size = 13)
  }
  return(res)
}

## Example use --------------------------
library(mgcv)
library(gratia)
library(tidyverse)
## model:
## This is based on creel data. Key aspects: doy is the day of year from each sample, mark_stage is a fish classification predictor (factor), and yearfac is year of survey (as a factor).

##simulating:

n.per.year = 60
years = 2021:2023
doy = sample(200:300, size = n.per.year*length(years), replace = TRUE)
samples_per_day = sample(4:10, size = length(doy), replace = TRUE)

year.vec = rep(years, times = n.per.year)

data = expand_grid(data.frame(doy = rep(doy, times = samples_per_day),
                              year = rep(year.vec, times = samples_per_day)),
                   mark_stage = as.factor(c("Jack", "AD Adult", "UM Adult"))) |> 
  mutate(yearfac = as.factor(year))

## Generating effects of year
coefs_yearfac_real = data.frame(yearfac = unique(data$yearfac))
coefs_yearfac_real$doy_mod_yearfac = rnorm(3, sd = 10)
coefs_yearfac_real$amp_mod_yearfac = runif(3, min = 0.8, max = 1.2)

## effects effects of mark_stage
coefs_mark_stage_real = data.frame(mark_stage = unique(data$mark_stage))
coefs_mark_stage_real$doy_mod_mark_stage = rnorm(3, sd = 5)
coefs_mark_stage_real$amp_mod_mark_stage = runif(3, min = 0.6, max = 1.4)
## make UM and AD smaller
coefs_mark_stage_real$amp_mod_mark_stage[coefs_mark_stage_real$mark_stage == "UM Adult"] = 
  coefs_mark_stage_real$amp_mod_mark_stage[coefs_mark_stage_real$mark_stage == "UM Adult"] *.5

coefs_mark_stage_real$amp_mod_mark_stage[coefs_mark_stage_real$mark_stage == "Jack"] = 
  coefs_mark_stage_real$amp_mod_mark_stage[coefs_mark_stage_real$mark_stage == "Jack"] *.3

middle.doy = 250


## generate data itself
data.full = data |> 
  left_join(coefs_mark_stage_real, by = "mark_stage") |> 
  left_join(coefs_yearfac_real, by = "yearfac") |>
  mutate(amplitude_eff = amp_mod_mark_stage * amp_mod_yearfac,
         doy_eff = doy_mod_mark_stage + doy_mod_yearfac) |> 
  mutate(expectation = dnorm(doy, 
                             mean = middle.doy + doy_eff, 
                             sd = 15) * amplitude_eff*200) |> 
  mutate(fish_count = rpois(length(expectation), lambda = expectation))

# ggplot(data.full, aes(x = doy, y = expectation, col = mark_stage))+
#   geom_point()+
#   facet_wrap(.~ yearfac)+
#   ggtitle("Expected catch per sample")
# 
# ggplot(data.full, aes(x = doy, y = fish_count, col = mark_stage))+
#   geom_point()+
#   facet_wrap(.~ yearfac)+
#   ggtitle("realized catch per sample")

## 
## 
out = gam(fish_count ~ mark_stage +
            s(doy, by = mark_stage, k = 20) + 
            yearfac,
          method = "REML",
          family = "nb",
          data = data.full)

## default plotting:
gratia::draw(out)
## if we have large datasets, the rugplots will eat up minutes of plotting time. 
gratia::draw(out, rug = FALSE)
## improved version:
draw_augmented_doy(out)


## tensor product model:
out = gam(fish_count ~ te(doy, mark_stage, yearfac, bs = c('tp', 'fs', 'fs')),
          method = "REML",
          family = "nb",
          data = data.full)

draw_gratia_tensor_doy(out, variable_greater = "mark_stage", cis = TRUE)
# What if we want to just overlay the point estimates?
draw_gratia_tensor_doy(out, variable_greater = "mark_stage", cis = FALSE)
## let's swap yearfac and mark_stage in terms of priority:
draw_gratia_tensor_doy(out, variable_greater = "yearfac",
                       variable_lesser = "mark_stage",
                       cis = FALSE)



