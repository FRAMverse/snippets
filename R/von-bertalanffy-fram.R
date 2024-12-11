## Visualize von Bertalanffy growth modeled for chinook in FRAM
## Updated based on development for growth report -- includes correction to 
## timesteps as well as additional calculations. Output is now a list with ggplot as
## well as some asymptotic behavior information


library(tidyverse)

vb_fun = function(Linf, k, t0, t){
  Linf * (1 - exp(-k * (t - t0)))
}

vb_growth_chin = function(Linf, k, t0,  ## Linf, k, and t0 are the arguments of the growth function.
                          thresh = 0.95,
                          include.exact.means = FALSE){ #thresh is used for calculating age where size is thresh*100 % of assymptotic size
  
  ## note: I calculated the "radius" of each time period with some algebra on paper, based
  ## on the distances between the midpoints
  ## ts 1 has a radius of 3.5 month, ts 2 has a radius of 1 month, ts 3 has a radius of 1.5 months
  df.ts = data.frame(ts = c("1", "2", "3"),
                     start.mo = c(3.5-3.5, 3.5+3.5, 8+1),
                     end.mo = c(3.5+3.5, 8+1, 3.5-3.5))
  # Calculate the growth curve
  dat = data.frame(t = seq(4, 12*5, by = .01)) #assuming age counting starts in TS 2, and that start of Jan = 0
  dat$size = vb_fun(Linf, k, t0, dat$t)
  dat = dat |> 
    filter(size >= 0) |> 
    mutate(age.agg = floor(t/12), # using this to aggregate ages, so fish w/months <1.5 are ts 3 of prev year. Will adjust that below.
           mo = t %% 12
    ) |> 
    mutate(ts = case_when(
      mo < df.ts$end.mo[1] & mo >= df.ts$start.mo[1] ~ df.ts$ts[1],
      mo < df.ts$end.mo[2] & mo >= df.ts$start.mo[2] ~ df.ts$ts[2],
      mo >= df.ts$start.mo[3] | mo < df.ts$end.mo[3] ~ df.ts$ts[3]
    ))
  
  dat$age.agg[dat$mo < df.ts$end.mo[3]] = dat$age.agg[dat$mo < df.ts$end.mo[3]] - 1
  
  
  ## calculate sizes for each time step
  df.means = expand_grid(data.frame(ts = c("1", "2", "3"),
                                    mo.center = c(3.5, 8, 10.5)),#12 = Dec 31st. Technically probs want Jan 1, but this makes handling years easier
                         age = 1:4
  )
  df.means$mo = df.means$age*12 + df.means$mo.center
  df.means$size = vb_fun(Linf, k, t0, df.means$mo)
  
  ## calculate the "exact" means, accounting for Jensen's annoying inequality.
  df.means.precise = dat |> 
    dplyr::rename(age = age.agg) |> 
    group_by(age, ts) |> 
    summarize(size.precise = mean(size))
  
  df.means = left_join(df.means, df.means.precise, by = c("age", "ts"))
  
  ## print out age where weight is 99% of assymptotic 
  t.horiz = t0 - log(1 - thresh)/k
  cli::cli_alert(paste0("Hypothetical assymptotic size:  ", round(Linf/1000, 2), " meters"))
  cli::cli_alert(paste0("Hypothetically reaches ", round(thresh*100, 0), "% of assymptotic size in ", round(t.horiz/12, 1), " years"))
  
  ## generate material to color the background by age status
  df.age = data.frame(x = (0:4)*12 + 6, ## assuming the year starts May 1, Jan = 0
                      y = rep(200, 5),
                      col = factor(c("Fingerlings", "year 2", "year 3", "year 4", "year 5"),
                                   levels = c("Fingerlings", "year 2", "year 3", "year 4", "year 5"))
  )  
  age.palette <- c("#E69F00", "#56B4E9", "#009E73",
                   "#D55E00", "#0072B2")
  
  if(!include.exact.means){
    gp = ggplot(dat)+
      geom_tile(data = df.age, aes(x = x, y = y, fill = col), height = Inf, alpha = 0.15)+
      geom_path(aes(x = t, y = size), linewidth = 0.8)+
      geom_point(data = df.means, aes(x = mo, y = size, shape = ts), size = 3.5, fill = 'white')+
      xlab("Time (months)")+
      ylab("Length (mm)")+
      scale_shape_manual(name = "Time step", values = c(21, 24, 22))+
      scale_fill_manual(name = "Age", values = age.palette)+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(limits = c(0, NA), expand = c(0,0))+
      theme_classic(base_size = 18)+
      labs(title = "von Bertalanffy growth model", subtitle = "Points = average size at timestep")
  }else{
    gp = ggplot(dat)+
      geom_tile(data = df.age, aes(x = x, y = y, fill = col), height = Inf, alpha = 0.15)+
      scale_fill_manual(name = "Age", values = age.palette)+
      new_scale_fill()+
      geom_path(aes(x = t, y = size), linewidth = 0.8)+
      geom_point(data = df.means, aes(x = mo, y = size.precise, shape = ts, fill = "Exact"), 
                 size = 3.5)+
      geom_point(data = df.means, aes(x = mo, y = size, shape = ts, fill = "Current"), 
                 size = 3.5)+
      scale_fill_manual(name = "Method of\naveraging", values = c("white", "gray70"))+
      guides(fill = guide_legend(override.aes = list(shape = 22)))+
      xlab("Time (months)")+
      ylab("Length (mm)")+
      scale_shape_manual(name = "Time step", values = c(21, 24, 22))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(limits = c(0, NA), expand = c(0,0))+
      theme_classic(base_size = 18)+
      labs(title = "von Bertalanffy growth model", subtitle = "Points = average size at timestep")
  }
  return(list(fig = gp, size.means = df.means,
              size.asymtotic = Linf,
              years.to.thoriz = round(t.horiz/12, 1)))
}

vb_growth_chin(1013, 0.035, 7.4) ## parms grabbed from BP Marked SPS Hatch year


## functions for caclulation proportion legal:

## helper function to calculate proportion legal from a given mean size, cv, and limit size.
limit_calc = function(size, cv, size.limit){ #size is the mean size, cv is the associated cv
  df.calc = data.frame(size.check = seq(0,4000, by = 0.1)) |> 
    mutate(density = dnorm(size.check, mean = size, sd = cv*size)*diff(size.check)[1])
  prop = df.calc |> filter(size.check >= size.limit) |> pull(density) |> sum()
  return(prop)
}

## Calculate the proportion legal for a given fishery-stock based on growth paremeters
vb_legal_chin = function(Linf, k, t0, 
                         df.cv, ## dataframe with `$age` and `$cv` corresponding to age and coefficient of variation.
                         ## this can be pulled directly from the growth table of a FRAM database (and reorganized to long form).
                         size.limit, ## fishery size limit, in mm
                         include.exact.means = FALSE){ #thresh is used for calculating age where size is thresh*100 % of assymptotic size
  stopifnot(all(c("age", "cv") %in% names(df.cv)))
  ## note: I calculated the "radius" of each time period with some algebra on paper, based
  ## on the distances between the midpoints
  ## ts 1 has a radius of 3.5 month, ts 2 has a radius of 1 month, ts 3 has a radius of 1.5 months
  df.ts = data.frame(ts = c("1", "2", "3"),
                     start.mo = c(3.5-3.5, 3.5+3.5, 8+1),
                     end.mo = c(3.5+3.5, 8+1, 3.5-3.5))
  # Calculate the growth curve
  ## calculate sizes for each time step
  df.means = expand_grid(data.frame(ts = c("1", "2", "3"),
                                    mo.center = c(3.5, 8, 10.5)),#12 = Dec 31st. Technically probs want Jan 1, but this makes handling years easier
                         age = 1:4
  )
  df.means$mo = df.means$age*12 + df.means$mo.center
  df.means$size = vb_fun(Linf, k, t0, df.means$mo)
  df.means = df.means |> 
    select(-mo.center) |>
    filter(age != 1) |> 
    left_join(df.cv, by = "age")
  
  df.means$prop.legal = -99
  for(i in 1:nrow(df.means)){
    df.means$prop.legal[i] = limit_calc(size = df.means$size[i], cv = df.means$cv[i],
                                        size.limit = size.limit)
  }
  return(df.means)  
}

df.cv = structure(list(age = c(2, 3, 4, 5), cv = c(0.116650969723547, 
                                                   0.0967817586430925, 0.0769125475626379, 0.0570433364821832)), 
                  row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame"))

## example: growth parameters from Stilliguamish Fall Fingerlings, size limit for fishery 14.
vb_legal_chin(Linf = 951.9,
              k = 0.04035,
              t0 = 5.308,
              df.cv = df.cv,
              size.limit = 620
)
