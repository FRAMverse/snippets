## Visualize von Bertalanffy growth modeled for chinook in FRAM
## Two pieces to check:
##   units of size
##   handling of start of age / start of months

## Consideration: comparing two or more stock?



library(tidyverse)

vb_fun = function(Linf, k, t0, t){
  Linf * (1 - exp(-k * (t - t0)))
}

vb_growth_chin = function(Linf, k, t0, 
                          thresh = 0.95){ #thresh is used for calculating age where size is thresh*100 % of assymptotic size
  
  # Calculate the growth curve
  dat = data.frame(t = seq(4, 12*5+4, by = .01)) #assuming age counting starts in TS 2, and that start of Jan = 0
  dat$size = vb_fun(Linf, k, t0, dat$t)
  dat = dat |> 
    filter(size >= 0)
  
  ## calculate sizes for each time step
  df.means = expand_grid(data.frame(ts = c("2", "3", "4"),
                                    mo.center = c(5, 7.5, 12)),#12 = Dec 31st. Technically probs want Jan 1, but this makes handling years easier
                         age = 1:4
  )
  df.means$mo = df.means$age*12 + df.means$mo.center
  df.means$size = vb_fun(Linf, k, t0, df.means$mo)
  
  
  ## print out age where weight is 99% of assymptotic 
  t.horiz = t0 - log(1 - thresh)/k
  cli::cli_alert(paste0("Hypothetical assymptotic size:  ", round(Linf/1000, 2), " meters"))
  cli::cli_alert(paste0("Hypothetically reaches ", round(thresh*100, 0), "% of assymptotic size in ", round(t.horiz/12, 1), " years"))
  
  ## generate material to color the background by age status
  df.age = data.frame(x = (0:4)*12 + 4 + 6, ## assuming the year starts May 1, Jan = 0
                      y = rep(200, 5),
                      col = factor(c("Fingerlings", "year 2", "year 3", "year 4", "year 5"),
                                   levels = c("Fingerlings", "year 2", "year 3", "year 4", "year 5"))
  )  
  age.palette <- c("#E69F00", "#56B4E9", "#009E73",
                   "#D55E00", "#0072B2")
  
  
  ggplot(dat)+
    geom_tile(data = df.age, aes(x = x, y = y, fill = col), height = Inf, alpha = 0.15)+
    geom_path(aes(x = t, y = size), linewidth = 0.8)+
    geom_point(data = df.means, aes(x = mo, y = size, shape = ts), size = 3.5, fill = 'white')+
    xlab("Time (months)")+
    ylab("Size (mm)")+
    scale_shape_manual(name = "Time step", values = c(21, 24, 22))+
    scale_fill_manual(name = "Age", values = age.palette)+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(limits = c(0, NA), expand = c(0,0))+
    theme_classic(base_size = 18)+
    labs(title = "von Bertalanffy growth model", subtitle = "Points = average size at timestep")
}

vb_growth_chin(1013, 0.035, 7.4) ## parms grabbed from BP Marked SPS Hatch year
