## Template for plotting regs

## make example data
regs.df = data.frame(start = as.Date(c("5/1/2022", "7/5/2022", "8/16/2022", "5/1/2022"), format = c("%m/%d/%Y")),
                     end = as.Date(c("6/30/2022", "7/20/2022", "9/28/2022", "4/30/2023"), format = c("%m/%d/%Y")),
                     area = c(5, 5, 5, 10))

## make a factored version of areas with levels in convenient order for plotting
regs.df$area.lab = droplevels(factor(regs.df$area, levels = rev(c(1:7, 81, 82, 9:13))))

ggplot(regs.df, aes(y = area.lab, col = area.lab))+ ## For data with multiple fishery statuses 
  ##  (e.g. `fishery = c("NR", "MSF"...)` column), consider coloring by this status variable,
  ##  and removing the theme(legend.position = "none") part below.
  geom_segment(aes(x = start, xend = end), linewidth = 4)+
  theme_bw(base_size = 24)+
  ylab("Area")+
  scale_x_date(date_breaks = "2 months",
               date_minor_breaks = "1 months",
               date_labels = "%b %Y",
               limits = c(as.Date("2022-5-1"), ## optional, to present a single FRAM season; update year as appropriate
                          as.Date("2023-4-30")))+
  xlab("")+
  ggtitle("Title")+
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())


