## functions for use in pairs()
panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE, breaks=20)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
panel.cor <- function(x, y,
                      digits = 2,
                      prefix = "",
                      # cex.cor,
                      ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="complete.obs"))
  rsigned=cor(x, y, use="complete.obs")
  p = cor.test(x, y, use="complate.obs")$p.value
  txt <- format(c(rsigned, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  cex.cor <- 0.8/strwidth(txt)
  col="darkgrey"
  if(p<.05){col="blue"}
  text(0.5, 0.5, txt, cex = 2.5, col=col)
  text(.5,.1, paste("p <",signif(p,2)),cex=1.8)
}

## Example from Edwards et al. 2023:
pairs(dat.pairplot,
      lower.panel=panel.cor,
      diag.panel=panel.hist,
      cex=1,
      cex.labels = 2.8,
      cex.axis = 2,
      labels=c("% Carbon", 
               "cardenolide", 
               "latex",
               "% Nitrogen",
               "toughness",
               "trichome")
)