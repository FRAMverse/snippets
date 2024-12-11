#Suite of useful colors for accessibility

library(tidyverse)
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                "#cc0000", "#000066", "#FF6666", "#666633", "#b3b3b3")

df = data.frame(height = rep(1, length(cbPalette)),
                ident = as.character(1:length(cbPalette)))
df$ident = factor(df$ident, levels = df$ident)
ggplot(df)+
  geom_col(aes(y = height,
               x = ident),
           fill = cbPalette)+
  ylab("")+
  xlab("Color order")+
  theme_minimal()+
  theme(panel.grid = element_blank())+
  theme(axis.text.y = element_blank())
