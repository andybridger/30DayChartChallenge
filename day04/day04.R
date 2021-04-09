# load libraries ----
library(data.table)
library(tidyverse)
library(readxl)
library(ggplot2)
library(aigtheme)
devtools::install_github("lenkiefer/darklyplot")
library(ggfx)
library(darklyplot)


#load data
d_choccy <- read_csv("data/day04.csv")
str(d_choccy)
view(d_choccy)

#make chart
ch_choccy <- ggplot(data=d_choccy, aes(x = date, y = mn, group =1)) +
  with_outer_glow(geom_line(color="white",size=1.1),colour="dodgerblue",sigma=15,expand=2)+
  darklyplot::theme_dark2()+
  theme(plot.caption=element_text(hjust=0))+
  theme(axis.title.y=element_blank(),axis.title.x = element_blank())+
  aig_y_continuous(limits=c(0,350),breaks = seq(0,350, by = 50))+
  labs(title = "Happy Easter! Australian chocolate exports hit a record high of
$338 million in 2020",
       subtitle = "Australian chocolate exports (A$mn), 12-month rolling sum",
       caption = "Viz: @AndyBridger | Inspiration: @lenkiefer | Data: Australian Bureau of Statistics
Note: Data are SITC code 073 - 'Chocolate and other food preparations containing cocoa,
not elsewhere specified.'")
ch_choccy

#save plot
ggsave('...charts/day04.png',
       width = 6.4,
       height = 4.5,
       scale = 3,
       units = 'cm')
