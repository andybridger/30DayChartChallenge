#30DayChartChallenge 
#Day 4 | magical
#Viz: @AndyBridger
#Inspiration for glowing chart: @lenkiefer

#download packages if needed
list.of.packages <- c("ggplot2", "readxl", 'data.table', 'ggfx', 'tidyverse',
                      'readr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load packages
lapply(c("ggplot2", "readxl", 'data.table', 'ggfx', 'tidyverse',
         'readr'), require, character.only = TRUE)

# load chocolate export data
urlfile="https://raw.githubusercontent.com/andybridger/30DayChartChallenge/main/day04/day04.csv"
d_choccy <-read_csv(url(urlfile))
View(d_choccy)

# load libraries ----
devtools::install_github("andybridger/aigtheme")
library(aigtheme)
devtools::install_github("lenkiefer/darklyplot")
library(darklyplot)


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
       caption = "
Viz: @AndyBridger | Inspiration: @lenkiefer | Data: Australian Bureau of Statistics
Note: Data are SITC code 073 - 'Chocolate and other food preparations containing cocoa, not elsewhere specified.'")
ch_choccy

#save plot
ggsave('charts/day04.png',
       width = 6.4,
       height = 4.5,
       scale = 3,
       units = 'cm')


