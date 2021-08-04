#30DayChartChallenge 
#Day 5 | slope
#Viz: @AndyBridger

#download packages if needed
list.of.packages <- c("ggplot2", "ggthemes", 'dplyr', 'tidyr', 'tidyverse',
                      'readr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load packages
lapply(c("ggplot2", "ggthemes", 'dplyr', 'tidyr', 'tidyverse',
         'readr'), require, character.only = TRUE)

#extra download for slope graph
install.packages("devtools")
devtools::install_github("ibecav/CGPfunctions")
library(CGPfunctions)

#load data
urlfile="https://raw.githubusercontent.com/andybridger/30DayChartChallenge/main/day05/day05.csv"
d_pay <-read_csv(url(urlfile))
View(d_pay)

#set theme
theme_slope <- theme_minimal(base_family = 'Arial') +
  theme(plot.title = element_text(size=rel(1.2), face = "bold", hjust = 0.5),
        plot.subtitle    = element_text(hjust = 0.5),
        plot.caption = element_text(size=rel(0.5))) +
  theme(axis.text.x.top = element_text(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") +
  theme(axis.title.y     = element_blank(),
        axis.text.y      = element_blank(),
        axis.title.x     = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(axis.ticks = element_blank())+
  theme(plot.margin = margin(t = 0.5, r = 1, b = 0.5, l = 1, unit = "cm"))

theme_set(theme_slope)

#wrangle wide data to long data and filter data
d_long <- gather(d_pay, year, percent, '2007':'2019', factor_key=TRUE)
View(d_long)
str(d_long)

d_pay <- d_long %>% 
  filter(type %in% c("Cash", "Cards", "Cheque"))

# set colours
color <- c("#1a698a","#1ab0d4", "#333F48")

#create slope graph
newggslopegraph(dataframe = d_pay,
                Times = year,
                Measurement = percent,
                Grouping = type,
                LineColor = color,
                LineThickness = 1.5,
                YTextSize = 2,
                Title = "Cash is no longer king in Australia",
                SubTitle = "Share of payments made (%)",
                Caption = "Note: Share of payments is by the number of payments and not the value of payments
Viz: @AndyBridger | Source: Reserve Bank of Australia") +
  theme_slope

#save plot
ggsave('charts/day05.png',
       width = 5,
       height = 3,
       scale = 3,
       units = 'cm')