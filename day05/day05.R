library(tidyverse)
library(readr)
library(ggthemes)
library(tidyr)
library(dplyr)


d_pay <- read_csv("data/day05.csv")
str(d_pay) 

#theme

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


# read data
d_pay <- read_csv("data/day05.csv")
str(d_pay) 

#wrangle wide data to long data and filter data
d_long <- gather(d_pay, year, percent, '2007':'2019', factor_key=TRUE)
View(d_long)
str(d_long)

d_pay <- d_long %>% 
  filter(type %in% c("Cash", "Cards", "Cheque"))

# chart

color <- c("#1a698a","#1ab0d4", "#333F48")

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

ggsave('charts/day05.png',
       width = 5,
       height = 3,
       scale = 3,
       units = 'cm')
