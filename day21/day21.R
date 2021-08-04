#30DayChartChallenge 
#Day 20 | downwards
#Viz: @AndyBridger

#download packages if needed
list.of.packages <- c('ggplot2', 'data.table', 'tidyverse', 'readxl')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load packages
lapply(c('ggplot2', 'data.table', 'tidyverse', 'readxl'), require, character.only = TRUE)

# load devtools for aigtheme
install.packages("devtools")
library(devtools)
#load aigtheme
devtools::install_github("andybridger/aigtheme")
library(aigtheme)

# load data
urlfile="https://raw.githubusercontent.com/andybridger/30DayChartChallenge/main/day21/day21.csv"
d_avi <-read_csv(url(urlfile))
View(d_avi)

aig_lightgrey <- "#dcdcdc"

ch_avi <- ggplot(data=d_avi, aes(x = date)) +
  geom_rect(data=d_avi, aes(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-12-01"),
                            ymin=0,ymax=140),
            fill=aig_lightgrey)+
  geom_line(aes(y=r_gdp),size = 1, color = "#1a698a") +
  geom_line(aes(y=air_trans),size = 1, color = "#1ab0d4")+
  aig_style()+
  aig_y_continuous(limits=c(0,140),breaks = seq(0,140, by = 20))+
  scale_x_date(limits=c(as.Date("2015-01-01"),as.Date("2020-12-01")))+
  labs(title = "While the broader economy is experiencing a 'V-shaped' recovery,
the aviation sector recovery is closer to 'L-shaped'",
       subtitle = "Real GDP and real air & space transport output, Index 100 = Q1-2015",
       caption = 'Note: real means data are adjusted for price changes. ')+
  geom_label(aes(x=as.Date("2019-03-01"), y=40, label = "Australian air and 
space transport sector"),
             size = 5,
             label.size = NA,
             colour = "#1ab0d4",
             fill = NA)+
  geom_label(aes(x=as.Date("2018-03-01"), y=90, label = "Australian economy"),
             size = 5,
             label.size = NA,
             colour = "#1a698a",
             fill = NA)+
geom_label(aes(x=as.Date("2020-07-01"), y=135, label = "COVID-19"),
           size = 5,
           label.size = NA,
           colour = "black",
           fill = NA)
ch_avi

finalise_plot(plot_name = ch_avi,
              source = "Source: Australian Bureau of Statistics",
              #file path to save chart
              save_filepath = "...day21.png",
              width_pixels = 640,
              height_pixels = 450,
              #file path to logo in bottom right corner
              logo_image_path = "...aig_logo.png")




