#30DayChartChallenge
#day28 | uncertainties - future
#OECD forecasts from March 2021 Interim Outlook

# load libraries ----
library(data.table)
library(tidyverse)
library(readxl)
library(ggplot2)
library(aigtheme)

#load data
d_oecd <- read_csv("...data/day28.csv")
#look at the data
str(d_oecd)
view(d_oecd)

aig_lightgrey <- "#dcdcdc"

#plot
ch_oecd <- ggplot(data=d_oecd, aes(x = date)) +
  geom_rect(data=d_oecd, aes(xmin=as.Date("2021-01-01"), xmax=as.Date("2022-12-01"),
                             ymin=90,ymax=110),
            fill=aig_lightgrey)+
  geom_line(aes(y=nov_19_pro),size = 1, color = "#333F48", group = 4) +
  geom_line(aes(y=mar_21_pro),size = 1, color = "#1a698a")+
  geom_line(aes(y=up),size = 1, color = "#1ab0d4", linetype = "dashed")+
  geom_line(aes(y=down),size = 1, color = "#a6192e", linetype = "dashed")+
#please note this style comes from aigtheme on my github
  aig_style()+
  aig_y_continuous(limits=c(90,110),breaks = seq(90,110, by = 5))+
  labs(title = "The OECD says global GDP could approach pre-pandemic 
projections with faster deployment of vaccines (upside scenario)",
       subtitle = "World GDP and OECD projections, Index 100 = Q4-2019",
       caption = 'Note: Constant prices')+
  geom_label(aes(x=as.Date("2020-05-14"), y=103.5, label = "Nov-2019 projections"),
             size = 5,
             label.size = NA,
             colour = "#333F48",
             fill = NA)+
  geom_label(aes(x=as.Date("2020-10-01"), y=92.5, label = "Mar-2021
projections",lineheight = 0.8),
             size = 5,
             label.size = NA,
             colour = "#1a698a",
             fill = NA)+
  geom_label(aes(x=as.Date("2021-03-07"), y=102.2, label = "Upside
scenario",lineheight = 0.8),
             size = 5,
             label.size = NA,
             colour = "#1ab0d4",
             fill = NA)+
  geom_label(aes(x=as.Date("2022-09-10"), y=101.5, label = "Downside
scenario", lineheight = 0.8),
           size = 5,
           label.size = NA,
           colour = "#a6192e",
           fill = NA)+
geom_label(aes(x=as.Date("2022-08-01"), y=91, label = "Projection period"),
           size = 5,
           label.size = NA,
           colour = "#767676",
           fill = NA)
ch_oecd


#this function also comes from aigtheme and it based of the saving function from the BBC
finalise_plot(plot_name = ch_oecd,
              source = "Source: OECD, Interim Economic Outlook, March 2021",
              save_filepath = "...charts/day28.png",
              width_pixels = 640,
              height_pixels = 450,
              logo_image_path = "yourlogo...charts/aig_logo.png")
