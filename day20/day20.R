#30DayChartChallenge 
#Day 20 | upwards
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
urlfile="https://raw.githubusercontent.com/andybridger/30DayChartChallenge/main/day20/day20.csv"
d_ret <-read_csv(url(urlfile))
View(d_ret)

d_ret <- d_ret %>%
  mutate(retail = retail/1000) 

d_ret <- d_ret %>%
  mutate(ave = ave/1000) 
str(d_ret)

#create plot
ch_retail <- ggplot(data=d_ret, aes(x = date)) +
  geom_line(aes(y=ave),size = 1, color = "#a6192e", linetype = 2)+
  geom_line(aes(y=retail),size = 1, color = "#1a698a") +
  aig_style()+
  aig_y_continuous(limits=c(24,32),breaks = seq(24,32, by = 2))+
  scale_x_date(limits=c(as.Date("2017-01-01"),as.Date("2021-12-01")), date_breaks = "1 year", date_labels = "%Y")+
  labs(title = "Retail therapy? Australian retail sales remain well above the\npre-COVID-19 trend",
       subtitle = "Australian nominal retail sales ($bn) per month")+
  geom_label(aes(x=as.Date("2019-11-01"), y=31.4, label = "Retail sales"),
             size = 5,
             label.size = NA,
             colour = "#1a698a",
             fill = NA)+
  geom_label(aes(x=as.Date("2021-03-01"), y=27.5, label = "Trend 2018 & 2019"),
             size = 5,
             label.size = NA,
             colour = "#a6192e",
             fill = NA)
ch_retail

finalise_plot(plot_name = ch_retail,
              source = "Source: Australian Bureau of Statistics",
              #file path to save chart
              save_filepath = "...day20.png",
              width_pixels = 640,
              height_pixels = 450,
              #file path to logo in bottom right corner
              logo_image_path = "...aig_logo.png")




