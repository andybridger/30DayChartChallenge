#day 12 | distributions | strips

#load libraries
library(tidyverse)
library(RColorBrewer)
library(ggplot2)

#read data
d12 <- read_csv("...data/day12.csv")
str(d12)

#create the strips
theme_strip <- theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(vjust = 3, color = 'black'),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin=unit(c(0.2,0,0.2,0.5),"cm")
  )

col_strip <- brewer.pal(11, "RdBu")
brewer.pal.info

#create the plot
ch12<- ggplot(d12,
       aes(x = year, y = 1, fill = temp))+
  geom_tile()+
  scale_x_continuous(limits=c(1910, 2021), breaks = seq(1910,2020, by = 10),
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "Australian mean temperature anomaly 1910-2020",
       caption = "Anomalies are based on 30-year climatology from 1961 to 1990
#30DayChartChallenge | Viz: @AndyBridger | Data: Bureau of Meteorology")+
  theme_strip

#change legend title
ch12<-ch12+labs(fill="Anomaly [°C]")

#save the plot
ggsave(plot=ch12, '...charts/day12.png',width=6, height=4, scale = 3, units = 'cm')


