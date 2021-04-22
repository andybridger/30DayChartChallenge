#code inspiration from Len Kiefer @ http://lenkiefer.com/2019/02/06/animated-labor-force-participation-chart/
#30DayChartChallenge
#Day 22 | Animate | Time Series

# load libraries ----
library(data.table)
library(tidyverse)
library(gganimate)

#download CSV file which is in a long data format
dfp <- read_csv("data/day22.csv")

#add colours
aig_blue <- "#1a698a"
aig_teal <- "#1ab0d4"
aig_purple <- "#663882"
aig_grey <- "#333F48"
aig_coolgrey <- "#8e9fbc"
aig_red <- "#a6192e"
aig_orange <- "#ffb533"
aig_green <- "#87a31a"
aig_lightblue <- "#e5f5fa"

#slowdown showing the COVID-19 data
dfp_slowdown <- dfp %>%
  mutate(show_time = case_when(variable == "COVID-19" ~ 20,
                               TRUE           ~ 1),
         reveal_time = cumsum(show_time))

#R code for animation with showtime
d <- 
  ggplot(data=dfp_slowdown, aes(x=time,y=values, group = variable, color=variable, label=variable)) +
  geom_path(size=1.05)+
  view_follow(fixed_x=c(0,66))+
  transition_reveal(reveal_time)+
  geom_hline(yintercept = 0,
             color = "red",
             linetype = "dashed") +
  geom_text(fontface="bold",size=4,nudge_x=3,nudge_y=0.2) +
  scale_colour_manual(values = c(aig_purple,
                                 aig_grey,
                                 aig_teal,
                                 aig_blue))+
  theme_minimal()+
  theme(legend.position="none",
        axis.line.x.bottom = element_line(colour = "black", size = 0.5, linetype = "solid"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(colour = '#FDFAF3', fill = '#FDFAF3'),
        panel.background = element_rect(colour = '#FDFAF3', fill = '#FDFAF3'),
        plot.caption=element_text(hjust= 0, size = 10,),
        plot.subtitle=element_text(hjust= 8.4,size=16),
        plot.title=element_text(hjust=1.35,size=20,face="bold", color="#1a698a"),
        axis.text=element_text(size = 16, color = "#000000"),
        axis.title.x = element_text(margin = margin(5, b = 10), size = 16, color = "#000000"),
        axis.ticks.x.bottom = element_line(size = 0.5, color = "#000000"))+
  labs(x="Months since lowest unemployment rate in cycle", y="",title="Australian job losses during modern downturns",
       subtitle="Per cent job losses since lowest unemployment rate in cycle",
       caption= "#30DayChartChallenge | Viz: @AndyBridger | Data: Australian Bureau of Statistics")
d
animate(d, end_pause=100, nframes=300,fps=10)
save_animation(last_animation(), file="...charts/day22.gif")

