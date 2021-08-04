#30DayChartChallenge 
#Day 29 | deviations
#Viz: @AndyBridger

#download packages if needed
list.of.packages <- c('ggplot2', 'lubridate', 'tidyverse', 'readxl', 'scales')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load packages
lapply(c('ggplot2', 'lubridate', 'tidyverse', 'readxl', 'scales'), require, character.only = TRUE)

#read data
#load data
urlfile="https://raw.githubusercontent.com/andybridger/30DayChartChallenge/main/day29/day29.csv"
d_ue <-read_csv(url(urlfile))
str(d_ue)

#import Roboto font
sysfonts::font_add_google('Roboto')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 400)

#mutate date column
d_ue <- d_ue %>%
  mutate(date=as.Date(date, format = "%Y-%m-%d"))
str(d_ue)

#plot
ch_ue<-ggplot(d_ue, aes(x = date, y = ue, group = 1))+
  geom_line(size = 1, colour = "#1a698a")+
  scale_y_continuous(limits=c(4.5, 8), breaks = seq(4.5,8, by = 0.5))+
  scale_x_date(limit=c(as.Date("2020-01-01"),as.Date("2021-03-01")))+
  labs(title = 'Australian unemployment rate (%) with 95% confidence interval',
       subtitle = 'The Australian unemployment rate comes from the Australian Bureau of Statistics (ABS) Labour Force Survey. This\nis a household survey conducted with about 26,000 households (52,000 people) or 0.32% of the population every\nmonth. Despite plunging household response rates around the world, the ABS reports a response rate of 93% to the\nLabour Force Survey. In contrast, response rates in Britain have fallen from 70% in 2001 to just 43% by 2018.
\nThe result for Australia is a remarkably accurate picture of the labour market. However, as with any survey data, it\nis subject to sampling error (i.e. because a sample, rather than the entire population, is surveyed). Indeed, the ABS\nare usually only 95% confident the actual unemployment rate is about 0.3 percentage points higher or lower than\nthe headline rate (or central estimate).',
       caption = '#30DayChartChallenge | @AndyBridger | Data: Australian Bureau of Statistics') +
  geom_ribbon(data = d_ue,
              aes(x=date,
                  ymin=minus, ymax=plus), fill="#1a698a", alpha=0.2)+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(colour = 'black'),
        axis.text = element_text(colour = 'black', size = 20, family = 'Roboto'),
        axis.text.x = element_text(vjust= -1),
        axis.title = element_blank(),
        plot.title = element_text(colour = "#1a698a", family = 'Roboto', size = 30),
        plot.title.position = 'plot',
        plot.caption = element_text(size = 14, margin = margin(t = 20), hjust = 0, family = 'Roboto', colour = "#1a698a",lineheight = 1.05),
        plot.subtitle = element_text(size = 19, margin = margin(t = 20, b = 30), family = 'Roboto', lineheight = 1.15),
        plot.background = element_rect(colour = '#FDFAF3', fill = '#FDFAF3'),
        panel.background = element_rect(colour = '#FDFAF3', fill = '#FDFAF3'),
        plot.margin = margin(20,40,20,20),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(color = "#8e9fbc"),
        axis.line.x.bottom = element_line(color="#000000",size = 0.5, linetype = 1),
        axis.ticks.x.bottom = element_line(color="#000000",size = 0.5)) +
  coord_cartesian(clip = 'off')

#save plot
ggsave('charts/day29.png', ch_ue, width = 14, height = 12, unit = 'in', dpi = 400)

