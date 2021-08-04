#30DayChartChallenge 
#Day 13 | correlations
#Viz: @AndyBridger
#code inspired from: https://github.com/Sarah145/30DayChartChallenge/blob/master/day13/correlation.R

#download packages if needed
list.of.packages <- c('ggplot2', "scales", 'ggrepel', 'tidyverse')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load packages
lapply(c('ggplot2', "scales", 'ggrepel', 'tidyverse'), require, character.only = TRUE)

# load data
urlfile="https://raw.githubusercontent.com/andybridger/30DayChartChallenge/main/day13/day13.csv"
d13 <-read_csv(url(urlfile))
View(d13)

#import Roboto font
sysfonts::font_add_google('Roboto')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 400)

# create plot with the separated groups
ch13 <- ggplot(d13, aes(x = rate, y = wpi)) +
  geom_point(aes(fill = type), shape = 21, size = 4, alpha = 0.8, colour = "black", stroke = 1.5,show.legend = FALSE) +
  geom_smooth(aes(color = type), method = "lm",show.legend = FALSE,se=F) +
  theme_void() +
  geom_text(data = data.frame(x = 7.5, y = 4.9, label = 'Unemployment rate', family = 'Roboto'), 
            aes(x = x, y = y, label = label), size = 7, color = "#1ab0d4") +
  geom_curve(data = data.frame(x = 7.4, xend = 6.3, y = 4.75, yend = 4.1), 
             aes(x = x, y = y, xend = xend, yend = yend), curvature = -0.25, arrow = arrow(20L, unit(0.15, "inches"), "last", "closed"), inherit.aes = FALSE, color = "#1ab0d4") +
  scale_x_continuous(limits = c(3, 15), expand = c(0.01,0.01)) +
  geom_text(data = data.frame(x = 11.5, y = 1.5, label = 'Underutilisation rate', family = 'Roboto'), 
            aes(x = x, y = y, label = label), size = 7, color = "#663882") +
  geom_curve(data = data.frame(x = 11.5, xend = 12, y = 1.7, yend = 2.4), 
             aes(x = x, y = y, xend = xend, yend = yend), curvature = -0.25, arrow = arrow(20L, unit(0.15, "inches"), "last", "closed"), inherit.aes = FALSE, color = "#663882") +
  scale_y_continuous(limits = c(1, 5), expand = c(0.01,0.01)) +
  scale_colour_manual(values = c("#663882", "#1ab0d4")) +
  scale_fill_manual(values = c("#663882", "#1ab0d4")) +
#add labels and theme
  labs(title = 'The Australian Phillips Curve',
       subtitle = str_wrap('The Phillips Curve is named after New Zealand Economist, William Phillips, who found an inverse relationship between the level of unemployment and the rate of change in wages. \nIn Australia, wage growth is more tightly linked to the underutilisation rate than the unemployment rate.', width = 98),
       x = '\nSpare Capacity (%)', y = 'Wage growth (% p.a.)\n',
       caption = '@AndyBridger | #30DayChartChallenge \nNote: Underutilisation is a broader measure of spare capacity in the labour market and includes those who are unemployed\nplus those who are underemployed (i.e. looking for more hours).\nData source: Australian Bureau of Statistics') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(colour = 'black'),
        axis.text = element_text(colour = 'black', size = 20, family = 'Roboto'),
        axis.title = element_text(colour = "#1a698a", size = 24, family = 'Roboto', face = 'bold'),
        plot.title = element_text(colour = "#1a698a", family = 'Roboto', size = 30),
        plot.title.position = 'plot',
        plot.caption = element_text(size = 14, margin = margin(t = 20), hjust = 0, family = 'Roboto', colour = "#1a698a",lineheight = 1.05),
        plot.subtitle = element_text(size = 19, margin = margin(t = 20, b = 30), family = 'Roboto', lineheight = 1.15),
        plot.background = element_rect(colour = '#FDFAF3', fill = '#FDFAF3'),
        panel.background = element_rect(colour = '#FDFAF3', fill = '#FDFAF3'),
        plot.margin = margin(20,40,20,20),
        axis.line.y = element_line(color="#000000",size = 0.5, linetype = 1),
        axis.line.x.bottom = element_line(color="#000000",size = 0.5, linetype = 1),
        axis.ticks.x.bottom = element_line(color="#000000",size = 0.5)) +
  coord_cartesian(clip = 'off')

#save plot
ggsave('charts/day13.png', ch13, width = 12.5, height = 12, unit = 'in', dpi = 400)

