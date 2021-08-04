#30DayChartChallenge 
#Day 7 | physical
#Viz: @AndyBridger
#Inspiration from Carar Thompson: https://github.com/cararthompson/30DayChartChallenge/blob/main/scripts/2.1_physical.R

#download packages if needed
list.of.packages <- c("tidyverse",'ggmap', 'ggthemes','viridis',
                      'mapproj', 'extrafont')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load packages
lapply(c("tidyverse",'ggmap', 'ggthemes','viridis',
         'mapproj', 'extrafont'), require, character.only = TRUE)

# load Australian population data
urlfile="https://data.worldpop.org/GIS/Population_Density/Global_2000_2020_1km/2020/AUS/aus_pd_2020_1km_ASCII_XYZ.zip"
#read in the data and mutate (log) density data
pop_density <-read_csv(url(urlfile), header = T) %>%
  mutate(Country = "Australia") %>%
  mutate(log10_density = log10(Z))

#plot
p <- ggplot(pop_density, aes(x = X, y = Y)) +
  geom_point(aes(colour = log10_density), 
             size = 0.01, alpha = 0.4,
             show.legend = F) +
  scale_colour_viridis() +
  coord_map() +
  labs(title = "More than 90% of Australia's population lives\nwithin 100km of the coast",
       subtitle = "\nPopulation density in Australia",
       caption = "\n#30DayChartChallenge | Inspiration: @cararthompson | Viz: @AndyBridger\nSource: worldpop.org") +
  theme_map() %+replace%
  theme(
    text = element_text(family = "Arial", colour = "#36413d"),
    plot.title = element_text(hjust = 0, size = 16, lineheight = 1.0),
    plot.subtitle = element_text(hjust = 0, size = 12),
    plot.caption = element_text(hjust = 1, size = 8))

p

#save plot
#you will need to assign your own file path here
ggsave(plot = p,'charts/day07.png', 
       dpi = 400, height = 4, width = 6)




