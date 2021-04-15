#Inspiration and base code from Carar Thompson
#https://github.com/cararthompson/30DayChartChallenge/blob/main/scripts/2.1_physical.R

# Load/install libraries
library(tidyverse)
library(extrafont)
library(ggmap)
library(ggthemes)
library(viridis)
library(mapproj)

#read in the data and mutate (log) density data
pop_density <- read.csv("...data/aus_pd_2020_1km_ASCII_XYZ 2.csv",
                        header = T) %>%
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
       subtitle = "Population density in Australia",
       caption = "\n#30DayChartChallenge | Inspiration: @cararthompson | Viz: @AndyBridger\nSource: worldpop.org") +
  theme_map() %+replace%
  theme(
    text = element_text(family = "Arial", colour = "#36413d"),
    plot.title = element_text(hjust = 0, size = 16, lineheight = 1.1),
    plot.subtitle = element_text(hjust = 0, size = 12),
    plot.caption = element_text(hjust = 1, size = 8))

p
# Export plot
ggsave(plot = p,'...charts/day07.png', 
       dpi = 400, height = 4, width = 6)
