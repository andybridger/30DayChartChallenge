#30DayChartChallenge 
#Day 1 | part-to-whole
#Viz: @AndyBridger
#Inspiration: @Amit_Levinson

#download packages if needed
list.of.packages <- c("ggplot2", "sp", 'rnaturalearth','ggspatial','rgeos',
                      'raster','ggtext', 'sf', 'dplyr', 'glue', 'extrafont',
                      'readr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load packages
lapply(c("ggplot2", "sp", 'rnaturalearth','ggspatial','rgeos',
         'raster','ggtext', 'sf', 'dplyr', 'glue', 'extrafont',
         'readr'), require, character.only = TRUE)

# load Australia vaccination data
urlfile="https://raw.githubusercontent.com/andybridger/30DayChartChallenge/main/day01/day01.csv"
aus_data <-read_csv(url(urlfile))
View(aus_data)

#load map of Australia
sf_australia <- (rnaturalearth::ne_countries(country = "Australia", scale = 110, returnclass = "sf") %>% 
  st_transform(crs = "+proj=tmerc +lon_0=130 +lat_1=-20 +lat_2=-30"))
#raster
r <- raster(sf_australia, res = 2500)
ras_australia <- rasterize(sf_australia, r, field = "scalerank", fun = "sum")

# Turn raster object to data.frame
aus_tiles <- ras_australia %>% 
  as.data.frame(xy = TRUE) %>%
  filter(!is.na(layer)) %>% 
  arrange(y) %>% 
  # Here we split the x-y coordinates according to % of each (non/)vaccinated group
  mutate(id = 1:nrow(.),
         group_name = as.factor(case_when(
           id <= nrow(.) * aus_data$percent_second ~ "second",
           id >= nrow(.) * aus_data$percent_second & id <= nrow(.) * aus_data$percent_first ~ "first",
           TRUE ~ "nonvac"))) %>% 
  dplyr::select(-layer)

# Identify percent labels as y-axis
id_to_filter  <-  data.frame(id = 1:nrow(aus_tiles), 
                             point_to_label = as.character(cut(aus_tiles$id, breaks = quantile(aus_tiles$id,probs = 0:5/5),labels = paste0(seq(0, 80, 20), "%"), include.lowest = TRUE)) %>% 
                               ifelse(duplicated(.), NA, .)) 


pct_labels <- inner_join(aus_tiles, id_to_filter) %>% 
  filter(!is.na(point_to_label)) %>% 
  mutate(start_line = min(aus_tiles$x) -200000,
         # We want the line to end a little before the shape of the map, which means where X is at minimum.
         # The 0 is a place holder for the first value
         end_line = with(aus_tiles[aus_tiles$y %in% .$y,], tapply(x,y,  min)) + 5000) %>% 
  filter(id != 1) 

# Vertical bars for annotation
ver_bars <- data.frame(
  # Point where the line ends (maximum y value for each group)
  yend = unname(sort(with(aus_tiles, tapply(y, group_name, max)), decreasing = TRUE)),
  # Identify min value for the non-vaccinated and 2 spots at min(y) for the vaccinated groups.
  y = c(min(aus_tiles[aus_tiles$group_name == "nonvac","y"]), rep(min(aus_tiles$y), 2)),
  # Add some white space on the side of the map
  x = max(aus_tiles$x) - 80000,
  xend = max(aus_tiles$x) + c(70e4,-1e4,70e4),
  # This is mostly for some conventions on which value represent which group/row in the df
  group_name = c("nonvac","first", "second"))
ver_bars[1, 2] = -4321741


# Same idea but for easier reading in the plot itself
hor_bars <- ver_bars

# Colors info
aes_details <- data.frame(
  group_name = c("nonvac", "first", "second"),
  group_color = c("gray70", "#1ab0d4", "#1a698a")
)

# Position of annotations
text_pos <- data.frame(
  # Find the middle location between where the segments starts and ends
  transmute(ver_bars, y = (yend- y)/2 + y),
  group_name = ver_bars$group_name,
  x = ver_bars$xend + 4e3,
  group_color = aes_details$group_color,
  # Create label using {glue} and the downloaded df so it's easy to update when necessary
label = c(glue("{round(aus_data$nonvac/1e6, 1)}M vaccines <br><span style='color:gray50'><b>not yet received ({round({1 - aus_data$percent_second}*100, 1)}%)</b></span>"),
          glue("{round(aus_data$first/1e6, 1)}M <br> <span style='color:{aes_details$group_color[2]}'><b>target<br>({round({aus_data$percent_first}*100, 1)}%)</b></span>"),
          glue("{round(aus_data$second/1e6, 1)}M<br>vaccines<br><span style='color:{aes_details$group_color[3]}'><b>administered<br>({round({aus_data$percent_second}*100, 1)}%)</b></span>")
)) 


p <- ggplot(aus_tiles)+
  # The tiles that fill the map
  geom_tile(aes(x = x,y = y,fill = group_name), size =.3, show.legend = FALSE)+
  # Segments representing the y axis percentages
  geom_segment(data = pct_labels , aes(x = start_line, xend = end_line, y = y, yend = y), color = "gray80", linetype = "dashed")+
  # Percent labels
  geom_text(data = pct_labels,aes(x = start_line - 9e3, y = y, label = point_to_label), size = 4.5, color = "gray60", family = "Arial")+
  # vertical bars on the side for each group
  geom_segment(data = ver_bars, aes(x = xend, xend = xend, y = y- 2000, yend = yend), color = "gray70")+
  # horizontal bars with the *minimum* y value (y at minimum for each group)
  geom_segment(data = hor_bars[2:3,], aes(x = xend - 2e4, xend = xend - 75, y = y - 1.75e3, yend = y-1.75e3), color = "gray70")+
  # horizontal bars with the *maximum* y values (y at max for each group)
  geom_segment(data = hor_bars, aes(x = xend - 2e4, xend = xend - 50, y = yend, yend = yend), color = "gray70")+
  # Annotation text
  geom_richtext(data = text_pos,aes(x = x, y =y, label = label),  fill = NA, label.color = NA, hjust = 0, size = 6, color = "gray55", family = "Arial")+
  coord_equal(clip = "off")+
  # Add some padding for the percent labels
  scale_x_continuous(limits = c(min(pct_labels$start_line) - 1e4,max(text_pos$x) + 8e4))+
  scale_fill_manual(values = c( "nonvac" = "gray65" ,"first" = "#1ab0d4", "second" ="#1a698a"))+
  labs(title = "Australia's Vaccine Admission",
       subtitle = glue("With a population of 25.7M, up to {round(aus_data$population/1e6, 1)}M vaccines could be administered in Australia.<br><b><span style='color:{aes_details$group_color[2]}'>{round(aus_data$first/1e6, 1)}M administered vaccines was the target by 31 March 2021 ({round(aus_data$percent_first*100, 1)}%)</span></b>.<br>But only <b><span style='color:{aes_details$group_color[3]}'>{round(aus_data$second/1e6, 1)}M vaccines were administered by 31 March 2021 ({round(aus_data$percent_second*100, 1)}%)</span></b>.<br>Color represents portion of group out of the whole shape, and not of where vaccinated individuals reside."),
       # Take the most up to date from the aus_data df we created above
       caption = glue("Data: Covid19data.com.au | {format(aus_data$date, '%B %d, %Y')} | Viz: @AndyBridger | Inspiration: @Amit_Levinson"))+
  theme_void()+
  theme(
    text = element_text(family = "Arial"),
    plot.title = element_text(size = 32, face = "bold", family = "Arial", hjust = 0),
    #adjust subtitle space between lines
    plot.subtitle = element_markdown(size=18, color = "gray25", lineheight = 1.2),
    plot.caption = element_text(color = "gray50", hjust = 0, size = 11),
    plot.margin = margin(8,6,6,8,"mm"),
    plot.background = element_rect(fill = "white", color = NA)
  )
p

#save chart 
#You will need to assign your own file path here
ggsave("charts/day01.png", p, height = 10, width = 15)





