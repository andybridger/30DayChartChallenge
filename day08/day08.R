#30DayChartChallenge 
#Day 8 | animal
#Viz: @AndyBridger

#download packages if needed
list.of.packages <- c('magick', 'png', 'dplyr', 'cowplot', 'ggplot2', 'readxl',
                      'zoo', 'lubridate', 'tidyverse', ' ggimage', 'data.table')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load packages
lapply(c('magick', 'png', 'dplyr', 'cowplot', 'ggplot2', 'readxl',
         'zoo', 'lubridate', 'tidyverse', ' ggimage', 'data.table'), require, character.only = TRUE)

# load nz data
urlfile="https://raw.githubusercontent.com/andybridger/30DayChartChallenge/main/day08/day08.csv"
d_nz <-read_csv(url(urlfile))
View(d_nz)

#linearly interpolate missing data
d_nz <- d_nz %>%
  mutate(approx = na.approx(no))

#create percentage change (I called the new column 'index')
d_nz <- d_nz %>% group_by(animal) %>% 
  mutate(index=(approx/approx[year=="1990"])*100-100) %>% ungroup()

#setup theme
moo_style <- function() {
  font <- "Arial"
  
  ggplot2::theme(
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=20,
                                       face="bold",
                                       color="black"),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=14,
                                          margin=ggplot2::margin(1.5,0,3,0)),
    plot.caption = ggplot2::element_text(family=font,
                                         size=10,
                                         hjust = 0.0
    ),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and background for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "none",
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font,
                                      size=14,
                                      color="#000000"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks.y = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank(),
    axis.line.x.bottom = ggplot2::element_line(color="#000000",size = 0.5, linetype = 1),
    axis.ticks.x.bottom = ggplot2::element_line(color="#000000",size = 0.5),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#dcdcdc"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 14,  hjust = 0)
  )
}

#create plot
ch_animal <- ggplot(data=d_nz, aes(x = year, y = index, group = animal, color = animal)) +
  geom_line()+
  moo_style()+
  scale_y_continuous(limits=c(-100, 750), breaks = seq(-100,700, by = 100))+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_color_manual(values = c("black", "#8e9fbc"))+
  geom_text(data = d_nz %>% filter(year == 2005), 
            aes(label = animal, x = year, y = index, color = animal), position = position_nudge(y = -40))+
  labs(title = "MOO-ve over sheep",
        subtitle = "Percentage change (%) in the number of dairy cows and sheep on New Zealand's 
South Island since 1990",
        caption = "Viz: @AndyBridger | Source: NZ Statistics | #30DayChartChallenge")+
  theme(plot.caption = element_text(hjust = 0), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot",
        plot.subtitle = element_text(lineheight = 1.15),
        plot.margin=unit(c(0.1,1.5,0.1,0.1),"cm"))
ch_animal

#import sheep and cow image and add to image 
sheep_image <- readPNG('data/sheep.png')
cow_image <- readPNG('data/dairycow.png')
ggdraw()+
  draw_plot(ch_animal)+
  draw_image(
    sheep_image, x = 1, y = 0.25, hjust = 1, vjust = 1, halign = 0.5, valign = 1,
    width = 0.10)+
  draw_image(
    cow_image, x = 1, y = 0.8, hjust = 1, vjust = 1, halign = 0.5, valign = 1,
    width = 0.10)

#save plot
ggsave('charts/day08.png',
       width = 6.4,
       height = 4.5,
       scale = 3,
       units = 'cm')

#####number chart
ch_animal <- ggplot(data=d_nz, aes(x = year, y = mn, group = animal, color = animal)) +
  geom_line()+
  moo_style()+
  scale_y_continuous(limits=c(0, 35), breaks = seq(0,35, by = 5))+
  scale_color_manual(values = c("black", "#8e9fbc"))+
  geom_text(data = d_nz %>% filter(year == 2005), 
            aes(label = animal, x = year, y = mn, color = animal), position = position_nudge(y = 2))+
  labs(title = "MOO-ve over sheep",
       subtitle = "Millions of dairy cows and sheep on New Zealand's South Island",
       caption = "Viz: @AndyBridger | Source: NZ Statistics | #30DayChartChallenge")+
  theme(plot.caption = element_text(hjust = 0), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot",
        plot.subtitle = element_text(lineheight = 1.15),
        plot.margin=unit(c(0.1,1.5,0.1,0.1),"cm"))
ch_animal

sheep_image<- system.file("/data","sheep.png", package = "cowplot")
sheep_image <- readPNG('data/sheep.png')
cow_image <- readPNG('data/dairycow.png')
ggdraw()+
  draw_plot(ch_animal)+
  draw_image(
    sheep_image, x = 1, y = 0.5, hjust = 1, vjust = 1, halign = 0.5, valign = 1,
    width = 0.10)+
  draw_image(
    cow_image, x = 1, y = 0.3, hjust = 1, vjust = 1, halign = 0.5, valign = 1,
    width = 0.10)

#save plot
ggsave('charts/day08_number.png',
       width = 6.4,
       height = 4.5,
       scale = 3,
       units = 'cm')



