###WSL###
#a good tutorial: https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
#https://github.com/slowkow/ggrepel/issues/89
#https://stackoverflow.com/questions/50044835/can-geom-label-draw-points-to-a-position-on-a-map/50045289

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(sf)
library(ggplot2)
library(ggmap)
library(gridExtra)

#c(left = 110, bottom = -40, right = 160, top = -10)

#import Roboto font
sysfonts::font_add_google('Roboto')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 400)


# define data frame ensuring lat and lon are numeric vectors
#2019 data
city_name <-  c("Snapper Rocks (AU)", "Bells Beach (AU)", "Bali (ID)",
                                     "Margaret River (AU)", "Rio De Janeiro (BR)", "J-Bay (SA)",
                                     "Teahupo'o (Tahiti)", "Surf Ranch (US)", "Landes (FR)",
                                     "Supertubos (PT)", "Oahu (HI)")
                 longitude <-  c(153.5488,144.2847,115.0920,
                                115.0738,-42.4867,24.9102,
                                -149.2677,-119.79079,-0.7533,
                                -9.3609,-158.0001)
                 latitude <- c(-28.1633,-38.3669,-8.3405,
                               -33.9509,-22.9324,-34.0507,
                               -17.8471,36.25685,43.9412,
                               39.3407,21.4389)
df_2019 <- data.frame(city_name, longitude, latitude)

# 2020 data
city_name <-  c("Oahu (HI)")
longitude <-  c(-158.0001)
latitude <- c(21.4389)
df_2020 <- data.frame(city_name, longitude, latitude)

# 2021 data
city_name <-  c("Newcastle (AU)", "Narrabeen (AU)",
                "Margaret River (AU)", "Rottnest Island (AU)", "Surf Ranch (US)",
                "Barra de la Cruz (MX)", "Rio De Janeiro (BR)",
                "San Clemente (US)")
longitude <-  c(151.7817,151.2952,
                115.0738, 115.5073,-119.79079,
                -95.9708,-42.4867,
                -117.6126)
latitude <- c(-32.9283,-33.7231,
              -33.9509, -32.0064, 36.25685,
              15.8406,-22.9324,
              33.4274)
df_2021 <- data.frame(city_name, longitude, latitude)

# For stamen map, you have to give the extremity of the window you are looking at. here is an example with the watercolor background (Around brisbane)
map <- get_stamenmap( bbox = c(left = -195, bottom = -58, right = 185, top = 60), zoom = 2, maptype = "watercolor")
ch_wsl_2019<-ggmap(map) + 
  theme_void() + 
  theme(
    plot.title = element_text(colour = "orange"), 
    panel.border = element_rect(colour = "white", fill=NA, size=1)
    )+ 
  geom_point(data=df_2019, aes(x=longitude, y=latitude), color="black")+
  geom_text_repel(data=df_2019, aes(x=longitude, y=latitude, label = city_name),
                  color = "black", fontface = "bold",size=4)+
  labs(title = "2019")

map <- get_stamenmap( bbox = c(left = -195, bottom = -58, right = 185, top = 60), zoom = 2, maptype = "watercolor")
ch_wsl_2020<-ggmap(map) + 
  theme_void() + 
  theme(
    plot.title = element_text(colour = "orange"), 
    panel.border = element_rect(colour = "white", fill=NA, size=1)
  )+ 
  geom_point(data=df_2020, aes(x=longitude, y=latitude), color="black")+
  geom_text_repel(data=df_2020, aes(x=longitude, y=latitude, label = city_name),
                  color = "black", fontface = "bold",size=4)+
  labs(title = "\n2020")

map <- get_stamenmap( bbox = c(left = -195, bottom = -58, right = 185, top = 60), zoom = 2, maptype = "watercolor")
ch_wsl_2021<-ggmap(map) + 
  theme_void() + 
  theme(
    plot.title = element_text(size = 14), 
    panel.border = element_rect(colour = "white", fill=NA, size=1)
  )+ 
  geom_point(data=df_2021, aes(x=longitude, y=latitude), color="black")+
  geom_text_repel(data=df_2021, aes(x=longitude, y=latitude, label = city_name),
                  color = "black", fontface = "bold",size=4)+
  labs(title = "\n2021")

#join plots together
ch_wsl <- ch_wsl_2019 / ch_wsl_2020  / ch_wsl_2021 +
  plot_annotation(title = "World Surf League Event Changes During COVID-19\n",
    subtitle = "The World Surf League was cancelled in 2020 due to the COVID-19 pandemic.<br>Restrictions on international\nand domestic travel (most notably within Australia),<br>forced a number of changes to the schedule for the 2021 season. The 2021 season<br>kicked off in Hawaii at the end of 2020.<span style = 'color: #a6192e'>2019 event only</span><br><span style = 'color: #663882'>2021 event only</span><br><span style = 'color: #FAF9F6'>Both</span>",
    caption = '#30DayChartChallenge | Viz: @AndyBridger | Data: WSL and Google Maps'
  )  &
  theme(panel.background = element_rect(fill = "#8e9fbc", colour=NA),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "#8e9fbc", colour=NA),
        plot.title = element_text(colour = "white", size=18, face="bold", hjust = 0.5, family="Roboto"),
        plot.subtitle = element_markdown(colour = "white", size=14, family="Roboto", lineheight = 1.1),
        plot.caption = element_text(colour = "white", size=8, face="bold", hjust = 0, family="Roboto"))

#save plot
ggsave('charts/ch_wsl.png', ch_wsl, width = 8, height = 12, unit = 'in', dpi = 400)


#############
#version 2
#############
# For stamen map, you have to give the extremity of the window you are looking at. here is an example with the watercolor background (Around brisbane)
map <- get_stamenmap( bbox = c(left = -195, bottom = -58, right = 185, top = 60), zoom = 2, maptype = "watercolor")
ch_wsl_2019<-ggmap(map) + 
  theme_void() + 
  theme(
    plot.title = element_text(colour = "orange"), 
    panel.border = element_rect(colour = "white", fill=NA, size=1)
  )+ 
  geom_point(data=df_2019, aes(x=longitude, y=latitude), color="black")+
  geom_text_repel(data=df_2019, aes(x=longitude, y=latitude, label = city_name),
                  color = "black", fontface = "bold",size=4)+
  labs(title = "2019")

map <- get_stamenmap( bbox = c(left = -195, bottom = -58, right = 185, top = 60), zoom = 2, maptype = "watercolor")
ch_wsl_2020<-ggmap(map) + 
  theme_void() + 
  theme(
    plot.title = element_text(colour = "orange"), 
    panel.border = element_rect(colour = "white", fill=NA, size=1)
  )+ 
  geom_point(data=df_2020, aes(x=longitude, y=latitude), color="black")+
  geom_text_repel(data=df_2020, aes(x=longitude, y=latitude, label = city_name),
                  color = "black", fontface = "bold",size=4)+
  labs(title = "\n2020")

map <- get_stamenmap( bbox = c(left = -195, bottom = -58, right = 185, top = 60), zoom = 2, maptype = "watercolor")
ch_wsl_2021<-ggmap(map) + 
  theme_void() + 
  theme(
    plot.title = element_text(size = 14), 
    panel.border = element_rect(colour = "white", fill=NA, size=1)
  )+ 
  geom_point(data=df_2021, aes(x=longitude, y=latitude), color="black")+
  geom_text_repel(data=df_2021, aes(x=longitude, y=latitude, label = city_name),
                  color = "black", fontface = "bold",size=4)+
  labs(title = "\n2021")

#join plots together
ch_wsl <- ch_wsl_2019 / ch_wsl_2020  / ch_wsl_2021 +
  plot_annotation(title = "World Surf League Event Changes During COVID-19\n",
                  subtitle = "The World Surf League was cancelled in 2020 due to the COVID-19 pandemic.<br>Restrictions on international\nand domestic travel (most notably within Australia),<br>forced a number of changes to the schedule for the 2021 season. The 2021 season<br>kicked off in Hawaii at the end of 2020.",
                  caption = '#30DayChartChallenge | Viz: @AndyBridger | Data: WSL and Google Maps'
  )  &
  theme(panel.background = element_rect(fill = "black", colour=NA),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "black", colour=NA),
        plot.title = element_text(colour = "white", size=18, face="bold", hjust = 0.5, family="Roboto"),
        plot.subtitle = element_markdown(colour = "white", size=14, family="Roboto", lineheight = 1.1),
        plot.caption = element_text(colour = "white", size=8, face="bold", hjust = 0, family="Roboto"))

#save plot
ggsave('charts/ch_wsl1.png', ch_wsl, width = 8, height = 12, unit = 'in', dpi = 400)



