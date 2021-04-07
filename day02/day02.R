library(waffle)
library(extrafont)
library(ggtext)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(waffle)
library(readr)
library(patchwork)

#failed code to upload pictograms???
install.packages("remotes")
remotes::install_github("rstudio/fontawesome")
extrafont::loadfonts(quiet = TRUE)
install_fa_fonts()
extrafont::fonttable() %>% 
  as_tibble() %>% 
  filter(grepl("Awesom", FamilyName)) %>% 
  select(afmfile, FullName, FamilyName, FontName)

#for some reason on a mac I needed to run this code??
hrbrthemes::import_roboto_condensed()
library(hrbrthemes)
d <- read.csv(extrafont:::fonttable_file(), stringsAsFactors = FALSE)
d[grepl("Light", d$FontName),]$FamilyName <- font_rc_light
write.csv(d,extrafont:::fonttable_file(), row.names = FALSE)
extrafont::loadfonts()

#load_data

d_pic <- read_csv("data/pictogram_data.csv")
str(d_pic) 

#create the 8 waffle charts
plot_1 <- d_pic %>%
  filter(type == "Population") %>%
  ggplot(aes(fill = sex, values = percent)) +
             geom_waffle(n_rows = 10, make_proportional = TRUE, color = "white", size = 0.2) +
             coord_equal() +
             theme_ipsum(grid="")+
  theme_enhance_waffle()+
  scale_fill_manual(
    values = c("#1ab0d4", "#8e9fbc")) +
  theme(legend.position = "None")+
  labs(subtitle = "50% of the population" ,size = 12)+
  theme(legend.position = "None", plot.subtitle = element_text(hjust = 0.5, vjust=-5), plot.margin = margin(t=2,r=0,b=0,l=0, "mm"))

plot_2 <- d_pic %>%
  filter(type == "Primary carer") %>%
  ggplot(aes(fill = sex, values = percent)) +
    geom_waffle(n_rows = 10, make_proportional = TRUE, color = "white", size = 0.2) +
    coord_equal() +
    theme_ipsum(grid="")+
    theme_enhance_waffle()+
    scale_fill_manual(
    values = c("#1ab0d4", "#8e9fbc")) +
    labs(subtitle = "71% of primary carers", size = 12)+
  theme(legend.position = "None", plot.subtitle = element_text(hjust = 0.5, vjust=-5), plot.margin = margin(t=2,r=0,b=0,l=0, "mm"))

plot_3 <- d_pic %>%
  filter(type == "Primary parental leave takers") %>%
  ggplot(aes(fill = sex, values = percent)) +
  geom_waffle(n_rows = 10, make_proportional = TRUE, color = "white", size = 0.2) +
  coord_equal() +
  theme_ipsum(grid="")+
  theme_enhance_waffle()+
  scale_fill_manual(
    values = c("#1ab0d4", "#8e9fbc")) +
  labs(subtitle = "94% of primary parental leave takers", size = 12)+
  theme(legend.position = "None", plot.subtitle = element_text(hjust = 0.5, vjust=-5), plot.margin = margin(t=2,r=0,b=0,l=0, "mm"))

plot_4 <- d_pic %>%
  filter(type == "Prisoners") %>%
  ggplot(aes(fill = sex, values = percent)) +
  geom_waffle(n_rows = 10, make_proportional = TRUE, color = "white", size = 0.2) +
  coord_equal() +
  theme_ipsum(grid="")+
  theme_enhance_waffle()+
  scale_fill_manual(
    values = c("#1ab0d4", "#8e9fbc")) +
  labs(subtitle = "7% of prisoners", size = 12)+
  theme(legend.position = "None", plot.subtitle = element_text(hjust = 0.5, vjust=-5), plot.margin = margin(t=2,r=0,b=0,l=0, "mm"))

plot_5 <- d_pic %>%
  filter(type == "Parliament") %>%
  ggplot(aes(fill = sex, values = percent)) +
  geom_waffle(n_rows = 10, make_proportional = TRUE, color = "white", size = 0.2) +
  coord_equal() +
  theme_ipsum(grid="")+
  theme_enhance_waffle()+
  scale_fill_manual(
    values = c("#1ab0d4", "#8e9fbc")) +
  labs(subtitle = "37% of parliament", size = 12)+
  theme(legend.position = "None", plot.subtitle = element_text(hjust = 0.5, vjust=-5), plot.margin = margin(t=2,r=0,b=0,l=0, "mm"))

plot_7 <- d_pic %>%
  filter(type == "CEOs") %>%
  ggplot(aes(fill = sex, values = percent)) +
  geom_waffle(n_rows = 10, make_proportional = TRUE, color = "white", size = 0.2) +
  coord_equal() +
  theme_ipsum(grid="")+
  theme_enhance_waffle()+
  scale_fill_manual(
    values = c("#1ab0d4", "#8e9fbc")) +
  labs(subtitle = "18% of CEOs", size = 12)+
  theme(legend.position = "None", plot.subtitle = element_text(hjust = 0.5, vjust=-5), plot.margin = margin(t=2,r=0,b=0,l=0, "mm"))

plot_6 <- d_pic %>%
  filter(type == "Sportspersons") %>%
  ggplot(aes(fill = sex, values = percent)) +
  geom_waffle(n_rows = 10, make_proportional = TRUE, color = "white", size = 0.2) +
  coord_equal() +
  theme_ipsum(grid="")+
  theme_enhance_waffle()+
  scale_fill_manual(
    values = c("#1ab0d4", "#8e9fbc")) +
  labs(subtitle = "30% of paid sportspersons", size = 12, element_text(hjust = 0.5))+
  theme(legend.position = "None", plot.subtitle = element_text(hjust = 0.5, vjust=-5), plot.margin = margin(t=2,r=0,b=0,l=0, "mm"))

plot_8 <- d_pic %>%
  filter(type == "Music royalties paid") %>%
  ggplot(aes(fill = sex, values = percent)) +
  geom_waffle(n_rows = 10, make_proportional = TRUE, color = "white", size = 0.2) +
  coord_equal() +
  theme_ipsum(grid="")+
  theme_enhance_waffle()+
  scale_fill_manual(
    values = c("#1ab0d4", "#8e9fbc")) +
  labs(subtitle = "17% of music royalties paid", size = 12)+
  theme(legend.position = "None", plot.subtitle = element_text(hjust = 0.5, vjust=-5), plot.margin = margin(t=2,r=0,b=0,l=0, "mm"))
 
# patchwork plot
my_plot <- (plot_1 + plot_2 + plot_3 + plot_4) /
    (plot_5 + plot_6 + plot_7+ plot_8) + plot_layout(guides = "collect")+
    plot_annotation(title = "Females in Australia represent...",
                    caption = "Data: ABS Gender Indicators; ABS Disability, Ageing and Carers; ATO Taxable Income Tables; WGEA; APRA AMCOS
Viz: @AndyBridger | #30DayChartChallenge") &
    theme(plot.title = element_text(family = "Arial", size = 18, color = 'black', hjust = .5, margin = margin(0,0,0,0,"mm")),
          plot.caption = element_text(family = "Arial", size = 8, color = 'black', hjust = 0.5),
          legend.position="bottom", legend.title = element_blank())
  
# save image
ggsave("charts/day02.png", my_plot, height = 11, width = 6)

