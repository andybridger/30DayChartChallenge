# load libraries ----
library(data.table)
library(tidyverse)
library(readxl)
library(ggplot2)
library(aigtheme)

d_int <- read_csv("data/day03.csv")
str(d_int)
view(d_int)

ch_int <- ggplot(data=d_int, aes(x = year)) +
  geom_line(aes(y=short_term),size = 1, color = "#1ab0d4", group = 2) +
  geom_line(aes(y=long_term),size = 1, color = "#1a698a")+
  aig_style()+
  aig_y_continuous(limits=c(0,20),breaks = seq(0,20, by = 4))+
  scale_x_discrete(year)+
  labs(title = "5000 years of interest rates",
       subtitle = "Short-term and long-term interest rates (%)",
       caption = "Note: insert note")


