# load libraries ----
library(data.table)
library(tidyverse)
library(readxl)
library(ggplot2)
library(aigtheme)
library(ggpubr)
library(gridExtra)
library(scales)

#load datasets
d_gfc <- read_csv("data/ps_gfc.csv")
d_covid <- read_csv("data/ps_covid.csv")

#check data
str(d_gfc)
str(d_covid)

#plot GFC chart
ch_gfc <- ggplot(data=d_gfc, aes(x = date)) +
  geom_line(aes(y=PMI_GFC),size = 1, color = "#1a698a", group = 3) +
  geom_line(aes(y=PSI_GFC),size = 1, color = "#1ab0d4")+
  geom_line(aes(y=PCI_GFC),size = 1, color = "#663882")+
  aig_style()+
  aig_y_continuous(limits=c(20,65),breaks = seq(20,65, by = 10))+
  scale_x_date(limit=c(as.Date("2008-01-01"),as.Date("2009-12-30")),
               date_breaks = ("1 year"),labels = date_format("%Y"))+
  geom_hline(yintercept=50, linetype="dashed", color = "red")+
  geom_label(aes(x=as.Date("2008-10-01"), y=55, label = "Manufacturing"),
             size = 4.5,
             label.size = NA,
             colour = "#1a698a",
             fill = NA)+
  geom_label(aes(x=as.Date("2009-03-10"), y=47, label = "Services"),
             size = 4.5,
             label.size = NA,
             colour = "#1ab0d4",
             fill = NA)+
  geom_label(aes(x=as.Date("2008-11-01"), y=28, label = "Construction"),
             size = 4.5,
             label.size = NA,
             colour = "#663882",
             fill = NA)+
  geom_label(aes(x=as.Date("2009-01-01"), y=61.5, label = "Global Financial Crisis"),
             size = 5,
             label.size = NA,
             colour = "white",
             fill = "#1a698a")
ch_gfc

#plot COVID-19 chart
ch_covid <- ggplot(data=d_covid, aes(x = date)) +
  geom_line(aes(y=PMI_COVID),size = 1, color = "#1a698a", group = 3) +
  geom_line(aes(y=PSI_COVID),size = 1, color = "#1ab0d4")+
  geom_line(aes(y=PCI_COVID),size = 1, color = "#663882")+
  aig_style()+
  aig_y_continuous(limits=c(20,65),breaks = seq(20,65, by = 10))+
  scale_x_date(limit=c(as.Date("2020-01-01"),as.Date("2021-12-30")),
               date_breaks = ("1 year"), labels = date_format("%Y"))+
  geom_hline(yintercept=50, linetype="dashed", color = "red")+
  theme(axis.text.y=element_blank())+
  geom_label(aes(x=as.Date("2021-10-01"), y=55, label = "Sector expanding
(over 50 points)"),
             size = 3,
             label.size = NA,
             colour = 'black',
             fill = NA)+
  geom_label(aes(x=as.Date("2021-10-01"), y=25, label = "Sector contracting
(under 50 points)"),
             size = 3,
             label.size = NA,
             colour = 'black',
             fill = NA)+
  geom_label(aes(x=as.Date("2021-01-01"), y=61.5, label = "COVID-19"),
             size = 5,
             label.size = NA,
             colour = "white",
             fill = "#1a698a")
ch_covid

#using ggarrange to make plots side by side
ch_ps <- ggarrange(ch_gfc, ch_covid, ncol=2, nrow=1)
ch_ps

#Add title and subtitle
ch_ps_title<- ch_ps +
  annotate("text", label="Index, 3-month average",
           x=-Inf, y=Inf, vjust=3,hjust=-0.025,size=6)+
  annotate("text", label="Ai Group Performance Indicators during the GFC & COVID-19",
           x=-Inf, y=Inf, vjust=1.2,hjust=-0.01,size=7, color="#1a698a",fontface="bold")

#use finalise plot function from aig_theme on my github
finalise_plot(plot_name = ch_ps_title,
              source = "Source: Ai Group",
              save_filepath = "..day09.png",
              width_pixels = 640,
              height_pixels = 450,
              logo_image_path = "[logopath]...aig_logo.png")
