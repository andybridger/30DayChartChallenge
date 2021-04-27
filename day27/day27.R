# #30DayChartChallenge | Day 27 - educational (uncertainties)
###load packages###
library(data.table)
library(tidyverse)
library(readxl)
library(ggplot2)
library(aigtheme)
library(ggalt)
library(tidyr)

###load data###
d_edu <- read_csv("data/day27.csv")
str(d_edu)

###load colours###
aig_blue <- "#1a698a"
aig_teal <- "#1ab0d4"
aig_purple <- "#663882"
aig_grey <- "#333F48"
aig_coolgrey <- "#8e9fbc"
aig_red <- "#a6192e"
aig_orange <- "#ffb533"
aig_green <- "#87a31a"
aig_lightblue <- "#e5f5fa"

###aig_style function###
#' Add Ai Group theme to ggplot chart - the code is based bbplot from the BBC
#' This function allows you to add the Ai Group theme to your ggplotgraphics.

# function
aig_style <- function() {
  font <- "Arial"
  
  ggplot2::theme(
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=20,
                                       face="bold",
                                       color="#1a698a"),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=16,
                                          margin=ggplot2::margin(3,0,3,0)),
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
                                      size=16,
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
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 16,  hjust = 0)
  )
}

###finalise plot function###
#save plot function 
save_plot <- function (plot_grid, width, height, save_filepath) {
  grid::grid.draw(plot_grid)
  #save it
  ggplot2::ggsave(filename = save_filepath,
                  plot=plot_grid, width=(width/72), height=(height/72),  bg="white")
}

#Left align text
left_align <- function(plot_name, pieces){
  grob <- ggplot2::ggplotGrob(plot_name)
  n <- length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] <- 2
  return(grob)
}

#function to create footer (source and log)
create_footer <- function (source_name, logo_image_path) {
  #Make the footer
  footer <- grid::grobTree(grid::linesGrob(x = grid::unit(c(0, 1), "npc"), y = grid::unit(1.1, "npc")),
                           grid::textGrob(source_name,
                                          x = 0.004, hjust = -0.01, gp = grid::gpar(fontsize=10)),
                           grid::rasterGrob(png::readPNG(logo_image_path), x = 0.944))
  return(footer)
  
}

#' Arrange alignment and save Ai Group ggplot chart
#'
#' Running this function will save your plot with the correct guidelines for publication for a Ai Group blog/media graphic.
#' It will left align your title, subtitle and source, add the Ai Group logo at the bottom right and save it to your specified location.
#' @param plot_name The variable name of the plot you have created that you want to format and save
#' @param source_name The text you want to come after the text 'Source:' in the bottom left hand side of your side
#' @param save_filepath Exact filepath that you want the plot to be saved to
#' @param width_pixels Width in pixels that you want to save your chart to - defaults to 640
#' @param height_pixels Height in pixels that you want to save your chart to - defaults to 450
#' @param logo_image_path File path for the logo image you want to use in the right hand side of your chart,
#'  which needs to be a PNG file - defaults to BBC blocks image that sits within the data folder of your package
#' @return (Invisibly) an updated ggplot object.

#' @keywords finalise_plot
#' @examples
#' finalise_plot(plot_name = myplot,
#' source = "The source for my data",
#' save_filepath = "filename_that_my_plot_should_be_saved_to-nc.png",
#' width_pixels = 640,
#' height_pixels = 450,
#' logo_image_path = "logo_image_filepath.png"
#' )
#'
#' @export
finalise_plot <- function(plot_name,
                          source_name,
                          save_filepath=file.path(Sys.getenv("TMPDIR"), "tmp-nc.png"),
                          width_pixels=640,
                          height_pixels=450,
                          logo_image_path = file.path(system.file("data", package = 'bbplot'),"placeholder.png")) {
  
  footer <- create_footer(source_name, logo_image_path)
  
  #Draw your left-aligned grid
  plot_left_aligned <- left_align(plot_name, c("subtitle", "title", "caption"))
  plot_grid <- ggpubr::ggarrange(plot_left_aligned, footer,
                                 ncol = 1, nrow = 2,
                                 heights = c(1, 0.045/(height_pixels/450)))
  ## print(paste("Saving to", save_filepath))
  save_plot(plot_grid, width_pixels, height_pixels, save_filepath)
  ## Return (invisibly) a copy of the graph. Can be assigned to a
  ## variable or silently ignored.
  invisible(plot_grid)
}

###plot the data###
ch_edu <- ggplot(data=d_edu, aes(x = Women, xend = Men, y = reorder(Education,-diff), group = Education)) +
  geom_dumbbell(colour = aig_coolgrey,
                size = 3,
                colour_x = aig_red,
                colour_xend = "#1a698a") +
  aig_style()+
  scale_x_continuous(limit=c(27,63))+
  labs(title = "The gender pay gap is evident across all education levels except
those without non-school qualifications",
       subtitle = "Median hourly earnings ($A) in main job by sex and education, August 2020",
       caption = 'Note: ndf. is not further defined')+
  geom_text(data=filter(d_edu, Education=="Postgraduate degree"),
          aes(x=Women, y=Education, label="Women"),
          colour=aig_red, size = 4, vjust = -1.0, fontface="bold")+
  geom_text(data=filter(d_edu, Education=="Postgraduate degree"),
            aes(x=Men, y=Education, label="Men"),
            colour="#1a698a", size = 4, vjust = -1.0, fontface="bold")+
  geom_text(data=d_edu,
            aes(x=Women, y=Education, label=paste0("$",Women)),
            colour=aig_red, size = 3, vjust = 2.0)+
  geom_text(data=d_edu,
            aes(x=Men, y=Education, label=paste0("$",Men)),
            colour=aig_blue, size = 3, vjust = 2.0)+
  geom_rect(data=d_edu, aes(xmin=57, xmax = 63, ymin=-Inf,ymax=Inf),
            fill=aig_lightblue)+
  geom_text(data=d_edu,aes(label=paste0("$",-diff),
            y=Education,x=60),fontface = "bold", size = 3)+
  geom_text(data=filter(d_edu, Education=="Postgraduate degree"),
            aes(x=60, y = Education, label = "Difference"),
            colour = "black", size = 4, vjust = -1, fontface="bold")
ch_edu

###save plot using the finalise plot function###
finalise_plot(plot_name = ch_edu,
              source = "Source: Australian Bureau of Statistics",
              save_filepath = "...charts/day25.png",
              width_pixels = 640,
              height_pixels = 450,
              #file path for logo in bottom right corner
              logo_image_path = "charts/aig_logo.png")
