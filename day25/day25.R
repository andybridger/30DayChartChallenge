#code largely copied from @SiphuLangeni

#load required packages
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, ggplot2, gganimate, gifski, kableExtra, ggpol)

#download data from github
urlfile <- "https://raw.githubusercontent.com/andybridger/animations/main/data/aus_pop.csv?token=AQQH5KRLNKC7NJAVRHE5Y4TACE326"
aus_pop <- read.csv(url(urlfile), check.names = FALSE)

#check column names
colnames(aus_pop)

aus_pop <- aus_pop %>%
#mutate '85 and over' to '85+'
  mutate(age = as.factor(case_when(age=='85 and over' ~ '85+', TRUE~age))) %>%
#wide to long data using gather function
#converts 1901:2031 to years and the values as pop
  gather(year, pop, "1901":"2031") %>%
#change year to integer and make female pop values to negative for the pyramid  
  mutate(year = as.integer(year),
         pop = ifelse(sex == 'Females', as.integer(pop * -1), as.integer(pop)))

#have a peak at what the data looks like
dim(aus_pop)
kable(head(aus_pop)) %>%
  kable_styling(bootstrap_options = 'striped', font_size = 12, full_width = FALSE)
str(aus_pop)

#next, we create the animation in  3 steps
#firstly, create the static plot
pop_pyra <- aus_pop %>%
  ggplot(aes(
    x = age,
    y = pop/1000,
    fill = sex
    )
  ) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("#1ab0d4", "#8e9fbc")) + 
      coord_flip() + 
      facet_share(
      ~ sex,
      dir = 'h',
      scales = 'free',
      reverse_num = TRUE
      )

#set the theme - see if aig_style works
pop_pyra <- pop_pyra +
    theme(
      plot.background = element_blank(),
      panel.background = element_blank(),
      axis.title.y = element_blank(),
      legend.title = element_blank(),
      panel.border = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(), 
      axis.text = element_text(size = 14),
      legend.key.size = unit(0.75, 'cm'),
      legend.text = element_text(size = 15, face = 'bold'),
      plot.title = element_text(
        size = 18,
        hjust = 0.5,
        face = 'bold'
      ),
      plot.subtitle = element_text(
        size = 14,
        hjust = 0.5,
        face = 'bold'
      ),
      axis.title.x = element_text(
        size = 16,
        face = 'bold'
      ),
      plot.caption = element_text(
        size = 10,
        hjust = 0.0
      )
    ) +
#add labels - animation occurs with variable of time. This is done using {closest state}.
  labs(
    title = 'Australian population pyramid since\nfederation and forecast to 2031\n\n{closest_state}',
    subtitle = '\n\nAge Group',
    y = '\n\nPopulation (in thousands)',
    caption = 'Note: ABS data includes estimates of Indigenous populations from 1961 onwards. Before 1971, 
    estimates of the population were based on the number of people actually present in Australia. 
    From 1971 onwards, the concept of estimated resident population (ERP) was introduced.
    #30DayChartChallenge | Viz: @AndyBridger | Inspiration: @SiphuLangeni
    Data Source: Australian Bureau of Statistics and Australian Government Centre for Population'
  )

#secondly apply animation parameters
#use transition states to make it happen across time
#The transition_length and state_length give a relative length of the the transition and state
pop_pyra <- pop_pyra + 
  transition_states(
    year,
    transition_length = 1.5,
    state_length = 1.5
  ) + 
  enter_fade() +
  exit_fade() + 
  ease_aes('cubic-in-out')

#Third, render to file
animate(
  pop_pyra,
  fps = 15,
  duration = 45,
  width = 500,
  height = 500,
  end_pause = 5,
  renderer = gifski_renderer('pop_pyra.gif')
)
