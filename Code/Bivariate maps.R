#Bi-variate maps

rm(list=ls())

##library
library(sf)
library(dplyr)
library(mapview)
library(gstat)
library(tmap)
library(tidyverse)
library(osmdata)
library(tmaptools)
library(OpenStreetMap)
library(spdep)
library(rstudioapi)
library(magrittr) # pipes
library(lintr) # code linting
library(viridis) # viridis color scale
library(cowplot) # stack ggplots
windowsFonts(Cambria=windowsFont("Cambria"))

##Databases
setwd("D:/~")
CAT1_2022<-read.csv("CAT1_2022.csv",sep = ',',header = TRUE)
CAT1_2023<-read.csv("CAT1_2023.csv",sep = ',',header = TRUE)
CAT2_2022<-read.csv("CAT2_2022.csv",sep = ',',header = TRUE)
CAT2_2023<-read.csv("CAT2_2023.csv",sep = ',',header = TRUE)
CAT3_2022<-read.csv("CAT3_2022.csv",sep = ',',header = TRUE)
CAT3_2023<-read.csv("CAT3_2023.csv",sep = ',',header = TRUE)
census_zones <- st_read("D:/~/Census zones - shp.shp")
census_zones$link <- as.integer(census_zones$link)

##Table join
CAT1_2022 <- left_join(census_zones,CAT1_2022,by = "link")
CAT1_2023 <- left_join(census_zones,CAT1_2023,by = "link")
CAT2_2022 <- left_join(census_zones,CAT2_2022,by = "link")
CAT2_2023 <- left_join(census_zones,CAT2_2023,by = "link")
CAT3_2022 <- left_join(census_zones,CAT3_2022,by = "link")
CAT3_2023 <- left_join(census_zones,CAT3_2023,by = "link")

##Basic map config
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "serif",
                          color = "Black"),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = "#F5FCEB",
                                     color = NA),
      panel.background = element_rect(fill = "#FCF7F9",
                                      color = NA),
      legend.background = element_rect(fill = "#FCF7F9",
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.2, .2, .2, .2), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = "Black"),
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = "Black"),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = "Black",
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}

##Map
###Quantiles %NBI
quantiles_NBI_2022 <- CAT1_2022 %>%
  pull(X.hogaresNB) %>%
  quantile(probs = seq(0, 1, length.out = 4))
quantiles_NBI_2023 <- CAT1_2023 %>%
  pull(X.hogaresNB) %>%
  quantile(probs = seq(0, 1, length.out = 4))

###Quantiles travel time
quantiles_travel_time_CAT1_2022 <- CAT1_2022 %>%
  pull(avg_travel_time) %>%
  quantile(probs = seq(0, 1, length.out = 4),na.rm=TRUE)
quantiles_travel_time_CAT2_2022 <- CAT2_2022 %>%
  pull(avg_travel_time) %>%
  quantile(probs = seq(0, 1, length.out = 4),na.rm=TRUE)
quantiles_travel_time_CAT3_2022 <- CAT3_2022 %>%
  pull(avg_travel_time) %>%
  quantile(probs = seq(0, 1, length.out = 4),na.rm=TRUE)
quantiles_travel_time_CAT1_2023 <- CAT1_2023 %>%
  pull(avg_travel_time) %>%
  quantile(probs = seq(0, 1, length.out = 4),na.rm=TRUE)
quantiles_travel_time_CAT2_2023 <- CAT2_2023 %>%
  pull(avg_travel_time) %>%
  quantile(probs = seq(0, 1, length.out = 4),na.rm=TRUE)
quantiles_travel_time_CAT3_2023 <- CAT3_2023 %>%
  pull(avg_travel_time) %>%
  quantile(probs = seq(0, 1, length.out = 4),na.rm=TRUE)

##Colour scale
###Red %HNBI and blue for travel times
bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949", # high NBI, high travel time
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1", # low NBI, high travel time
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A", # medium NBI, medium travel time
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E", # high NBI, low travel time
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0" # low NBI, low travel time
) %>%
  gather("group", "fill")

#### #### ####

###Cut base by groups and left_join with color
CAT3_2023 %<>%
  mutate(
    NBI_quantiles = cut(
      X.hogaresNB,
      breaks = quantiles_NBI_2023,
      include.lowest = TRUE
    ),
    travel_time_quantiles = cut(
      avg_travel_time,
      breaks = quantiles_travel_time_CAT3_2023,
      include.lowest = TRUE
    ),
    group = paste(
      as.numeric(NBI_quantiles), "-",
      as.numeric(travel_time_quantiles)
    )
  ) %>%
  left_join(bivariate_color_scale, by = "group")

##Map drawing
map <- ggplot(
  data = CAT3_2023
) +
  geom_sf(
    aes(
      fill = fill
    ),
    color = "black",
    size = 0.1
  ) +
  scale_fill_identity() +
  # add the theme
  theme_map()

#Legend
bivariate_color_scale %<>%
  separate(group, into = c("X.hogaresNB", "avg_travel_time"), sep = " - ") %>%
  mutate(X.hogaresNB = as.integer(X.hogaresNB),
         avg_travel_time = as.integer(avg_travel_time))

legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = X.hogaresNB,
      y = avg_travel_time,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Higher %NBI",
       y = "Higher travel times") +
  theme_map() +
  theme(
    axis.title = element_text(size = 30)
  ) +
  # quadratic tiles
  coord_fixed()

#Plot
ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  theme(text = element_text(family = "serif"))

legend