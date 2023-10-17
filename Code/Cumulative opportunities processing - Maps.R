#Cumulative opportunities processing - Maps

rm(list=ls())

##install.packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("leaflet")
install.packages("patchwork")
install.packages("xlsx")
install.packages("sf")
install.packages("mapview")
install.packages("gstat")
install.packages("tmap")
install.packages("osmdata")
install.packages("tmaptools")
install.packages("OpenStreetMap")
install.packages("spdep")

##library
library(tidyverse)
library(dplyr)
library(leaflet)
library(patchwork)
library(xlsx)
library(sf)
library(mapview)
library(gstat)
library(tmap)
library(osmdata)
library(tmaptools)
library(OpenStreetMap)
library(spdep)

##Databases
setwd("D:/~")
CO_2022<-read.csv("CO_2022.csv",sep = ',',header = TRUE)
CO_2023<-read.csv("CO_2023.csv",sep = ',',header = TRUE)
CO_2022<-rename(CO_2022,"link"=id)
CO_2023<-rename(CO_2023,"link"=id)
census_zones <- st_read("D:/~/Census zones - shp.shp")
census_zones$link <- as.integer(census_zones$link)
ejido <- st_read("D:/~/EjidoMunicipal.shp")
hospitals <- st_read("D:/~/Public hospitals - shp.shp")

##Table join
CO_2022 <- left_join(census_zones,CO_2022,by = "link")
CO_2023 <- left_join(census_zones,CO_2023,by = "link")

##Maps config
data(ejido)
osm_CBA <- read_osm(ejido, ext=1.1)
tmap_mode('plot') ### static maps mode
pal <- c('#f0eb54','#efc847','#d8854a','#b52150', '#a90052')
# scales::show_col(c('#f0eb54','#efc847','#d8854a','#b52150', '#a90052'))

##Maps
hospitals_icon <- tmap_icons(c("D:/~/first-aid-99069_640.png")) #choose any icon of preference
map_CO_2022 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CO_2022) + 
  tm_polygons(col = "accessibility",  
              title = "Cumulative opportunities in 60 minutes  - 2022", 
              palette = pal ,
              n=5,
              alpha = 0.7, 
              breaks = c(0,3,6,9,12,14),
              textNA = "N/D",
              legend.format = c(text.separator = "-"),
              border.alpha = 0.5,
              lwd = 0.2)+
  tm_layout(frame = FALSE,
            fontfamily = "serif",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1,
            legend.text.size = 0.8)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_shape(hospitals)+
  tm_symbols(size = 0.08,
             shape = hospitals_icon)

map_CO_2023 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CO_2023) + 
  tm_polygons(col = "accessibility",  
              title = "Cumulative opportunities in 60 minutes  - 2023", 
              palette = pal ,
              n=5,
              alpha = 0.7, 
              breaks = c(0,3,6,9,12,14),
              textNA = "N/D",
              legend.format = c(text.separator = "-"),
              border.alpha = 0.5,
              lwd = 0.2)+
  tm_layout(frame = FALSE,
            fontfamily = "serif",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1,
            legend.text.size = 0.8)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_shape(hospitals)+
  tm_symbols(size = 0.08,
             shape = hospitals_icon)
map <- tmap_arrange(map_CO_2022,map_CO_2023,ncol=2,nrow = 1)
tmap_save(map, filename="Map_CO_2022_2023.png", height=4, width=6, units="in", dpi=300)

rm(list=ls())
