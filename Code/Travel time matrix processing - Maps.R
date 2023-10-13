#Travel time matrix processing - Maps

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
library(Hmisc)

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
ejido <- st_read("D:/~/EjidoMunicipal.shp")
hospitals <- st_read("D:/~/Public hospitals - shp/Public hospitals - shp.shp")
hospitals_CAT1 <- filter(hospitals, Categoría == "Alto riesgo con terapia intensiva especializada")
hospitals_CAT2 <- filter(hospitals, Categoría == "Alto riesgo con terapia intensiva")
hospitals_CAT3 <- filter(hospitals, Categoría == "Bajo riesgo con internación simple")

##Table join
CAT1_2022 <- left_join(census_zones,CAT1_2022,by = "link")
CAT1_2023 <- left_join(census_zones,CAT1_2023,by = "link")
CAT2_2022 <- left_join(census_zones,CAT2_2022,by = "link")
CAT2_2023 <- left_join(census_zones,CAT2_2023,by = "link")
CAT3_2022 <- left_join(census_zones,CAT3_2022,by = "link")
CAT3_2023 <- left_join(census_zones,CAT3_2023,by = "link")

##Maps config
data(ejido)
osm_CBA <- read_osm(ejido, ext=1.1)
tmap_mode('plot') ### static maps mode
pal <- c('#f0eb54','#efc847','#d8854a','#b52150', '#a90052')
# scales::show_col(c('#f0eb54','#efc847','#d8854a','#b52150', '#a90052'))

##Maps: average travel time per quintiles
hospitals_icon <- tmap_icons(c("D:/~/first-aid-99069_640.png")) #choose any icon of preference
map_CAT1_2022 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT1_2022) + 
  tm_polygons(col = "avg_travel_time",  
              title = "Average travel time [minutes] - CATI - 2022", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(5,20,40,60,90,120),
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
  tm_shape(hospitals_CAT1)+
  tm_symbols(size = 0.08,
             shape = hospitals_icon)

map_CAT1_2023 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT1_2023) + 
  tm_polygons(col = "avg_travel_time",  
              title = "Average travel time [minutes] - CATI - 2023", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(5,20,40,60,90,120),
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
  tm_shape(hospitals_CAT1)+
  tm_symbols(size = 0.08,
             shape = hospitals_icon)

map_CAT2_2022 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT2_2022) + 
  tm_polygons(col = "avg_travel_time",  
              title = "Average travel time [minutes] - CATII - 2022", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(5,20,40,60,90,120),
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
  tm_shape(hospitals_CAT2)+
  tm_symbols(size = 0.08,
             shape = hospitals_icon)

map_CAT2_2023 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT2_2023) + 
  tm_polygons(col = "avg_travel_time",  
              title = "Average travel time [minutes] - CATII - 2023", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(5,20,40,60,90,120),
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
  tm_shape(hospitals_CAT2)+
  tm_symbols(size = 0.08,
             shape = hospitals_icon)

map_CAT3_2022 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT3_2022) + 
  tm_polygons(col = "avg_travel_time",  
              title = "Average travel time [minutes] - CATIII - 2022", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(5,20,40,60,90,120),
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
  tm_shape(hospitals_CAT3)+
  tm_symbols(size = 0.08,
             shape = hospitals_icon)

map_CAT3_2023 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT3_2023) + 
  tm_polygons(col = "avg_travel_time",  
              title = "Average travel time [minutes] - CATIII - 2023", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(5,20,40,60,90,120),
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
  tm_shape(hospitals_CAT3)+
  tm_symbols(size = 0.08,
             shape = hospitals_icon)

map_2022_2023 <- tmap_arrange(map_CAT1_2022,map_CAT1_2023,map_CAT2_2022,map_CAT2_2023,map_CAT3_2022,map_CAT3_2023,ncol=2,nrow = 3)
tmap_save(map_2022_2023, filename="D:/~/Map_2022_2023.png", height=12, width=6, units="in", dpi=300)

##NBI map
census_zones <- mutate(census_zones,porc_hogaresSinNBI=100-X.hogaresNB)
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(census_zones$porc_hogaresSinNBI, census_zones$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
census_zones$quintilesNBI <- sapply(census_zones$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilesNBI, data = census_zones, sum)

census_zones[1491, 16] = "Q1"

data(ejido)
osm_CBA <- read_osm(ejido, ext=1.1)
tmap_mode('plot') ### static maps mode
pal <- c('#f0eb54','#efc847','#d8854a','#b52150', '#a90052')

map_NBI <-
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(census_zones) + 
  tm_polygons(col = "quintilesNBI",  
              title = "%NBI quintiles", 
              palette = pal ,
              n=5,
              alpha = 0.6,
              border.alpha = 0.5,
              lwd = 0.2)+
  tm_layout(frame = FALSE,
            fontfamily = "serif",
            legend.outside = TRUE,
            legend.outside.position = "right",
            legend.title.size = 1,
            legend.text.size = 0.8)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_shape(hospitals)+
  tm_symbols(size = 0.08,
             shape = hospitals_icon,
             sizes.legend = 0.08,
             labels = "Nombre",
             title.shape = "Public Hospitals IDERA - SISA",
             legend.shape.show = TRUE)

tmap_save(map_NBI, filename="D:/~/Map_NBI.png", height=6, width=6, units="in", dpi=300)


rm(list=ls())
