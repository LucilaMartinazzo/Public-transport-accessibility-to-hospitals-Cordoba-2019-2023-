#CS test

rm(list=ls())

##Library
library(RSQLite)
library(leaflet)
library(Hmisc)
library(r5r)
library(sf)
library(data.table)
library(ggplot2)
library(xlsx)
library(akima)
library(dplyr)
library(mapview)
library(patchwork)
library(osmdata)
library(tmaptools)
library(OpenStreetMap)
library(spdep)
library(tmap)
library(tmaptools)
mapviewOptions(platform = 'leafgl')

##Datasets
setwd("D:/~")
census_zones<-read.csv("Census zones (centroid) with NBI data.csv",sep = ',',header = TRUE)
ttm<-read.csv("TTM_2023.csv",sep = ',',header = TRUE)
ttm_CS<-read.csv("TTM_2023_CS_HPA.csv",sep = ',',header = TRUE)

##Create base
con<-dbConnect(RSQLite::SQLite(),":memory:")
dbWriteTable(con,"zones",census_zones)
dbWriteTable(con,"TTM_2023",ttm)
dbWriteTable(con,"TTM_2023_CS",ttm_CS)

##Data per hospital category
CAT3_2023_1<-dbGetQuery(con, "select from_id as id, min(travel_time_p85) as min_travel_time, to_id as hospital from TTM_2023 where to_id in (8,9,10,13,15) group by from_id")
dbWriteTable(con,"CAT3_2023_1",CAT3_2023_1)
CAT3_2023_1_CS<-dbGetQuery(con, "select from_id as id, min(travel_time_p85) as min_travel_time, to_id as hospital from TTM_2023_CS where to_id in (8,9,10,13,15,16,17,18) group by from_id")
dbWriteTable(con,"CAT3_2023_1_CS",CAT3_2023_1_CS)

##Table join
CAT3_2023<-dbGetQuery(con, "select zones.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, min_travel_time, hospital from zones inner join CAT3_2023_1 on zones.id=CAT3_2023_1.id")
CAT3_2023_CS<-dbGetQuery(con, "select zones.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, min_travel_time, hospital from zones inner join CAT3_2023_1_CS on zones.id=CAT3_2023_1_CS.id")

##Categorization in percentile 20 (quintiles) per % households with NBI
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT3_2023$porc_hogaresSinNBI, CAT3_2023$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT3_2023$quintilesNBI <- sapply(CAT3_2023$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilesNBI, data = CAT3_2023, sum)

CAT3_2023[163, 10] = "Q1"

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT3_2023_CS$porc_hogaresSinNBI, CAT3_2023_CS$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT3_2023_CS$quintilesNBI <- sapply(CAT3_2023_CS$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilesNBI, data = CAT3_2023_CS, sum)

CAT3_2023_CS[163, 10] = "Q1"

##Main stats
stat_CAT3_2023 <- CAT3_2023 %>% 
  group_by(quintilesNBI) %>% 
  summarise(mean = mean(min_travel_time), median(min_travel_time), sd(min_travel_time),min(min_travel_time),max(min_travel_time)) %>% 
  mutate(CAT=3) %>% 
  mutate(anio=2023) %>% 
  mutate(caminata=833) %>% 
  mutate(case="regular")
stat_CAT3_2023_CS <- CAT3_2023_CS %>% 
  group_by(quintilesNBI) %>% 
  summarise(mean = mean(min_travel_time), median(min_travel_time), sd(min_travel_time),min(min_travel_time),max(min_travel_time)) %>% 
  mutate(CAT=3) %>% 
  mutate(anio=2023) %>% 
  mutate(caminata=833) %>% 
  mutate(case="study")

stats_total <- rbind(stat_CAT3_2023,stat_CAT3_2023_CS)
write.xlsx(stats_total,"D:/~/stats_CS_HPA.xlsx")

##Table join
census_zones <- st_read("D:/~/Census zones - shp.shp")
census_zones$link <- as.integer(census_zones$link)
CAT3_2023 <- left_join(census_zones,CAT3_2023,by = "link")
CAT3_2023_CS <- left_join(census_zones,CAT3_2023_CS,by = "link")

##Maps config
ejido <- st_read("D:/~/EjidoMunicipal.shp")
hospitals <- st_read("D:/~/Public hospitals - shp.shp")
hospitals_CAT3 <- filter(hospitals, Categoría == "Bajo riesgo con internación simple")
points <- matrix(c(-64.25606416,-64.16458146,-64.252843,-31.34019214,-31.39362591,-31.433377), nrow = 3, ncol = 2)
HPA <- st_multipoint(points)
HPA <-st_sfc(HPA, crs = 4326)
data(ejido)
osm_CBA <- read_osm(ejido, ext=1.1)
tmap_mode('plot') ### static maps mode
pal <- c('#f0eb54','#efc847','#d8854a','#b52150', '#a90052')

##Maps: average travel time per quintiles
hospitals_icon <- tmap_icons(c("D:/~/first-aid-99069_640.png")) #choose any icon of preference
map_CAT3_2023 <-
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT3_2023) + 
  tm_polygons(col = "min_travel_time",  
              title = "Minimum travel time [minutes] - CATIII - 2023\nBefore HPA", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(0,20,40,60,90,120),
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

map_CAT3_2023_CS <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT3_2023_CS) + 
  tm_polygons(col = "min_travel_time",  
              title = "Minimum travel time [minutes] - CATIII - 2023\nAfter HPA", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(0,20,40,60,90,120),
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
             shape = hospitals_icon)+
  tm_shape(HPA)+
  tm_symbols(size = 0.08,
             shape = hospitals_icon)

map_2023_CS_HPA <- tmap_arrange(map_CAT3_2023,map_CAT3_2023_CS,ncol=2,nrow = 1)
tmap_save(map_2023_CS_HPA, filename="D:/~/Map_2023_CS_HPA.png", height=4, width=6, units="in", dpi=300)

#To finish
dbDisconnect(con)
rm(list=ls())
