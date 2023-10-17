#Travel time matrix and accessibility to public hospitals with r5r - Cordoba, Argentina

rm(list=ls())

##install.packages
install.packages('r5r')
install.packages('mapview')
install.packages('akima')

##Set JAVA memory (requires Java SE Development Kit version >= 11.0.8)
options(java.parameters = "-Xmx2G")

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
mapviewOptions(platform = 'leafgl')

##Databases
###GTFS, OSM base map, hospitals as points of interest and census zones centroid as origins
data_path <- file.path("D:/~")
list.files(data_path) # To verify
poi <- fread(file.path(data_path, "Public hospitals - IDERA-SISA.csv"))
poi <- add_row(poi, id=16,lon=-64.25606416,lat=-31.34019214, Name="", descriptio="",Nombre="Hospital de Pronta Atención Cura Brochero",Dirección="Av. Ricardo Rojas 6950",Localidad="Córdoba",Categoría="Bajo riesgo con internación simple",Dependenci="Municipal") #Bajo riesgo con internaciÃ³n simple
poi <- add_row(poi, id=17,lon=-64.16458146,lat=-31.39362591, Name="", descriptio="",Nombre="Hospital de Pronta Atención Comipaz",Dirección="Av. del Trabajo 830",Localidad="Córdoba",Categoría="Bajo riesgo con internación simple",Dependenci="Municipal")
poi <- add_row(poi, id=18,lon=-64.252843,lat=-31.433377, Name="", descriptio="",Nombre="Hospital de Pronta Atención Villa Adela",Dirección="Av. Fuerza Aerea Argentina 5085",Localidad="Córdoba",Categoría="Bajo riesgo con internación simple",Dependenci="Municipal")
points <- fread(file.path(data_path, "Census zones (centroid) with NBI data.csv"))

##Network from OMS base map and GTFS provided
r5r_core <- setup_r5(data_path = data_path, verbose = FALSE)

##Travel time matrix 2023
###Inputs
mode <- c("WALK", "TRANSIT")
max_walk_time <- 10   # minutes
max_trip_duration <- 120 # minutes
percentiles <- 85
time_window <- 120 # minutes
departure_datetime <- as.POSIXct("12-04-2023 09:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

ttm <- travel_time_matrix(r5r_core = r5r_core,
                          origins = points,
                          destinations = poi,
                          mode = mode,
                          departure_datetime = departure_datetime,
                          max_walk_time = max_walk_time,
                          max_trip_duration = max_trip_duration,
                          percentiles=percentiles,
                          time_window=time_window,
                          verbose = FALSE)
write.xlsx(ttm,"D:/~/TTM_2023_CS_HPA.xlsx")

##Accessibility 2023
###Inputs
mode <- c("WALK", "TRANSIT")
max_walk_time <- 10   # minutes
max_trip_duration <- 120 # minutes
percentiles <- 85
time_window <- 120 # minutes
travel_time_cutoff <- 61
departure_datetime <- as.POSIXct("12-04-2023 09:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

access <- accessibility(r5r_core,
                        origins = points,
                        destinations = points,
                        mode = mode,
                        opportunities_colname = "Hospitals",
                        decay_function = "step",
                        cutoffs = travel_time_cutoff,
                        departure_datetime = departure_datetime,
                        max_walk_time = max_walk_time,
                        time_window = time_window,
                        percentiles = percentiles,
                        verbose = FALSE)
write.xlsx(access,"D:/~/CO_2023_CS_HPA.xlsx")

#Limpiar despues del uso
stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)

##Datasets
setwd("D:/~")
census_zones<-read.csv("Census zones (centroid) with NBI data.csv",sep = ',',header = TRUE)

##Create base
con<-dbConnect(RSQLite::SQLite(),":memory:")
dbWriteTable(con,"zones",census_zones)
dbWriteTable(con,"TTM_2023_CS",ttm)

##Data per hospital category
CAT1_2023_1_CS<-dbGetQuery(con, "select from_id as id, avg(travel_time_p85) as avg_travel_time from TTM_2023_CS where to_id in (1,2,3,4) group by from_id")
CAT2_2023_1_CS<-dbGetQuery(con, "select from_id as id, avg(travel_time_p85) as avg_travel_time from TTM_2023_CS where to_id in (5,6,7,11,14) group by from_id")
CAT3_2023_1_CS<-dbGetQuery(con, "select from_id as id, avg(travel_time_p85) as avg_travel_time from TTM_2023_CS where to_id in (8,9,10,13,15,16,17,18) group by from_id")
dbWriteTable(con,"CAT1_2023_1_CS",CAT1_2023_1_CS)
dbWriteTable(con,"CAT2_2023_1_CS",CAT2_2023_1_CS)
dbWriteTable(con,"CAT3_2023_1_CS",CAT3_2023_1_CS)

##Table join
CAT1_2023_CS<-dbGetQuery(con, "select zones.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_travel_time from zones inner join CAT1_2023_1_CS on zones.id=CAT1_2023_1_CS.id")
CAT2_2023_CS<-dbGetQuery(con, "select zones.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_travel_time from zones inner join CAT2_2023_1_CS on zones.id=CAT2_2023_1_CS.id")
CAT3_2023_CS<-dbGetQuery(con, "select zones.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_travel_time from zones inner join CAT3_2023_1_CS on zones.id=CAT3_2023_1_CS.id")

##Categorization in percentile 20 (quintiles) per % households with NBI
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT1_2023_CS$porc_hogaresSinNBI, CAT1_2023_CS$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT1_2023_CS$quintilesNBI <- sapply(CAT1_2023_CS$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilesNBI, data = CAT1_2023_CS, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT2_2023_CS$porc_hogaresSinNBI, CAT2_2023_CS$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT2_2023_CS$quintilesNBI <- sapply(CAT2_2023_CS$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilesNBI, data = CAT2_2023_CS, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT3_2023_CS$porc_hogaresSinNBI, CAT3_2023_CS$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT3_2023_CS$quintilesNBI <- sapply(CAT3_2023_CS$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilesNBI, data = CAT3_2023_CS, sum)

CAT1_2023_CS[163, 9] = "Q1"
CAT2_2023_CS[163, 9] = "Q1"
CAT3_2023_CS[163, 9] = "Q1"

##Graph config
devtools::install_github('Mikata-Project/ggthemr',force=TRUE)
library(ggthemr)
palette_btext <- define_palette(background = '#e8ece5', 
                                text = c('black', 'black'), 
                                line = c('#bbb8ab', '#bbb8ab'),
                                gridline = '#bbb8ab',
                                swatch = (c('#14828C','#efc847','#d8854a','#cc644c','#b52150', '#a90052')),
                                gradient = c(low='#efc847', high='#a90052'))

ggthemr(palette_btext,layout = "scientific",text_size   = 11)
# scales::show_col(c('#14828C','#efc847','#d8854a','#cc644c','#b52150', '#a90052'))

##Density plots
d_CAT1_2023_CS <- 
  ggplot(data=CAT1_2023_CS, aes(x=avg_travel_time))+
  geom_density(mapping=aes(group=quintilesNBI,fill=quintilesNBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Average travel time [minutes]",y="",title="CATI - 2023")+
  facet_wrap(~quintilesNBI,ncol = 1)
d_CAT2_2023_CS <- 
  ggplot(data=CAT2_2023_CS, aes(x=avg_travel_time))+
  geom_density(mapping=aes(group=quintilesNBI,fill=quintilesNBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Average travel time [minutes]",y="",title="CATII - 2023")+
  facet_wrap(~quintilesNBI,ncol = 1)
d_CAT3_2023_CS <- 
  ggplot(data=CAT3_2023_CS, aes(x=avg_travel_time))+
  geom_density(mapping=aes(group=quintilesNBI,fill=quintilesNBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Average travel time [minutes]",y="",title="CATIII - 2023")+
  facet_wrap(~quintilesNBI,ncol = 1)

d_CAT1_2023_CS + theme(text = element_text(family = "serif")) + scale_x_continuous(name="Average travel time [minutes]", breaks=c(20,40,60,80,100,120), limits=c(20,120)) + scale_y_continuous(name="", breaks=c(0.00,0.01,0.02,0.03,0.04,0.05), limits=c(0.00,0.05)) +
  d_CAT2_2023_CS + theme(text = element_text(family = "serif")) + scale_x_continuous(name="Average travel time [minutes]", breaks=c(20,40,60,80,100,120), limits=c(20,120)) + scale_y_continuous(name="", breaks=c(0.00,0.01,0.02,0.03,0.04,0.05), limits=c(0.00,0.05)) +
  d_CAT3_2023_CS + theme(text = element_text(family = "serif")) + scale_x_continuous(name="Average travel time [minutes]", breaks=c(20,40,60,80,100,120), limits=c(20,120)) + scale_y_continuous(name="", breaks=c(0.00,0.01,0.02,0.03,0.04,0.05), limits=c(0.00,0.05)) # 1050x1000

##Graph config
palette_bbtext <- define_palette(background = '#e8ece5', 
                                 text = c('black', 'black'), 
                                 line = c('#bbb8ab', '#bbb8ab'),
                                 gridline = '#bbb8ab',
                                 swatch = (c('black','#cc644c','#b52150', '#a90052')),
                                 gradient = c(low='#efc847', high='#a90052'))

ggthemr(palette_bbtext,layout = "scientific",text_size   = 11)

##Boxplot
b_CAT1_2023_CS <-
  ggplot(data=CAT1_2023_CS, aes(y=avg_travel_time, x=quintilesNBI))+
  geom_boxplot(mapping=aes(group=quintilesNBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Household quintiles per %NBI",y="Average travel time [minutes]",title="CATI - 2023")
b_CAT2_2023_CS <-
  ggplot(data=CAT2_2023_CS, aes(y=avg_travel_time, x=quintilesNBI))+
  geom_boxplot(mapping=aes(group=quintilesNBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Household quintiles per %NBI",y="Average travel time [minutes]",title="CATII - 2023")
b_CAT3_2023_CS <-
  ggplot(data=CAT3_2023_CS, aes(y=avg_travel_time, x=quintilesNBI))+
  geom_boxplot(mapping=aes(group=quintilesNBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Household quintiles per %NBI",y="Average travel time [minutes]",title="CATIII - 2023")

b_CAT1_2023_CS + theme(text = element_text(family = "serif")) + scale_y_continuous(name="Average travel time [minutes]", breaks=c(20,40,60,80,100,120), limits=c(20,120))+
  b_CAT2_2023_CS + theme(text = element_text(family = "serif")) + scale_y_continuous(name="Average travel time [minutes]", breaks=c(20,40,60,80,100,120), limits=c(20,120))+
  b_CAT3_2023_CS + theme(text = element_text(family = "serif")) + scale_y_continuous(name="Average travel time [minutes]", breaks=c(20,40,60,80,100,120), limits=c(20,120)) # 1200x500

##Main stats
stat_CAT1_2023_CS <- CAT1_2023_CS %>% 
  group_by(quintilesNBI) %>% 
  summarise(mean = mean(avg_travel_time), median(avg_travel_time), sd(avg_travel_time),min(avg_travel_time),max(avg_travel_time)) %>% 
  mutate(CAT=1) %>% 
  mutate(anio=2023) %>% 
  mutate(caminata=833)
stat_CAT2_2023_CS <- CAT2_2023_CS %>% 
  group_by(quintilesNBI) %>% 
  summarise(mean = mean(avg_travel_time), median(avg_travel_time), sd(avg_travel_time),min(avg_travel_time),max(avg_travel_time)) %>% 
  mutate(CAT=2) %>% 
  mutate(anio=2023) %>% 
  mutate(caminata=833) %>% 
  rows_append(stat_CAT1_2023_CS)
stat_2023_CS <- CAT3_2023_CS %>% 
  group_by(quintilesNBI) %>% 
  summarise(mean = mean(avg_travel_time), median(avg_travel_time), sd(avg_travel_time),min(avg_travel_time),max(avg_travel_time)) %>% 
  mutate(CAT=3) %>% 
  mutate(anio=2023) %>% 
  mutate(caminata=833) %>% 
  rows_append(stat_CAT2_2023_CS)
write.xlsx(stat_2023_CS,"D:/~/stat_2023_CS_HPA.xlsx")

stat_CAT1_2023_CS <-
  CAT1_2023_CS %>% 
  summarise(mean = mean(avg_travel_time), median(avg_travel_time), sd(avg_travel_time),min(avg_travel_time),max(avg_travel_time)) %>% 
  mutate(CAT=1) %>% 
  mutate(anio=2023) %>% 
  mutate(caminata=833)
stat_CAT2_2023_CS <-
  CAT2_2023_CS %>% 
  summarise(mean = mean(avg_travel_time), median(avg_travel_time), sd(avg_travel_time),min(avg_travel_time),max(avg_travel_time)) %>% 
  mutate(CAT=2) %>% 
  mutate(anio=2023) %>% 
  mutate(caminata=833)
stat_CAT3_2023_CS <-
  CAT3_2023_CS %>% 
  summarise(mean = mean(avg_travel_time), median(avg_travel_time), sd(avg_travel_time),min(avg_travel_time),max(avg_travel_time)) %>% 
  mutate(CAT=3) %>% 
  mutate(anio=2023) %>% 
  mutate(caminata=833)

stats_total <- rbind(stat_CAT1_2023_CS,stat_CAT2_2023_CS,stat_CAT3_2023_CS)
write.xlsx(stats_total,"D:/~/stats_total_CS_HPA.xlsx")

#To finish
rm(list=ls())
