#Cumulative opportunities - Processing & Stats

rm(list=ls())

##install.packages
install.packages("devtools")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("RSQLite")
install.packages("leaflet")
install.packages("xlsx")
install.packages("Hmisc")

##Library
library(RSQLite)
library(leaflet)
library(ggplot2)
library(xlsx)
library(dplyr)
library(Hmisc)

##Databases
setwd("D:/~")
census_zones<-read.csv("Census zones (centroid) with NBI data.csv",sep = ',',header = TRUE)
CO_2022<-read.csv("CO_2022.csv",sep = ',',header = TRUE)
CO_2023<-read.csv("CO_2023.csv",sep = ',',header = TRUE)

##Create base
con<-dbConnect(RSQLite::SQLite(),":memory:")
dbWriteTable(con,"zones",census_zones)
dbWriteTable(con,"CO_2022",CO_2022)
dbWriteTable(con,"CO_2023",CO_2023)

##Table join
CO_2022<-dbGetQuery(con, "select zones.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, opportunity, percentile, cutoff, accessibility from zones inner join CO_2022 on zones.id=CO_2022.id")
CO_2023<-dbGetQuery(con, "select zones.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, opportunity, percentile, cutoff, accessibility from zones inner join CO_2023 on zones.id=CO_2023.id")

##Categorization in percentile 20 (quintiles) per % households with NBI
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CO_2022$porc_hogaresSinNBI, CO_2022$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CO_2022$quintilesNBI <- sapply(CO_2022$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilesNBI, data = CO_2022, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CO_2023$porc_hogaresSinNBI, CO_2023$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CO_2023$quintilesNBI <- sapply(CO_2023$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilesNBI, data = CO_2023, sum)

CO_2022[119, 12] = "Q1"
CO_2023[119, 12] = "Q1"

##Main stats
stat_CO_2022 <- CO_2022 %>% 
  group_by(quintilesNBI) %>% 
  summarise(mean = mean(accessibility), median(accessibility), sd(accessibility),min(accessibility),max(accessibility)) %>% 
  mutate(anio=2022) %>% 
  mutate(caminata=833) %>% 
  mutate(opportunity="Hospitals") %>% 
  mutate(cutoff=61)
stat_CO_2023 <- CO_2023 %>% 
  group_by(quintilesNBI) %>% 
  summarise(mean = mean(accessibility), median(accessibility), sd(accessibility),min(accessibility),max(accessibility)) %>% 
  mutate(anio=2023) %>% 
  mutate(caminata=833) %>% 
  mutate(opportunity="Hospitals") %>% 
  mutate(cutoff=61) %>% 
  rows_append(stat_CO_2022)

write.xlsx(stat_CO_2023,"D:/~/stat_CO_2022_2023.xlsx")

summarise(CO_2022,mean = mean(accessibility), median(accessibility), sd(accessibility),min(accessibility),max(accessibility))
summarise(CO_2023,mean = mean(accessibility), median(accessibility), sd(accessibility),min(accessibility),max(accessibility))

#To finish
rm(list=ls())
