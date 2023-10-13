#Travel time matrix processing

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

##Datasets
setwd("D:/~")
census_zones<-read.csv("Census zones (centroid) with NBI data.csv",sep = ',',header = TRUE)
TTM_2022<-read.csv("TTM_2022.csv",sep = ',',header = TRUE)
TTM_2023<-read.csv("TTM_2023.csv",sep = ',',header = TRUE)

##Create base
con<-dbConnect(RSQLite::SQLite(),":memory:")
dbWriteTable(con,"zones",census_zones)
dbWriteTable(con,"TTM_2022",TTM_2022)
dbWriteTable(con,"TTM_2023",TTM_2023)

##Data per hospital category
CAT1_2022_1<-dbGetQuery(con, "select from_id as id, avg(travel_time_p85) as avg_travel_time from TTM_2022 where to_id in (1,2,3,4) group by from_id")
CAT2_2022_1<-dbGetQuery(con, "select from_id as id, avg(travel_time_p85) as avg_travel_time from TTM_2022 where to_id in (5,6,7,11,14) group by from_id")
CAT3_2022_1<-dbGetQuery(con, "select from_id as id, avg(travel_time_p85) as avg_travel_time from TTM_2022 where to_id in (8,9,10,13,15) group by from_id")
dbWriteTable(con,"CAT1_2022_1",CAT1_2022_1)
dbWriteTable(con,"CAT2_2022_1",CAT2_2022_1)
dbWriteTable(con,"CAT3_2022_1",CAT3_2022_1)
CAT1_2023_1<-dbGetQuery(con, "select from_id as id, avg(travel_time_p85) as avg_travel_time from TTM_2023 where to_id in (1,2,3,4) group by from_id")
CAT2_2023_1<-dbGetQuery(con, "select from_id as id, avg(travel_time_p85) as avg_travel_time from TTM_2023 where to_id in (5,6,7,11,14) group by from_id")
CAT3_2023_1<-dbGetQuery(con, "select from_id as id, avg(travel_time_p85) as avg_travel_time from TTM_2023 where to_id in (8,9,10,13,15) group by from_id")
dbWriteTable(con,"CAT1_2023_1",CAT1_2023_1)
dbWriteTable(con,"CAT2_2023_1",CAT2_2023_1)
dbWriteTable(con,"CAT3_2023_1",CAT3_2023_1)

##Table join
CAT1_2022<-dbGetQuery(con, "select zones.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_travel_time from zones inner join CAT1_2022_1 on zones.id=CAT1_2022_1.id")
CAT1_2023<-dbGetQuery(con, "select zones.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_travel_time from zones inner join CAT1_2023_1 on zones.id=CAT1_2023_1.id")
CAT2_2022<-dbGetQuery(con, "select zones.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_travel_time from zones inner join CAT2_2022_1 on zones.id=CAT2_2022_1.id")
CAT2_2023<-dbGetQuery(con, "select zones.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_travel_time from zones inner join CAT2_2023_1 on zones.id=CAT2_2023_1.id")
CAT3_2022<-dbGetQuery(con, "select zones.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_travel_time from zones inner join CAT3_2022_1 on zones.id=CAT3_2022_1.id")
CAT3_2023<-dbGetQuery(con, "select zones.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_travel_time from zones inner join CAT3_2023_1 on zones.id=CAT3_2023_1.id")

##Categorization in percentile 20 (quintiles) per % households with NBI
###CAT1
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT1_2022$porc_hogaresSinNBI, CAT1_2022$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT1_2022$quintilesNBI <- sapply(CAT1_2022$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilesNBI, data = CAT1_2022, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT1_2023$porc_hogaresSinNBI, CAT1_2023$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT1_2023$quintilesNBI <- sapply(CAT1_2023$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilesNBI, data = CAT1_2023, sum)

###CAT2
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT2_2022$porc_hogaresSinNBI, CAT2_2022$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT2_2022$quintilesNBI <- sapply(CAT2_2022$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilesNBI, data = CAT2_2022, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT2_2023$porc_hogaresSinNBI, CAT2_2023$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT2_2023$quintilesNBI <- sapply(CAT2_2023$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilesNBI, data = CAT2_2023, sum)

###CAT3
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT3_2022$porc_hogaresSinNBI, CAT3_2022$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT3_2022$quintilesNBI <- sapply(CAT3_2022$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilesNBI, data = CAT3_2022, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT3_2023$porc_hogaresSinNBI, CAT3_2023$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT3_2023$quintilesNBI <- sapply(CAT3_2023$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilesNBI, data = CAT3_2023, sum)


write.xlsx(CAT1_2022,"D:/~/CAT1_2022.xlsx")
write.xlsx(CAT1_2023,"D:/~/CAT1_2023.xlsx")

write.xlsx(CAT2_2022,"D:/~/CAT2_2022.xlsx")
write.xlsx(CAT2_2023,"D:/~/CAT2_2023.xlsx")

write.xlsx(CAT3_2022,"D:/~/CAT3_2022.xlsx")
write.xlsx(CAT3_2023,"D:/~/CAT3_2023.xlsx")

##Disconnect base
dbDisconnect(con)
