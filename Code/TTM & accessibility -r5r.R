#Travel time matrix and accessibility to public hospitals with r5r - Cordoba, Argentina

rm(list=ls())

##install.packages
install.packages('r5r')
install.packages('mapview')
install.packages('akima')

##Set JAVA memory (requires Java SE Development Kit version >= 11.0.8)
options(java.parameters = "-Xmx2G")

##Library
library(r5r)
library(sf)
library(data.table)
library(ggplot2)
library(xlsx)
library(akima)
library(dplyr)
library(mapview)
mapviewOptions(platform = 'leafgl')

##Databases
###GTFS, OSM base map, hospitals as points of interest and census zones centroid as origins
data_path <- file.path("D:/~")
list.files(data_path) # To verify
poi <- fread(file.path(data_path, "Public hospitals - IDERA-SISA.csv"))
points <- fread(file.path(data_path, "Census zones (centroid) with NBI data.csv"))

##Network from OMS base map and GTFS provided
r5r_core <- setup_r5(data_path = data_path, verbose = FALSE)

##Travel time matrix 2022
###Inputs
mode <- c("WALK", "TRANSIT")
max_walk_time <- 10   # minutes
max_trip_duration <- 120 # minutes
percentiles <- 85
time_window <- 120 # minutes
departure_datetime <- as.POSIXct("13-04-2022 09:00:00",
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
write.xlsx(ttm,"D:/~/TTM_2022.xlsx")

##Accessibility 2022
###Inputs
# The time frame was selected considering that in Córdoba over 95% of the trips last less than an hour (De Beláustegui, 2014).
mode <- c("WALK", "TRANSIT")
max_walk_time <- 10   # minutes
max_trip_duration <- 120 # minutes
percentiles <- 85
time_window <- 120 # minutes
travel_time_cutoff <- 61
departure_datetime <- as.POSIXct("13-04-2022 09:00:00",
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
write.xlsx(access,"D:/~/CO_2022.xlsx")


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
write.xlsx(ttm,"D:/~/TTM_2023.xlsx")

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
write.xlsx(access,"D:/~/CO_2023.xlsx")

##Clear
stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)
