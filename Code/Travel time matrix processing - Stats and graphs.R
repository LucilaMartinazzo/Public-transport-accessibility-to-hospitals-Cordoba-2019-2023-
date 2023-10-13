#Travel time matrix processing - Stats and graphs

rm(list=ls())

##install.packages
install.packages("devtools")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("leaflet")
install.packages("patchwork")
install.packages("xlsx")

##library
library(leaflet)
library(ggplot2)
library(dplyr)
library(patchwork)
library(xlsx)

##Databases
setwd("D:/~")
CAT1_2022<-read.csv("CAT1_2022.csv",sep = ',',header = TRUE)
CAT1_2023<-read.csv("CAT1_2023.csv",sep = ',',header = TRUE)
CAT2_2022<-read.csv("CAT2_2022.csv",sep = ',',header = TRUE)
CAT2_2023<-read.csv("CAT2_2023.csv",sep = ',',header = TRUE)
CAT3_2022<-read.csv("CAT3_2022.csv",sep = ',',header = TRUE)
CAT3_2023<-read.csv("CAT3_2023.csv",sep = ',',header = TRUE)

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
windowsFonts(Cambria=windowsFont("serif"))
# scales::show_col(c('#14828C','#efc847','#d8854a','#cc644c','#b52150', '#a90052'))

##Density plots
d_CAT1_2022 <- 
  ggplot(data=CAT1_2022, aes(x=avg_travel_time))+
  geom_density(mapping=aes(group=quintilesNBI,fill=quintilesNBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Average travel time [minutes]",y="",title="CATI - 2022")+
  facet_wrap(~quintilesNBI,ncol = 1)
d_CAT2_2022 <- 
  ggplot(data=CAT2_2022, aes(x=avg_travel_time))+
  geom_density(mapping=aes(group=quintilesNBI,fill=quintilesNBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Average travel time [minutes]",y="",title="CATII - 2022")+
  facet_wrap(~quintilesNBI,ncol = 1)
d_CAT3_2022 <- 
  ggplot(data=CAT3_2022, aes(x=avg_travel_time))+
  geom_density(mapping=aes(group=quintilesNBI,fill=quintilesNBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Average travel time [minutes]",y="",title="CATIII - 2022")+
  facet_wrap(~quintilesNBI,ncol = 1)

d_CAT1_2022 + theme(text = element_text(family = "serif")) + scale_x_continuous(name="Average travel time [minutes]", breaks=c(20,40,60,80,100,120), limits=c(20,120)) + scale_y_continuous(name="", breaks=c(0.00,0.01,0.02,0.03,0.04,0.05), limits=c(0.00,0.05)) +
  d_CAT2_2022 + theme(text = element_text(family = "serif")) + scale_x_continuous(name="Average travel time [minutes]", breaks=c(20,40,60,80,100,120), limits=c(20,120)) + scale_y_continuous(name="", breaks=c(0.00,0.01,0.02,0.03,0.04,0.05), limits=c(0.00,0.05)) +
  d_CAT3_2022 + theme(text = element_text(family = "serif")) + scale_x_continuous(name="Average travel time [minutes]", breaks=c(20,40,60,80,100,120), limits=c(20,120)) + scale_y_continuous(name="", breaks=c(0.00,0.01,0.02,0.03,0.04,0.05), limits=c(0.00,0.05)) # 1050x850

d_CAT1_2023 <- 
  ggplot(data=CAT1_2023, aes(x=avg_travel_time))+
  geom_density(mapping=aes(group=quintilesNBI,fill=quintilesNBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Average travel time [minutes]",y="",title="CATI - 2023")+
  facet_wrap(~quintilesNBI,ncol = 1)
d_CAT2_2023 <- 
  ggplot(data=CAT2_2023, aes(x=avg_travel_time))+
  geom_density(mapping=aes(group=quintilesNBI,fill=quintilesNBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Average travel time [minutes]",y="",title="CATII - 2023")+
  facet_wrap(~quintilesNBI,ncol = 1)
d_CAT3_2023 <- 
  ggplot(data=CAT3_2023, aes(x=avg_travel_time))+
  geom_density(mapping=aes(group=quintilesNBI,fill=quintilesNBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Average travel time [minutes]",y="",title="CATIII - 2023")+
  facet_wrap(~quintilesNBI,ncol = 1)

d_CAT1_2023 + theme(text = element_text(family = "serif")) + scale_x_continuous(name="Average travel time [minutes]", breaks=c(20,40,60,80,100,120), limits=c(20,120)) + scale_y_continuous(name="", breaks=c(0.00,0.01,0.02,0.03,0.04,0.05), limits=c(0.00,0.05)) +
  d_CAT2_2023 + theme(text = element_text(family = "serif")) + scale_x_continuous(name="Average travel time [minutes]", breaks=c(20,40,60,80,100,120), limits=c(20,120)) + scale_y_continuous(name="", breaks=c(0.00,0.01,0.02,0.03,0.04,0.05), limits=c(0.00,0.05)) +
  d_CAT3_2023 + theme(text = element_text(family = "serif")) + scale_x_continuous(name="Average travel time [minutes]", breaks=c(20,40,60,80,100,120), limits=c(20,120)) + scale_y_continuous(name="", breaks=c(0.00,0.01,0.02,0.03,0.04,0.05), limits=c(0.00,0.05)) # 1050x850

##Graph config
devtools::install_github('Mikata-Project/ggthemr',force=TRUE)
library(ggthemr)
palette_bbtext <- define_palette(background = '#e8ece5', 
                                 text = c('black', 'black'), 
                                 line = c('#bbb8ab', '#bbb8ab'),
                                 gridline = '#bbb8ab',
                                 swatch = (c('black','#cc644c','#b52150', '#a90052')),
                                 gradient = c(low='#efc847', high='#a90052'))

ggthemr(palette_bbtext,layout = "scientific",text_size   = 11)
windowsFonts(Cambria=windowsFont("serif"))

##Boxplot
b_CAT1_2022 <-
  ggplot(data=CAT1_2022, aes(y=avg_travel_time, x=quintilesNBI))+
  geom_boxplot(mapping=aes(group=quintilesNBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Household quintiles per %NBI",y="Average travel time [minutes]",title="CATI - 2022")
b_CAT2_2022 <-
  ggplot(data=CAT2_2022, aes(y=avg_travel_time, x=quintilesNBI))+
  geom_boxplot(mapping=aes(group=quintilesNBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Household quintiles per %NBI",y="Average travel time [minutes]",title="CATII - 2022")
b_CAT3_2022 <-
  ggplot(data=CAT3_2022, aes(y=avg_travel_time, x=quintilesNBI))+
  geom_boxplot(mapping=aes(group=quintilesNBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Household quintiles per %NBI",y="Average travel time [minutes]",title="CATIII - 2022")

b_CAT1_2022 + theme(text = element_text(family = "serif")) + scale_y_continuous(name="Average travel time [minutes]", breaks=c(20,40,60,80,100,120), limits=c(20,120))+
  b_CAT2_2022 + theme(text = element_text(family = "serif")) + scale_y_continuous(name="Average travel time [minutes]", breaks=c(20,40,60,80,100,120), limits=c(20,120))+
  b_CAT3_2022 + theme(text = element_text(family = "serif")) + scale_y_continuous(name="Average travel time [minutes]", breaks=c(20,40,60,80,100,120), limits=c(20,120)) # 1200x500

b_CAT1_2023 <-
  ggplot(data=CAT1_2023, aes(y=avg_travel_time, x=quintilesNBI))+
  geom_boxplot(mapping=aes(group=quintilesNBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Household quintiles per %NBI",y="Average travel time [minutes]",title="CATI - 2023")
b_CAT2_2023 <-
  ggplot(data=CAT2_2023, aes(y=avg_travel_time, x=quintilesNBI))+
  geom_boxplot(mapping=aes(group=quintilesNBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Household quintiles per %NBI",y="Average travel time [minutes]",title="CATII - 2023")
b_CAT3_2023 <-
  ggplot(data=CAT3_2023, aes(y=avg_travel_time, x=quintilesNBI))+
  geom_boxplot(mapping=aes(group=quintilesNBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Household quintiles per %NBI",y="Average travel time [minutes]",title="CATIII - 2023")

b_CAT1_2023 + theme(text = element_text(family = "serif")) + scale_y_continuous(name="Average travel time [minutes]", breaks=c(20,40,60,80,100,120), limits=c(20,120))+
  b_CAT2_2023 + theme(text = element_text(family = "serif")) + scale_y_continuous(name="Average travel time [minutes]", breaks=c(20,40,60,80,100,120), limits=c(20,120))+
  b_CAT3_2023 + theme(text = element_text(family = "serif")) + scale_y_continuous(name="Average travel time [minutes]", breaks=c(20,40,60,80,100,120), limits=c(20,120)) # 1200x500

##Main stats
stat_CAT1_2022 <- CAT1_2022 %>% 
  group_by(quintilesNBI) %>% 
  summarise(mean = mean(avg_travel_time), median(avg_travel_time), sd(avg_travel_time),min(avg_travel_time),max(avg_travel_time)) %>% 
  mutate(CAT=1) %>% 
  mutate(anio=2022) %>% 
  mutate(caminata=833)
stat_CAT2_2022 <- CAT2_2022 %>% 
  group_by(quintilesNBI) %>% 
  summarise(mean = mean(avg_travel_time), median(avg_travel_time), sd(avg_travel_time),min(avg_travel_time),max(avg_travel_time)) %>% 
  mutate(CAT=2) %>% 
  mutate(anio=2022) %>% 
  mutate(caminata=833) %>% 
  rows_append(stat_CAT1_2022)
stat_2022 <- CAT3_2022 %>% 
  group_by(quintilesNBI) %>% 
  summarise(mean = mean(avg_travel_time), median(avg_travel_time), sd(avg_travel_time),min(avg_travel_time),max(avg_travel_time)) %>% 
  mutate(CAT=3) %>% 
  mutate(anio=2022) %>% 
  mutate(caminata=833) %>% 
  rows_append(stat_CAT2_2022)
write.xlsx(stat_2022,"D:/~/stat_2022.xlsx")

stat_CAT1_2023 <- CAT1_2023 %>% 
  group_by(quintilesNBI) %>% 
  summarise(mean = mean(avg_travel_time), median(avg_travel_time), sd(avg_travel_time),min(avg_travel_time),max(avg_travel_time)) %>% 
  mutate(CAT=1) %>% 
  mutate(anio=2023) %>% 
  mutate(caminata=833)
stat_CAT2_2023 <- CAT2_2023 %>% 
  group_by(quintilesNBI) %>% 
  summarise(mean = mean(avg_travel_time), median(avg_travel_time), sd(avg_travel_time),min(avg_travel_time),max(avg_travel_time)) %>% 
  mutate(CAT=2) %>% 
  mutate(anio=2023) %>% 
  mutate(caminata=833) %>% 
  rows_append(stat_CAT1_2023)
stat_2023 <- CAT3_2023 %>% 
  group_by(quintilesNBI) %>% 
  summarise(mean = mean(avg_travel_time), median(avg_travel_time), sd(avg_travel_time),min(avg_travel_time),max(avg_travel_time)) %>% 
  mutate(CAT=3) %>% 
  mutate(anio=2023) %>% 
  mutate(caminata=833) %>% 
  rows_append(stat_CAT2_2023)
write.xlsx(stat_2023,"D:/~/stat_2023.xlsx")

stat_2019 <- read.xlsx("D:/~/t_833_2019.xlsx", sheetName = "Sheet1")
stat_2019 <- mutate(stat_2019,NA. = NULL)
stat_2019 <- rename(stat_2019,quintilesNBI = quintil_NBI, "median(avg_travel_time)" = median.avg_tiempo_viaje., "sd(avg_travel_time)" = sd.avg_tiempo_viaje., "min(avg_travel_time)" = min.avg_tiempo_viaje., "max(avg_travel_time)" = max.avg_tiempo_viaje.)
stat_2021 <- read.xlsx("D:/~/t_833_2021.xlsx", sheetName = "Sheet1")
stat_2021 <- mutate(stat_2021,NA. = NULL)
stat_2021 <- rename(stat_2021,quintilesNBI = quintil_NBI, "median(avg_travel_time)" = median.avg_tiempo_viaje., "sd(avg_travel_time)" = sd.avg_tiempo_viaje., "min(avg_travel_time)" = min.avg_tiempo_viaje., "max(avg_travel_time)" = max.avg_tiempo_viaje.)

stats <- rbind(stat_2019,stat_2021,stat_2022,stat_2023)
write.xlsx(stats,"D:/~/stats.xlsx")

stat_CAT1_2022 <-
  CAT1_2022 %>% 
  summarise(mean = mean(avg_travel_time), median(avg_travel_time), sd(avg_travel_time),min(avg_travel_time),max(avg_travel_time)) %>% 
  mutate(CAT=1) %>% 
  mutate(anio=2022) %>% 
  mutate(caminata=833)
stat_CAT2_2022 <-
  CAT2_2022 %>% 
  summarise(mean = mean(avg_travel_time), median(avg_travel_time), sd(avg_travel_time),min(avg_travel_time),max(avg_travel_time)) %>% 
  mutate(CAT=2) %>% 
  mutate(anio=2022) %>% 
  mutate(caminata=833)
stat_CAT3_2022 <-
  CAT3_2022 %>% 
  summarise(mean = mean(avg_travel_time), median(avg_travel_time), sd(avg_travel_time),min(avg_travel_time),max(avg_travel_time)) %>% 
  mutate(CAT=3) %>% 
  mutate(anio=2022) %>% 
  mutate(caminata=833)
stat_CAT1_2023 <-
  CAT1_2023 %>% 
  summarise(mean = mean(avg_travel_time), median(avg_travel_time), sd(avg_travel_time),min(avg_travel_time),max(avg_travel_time)) %>% 
  mutate(CAT=1) %>% 
  mutate(anio=2023) %>% 
  mutate(caminata=833)
stat_CAT2_2023 <-
  CAT2_2023 %>% 
  summarise(mean = mean(avg_travel_time), median(avg_travel_time), sd(avg_travel_time),min(avg_travel_time),max(avg_travel_time)) %>% 
  mutate(CAT=2) %>% 
  mutate(anio=2023) %>% 
  mutate(caminata=833)
stat_CAT3_2023 <-
  CAT3_2023 %>% 
  summarise(mean = mean(avg_travel_time), median(avg_travel_time), sd(avg_travel_time),min(avg_travel_time),max(avg_travel_time)) %>% 
  mutate(CAT=3) %>% 
  mutate(anio=2023) %>% 
  mutate(caminata=833)

stats_total <- rbind(stat_CAT1_2022,stat_CAT2_2022,stat_CAT3_2022,stat_CAT1_2023,stat_CAT2_2023,stat_CAT3_2023)
write.xlsx(stats_total,"D:/~/stats_total.xlsx")

##Graph config
devtools::install_github('Mikata-Project/ggthemr',force=TRUE)
library(ggthemr)
palette_btext <- define_palette(background = '#e8ece5', 
                                text = c('black', 'black'), 
                                line = c('#bbb8ab', '#bbb8ab'),
                                gridline = '#bbb8ab',
                                swatch = (c('#14828C','#efc847','#d8854a','#cc644c','#b52150', '#a90052')),
                                gradient = c(low='#efc847', high='#a90052'))

ggthemr(palette_btext,layout = "scientific",text_size   = 14)
windowsFonts(Cambria=windowsFont("serif"))

##Scatterplot
hospital_cat <- c(
  `1` = "CATI",
  `2` = "CATII",
  `3` = "CATIII"
)

s_stats <-
  ggplot(data=stats, aes(y=mean, x=anio, color=quintilesNBI))+
  geom_point(size=2.5)+
  facet_wrap(stats$CAT,labeller = as_labeller(hospital_cat))+
  labs(x="Year",y="Average travel time [minutes] per %NBI quintiles", color="%NBI quintiles")+
  theme(legend.position = "bottom")
s_stats + theme(text = element_text(family = "serif"))

#To finish
rm(list=ls())
