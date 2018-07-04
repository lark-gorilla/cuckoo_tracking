########################
# 03/07/2018
# Defining the bioclimatic units cuckoos use on spring migration in Africa 
# Using environemntal niche modelling but will need to be dynamic over time
########################

rm(list=ls())

library(ggplot2)

if(Sys.info()['nodename']=="D9L5812"){
  setwd("C:/cuckoo_tracking")}else{
    setwd("N:/cuckoo_tracking")}

# Use data extracted with the 'detailed coords' method

dat<-read.csv('DATA/spring_rainfall_NDVI_GRIMMS_by_stopover_detailcoords_2018_dead.csv', h=T)

# summarise data for each stopover so that value per date is the mean
# of the detail coords values

library(dplyr)

dat2<- dat %>% group_by(year, ptt, SO_startDOY, variable) %>%
  summarise(country=first(country), SO_endDOY=first(SO_endDOY),
            cuck_pres=first(cuck_pres), precip=mean(value, na.rm=T),
            NDVI= mean(value2, na.rm=T), aquaNDVI=mean(aquaNDVI, na.rm=T),
            aquaANOM=mean(aquaANOM, na.rm=T),terrNDVI=mean(terrNDVI, na.rm=T),
            terrANOM=mean(terrANOM, na.rm=T))

# rearrange

dat2<- dat2 %>% select(year, ptt, SO_startDOY, SO_endDOY, country, everything())

precip<-dat2[,1:8]

# add cumulative rainfall
precip[which(is.na(precip$precip)),]$precip # fix na

precip<-precip %>% group_by(year, ptt, SO_startDOY) %>% dplyr::mutate(cumrf=cumsum(precip))

# drops duplicated daily values between 8 day modis images
ndvi<-dat2[dat2$variable %in% seq(1, 125, 8),] 

# rainfall plots

p<-ggplot(data=precip[precip$variable %in% seq(1:31),],
          aes(x=cumrf, colour=cuck_pres))

p+geom_histogram(position='dodge')