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
precip[which(precip$precip>500),]$precip<-0 # remove erroneous vals
precip[which(is.na(precip$precip)),]$precip<-0 # fix na

precip<-precip %>% group_by(year, ptt, SO_startDOY) %>% dplyr::mutate(cumrf=cumsum(precip))

# Not doing now as drops needed info
# drops duplicated daily values between 8 day modis images
#ndvi<-dat2[dat2$variable %in% seq(1, 125, 8),]

ndvi<-dat2
ndvi$precip<-NULL

# add month and region
ndvi$month=1
ndvi[ndvi$variable %in% (32:59),]$month=2
ndvi[ndvi$variable %in% (60:90),]$month=3
ndvi[ndvi$variable %in% (91:125),]$month=4

ndvi$region<-'WestAF'

ndvi[ndvi$country %in% c("Congo","Cameroon",
                     "Central African Republic",
                     "Democratic Republic of the Congo",
                     "Angola", "Gabon"),]$region<-"CentralAF"


# ndvi plots

#across all sites
p<-ggplot(data=ndvi, aes(x=NDVI, fill=cuck_pres))

p+geom_histogram(position='dodge')+facet_wrap(~month)

p+geom_histogram(position='dodge')+facet_grid(region~month, scales='free_y')

# order countries by longitude
levels(ndvi$country)
ndvi$country = factor(ndvi$country,
                      levels(ndvi$country)[c(1,2,11,13,10,6,9,14,12,3,4,8,5,7)])

p<-ggplot(data=ndvi[-which(ndvi$country %in% c('Angola', 'Burkina Faso', 'Liberia')),],
          aes(x=aquaNDVI, fill=cuck_pres))

jpeg("outputs/aquaNDVI_month_country.jpg",
     width =   11.69, height =8.27 , units ="in", res =300)

p+geom_histogram(position='dodge', binwidth=0.05)+
  scale_fill_manual(values=c("N" = "grey", "Y"="black"))+
   facet_grid(month~country, scales='free_y')+
  theme_bw()+theme(legend.position='none')

dev.off()

jpeg("outputs/aquaNDVI_year_country.jpg",
     width =   11.69, height =8.27 , units ="in", res =300)

p+geom_histogram(position='dodge', binwidth=0.05)+
  scale_fill_manual(values=c("N" = "grey", "Y"="black"))+
  facet_grid(year~country, scales='free_y')+
  theme_bw()+theme(legend.position='none')

dev.off()

# rainfall plots

precip$month=1
precip[precip$variable %in% (32:59),]$month=2
precip[precip$variable %in% (60:90),]$month=3
precip[precip$variable %in% (91:125),]$month=4

# ndvi plots

#across all sites

# order countries by longitude
levels(precip$country)

precip$country = factor(precip$country,
                      levels(precip$country)[c(13,11,10,6,9,14,2,12,3,4,8,5,7,1)])

p<-ggplot(data=precip[precip$cumrf<1000,], aes(y=..density..,x=cumrf, fill=cuck_pres))

p+geom_histogram(position='dodge', binwidth=20)+
  scale_fill_manual(values=c("N" = "grey", "Y"="black"))+
  facet_grid(country~., scales='free_y')+
  theme_bw()+theme(legend.position='none')

p<-ggplot(data=precip[precip$cumrf>0,], aes(x=cumrf, fill=cuck_pres))

 p+geom_histogram(position='dodge', binwidth=10)+
   scale_fill_manual(values=c("N" = "grey", "Y"="black"))+
  theme_bw()+theme(legend.position='none')