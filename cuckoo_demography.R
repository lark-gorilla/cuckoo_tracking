## 29/08/2018

## script to buffer capture locations per population and then
## extract cuckoo population chnage data from Bird Atlas 

library(dplyr)
library(sf)
library(sp)
library(tmap)
library(rgdal)

## get best of day locations

bod<-read.csv('C:/cuckoo_tracking/data/movebank_cuckoos_hybridfilter_bestofday_2018_clean.csv', h=T)

bod<-bod %>% group_by(ptt) %>%summarise_all(first)

#split scotland into 2 sites

bod[bod$capture.location=='Scotland',]

bod$capture.location<-as.character(bod$capture.location)

bod[bod$location.lat>57,]$capture.location<-'Kinloch_Skye'

bodsp<-SpatialPoints(cbind(bod$location.long,
                    bod$location.lat), 
              CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

bodspdf<-SpatialPointsDataFrame(bodsp, data=bod)

tmap_mode("view")
qtm(bodspdf)

# to british national grid proj
bodproj<-spTransform(bodspdf, 
                     CRS('+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs'))
bodsf<-st_as_sf(bodproj)

# read in Bird Atlas data

batl<-read.csv('C:/cuckoo_tracking/sourced_data/cuckoo_demography/cuckoo Atlas abundance change.csv', h=T)

batlsp<-SpatialPoints(cbind(batl$easting,
                            batl$northing), 
                     CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs"))

batlspdf<-SpatialPointsDataFrame(batlsp, data=batl)

batlsf<-st_as_sf(batlspdf)

#https://stackoverflow.com/questions/49200458/find-nearest-features-using-sf-in-r
closest <- list()
for(i in seq_len(nrow(bodsf))){
  closest[[i]] <- batlsf[which.min(
    st_distance(batlsf, bodsf[i,])),]
}

# bind up the list

centredat<- do.call('rbind', closest)

write.csv(centredat, 'C:/cuckoo_tracking/sourced_data/cuckoo_demography/Altas_centroids_nearest_tag_loc.csv', quote=F, row.names=F)

## Read in shapefile of edited Bird Atlas sites

setwd('C:/cuckoo_tracking/sourced_data')

bat_site<-readOGR(layer='Atlas_tagging_areas_added', dsn='cuckoo_demography')

qplot(data=bat_site@data, x=bat_site@coords[,1], y=bat_site@coords[,2], colour=tagloc)

site_dem<-bat_site@data

site_dem<-site_dem[site_dem$tagloc!='na',]

site_dem %>% group_by(tagloc) %>% 
  summarise(mean_diff=mean(diff), sd_diff=sd(diff))

