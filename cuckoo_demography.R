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

## extra section to filter to just birds used in spring analyses
dmod<-read.csv('C:/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead_attrib_modelready.csv', h=T)

# just uk arrivals
dmod<-dmod[-which(is.na(dmod$arrive_uk)),]

bod<-bod %>% filter(ptt %in% unique(dmod$ptt))

bod %>% mutate(lat1=ifelse(is.na(lat.tagged), location.lat, lat.tagged),
               long1=ifelse(is.na(long.tagged), location.long, long.tagged)) -> bod

write.csv(bod, 'C:/cuckoo_tracking/sourced_data/cuckoo_demography/first_point_ptt_bestofday_springbirds.csv', quote=F, row.names=F)

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

write.csv(centredat, 'C:/cuckoo_tracking/sourced_data/cuckoo_demography/Altas_centroids_nearest_tag_loc_spring_birds.csv', quote=F, row.names=F)

## Read in shapefile of edited Bird Atlas sites

setwd('C:/cuckoo_tracking/sourced_data')

bat_site<-readOGR(layer='Atlas_tagging_areas_added_spring_birds', dsn='cuckoo_demography')

qplot(data=bat_site@data, x=bat_site@coords[,1], y=bat_site@coords[,2], colour=tagloc)

site_dem<-bat_site@data

site_dem<-site_dem[site_dem$tagloc!='na',]

site_dem %>% group_by(tagloc) %>% 
  summarise(mean_diff=mean(diff), sd_diff=sd(diff))


# check version against Chris' sites


batl<-read.csv('C:/cuckoo_tracking/sourced_data/cuckoo_demography/cuckoo Atlas abundance change.csv', h=T)

c_cells<-read.csv('C:/cuckoo_tracking/sourced_data/cuckoo_demography/10km squares for Sam.csv', h=T)

b2<-batl[batl$tenkm%in%c_cells$TQ42,]

write.csv(b2, 'C:/cuckoo_tracking/sourced_data/cuckoo_demography/Atlas_centroids_hewson_nature.csv', quote=F, row.names=F)


# quick bit of analyses

## extra section to filter to just birds used in spring analyses
dmod<-read.csv('C:/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead_attrib_modelready.csv', h=T)

dmod$breeding_loc<-as.character(dmod$breeding_loc)
dmod[dmod$ptt==128300,]$breeding_loc<-'Kinloch Skye'
dmod[dmod$breeding_loc=='Scotland',]$breeding_loc<-'Trossachs'

pop_trend<-site_dem %>% group_by(tagloc) %>% 
  summarise(mean_diff=mean(diff), sd_diff=sd(diff))

pop_trend$tagloc<-recode(pop_trend$tagloc,
                         'Ashdown For'='Ashdown Forest',
                         'Lak District'='Lake District',
                         'N York Moors' = 'North York Moors',
                         'Norfolk Br'= 'Norfolk Broads',
                         'Sherwood For'='Sherwood Forest',
                         'Thetford For'='Thetford Forest')

dmod<-left_join(dmod, pop_trend, by=c('breeding_loc'='tagloc'))
# warning ok

qplot(data=dmod, y=arrive_breeding, x=mean_diff, colour=breeding_loc)

d2<-dmod %>% group_by(breeding_loc) %>% summarise_all(mean, na.rm=T)


ggplot(data=d2, aes(y=arrive_uk, x=mean_diff))+
  geom_point(aes(colour=breeding_loc), shape=2, size=2)+
  geom_point(data=dmod ,aes(colour=breeding_loc), alpha=0.2)

