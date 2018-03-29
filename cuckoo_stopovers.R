########################
# 31/01/18
# Defining cuckoo stopover sites
# using convex hull method
########################

rm(list=ls())

library(ggplot2)
library(ggmap)
library(rgeos)
library(maps)
library(maptools)


# Read in processed cuckoo data with stopover regions defined
# using Sam & Jenni's code

dat<-read.csv("~/BTO/cuckoo_tracking/data/processed_movebank_cuckoos_hybrid_filter_bestofday_clean_stopovers.csv", h=T)

# FORMAT!!
dat$timestamp <- as.POSIXct(strptime(dat$timestamp, "%Y-%m-%d %H:%M:%S"), "UTC")


# OK so stopover information provided in this dataset is:
# tcycle, which gives an identifier for the the 10 hr on transmission cycle, per bird
# days.btwn.trans, which gives the time (in days) between transmissions (rows)
# distance, which gives the distance (km) between the fix (row) and the last
# bearing, which gives the bearing (deg) between the fix (row) and the last
# mtype, which is movement type. if more than 50km from previous fix, then
# type is M (migration) otherwise it is S (stationary).
# mgroup and m_group_replicate, which is the movement group. Fixes are grouped
# if they do not have a >50km distance between points
# LOS, which is the length of stopover (days) assigned to each mgroup.

m1 <- get_map(location = c(lon = mean(dat$long),
                           lat = mean(dat$lat)), zoom=3)
g <- ggmap(m1) +geom_point(data =dat, aes(x=long, y=lat, colour=mtype))+
  facet_wrap(~year.x)
g

# have a look at West African region

m1 <- get_map(location = "Ghana", zoom=5)
g <- ggmap(m1) +geom_point(data =dat, aes(x=long, y=lat, colour=mtype))+
  facet_wrap(~year.x)
g

# Ok so lets build a loop that makes a convex hull around each group of stopover points
# attributes will be: ptt, start timestamp, end timestamp, ndays, month, year

stopovers_spdf<-NULL

birds<-unique(dat$ptt)

for(i in birds)
  {
  
  # for each bird give the mgroups which are not migratory ones
  bird_spdf<-NULL
  
  mgz<-na.omit(unique(dat[dat$ptt==i & dat$mtype!="M",]$mgroup))
  
  for (j in mgz)
      {
      ptt_mgroup<-dat[dat$ptt==i & dat$mgroup==j,]
    
      ptt_mgroup.sp<-SpatialPoints(cbind(ptt_mgroup$long,ptt_mgroup$lat),
                            proj4string= CRS("+proj=longlat +datum=WGS84"))
      
      ########## Drops stopovers with only two locations ########
      if(nrow(unique(ptt_mgroup.sp@coords))<3){next}
      ###########################################################
      
      ptt_mgroup.ch<-gConvexHull(ptt_mgroup.sp)
      
      ptt_mgroup.spdf<-SpatialPolygonsDataFrame(ptt_mgroup.ch, data=
                       data.frame(ptt=i, mgroup=j, 
                                  SO_start=min(ptt_mgroup$timestamp),
                                  SO_end=max(ptt_mgroup$timestamp),
                                  SO_days=unique(ptt_mgroup$LOS)))
      
      ptt_mgroup.spdf@polygons[[1]]@ID<-paste(i, j, sep=".")
      row.names(ptt_mgroup.spdf@data)<-paste(i, j, sep=".")
      
      if(is.null(bird_spdf)){bird_spdf<-ptt_mgroup.spdf}else{
        bird_spdf<-spRbind(bird_spdf, ptt_mgroup.spdf)}
      }
    
  
  if(which(i==birds)==1){stopovers_spdf<-bird_spdf}else{
    stopovers_spdf<-spRbind(stopovers_spdf, bird_spdf)}
  print(i)
  
  }  
  
  
plot(stopovers_spdf)
map("world", add=T, col="grey")

plot(stopovers_spdf[stopovers_spdf$stopover_duration>3,], add=T, col=2)

library(rgdal)

# need to do setwd() to level below dsn for linux??
setwd("~/BTO/cuckoo_tracking/data")
writeOGR(obj=stopovers_spdf, dsn="spatial",
         layer="movebank_cuckoos_bestofday_stopovers", driver="ESRI Shapefile",
         overwrite_layer = T)
  

# To create master spredsheet with stopovers for Chris

dat<-read.csv("data/processed_movebank_cuckoos_hybrid_filter_bestofday_clean_stopovers_recalc.csv", h=T)

# FORMAT!!
dat$timestamp <- as.POSIXct(strptime(dat$timestamp, "%Y-%m-%d %H:%M:%S"), "UTC")

stopovers_tab<-NULL

birds<-unique(dat$ptt)

for(i in birds)
{
  
  # for each bird give the mgroups which are not migratory ones
  bird_out<-NULL
  
  mgz<-na.omit(unique(dat[dat$ptt==i & dat$mtype=="S",]$mgroup))
  # removes 'M' migratory and 'C' deployment point
  
  for (j in mgz)
  {
    ptt_mgroup<-dat[dat$ptt==i & dat$mgroup==j,]
    
    ########## Drops stopovers with only two locations ########
    #if(nrow(ptt_mgroup)<3){next}
    ###########################################################

    ########## Allows a stopover to be defined based on time #
    # Here we use 1 hour - we're doing this on non duty-cycle data 
    # as it gives more accurate timings 
    
    if(unique(ptt_mgroup$LOS)<1){next}
    
    ###########################################################
    
    ptt_mgroup_out<-data.frame(ptt=i, name=unique(ptt_mgroup$name), mgroup=j, 
                                SO_start=min(ptt_mgroup$timestamp),
                                SO_end=max(ptt_mgroup$timestamp),
                                SO_days=unique(ptt_mgroup$LOS),
                                SO_median_long=median(ptt_mgroup$long),
                                SO_median_lat=median(ptt_mgroup$lat),
                                SO_month=round(median(ptt_mgroup$month)),
                                SO_year=round(median(ptt_mgroup$year.x)),
                                dead="N")    
    
    if(j==max(mgz)){ptt_mgroup_out$dead<-unique(ptt_mgroup$comments)}
    
   bird_out<-rbind(bird_out, ptt_mgroup_out) 
  }
  
  stopovers_tab<-rbind(stopovers_tab, bird_out)
  print(i)
  
}  

write.csv(stopovers_tab, "data/stopover_table_bestofday_1daymin_recalc.csv", quote=F, row.names=F)

# Add country and biome data to stopovers

rm(list=ls())
library(maps)
library(sp)
library(maptools)
library(rgdal)
library(ggplot2)
library(ggmap)

# using best of day data
dat<-read.csv("data/stopover_table_biomes_1daymin_recalc.csv", h=T)

setwd("sourced_data/")
countries<-readOGR(layer="TM_WORLD_BORDERS-0.3",
               dsn="country_borders") # different linux/windows

biomes<-readOGR(layer="tnc_terr_ecoregions",
                   dsn="biomes_TNC") #


datSP<-SpatialPointsDataFrame(SpatialPoints(cbind(dat$SO_median_long, dat$SO_median_lat), 
                                            CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")),
                              data=dat)


library(sf)

# convert to sf object
datSP<-st_as_sf(datSP)

class(datSP)

# nice
plot(datSP)

library(tmap)

tmap_mode('view')

qtm(datSP) #WHHHHHHAATTTTTTTT!!!!


dat$country<-"NA"
dat$biome1<-"NA"
dat$biome2<-"NA"

for(i in 1:nrow(dat))
{
  
  
  if(identical(paste(countries[datSP[i,],]$NAME), character(0))){
    dat[i,]$country<-"NA"}else{
  dat[i,]$country<-paste(countries[datSP[i,],]$NAME)}
  if(identical(paste(biomes[datSP[i,],]$WWF_MHTNAM), character(0))){
    dat[i,]$biome1<-"NA"}else{
  dat[i,]$biome1<-paste(biomes[datSP[i,],]$WWF_MHTNAM)}
  if(identical(paste(biomes[datSP[i,],]$ECO_NAME), character(0))){
    dat[i,]$biome2<-"NA"}else{
  dat[i,]$biome2<-paste(biomes[datSP[i,],]$ECO_NAME)}
  print(i)
}

#remove commas for write.csv
dat$biome1<-gsub(",", "", dat$biome1)

dat$biome2<-gsub(",", "", dat$biome2)

write.csv(dat, "data/stopover_table_bestofday_1daymin_recalc_biomes.csv", quote=F, row.names=F)

## Add columns with migration cohort to 
## sort the issue of crossing years on migration manually in excel


# Exporting as KML files 

setwd("~/BTO/cuckoo_tracking/data")

stopovers<-readOGR(layer="movebank_cuckoos_bestofday_stopovers",
                dsn="spatial") #

stopovers2<-readOGR(layer="movebank_cuckoos_bestofday_stopover_medianpoints",
                   dsn="spatial") #

linez<-readOGR(layer="movebank_cuckoos_lines",
                   dsn="spatial") #

setwd("~/BTO/cuckoo_tracking/data/spatial")

writeOGR(linez, "cuckoo_tracks.kml", "spatial", driver="KML", overwrite_layer=T)

writeOGR(stopovers, "bestofday_stopovers.kml", "spatial", driver="KML", overwrite_layer=T)

writeOGR(stopovers2, "bestofday_stopovers_centroids.kml", "spatial", driver="KML", overwrite_layer=T)

# may be different in windows! Check it puts the file
# in the right place and not somewhere lower/higher in directory