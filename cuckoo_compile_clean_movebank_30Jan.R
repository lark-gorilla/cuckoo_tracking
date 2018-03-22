##########################################################
#
#  Cuckoo ORIGINAL data - tidying source file, adding new variables (distance moved between points, bearing, movement groups, and movement types)
#
#  Samantha Franks
#	15 Nov 2013
# 4 Dec 2013
# 23 Dec 2013
# 6 May 2015 - revised to include 2014 cohort and working from original raw data messages
# August 2017 - modified by Jenni Border


# Jan 2018 - script lifted by MM from above paper trail

##########################################################

rm(list=ls())


library(lubridate)
library(plyr)
library(ggplot2)
library(ggmap)
library(maps)


library(rgdal)
library(maptools)
library(shape)
library(splancs)
library(geosphere)
library(adehabitatHR) # check this version works ok for what I need to use it for
###sam used adehabitats but that package is now depreciated and so use one of the new versions (adehabitatHR, adehabitatLT, adehabitatMA)
library(sp) # converts R dataframes into spatially explicit dta
library(rgeos)
library(raster)
library(chron)
library(lubridate)
library(plyr)


####======================== LOAD DATA ======================================

# cuckoo coordinate locations are in Lat-Long and UTM coordinates and WGS84 datum

#dat<-read.csv("N:/cuckoo_tracking/data/movebank_cuckoos_hybridfilter.csv", h=T)
dat<-read.csv("~/BTO/cuckoo_tracking/data/movebank_cuckoos_hybridfilter.csv", h=T)

#dat<-read.csv("N:/cuckoo_tracking/data/movebank_cuckoos_hybridfilter_bestofday.csv", h=T)

head(dat)

#---- remove lines with blank location_dates ---- NONE in movebank
#d1 <- dat[-which(is.na(dat$timestamp)),]

#-------------- convert UNIX timestamp to R date format timestamp --------------
#d1$timestamp <- as.POSIXct(as.numeric(as.character(d1$location_date)),origin="1970-01-01",tz="UTC")

d1<-dat
d1$timestamp <- as.POSIXct(strptime(substr(dat$timestamp,1,19), "%Y-%m-%d %H:%M:%S"), "UTC")

#-------------- Add julian date, year, month, day data to locations ----------------
d1$year <- year(d1$timestamp)
d1$month <- month(d1$timestamp)
d1$day <- mday(d1$timestamp)
d1$julian <- yday(d1$timestamp)

table(d1$year)

# have a look

m1 <- get_map(location = c(lon = mean(d1$location.long, na.rm=T),
                           lat = mean(d1$location.lat, na.rm=T)), zoom=3)
g <- ggmap(m1) +geom_point(data =d1, aes(x=location.long, y=location.lat, colour=algorithm.marked.outlier))+
  facet_wrap(~year)
g

##remove all records from 2017 and 2018
# No think! migration of a single bird splits the year: 'winter' in Africa
#d2a<-subset(d1, year!=2017)
#d2a<-subset(d1, year!=2018)

# load the reference spreadsheet with individual cuckoo specs, includes up to the 2014 cohort
# merge with raw dataset

#strategy.dat <- read.csv("N:/cuckoo_tracking/t_drive/scripts/cuckoo migratory strategy and Sahara crossing success 2014_bird year multiples_NEW1.csv", header=T)
strategy.dat <- read.csv("~/BTO/cuckoo_tracking/t_drive/scripts/cuckoo migratory strategy and Sahara crossing success 2014_bird year multiples_NEW1.csv", header=T)

strategy.dat <- rename(strategy.dat, c("tag"="ptt"))

d1<- rename(d1, c("tag.local.identifier"="ptt"))

unique(strategy.dat$date.tagged)
table(substr(strategy.dat$date.tagged,7,10))
# ok so what we have is cuckoos tagged between 2011 and 2016
# with some birds that were tagged in 2011 transmitting for several years
# e,g. Chris
strategy.dat[strategy.dat$ptt=="62608",]

table(strategy.dat$year)
table(strategy.dat$ptt)


#--------- Dataset specs ------------

# location_date = UNIX timestamp
# location_class refers to the Argos location class: from best to worst, 3,2,1,0,A,B,Z. Alpha location classes have no associated errors, so remove. Location class=0 have gigantic major axis errors, so also remove these. Use only location classes 1:3
# semi_major_axis = error radius along the major axis of the ellipse
# semi_minor_axis = error radius along the minor axis of the ellipse
# ellipse_orientation = orientation (degrees) of the error ellipse
# X_1 to X_4 are the sensor data ####as of 15/08/2016- now 8 classes
# ptt = the number code on the PTT (corresponds to a bird)

# what names are missing
cbind(as.character(unique(d1$individual.local.identifier)), unique(d1$individual.local.identifier)%in%unique(strategy.dat$name))
# some but they have different names in strategy.dat but ptt number picked up

# added "Selborne"
# "New Forest 1" is crap so does not get added


# Let the merge remove birds that were tagged in 2017 and 2018.
# This is because strategy.dat only lists birds that were tagged
# in 2016 and earlier.
#d2 <- merge(d1, strategy.dat, by=c("ptt","year"))

# actually merge just on PTT, because year limits birds still in 2017
d2 <- merge(d1, strategy.dat, by="ptt")

#note that duplicate names are reassigned name.x if they come from
# d1 and name.y if they come from strategy.dat

#-------------- convert date.tagged into R date format ---------------
d2$date.tagged_posix <- as.POSIXct(strptime(d2$date.tagged,format="%d/%m/%Y"))

g <- ggmap(m1) +geom_point(data =d2, aes(x=location.long, y=location.lat, colour=algorithm.marked.outlier))+
     facet_wrap(~year.x)
g

# Investigate mystery no name cuckoos

cbind(unique(d1$ptt), unique(d1$ptt)%in%unique(strategy.dat$ptt))

qplot(data =d1[d1$individual.local.identifier=="",], x=location.long, y=location.lat)+facet_wrap(~ptt)

#birds that are over china
table(d1[d1$location.long>100,]$ptt)

# Check birds of interest manually

ggmap(m1) +geom_point(data =d1[d1$ptt==170433,], aes(x=location.long, y=location.lat))+facet_wrap(~year.x)

# OK so after all the investigation, basically the merge method is best using jut 'ptt'

####======================== CLEAN DATA ======================================

#---- remove repeat timestamps (repeat locations) ----

#********** repeat data points with the same lat/long, same timestamp, but different sensor data, different message_dates, and differed ids
# not sure what this means, but since sensor data not being used, can remove these columns and then use unique rows only

##get rid of the columns that aren't needed
#d2.2 <- d2[,c("name","ptt","location_class","latitude","longitude","altitude","error_radius","semi_major_axis","semi_minor_axis","ellipse_orientation","Bird.number","name2","name3","lat.tagged","long.tagged","year.tagged","capture.location","age.at.capture","migratory.strategy","successful.Sahara.crossing","breeding.mgroup","other.mgroup.to.remove","comments","timestamp","year","month","day","julian","date.tagged_posix")]
#d2.2 <- rename(d2.2, c("semi_major_axis"="error.major","semi_minor_axis"="error.minor","latitude"="lat","longitude"="long","location_class"="location.class","error_radius"="error.radius","ellipse_orientation"="ellipse.orientation"))

# there are issues with duplicates so we should run remove 
# the agros flagged locations first so that the good ones dont
# get removed if they are dupliactes

# remove argos marked erroneous points
d3<-subset(d2, algorithm.marked.outlier!="true")

nrow(d2);nrow(d3)

#remove duplicates loop

d3$dupID<-paste(d3$ptt, d3$timestamp)
d3$dupLC<-duplicated(d3$dupID)

d4<-subset(d3, dupLC==FALSE)
nrow(d4);nrow(d3) # makes the dataset a bit smaller!



#---- remove test locations (where timestamp is before tag was deployed on bird or outside Afro-Palearctic) ----
# often will refer to points that were used as either reference points or tested outside Chris Hewson's house in Cambridge (long/lat = 0.125/52.135))
d4$test.location <- as.factor(ifelse(d4$timestamp < d4$date.tagged_posix, "Y", "N"))
d4.1 <- subset(d4, test.location=="N")

nrow(d4.1);nrow(d4)
##60614

# Do some plotting
plot(location.lat~location.long, d4.1, col=algorithm.marked.outlier)
map("world", add=T)



# removes locations outside Afro-Palearctic
d4.2 <- subset(d4.1, location.long > -30)
d4.2 <- subset(d4.2, location.long < 45)

plot(location.lat~location.long, d4.2)
map("world", add=T)


#---- remove birds without a migration strategy (didn't make it out of UK) ----
d5 <- subset(d4.2, migratory.strategy!="")
d5 <- droplevels(d5)

## Ordering of data!

d6<-d5[order(d5$ptt, d5$timestamp),]

# Manual cleaning of locations per bird

d6$ptt_yearID<-paste(d6$ptt, d6$year.x, sep="_")
d6$outlier_flagged<-""

out<-NULL
for(i in unique(d6$ptt_yearID))
  {
  
  print(i)
  print(table(d6[d6$ptt==strsplit(i, "_")[[1]][1],]$year.x))
  
  temp<- d6[d6$ptt_yearID==i,]
  
  plot(location.lat~location.long, temp, col=2)
  lines(location.lat~location.long, temp, col=3)
  map("world", add=T)
  
  a1<-"N"
  a1<-readline("are there outliers? (y?)")
  if(a1=="y")
    {id<-identify(x=temp$location.long, y=temp$location.lat)
    temp[id,]$outlier_flagged<-"Y"
    points(location.lat~location.long, 
           temp[temp$outlier_flagged=="Y",], col=4)}
  
  out<-rbind(out, temp)
  }

plot(location.lat~location.long, d6, col=2)
 map("world", add=T)
 points(location.lat~location.long, out[out$outlier_flagged=="Y",], col=4)

# final stragglers
 points(location.lat~location.long, out[out$location.lat>60,], col=5)
 
 out[out$location.lat>60,]$outlier_flagged<-"Y"
 
 points(location.lat~location.long, out[out$location.lat>45 &
                                          out$location.long< -10,], col=6)
 
 out[out$location.lat>45 &
           out$location.long< -10,]$outlier_flagged<-"Y"
 
 plot(location.lat~location.long, d6, col=2)
 map("world", add=T)
 points(location.lat~location.long, out[out$outlier_flagged=="Y",], col=4)

 # ok cool!
 
 write.csv(out, "~/BTO/cuckoo_tracking/data/movebank_cuckoos_hybridfilter_clean_RAW.csv", quote=F, row.names=F)

 out[out$outlier_flagged=="Y",]
 
 table(out$argos.lc) # still some 'Z' in there!
 
 #make dataset_smaller
 
 out$visible<-NULL
 out$algorithm.marked.outlier<-NULL
 out$argos.best.level<-NULL
 out$argos.calcul.freq<-NULL
 out$argos.gdop<-NULL
 out$argos.iq<-NULL
 out$argos.lat1<-NULL
 out$argos.lat2<-NULL
 out$argos.lon1<-NULL
 out$argos.lon2<-NULL
 out$argos.nb.mes<-NULL
 out$argos.nb.mes.120<-NULL
 out$argos.nopc<-NULL
 out$argos.pass.duration<-NULL
 out$argos.sat.id<-NULL
 out$argos.sensor.1<-NULL
 out$argos.sensor.2<-NULL
 out$argos.sensor.3<-NULL
 out$argos.sensor.4<-NULL
 out$argos.transmission.timestamp<-NULL
 out$argos.valid.location.algorithm<-NULL
 out$sensor.type<-NULL
 out$individual.taxon.canonical.name<-NULL
 out$study.name<-NULL
 out$dupID<-NULL
 out$dupLC<-NULL
 out$test.location<-NULL

# remove the outliers 
 clean<-out[out$outlier_flagged!="Y",]
 
 write.csv(clean, "~/BTO/cuckoo_tracking/data/movebank_cuckoos_hybridfilter_clean.csv", quote=F, row.names=F)
 
 
####======================== ADD TRANSMISSION CYCLE VARIABLE ======================================

 #read back in
 
 clean<-read.csv( "N:/cuckoo_tracking/data/movebank_cuckoos_hybridfilter_bestofday_clean.csv", h=T)
 
 # remember to change timestamp to POSIX format!!
 
 clean$timestamp <- as.POSIXct(strptime(clean$timestamp, "%Y-%m-%d %H:%M:%S"), "UTC")
 
#---- add transmission cycle data variable
# calulated based on logic
# requires dataset to be ordered 1) by bird name; 2) by timestamp
# IF((birdname t+1) = (birdname t), IF((timestamp t+1 - timestamp t) > 10/24, tcycle+1, tcycle), 1)
# first observation for each bird, tcycle=1, then tcycle increases by 1 if the difference between two successive timestamps is > 10/24 hours

# order dataset by name and timestamp
# already done

# function calculates tcycle variable over each level of name
tcyclefunc <- function(x) {
  ##function diff calculates the difference between consecutive values in a vector
  ##so- is the difference between the consecutive times greater than 10/24
  tcycle <- as.numeric(diff(x$timestamp), units="days") > 10/24
  ##if yes gets 1 else gets zero
  ##and add a one to start with
  tcycle <- c(1,ifelse(tcycle==TRUE,1,0))
  ##summuative sum as you go along the vector 
  tcycle <- cumsum(tcycle)
  ##new dataframe with data, tcycle and column showing the number of days between successive timestamps
  ##add an NA for the first record for each bird as can't have travelled any distance for that one
  newdataframe <- data.frame(x,tcycle,days.btwn.trans=c(NA,as.numeric(diff(x$timestamp), units="days")))
  ##return dataframe
  return(newdataframe)
}

##create series of lists one for each cuckoo
##apply tcycle function to this list
d7 <- do.call(rbind,by(clean, list(clean$ptt), tcyclefunc))
##not sure why we're adding row names here
##as we add ids later anyway
rownames(d7) <- c(1:nrow(d7))
str(d7)

####======================== ASSIGN UNIQUE IDs TO EACH OBSERVATION IN DATASET ======================================

# unique ids were removed earlier
# unique ids for each location for each individual will be needed later

d8 <- data.frame(id=1:nrow(d7), d7)

####======================== ADD MOVEMENT DATA ======================================

# Add distance, bearing, mgroup and mtype to original data

# if new filtering criteria used, modify variable fed to movdat
movdat <- d8

movdat<-rename(movdat, c('location.long'='long', 'location.lat'='lat'))

### FUNCTION TO ADD MOVEMENT DATA ###

addmovedata <- function(dat) { # distance, bearing, etc function
  
  # for each individual, do the following:
  
  ### --- DISTANCE & BEARING CALCULATION --- ###
  
  # calculate distances and bearings   
  # distbearmvmt <- function(input){ # requires 2 variables called newlongs & newlats or long & lat or centroidlong & centroidlat
  # use newlongs & newlats to calculate distances based on resampled points
  # use long & lat to calculate distances based on original measured location point
  
  input <- dat
  
  dist.cuckoo <- input[,c("long","lat")] # ok i think some package lets you do this, plyr? maybe?
  coordinates(dist.cuckoo) <- c("long", "lat")
  proj4string(dist.cuckoo) <- CRS("+proj=longlat +datum=WGS84")
  
  d1 <- dist.cuckoo@coords
  d2 <- dist.cuckoo@coords[-1,] # create new matrix minus the first row so that d2 starts at the second observation
  last.d2 <- matrix(c(0,0), nrow=1, dimnames=list(1, c("long","lat"))) # create a placeholder last row in the second distance matrix
  d2 <- rbind(d2, c(0,0))
  
  dist <- distCosine(d1,d2)/1000 # distance between points, in km
  bear <- bearing(d1,d2) # bearing between points
  dist <- c(NA,dist[-length(dist)])
  bear <- c(NA,bear[-length(bear)])
  
  distbear <- data.frame(input, distance=dist, bearing=bear)
  
  ### --- MOVEMENT GROUPS --- ###
  
  ##classifying as migratory if at least 30 km (now 50 km) from previous location otherwise is stationary
  
  mgroup <- rep(NA,nrow(distbear))
  
  for (n in 1:nrow(distbear)){
    if (is.na(distbear$distance[n])) {
      mgroup[n] <- 1
      ##if under 50km gets same group as previous point # 
      ## Previously used 30, changed to 50 as per nature paper
    } else if (distbear$distance[n] <= 50) {
      mgroup[n] <- mgroup[n-1]
      ##if greater than 50km gets new point
    } else {mgroup[n] <- mgroup[n-1] + 1}
  }
  
  ### --- MOVEMENT TYPES --- ###
  
  mtype <- c("C", rep(NA, nrow(distbear)-1))
  
  for (n in 2:(nrow(distbear)-1)){
    if (mgroup[n] != mgroup[n-1] & mgroup[n] != mgroup[n+1]) {
      mtype[n] <- "M"
    } else {
      mtype[n] <- "S"
    }
  }
  
  completedata <- data.frame(distbear,mgroup,mtype)
  return(completedata)
  
}

withmovedata <- by(movdat, list(movdat$ptt), addmovedata)
withmovedata.all <- do.call(rbind, withmovedata)

####======================== ADD STOPOVER DURATION DATA ======================================

# load functions for calculating LOS; calculate.LOS() and makeLOStable()
source("N:/cuckoo_tracking/t_drive/scripts/source_scripts/function to add stopover duration data for analysis.R")

withLOSdata <- lapply(withmovedata, calculate.LOS)
withLOSdata.all <- do.call(rbind, withLOSdata)

####======================== Correct LOS - Hewson nature paper ======================================

# need to extend LOS if next stopover > 1 duty cycle of time away.
# If so, we assume the bird stayed at the current stopover until
# the final duty cycle (2 days) prior to the next stopover.
# Note: we need to consider 'M' classed points as limiters to stopover duration

withLOSdata.all$LOS.recalc<-withLOSdata.all$LOS

for(j in 1:nrow(withLOSdata.all))

  {
  if(withLOSdata.all[j,]$mgroup==
     withLOSdata.all[j+1,]$mgroup |
     is.na(withLOSdata.all[j,]$days.btwn.trans+
     withLOSdata.all[j+1,]$days.btwn.trans) |
     withLOSdata.all[j+1,]$days.btwn.trans<3)
  
    {next}else{
              withLOSdata.all[which(withLOSdata.all$ptt==withLOSdata.all[j,]$ptt &
                          withLOSdata.all$mgroup==withLOSdata.all[j,]$mgroup),]$LOS.recalc<-
           
              withLOSdata.all[which(withLOSdata.all$ptt==withLOSdata.all[j,]$ptt &
                                   withLOSdata.all$mgroup==withLOSdata.all[j,]$mgroup),]$LOS+
              (withLOSdata.all[j+1,]$days.btwn.trans-2);
              print(j)
              }
    }       
    
# check rows which have mtype as "M", LOS as NA but LOS.recalc != NA
# these are effectively migration lengths, posing as stopovers. 
# not an issue as we strip them out using dat[dat$mytype!='M",]
  

####======================== WRITE FINAL DATA ======================================

fulloriginal <- withLOSdata.all
rownames(fulloriginal) <- c(1:nrow(fulloriginal))

# add mgroup column to end of dataset also for simpler visualisation

fulloriginal$mgroup_replicate<-fulloriginal$mgroup

write.csv(fulloriginal, "N:/cuckoo_tracking/data/processed_movebank_cuckoos_hybrid_filter_bestofday_clean_stopovers_recalc.csv", row.names=F, quote=F)

### Make summary table of stopovers

withLOStable <- lapply(withmovedata, makeLOStable)
withLOStable.all <- do.call(rbind, withLOStable)

withLOStable.all$ptt<-strsplit( row.names(withLOStable.all),'\\.')[[1]][1]
write.csv(withLOStable.all, "~/BTO/cuckoo_tracking/data/processed_movebank_cuckoos_hybrid_filter_clean_stopovers_summary.csv", row.names=F, quote=F)

## Make GIS spatial lines object
pttz<-unique(fulloriginal$ptt)

out_line<-NULL
for(i in pttz)
{
  Trip <- fulloriginal[fulloriginal$ptt == i,]
  
  b2<-NULL
  for(j in unique(Trip$year.x))
  {
    
  L1 <- Line(as.matrix(data.frame(Trip[Trip$year.x==j,]$long,
                                  Trip[Trip$year.x==j,]$lat)))
  Ls1 <- Lines(L1, ID=paste(i, j, sep="."))
  SpLs1 <- SpatialLines(list(Ls1), CRS("+proj=longlat + datum=wgs84"))
  
  Tbl <- data.frame(ptt=i, year=j)
  row.names(Tbl) <- paste(i, j, sep=".")
  SLDF<-SpatialLinesDataFrame(SpLs1, Tbl)  
  
  if(is.null(b2)) {b2 <- SLDF} else
    b2 <- spRbind(b2, SLDF)
  }
  
  if(is.null(out_line)){out_line<-b2}else{
    out_line<-spRbind(out_line, b2)}
}


setwd("~/BTO/cuckoo_tracking/data")
writeOGR(obj=out_line, dsn="spatial",
         layer="movebank_cuckoos_bestofday_lines", driver="ESRI Shapefile",
         overwrite_layer = T)


