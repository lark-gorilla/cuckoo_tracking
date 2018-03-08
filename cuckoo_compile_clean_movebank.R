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


# Let the merge remove birds that were tagged in 2017 and 2018.
# This is because strategy.dat only lists birds that were tagged
# in 2016 and earlier.
d2 <- merge(d1, strategy.dat, by=c("ptt","year"))

#-------------- convert date.tagged into R date format ---------------
d2$date.tagged_posix <- as.POSIXct(strptime(d2$date.tagged,format="%d/%m/%Y"))

####======================== CLEAN DATA ======================================

#---- remove repeat timestamps (repeat locations) ----

#********** repeat data points with the same lat/long, same timestamp, but different sensor data, different message_dates, and differed ids
# not sure what this means, but since sensor data not being used, can remove these columns and then use unique rows only

##get rid of the columns that aren't needed
d2.2 <- d2[,c("name","ptt","location_class","latitude","longitude","altitude","error_radius","semi_major_axis","semi_minor_axis","ellipse_orientation","Bird.number","name2","name3","lat.tagged","long.tagged","year.tagged","capture.location","age.at.capture","migratory.strategy","successful.Sahara.crossing","breeding.mgroup","other.mgroup.to.remove","comments","timestamp","year","month","day","julian","date.tagged_posix")]
d2.2 <- rename(d2.2, c("semi_major_axis"="error.major","semi_minor_axis"="error.minor","latitude"="lat","longitude"="long","location_class"="location.class","error_radius"="error.radius","ellipse_orientation"="ellipse.orientation"))
length(d2.2[,1])
##215674
d3 <- unique(d2.2)
length(d3[,1])
##63961

#---- remove test locations (where timestamp is before tag was deployed on bird or outside Afro-Palearctic) ----
# often will refer to points that were used as either reference points or tested outside Chris Hewson's house in Cambridge (long/lat = 0.125/52.135))
d3$test.location <- as.factor(ifelse(d3$timestamp < d3$date.tagged_posix, "Y", "N"))
d3.2 <- subset(d3, test.location=="N")
length(d3.2[,1])
##60614

# Do some plotting
plot(lat~long, d3, col=year)
map("world", add=T)

plot(lat~long, d3, col=location.class)
map("world", add=T)


# removes locations outside Afro-Palearctic (reference points in North America plus any really weird points in the Atlantic)
d3.3 <- subset(d3.2, long > -30)
length(d3.3[,1])
##60585

#---- remove poor location classes ----
d4 <- subset(d3.3, location.class=="1" | location.class=="2" | location.class=="3") # subset by best location classes
length(d4[,1])
##21693

#---- remove obs with negative error values AND major error > 5000 ----
d5 <- subset(d4, error.major>0 & error.minor>0)
d5 <- subset(d5, error.major <= 5000)
length(d5[,1])
##19460

#---- remove birds without a migration strategy (didn't make it out of UK) ----
d6 <- subset(d5, migratory.strategy!="")
d6 <- droplevels(d6)

length(d6[,1])
##19301

# make maps
#sbbox <- make_bbox(lon = d6$long, lat = d6$lat, f = 1)
#sbbox

####======================== ADD TRANSMISSION CYCLE VARIABLE ======================================

#---- add transmission cycle data variable
# calulated based on logic
# requires dataset to be ordered 1) by bird name; 2) by timestamp
# IF((birdname t+1) = (birdname t), IF((timestamp t+1 - timestamp t) > 10/24, tcycle+1, tcycle), 1)
# first observation for each bird, tcycle=1, then tcycle increases by 1 if the difference between two successive timestamps is > 10/24 hours

# order dataset by name and timestamp
d6.2 <- d6[order(d6$name, d6$timestamp),]

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
d7 <- do.call(rbind,by(d6.2, list(d6.2$name), tcyclefunc))
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

### FUNCTION TO ADD MOVEMENT DATA ###

addmovedata <- function(dat) { # distance, bearing, etc function
  
  # for each individual, do the following:
  
  ### --- DISTANCE & BEARING CALCULATION --- ###
  
  # calculate distances and bearings   
  # distbearmvmt <- function(input){ # requires 2 variables called newlongs & newlats or long & lat or centroidlong & centroidlat
  # use newlongs & newlats to calculate distances based on resampled points
  # use long & lat to calculate distances based on original measured location point
  
  input <- dat
  
  dist.cuckoo <- input[,c("long","lat")]
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
  
  ##classifying as migratory if at least 30 km from previous location otherwise is stationary
  
  mgroup <- rep(NA,nrow(distbear))
  
  for (n in 1:nrow(distbear)){
    if (is.na(distbear$distance[n])) {
      mgroup[n] <- 1
      ##if under 30km gets same group as previous point
    } else if (distbear$distance[n] <= 30) {
      mgroup[n] <- mgroup[n-1]
      ##if greater than 30km gets new point
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

withmovedata <- by(movdat, list(movdat$name), addmovedata)
withmovedata.all <- do.call(rbind, withmovedata)

####======================== ADD STOPOVER DURATION DATA ======================================

# load functions for calculating LOS; calculate.LOS() and makeLOStable()
source("T:/Mark M/scripts/source_scripts/function to add stopover duration data for analysis.R")

withLOSdata <- lapply(withmovedata, calculate.LOS)
withLOSdata.all <- do.call(rbind, withLOSdata)

plot(lat~long, withLOSdata.all[withLOSdata.all$ptt=="134963",],
     col=factor(tcycle))
map("world", add=T)


m1 <- get_map(location = c(lon = mean(withLOSdata.all$long),
                           lat = mean(withLOSdata.all$lat)), zoom=3)
g <- ggmap(m1) +geom_point(data =withLOSdata.all, aes(x=long, y=lat, colour=mtype) ,pch=16)+
  facet_wrap(~year)
g

jpeg("N:/cuckoo_tracking/stopover_map1.jpg", width=9, height=4, res=600, units="in")
g
dev.off()

####======================== WRITE FINAL DATA ======================================

fulloriginal <- withLOSdata.all
rownames(fulloriginal) <- c(1:nrow(fulloriginal))


write.csv(fulloriginal, "N:/cuckoo_tracking/data/processed_cuckoos.csv", row.names=F, quote=F)

### Make my own summary table of stopovers

#withLOStable <- lapply(withmovedata, makeLOStable)
#withLOStable.all <- do.call(rbind, withLOStable)

just_sto<-fulloriginal[fulloriginal$mtype=="S",]






