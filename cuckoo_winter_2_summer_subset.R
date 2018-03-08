########################
# 02/02/18
# Subsetting data to West Africa region using
# dates from wintering to summer return to UK
# Test hypotheses about timing of spring migration
# movement, W. African stopovers and return to UK
########################

rm(list=ls())
library(maps)
library(sp)
library(maptools)
library(rgdal)
library(ggplot2)
library(ggmap)

# using best of day data
dat<-read.csv("~/BTO/cuckoo_tracking/data/processed_movebank_cuckoos_hybrid_filter_bestofday_clean_stopovers.csv", h=T)

# FORMAT!!
dat$timestamp <- as.POSIXct(strptime(dat$timestamp, "%Y-%m-%d %H:%M:%S"), "UTC")

# subset from wintering period to first landfall in UK,
# first limit is a time boundary, second is a geographical overlap

# first subset to points in UK
setwd("~/BTO/cuckoo_tracking/sourced_data/")
world<-readOGR(layer="TM_WORLD_BORDERS-0.3",
            dsn="country_borders") # different linux/windows

head(world@data)

uk<-world[world$NAME=="United Kingdom",]

plot(world);plot(uk, add=T, col=2)

datSP<-SpatialPointsDataFrame(SpatialPoints(cbind(dat$long, dat$lat), 
      CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")),
      data=dat)

plot(uk); plot(datSP, add=T)
plot(datSP[uk,], col=2, add=T)

# bit of a hack to get rows that sit in uk
datSP$rownum<-1:nrow(dat)
datSP$inUK<-"N"
ukptz<-datSP[uk,]
# back to data.frame
dat2<-datSP@data
dat2[ukptz$rownum,]$inUK<-"Y"
#make sure it worked

plot(lat~long, dat2[dat2$inUK=="Y",])
map("world", add=T)
points(lat~long, dat2[dat2$inUK=="N",], col=2)
# ok
dat2$rownum<-NULL

# Now to figure out when wintering starts and ends

qplot(data=dat2[dat2$lat<20,], x=lat, geom="histogram", binwidth=2)
# so lots of points around equator, should be wintering location

qplot(data=dat2[dat2$lat<20,], x=lat,
      geom="histogram", binwidth=2)+facet_wrap(~ptt)

ggplot(data=dat2[dat2$lat<20,], aes(x=long, y=lat)) +
  geom_hex()

ggplot(data=dat2[dat2$lat<10,], aes(x=long, y=lat, size=LOS, colour=as.factor(month)))+
  geom_point()+facet_wrap(~ptt)


# Now we want points from december up 
# until the bird enters the uk, this will be different
# for each bird so a loop that counts from

# hack to put months in correct numeric order
dat2$month2<-ifelse(dat2$month>9, dat$month, paste(0, dat$month, sep=""))
dat2$yrmo<-as.numeric(paste(dat2$year.x, dat2$month2, sep="."))

worlddat<-fortify(world,region="NAME")

m1 <- get_map(location = c(lon = mean(dat2$long, na.rm=T),
                           lat = mean(dat2$lat, na.rm=T)), zoom=3)

m1 <- get_map(location = "Nigeria", zoom=4)
g <- ggmap(m1)

i=62518
  g<-ggmap(m1)+
    geom_point(data=dat2[dat2$ptt==i,], aes(x=long, y=lat, size=LOS))+
   
    facet_wrap(~yrmo)
  
  g<-ggmap(m1)+
    geom_line(data=dat2[dat2$ptt==i,], aes(x=long, y=lat,
                                             colour=factor(yrmo)))+
    geom_jitter(data=dat2[dat2$ptt==i,], aes(x=long, y=lat,
                colour=factor(yrmo)), shape=1, width = 1, height = 1)
  
  
# old attempt
  for(i in unique(dat2$ptt))
  {
    g<-ggplot()
    
    g+geom_polygon(data=worlddat, aes(x=long, y=lat, group=group),colour="black", fill=NA)+
      
      geom_point(data=dat2[dat2$ptt==i,], aes(x=long, y=lat, size=LOS))+
      
      coord_cartesian(xlim = c((min(dat2[dat2$ptt==i,]$long)-2),
                               max(dat2[dat2$ptt==i,]$long)+2),
                      ylim = c((min(dat2[dat2$ptt==i,]$lat)-2),
                               max(dat2[dat2$ptt==i,]$lat)+2))+
      facet_wrap(~yrmo, scales="free")
    
# use tracktime


dat2$date_tracktime<-as.double(as.POSIXct(strptime(dat$timestamp, "%Y-%m-%d"), "UTC"))

worlddat<-fortify(world,region="NAME")

for(i in sort(unique(dat2$date_tracktime)))
{
  g<-ggplot()
  
  g+geom_polygon(data=worlddat, aes(x=long, y=lat, group=group),colour="black", fill=NA)+
  
    geom_point(data=dat2[dat2$date_tracktime==i,], aes(x=long, y=lat, size=LOS))+
    geom_line(data=dat2[dat2$date_tracktime<i,], aes(x=long, y=lat, group=ptt))+
      coord_map(xlim = c((min(dat2[dat2$date_tracktime<i,]$long)-2),
                         max(dat2[dat2$date_tracktime<i,]$long)+2),
                ylim = c((min(dat2[dat2$date_tracktime<i,]$lat)-2),
                        max(dat2[dat2$date_tracktime<i,]$lat)+2))+facet_wrap(~ptt)
  
}

# use wintering ground


# scrap that approach 

dat2$timestamp <- as.POSIXct(strptime(dat$timestamp, "%Y-%m-%d %H:%M:%S"), "UTC")

dat2$stage<-"breeding_autumn_mig"

i=62518

dec_bird<-dat2[dat2$ptt==i & dat2$month==12,]

for(j in unique(dat2[dat2$ptt==i,]$year.x ))

bird_yr<-dat2[dat2$ptt==i & dat2$year.x,]

bird2<-bird[bird]

bird2


