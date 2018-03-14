#### 14/03/18

## Interpolate tracking data to gap fill duty cycling 
## Make animation of spring migration using time-step
## approach. Could facet by year but could be hectic.

# best of day data

dat<-read.csv("~/BTO/cuckoo_tracking/data/processed_movebank_cuckoos_hybrid_filter_bestofday_clean_stopovers.csv", h=T)

dat$timestamp <- as.POSIXct(strptime(dat$timestamp, "%Y-%m-%d %H:%M:%S"), "UTC")
dat$TrackTime<-as.double(dat$timestamp)
names(dat)[names(dat)=="lat"]<-"Latitude"
names(dat)[names(dat)=="long"]<-"Longitude"

# Interpolation using old seabird code.

####@@@@ RESAMPLE @@@@####

source("~/BTO/cuckoo_tracking/sourced_data/code/Resample.r")
# note I hased out the plot part of function to speed up

## set up to not interpolate between points > 1 hr apart

results<-NULL
for(i in unique(dat$ptt))
{
  Track<-dat[dat$ptt == i,]
  
  #Track<-Track[-which(duplicated(Track$TrackTime)==TRUE),]
  #print(paste(length(which(duplicated(Track$TrackTime)==TRUE)), "duplicates removed", sep=" "))
  
  resample_output<-resample(Track, timeStep=1)  
  ## timeStep set for 1 hour interval as anything above makes weird
  
  #readline("ok")
  results<-rbind(results,resample_output)
  print(i)
}

plot(Latitude~Longitude, results, pch=16, cex=0.4, col=Bird_ID)
map("worldHires", add=T, col=3)

results$timestamp<-
paste(as.Date(as.POSIXlt(results$TrackTime, origin="1970-01-01", "GMT")),
      format((as.POSIXlt(results$TrackTime, origin="1970-01-01", "GMT")), "%H:%M:%S"))

# maybe re-run with ID for stopover or migratory pioint to
# tell if point is interpolated (majorly) or not

write.csv(results, "~/BTO/cuckoo_tracking/data/processed_movebank_cuckoos_hybrid_filter_bestofday_clean_1hrINTERP.csv", quote=F, row.names=F)

#### Visualisation

results<-read,csv("~/BTO/cuckoo_tracking/data/processed_movebank_cuckoos_hybrid_filter_bestofday_clean_1hrINTERP.csv", h=T)

library(lubridate)

results$yday<-yday(results$timestamp)

library(ggplot2)

for (in 1:365)
  
{
  
  
}


UK_travel<-read.csv("~/BTO/cuckoo_tracking/data/complete_cycles_UK_all_timing.csv", h=T)

UK_travel<-UK_travel[UK_travel$deployment_entry==FALSE,]

dat2$key<-paste(dat2$ptt, dat2$year.x)

dat3<-dat2[dat2$key %in% unique(paste(UK_travel$ptt, UK_travel$year)),]

dat3<-dat3[dat3$month<6,]

m1 <- get_map(location = c(5, 30), zoom=4)


for(i in unique(dat3$ptt))
{
  cols <- c("2012" = "red", "2013" = "blue", "2014" = "darkgreen", "2015" = "orange", "2016"= "cyan", "2017"= "black")
  
  
  g<-ggmap(m1)+
    geom_path(data=dat3[dat3$ptt==i,], aes(x=long, y=lat,
                                           colour=factor(year.x)))+
    geom_jitter(data=dat3[dat3$ptt==i,], aes(x=long, y=lat,
                                             colour=factor(year.x)), shape=1, width = 0.5, height= 0.5)+ggtitle(i)+
    
    geom_label_repel(data=dat[dat$ptt==i & dat$deployment_entry==FALSE,], aes(x=SO_median_long, y=SO_median_lat,
                                                                              colour=factor(year), label=substr(SO_start, 6,10)))+
    geom_label_repel(data=aggregate(year~breeding_entry, data=dat[dat$ptt==i & dat$deployment_entry==FALSE,], FUN=function(x){unique(x)})
                     , aes(x=-18, y=45,
                           colour=factor(year), label=unique(substr(breeding_entry, 6,10)))) + scale_colour_manual(values = cols) 
  
  print(g)
  
}

  


