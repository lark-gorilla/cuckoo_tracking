########################
# 13/02/18
# Investigating wintering and west africa stopover movements
########################

rm(list=ls())

library(ggplot2)

library(readxl)

# read the xlsx file as it has manually corrected country column
# and has the mig_cohort column added
dat<-read_excel("~/BTO/cuckoo_tracking/data/stopover_table_bestofday_biomes_Chris.xlsx")


# these cuckoos complete at least one annual migration
gd_cucks<-c(62608,62688,115586,115591,115594,
            115602,121792,128297,
            128300,128302,134951,134952,
            134955,134956,134957,134963,
            146754,146757,146758,146759,146760,
            161318,161321,161324)

gd_cucks %in% unique(dat$ptt)  

dat<-subset(dat, dat$ptt %in% gd_cucks)

dat$stage<-"migration"
dat[dat$country=="United Kingdom",]$stage<-"breeding"

# remove migration cohorts from multi-year birds 
# that do not complete the full cycle

dat$complete_mig<-"Y"

dat[dat$ptt==62608 & dat$mig_cohort==2015,]$complete_mig<-"N"
dat[dat$ptt==62688 & dat$mig_cohort==2012,]$complete_mig<-"N"
dat[dat$ptt==115586 & dat$mig_cohort==2015,]$complete_mig<-"N"
dat[dat$ptt==115591 & dat$mig_cohort==2014,]$complete_mig<-"N"
dat[dat$ptt==115594 & dat$mig_cohort==2016,]$complete_mig<-"N"
dat[dat$ptt==115602 & dat$mig_cohort==2013,]$complete_mig<-"N"
dat[dat$ptt==128297 & dat$mig_cohort==2014,]$complete_mig<-"N"
dat[dat$ptt==128300 & dat$mig_cohort==2014,]$complete_mig<-"N"
dat[dat$ptt==128302 & dat$mig_cohort==2014,]$complete_mig<-"N"
dat[dat$ptt==134952 & dat$mig_cohort==2015,]$complete_mig<-"N"
dat[dat$ptt==134955 & dat$mig_cohort==2015,]$complete_mig<-"N"
dat[dat$ptt==134956 & dat$mig_cohort==2015,]$complete_mig<-"N"
dat[dat$ptt==134957 & dat$mig_cohort==2015,]$complete_mig<-"N"
dat[dat$ptt==134963 & dat$mig_cohort==2015,]$complete_mig<-"N"
dat[dat$ptt==146758 & dat$mig_cohort==2017,]$complete_mig<-"N"
dat[dat$ptt==146759 & dat$mig_cohort==2017,]$complete_mig<-"N"
dat[dat$ptt==146760 & dat$mig_cohort==2016,]$complete_mig<-"N"
dat[dat$ptt==161318 & dat$mig_cohort==2017,]$complete_mig<-"N"
dat[dat$ptt==161321 & dat$mig_cohort==2017,]$complete_mig<-"N"
dat[dat$ptt==161324 & dat$mig_cohort==2017,]$complete_mig<-"N"

# Note some birds aren't listed as they completed exactly one year           

# Do the subset and check in Excel
dat2<-dat[dat$complete_mig=="Y" | dat$stage=="breeding",]

write.csv(dat2, "~/BTO/cuckoo_tracking/data/stopover_bestofday_complete_cycles.csv", quote=F, row.names=F)

# get UK arrival date from non-best-of-day data

hires<-read.csv("~/BTO/cuckoo_tracking/data/movebank_cuckoos_hybridfilter_clean.csv", h=T)

# find points in UK

library(rgdal)

datSP<-SpatialPointsDataFrame(SpatialPoints(cbind(hires$location.long, hires$location.lat), 
                                            CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")),
                              data=hires)

setwd("~/BTO/cuckoo_tracking/sourced_data/")
world<-readOGR(layer="TM_WORLD_BORDERS-0.3",
               dsn="country_borders") # different linux/windows

head(world@data)

uk<-world[world$NAME=="United Kingdom",]

plot(uk); plot(datSP, add=T)
plot(datSP[uk,], col=2, add=T)

# bit of a hack to get rows that sit in uk
datSP$rownum<-1:nrow(hires)
datSP$inUK<-"N"
ukptz<-datSP[uk,]

# back to data.frame overwrite old hires
hires<-datSP@data
hires[ukptz$rownum,]$inUK<-"Y"

# only get birds we're interested in from dat2 and when in UK

hires2<-hires[hires$ptt %in% gd_cucks & hires$inUK=="Y",]

hires2$timestamp <- as.POSIXct(strptime(hires2$timestamp, "%Y-%m-%d %H:%M:%S"), "UTC")

library(ggplot2)

UK_travel<-NULL
for(i in gd_cucks)
  {
  m1<-data.frame(ptt=i,year=aggregate(timestamp~year.x, hires2[hires2$ptt==i,], min)[,1],
             UK_entry=aggregate(timestamp~year.x, hires2[hires2$ptt==i,], min)[,2],
              UK_exit=aggregate(timestamp~year.x, hires2[hires2$ptt==i,], max)[,2])

  UK_travel<-rbind(UK_travel, m1)
  }

# add column to pull up entry dates to UK that are actually 
# deployments i.e. already in UK and a column to give potentially
# erroneous exits from the UK, i.e. logger died in UK, these 
# may be very few and not the focus of work so just a flag.
UK_travel$deployment_entry<-unlist(by(UK_travel, UK_travel$ptt, FUN=function(x) {x$year==min(x$year)}))
UK_travel$unreliable_exit<-unlist(by(UK_travel, UK_travel$ptt, FUN=function(x) {x$year==max(x$year)}))

write.csv(UK_travel, "~/BTO/cuckoo_tracking/data/complete_cycles_UK_timing.csv", quote=F, row.names=F)

# OK we need better than just arrival into the UK, no add arrival at breeding ground
# this is defined as a 10km buffer around breeding stopovers from the best of day data
# these data are read in from shapefile created in qgis

# read in necessary datatsets
library(rgdal)

setwd("~/BTO/cuckoo_tracking/data/")
breeding_site<-readOGR(layer="complete_cycle_cuckoo_breeding_buffers",
               dsn="spatial") # different linux/windows

UK_travel<-read.csv("~/BTO/cuckoo_tracking/data/complete_cycles_UK_timing.csv", h=T)
hires<-read.csv("~/BTO/cuckoo_tracking/data/movebank_cuckoos_hybridfilter_clean.csv", h=T)

# loop will run per ptt and isolate points within breeding buffers
# then, per year pull the first and last points

breeding_timing<-NULL
for(i in gd_cucks)
{
  pttSP<-SpatialPointsDataFrame(
    SpatialPoints(cbind(hires[hires$ptt==i,]$location.long,
                        hires[hires$ptt==i,]$location.lat), 
                        CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")),
                                data=hires[hires$ptt==i,]) 
  
  breedingpts<-pttSP[breeding_site[breeding_site$ptt==i,],]@data
  
  breedingpts$timestamp <- as.POSIXct(strptime(breedingpts$timestamp, "%Y-%m-%d %H:%M:%S"), "UTC")

  m1<-data.frame(ptt=i,year=aggregate(timestamp~year.x, breedingpts, min)[,1],
                 breeding_entry=aggregate(timestamp~year.x, breedingpts, min)[,2],
                 breeding_exit=aggregate(timestamp~year.x, breedingpts, max)[,2])
  
  breeding_timing<-rbind(breeding_timing, m1)
}


library(plyr)

UK_travel<-read.csv("~/BTO/cuckoo_tracking/data/complete_cycles_UK_timing.csv", h=T)

travel2<-join_all(list(UK_travel, breeding_timing), by=c("ptt", "year"))

# first NA in breeding times is fine as he bolted after deployment, the other one 
# is because tracker died just at breeing location so manually fill
travel2[45,7:8]<-travel2[45,3:4]
travel2[1,7:8]<-travel2[1,3:4]

#write out
write.csv(travel2,"~/BTO/cuckoo_tracking/data/complete_cycles_UK_all_timing.csv", quote=F, row.names=F)

# have a look

travel2$UK_entryHACK<-as.Date(paste("2000",
  substr(travel2$UK_entry,5,11), sep=""))

travel2$breeding_entryHACK<-as.Date(paste("2000",
  substr(travel2$breeding_entry,5,11), sep=""))

travel2$UK_entry-travel2$breeding_entry

#d_limit<-c(min(UK_travel[UK_travel$deployment_entry==FALSE,]$UK_entryHACK),
#          max(UK_travel[UK_travel$deployment_entry==FALSE,]$UK_entryHACK))

# need to specify limits bit further from the data otherwise it
# cuts bits off the plot

d_limit<-as.Date(c("2000-04-11", "2000-05-22"))

p<-ggplot(data=UK_travel[UK_travel$deployment_entry==FALSE,], 
          aes(x=UK_entryHACK))

p1<-p+geom_histogram(binwidth=1, col='black' )+facet_wrap(~year)+
          scale_x_date(limits=d_limit, 
          date_breaks = "1 week", 
          date_minor_breaks = "1 day",
          date_labels = "%b %d")
          

jpeg("~/BTO/cuckoo_tracking/outputs/UK_return_years.jpg",
     width = 12, height =6 , units ="in", res =300)
p1
dev.off()

aggregate(UK_entryHACK~year,
UK_travel[UK_travel$deployment_entry==FALSE,], mean)

aggregate(UK_entryHACK~year,
UK_travel[UK_travel$deployment_entry==FALSE,], sd)
#  year mean    sd
# 2012  30/04  0.7071068
# 2013  28/04  4.0414519
# 2014  01/05  7.0440789
# 2015  27/04  10.0332780
# 2016  28/04  6.5246784
# 2017  01/05  12.5286339

p1<-p+geom_histogram(binwidth=1, col='black' )+facet_wrap(~ptt)+
  scale_x_date(limits=d_limit, 
               date_breaks = "1 week", 
               date_minor_breaks = "1 day",
               date_labels = "%b %d")

jpeg("~/BTO/cuckoo_tracking/outputs/UK_return_birds.jpg",
     width = 15, height =6 , units ="in", res =300)
p1
dev.off()


aggregate(UK_entryHACK~ptt,
          UK_travel[UK_travel$deployment_entry==FALSE,], mean)

aggregate(UK_entryHACK~ptt,
          UK_travel[UK_travel$deployment_entry==FALSE,], sd)

#   ptt     mean          sd 
#   62608   2000-04-29    4.2426407
#   62688   2000-04-30
#   115586   2000-04-30   5.6568542
#   115591   2000-04-30   2.1213203
#   115594   2000-05-02   7.8049130
#   115602   2000-04-25   
#   121792   2000-04-21   1.4142136
#   128297   2000-05-02
#  128300   2000-05-04
#  128302   2000-04-21
#  134951   2000-05-08
#  134952   2000-04-16
#  134955   2000-05-19
#  134956   2000-04-15
#  134957   2000-04-27
#  134963   2000-04-24
#  146754   2000-04-29
#  146757   2000-04-23
#  146758   2000-05-07    19.7989899
#  146759   2000-05-01    0.7071068
#  146760   2000-05-08
#  161318   2000-04-29    0.000000
#  161321   2000-04-12
#  161324   2000-05-05


# join stopover data and UK_travel summary table

UK_travel<-read.csv("~/BTO/cuckoo_tracking/data/complete_cycles_UK_timing.csv", h=T)

dat<-read.csv("~/BTO/cuckoo_tracking/data/stopover_bestofday_complete_cycles.csv", h=T)

UK_travel$UK_entryHACK<-as.Date(paste("2000",
              substr(UK_travel$UK_entry,5,11), sep=""))


library(plyr)
dat<-rename(dat, c("SO_year" = "year"))

d2<-join_all(list(dat, UK_travel), by=c("ptt", "year"))

d2$SO_start <- as.POSIXct(strptime(d2$SO_start, "%Y-%m-%d %H:%M:%S"), "UTC")
d2$SO_end <- as.POSIXct(strptime(d2$SO_end, "%Y-%m-%d %H:%M:%S"), "UTC")
d2$UK_entry <- as.POSIXct(strptime(d2$UK_entry, "%Y-%m-%d %H:%M:%S"), "UTC")

d2$is_spring_mig<-ifelse(d2$SO_start<d2$UK_entry, TRUE, FALSE)

d3<-d2[d2$is_spring_mig==TRUE,]

# write out

write.csv(d3, "~/BTO/cuckoo_tracking/data/stopover_bestofday_spring_mig.csv", quote=F, row.names=F)

# A couple of steps needed for visualisation

# remove 2 NA rows from the join?

d3<-na.omit(d3)

# sort to regions
table(d3$country)
library(maps)
plot(SO_median_lat~SO_median_long, d3)
map("world", add=T,  col=3)

d3$region<-"N_Africa_Europe"
d3[d3$country=="United Kingdom",]$region<-"UK"
d3[d3$SO_median_lat<20,]$region<-"tropical_Africa"

d3$SO_endHACK<-as.Date(paste("2000",
               substr(d3$SO_end,5,11), sep=""))

d3$SO_startHACK<-as.Date(paste("2000",
                             substr(d3$SO_start,5,11), sep=""))

library(ggplot2)

p<-ggplot(data=d3[d3$deployment_entry!=TRUE & d3$stage=="migration" ,],
          aes(x=SO_endHACK, y=UK_entryHACK,
              colour=factor(ptt), shape=region))
p1<-p+geom_point()+geom_line()+facet_wrap(~year)



# so do birds go further west within west africa in relation to date

p<-ggplot(data=d3[d3$region=="tropical_Africa",],
          aes(x=SO_endHACK, y=SO_median_long, col=factor(ptt)))
p1<-p+geom_point(aes(size=SO_days))+geom_line()+facet_wrap(~year)

jpeg("~/BTO/cuckoo_tracking/outputs/Africa_westward_spring_year.jpg",
     width = 12, height =6 , units ="in", res =300)
p1
dev.off()


p<-ggplot(data=d3[d3$region=="tropical_Africa",],
          aes(x=SO_endHACK, y=SO_median_long))
p1<-p+geom_point()+geom_line(aes(group=paste(ptt, year)))+
  geom_smooth(method="lm", col=2)

jpeg("~/BTO/cuckoo_tracking/outputs/Africa_westward_spring.jpg",
     width = 6, height =6 , units ="in", res =300)
p1
dev.off()

# OK now just want last stopover in west africa before bird flew

by_obj<-by(d3[d3$region=="tropical_Africa",], 
   list(d3[d3$region=="tropical_Africa",]$ptt, 
        d3[d3$region=="tropical_Africa",]$year),
   FUN=function(x){x[which(x$SO_end==max(x$SO_end)),]})

d4<-do.call(rbind, by_obj) # whay!
# alternate apprach using ddply from plyr? could get to work

p<-ggplot(data=d4,aes(x=SO_median_long, y=SO_startHACK))
          
p1<-p+geom_point(aes(colour=factor(year)))+scale_x_date(date_breaks = "1 week", 
                           date_minor_breaks = "1 day",
                           date_labels = "%b %d")+
  scale_y_date(date_breaks = "1 week", 
               date_minor_breaks = "1 day",
               date_labels = "%b %d")+geom_smooth(method="lm")

jpeg("~/BTO/cuckoo_tracking/outputs/Africa_UK_spring_timing.jpg",
     width = 10, height =6 , units ="in", res =300)
p1
dev.off()


p1<-p+geom_point(aes(colour=SO_median_long))+scale_x_date(date_breaks = "1 week", 
         date_minor_breaks = "1 day",
         date_labels = "%b %d")+
  scale_y_date(date_breaks = "1 week", 
               date_minor_breaks = "1 day",
               date_labels = "%b %d")+geom_smooth(method="lm", col=2)+
scale_colour_gradientn(colours=terrain.colors(15))

jpeg("~/BTO/cuckoo_tracking/outputs/Africa_UK_spring_timing_longitude.jpg",
     width = 10, height =6 , units ="in", res =300)
p1
dev.off()

p1<-p+geom_point(aes(colour=SO_median_long))+scale_x_date(date_breaks = "1 week", 
               date_minor_breaks = "1 day",
               date_labels = "%b %d")+
  scale_y_date(date_breaks = "1 week", 
               date_minor_breaks = "1 day",
               date_labels = "%b %d")+
  geom_smooth(method="lm", col=2)+facet_wrap(~year)+
  scale_colour_gradientn(colours=terrain.colors(15))

jpeg("~/BTO/cuckoo_tracking/outputs/Africa_UK_spring_timing_long_year.jpg",
     width = 14, height =6 , units ="in", res =300)
p1
dev.off()


p1<-p+geom_point()+geom_line()+facet_wrap(~year)





p1<-p+geom_histogram(binwidth=1, col='black' )+facet_wrap(~ptt)+
  scale_x_date(limits=d_limit, 
               date_breaks = "1 week", 
               date_minor_breaks = "1 day",
               date_labels = "%b %d")

jpeg("~/BTO/cuckoo_tracking/outputs/UK_return_birds.jpg",
     width = 15, height =6 , units ="in", res =300)
p1
dev.off()



library(lubridate)
dat$start_yday<-yday(dat$SO_start)

p<-ggplot(data=dat[dat$SO_days<100 & dat$SO_month %in% c(1,2,3,4,11,12),], aes(x=factor(SO_month), y=SO_days))

p+geom_jitter(aes(colour=biome1))

p+geom_boxplot(aes(colour=biome1))

p+geom_violin(aes(colour=biome1))

p+geom_boxplot(aes(colour=country))

# visualise using day of year

dat$dofy_hack<-ifelse(dat$start_yday<200, 
                      dat$start_yday,
                      dat$start_yday-365)

p<-ggplot(data=dat[dat$SO_days<100 & dat$SO_month %in% c(1,2,3,4,11,12),],
          aes(x=dofy_hack, y=SO_days))

p+geom_point(aes(colour=biome1))+scale_x_continuous()


p+geom_line(aes(group=ptt))+geom_point(aes(colour=biome1))

p+geom_line(aes(group=ptt))+geom_point(aes(colour=biome1))+
  facet_wrap(~ptt)

