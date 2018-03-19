### 13/03/18

### Outputs for Chris H 

### 1) Create summary master table for hypotheses testing

# First we need to use the non-best of day dataset and re-define 
# what we are calling a stopovr. previously we used => 3 days, as per
# the nature paper so need something smaller

# Load non best-of-day data which has stopover defined as >= 1 day


dat2<-read.csv("~/BTO/cuckoo_tracking/data/stopover_table_1daymin_biomes.csv", h=T)

UK_travel<-read.csv("~/BTO/cuckoo_tracking/data/complete_cycles_UK_all_timing.csv", h=T)

#UK_travel<-UK_travel[UK_travel$deployment_entry==FALSE,]

# Add columns to subset for birds that do a complete cycle

library(plyr)
dat2<-rename(dat2, c("SO_year" = "year"))

d2<-join_all(list(dat2, UK_travel), by=c("ptt", "year"))

# this line subsets birds and years that I want to exclude from the 
# the analyses i.e they do not complete a full cycle
d2<-d2[-which(is.na(d2$deployment_entry)),]

d2$SO_start <- as.POSIXct(strptime(d2$SO_start, "%Y-%m-%d %H:%M:%S"), "UTC")
d2$SO_end <- as.POSIXct(strptime(d2$SO_end, "%Y-%m-%d %H:%M:%S"), "UTC")
d2$UK_entry <- as.POSIXct(strptime(d2$UK_entry, "%Y-%m-%d %H:%M:%S"), "UTC")

d2$is_spring_mig<-ifelse(d2$SO_start<d2$UK_entry, TRUE, FALSE)

# two importatnt subsets to run here that basically do
# the same thing but catch each other's stragglers

d3<-d2[d2$deployment_entry==FALSE,]
# removes stopovers that are joined to UK deployment
# years i.e. deployment to Autumn migration e.g. 2011

d4<-d3[d3$is_spring_mig==TRUE,]
# only keeps stopovers that are between o1 Jan and UK
# entry i.e. spring migration. Because we have joined on ptt
# and year we basically have the calender year
# as a cutoff point.

write.csv(d4, "~/BTO/cuckoo_tracking/data/stopover_1daymin_spring_mig.csv", quote=F, row.names=F)



# write out data



#### OLD

d3<-read.csv("~/BTO/cuckoo_tracking/data/stopover_bestofday_spring_mig.csv", h=T)

d3<-na.omit(d3)

library(lubridate)

d3$breeding_entryDOY<-yday(
  d3$breeding_entry) 

d3$SO_startDOY<-yday(
  d3$SO_start) 

d3$SO_endDOY<-yday(
  d3$SO_end) 


# sort to regions
table(d3$country)
library(maps)
plot(SO_median_lat~SO_median_long, d3)
map("world", add=T,  col=3)

d3$region<-"N_Africa_Europe"
d3[d3$country=="United Kingdom",]$region<-"UK"
d3[d3$SO_median_lat<20,]$region<-"tropical_Africa"

# OK now just want last stopover in west africa before bird flew

by_obj<-by(d3[d3$region=="tropical_Africa",], 
           list(d3[d3$region=="tropical_Africa",]$ptt, 
                d3[d3$region=="tropical_Africa",]$year),
           FUN=function(x){x[which(x$SO_endDOY==max(x$SO_endDOY)),]})

d4<-do.call(rbind, by_obj) # whay!
# alternate apprach using ddply from plyr? could get to work


