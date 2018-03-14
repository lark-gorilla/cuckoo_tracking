### 13/03/18

### Outputs for Chris H 

### 1) Create summary master table for hypotheses testing

# First we need to use the non-best of day dataset and re-define 
# what we are calling a stopovr. previously we used => 3 days, as per
# the nature paper so need something smaller


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


