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

d2$SO_month<-NULL

library(lubridate)

d2$SO_startDOY<-yday(
  d2$SO_start) 

d2$SO_endDOY<-yday(
  d2$SO_end) 

d2$is_spring_mig<-ifelse(d2$SO_start<d2$UK_entry, TRUE, FALSE)

# add the migration_cohort column

d2$mig_cohort<-ifelse(d2$is_spring_mig==FALSE, d2$year, d2$year-1)
#
d2$mig_cohort2<-ifelse(d2$is_spring_mig==TRUE &
                         d2$deployment_entry==TRUE,
                       d2$mig_cohort+1, d2$mig_cohort)
d2$mig_cohort<-NULL
d2<-rename(d2, c("mig_cohort2"="mig_cohort"))

# two importatnt subsets to run here that basically do
# the same thing but catch each other's stragglers

# summary table of min lat SO dates per year thehn join back
# winertierng SO timing end

d3<-d2[d2$deployment_entry==FALSE,]
# removes stopovers that are joined to UK deployment
# years i.e. deployment to Autumn migration e.g. 2011

d4<-d3[d3$is_spring_mig==TRUE,]
# only keeps stopovers that are between o1 Jan and UK
# entry i.e. spring migration. Because we have joined on ptt
# and year we basically have the calender year
# as a cutoff point.

# write out data
write.csv(d4, "~/BTO/cuckoo_tracking/data/stopover_1daymin_spring_mig.csv", quote=F, row.names=F)

# Now join in extra metadata to create master file
# tidy to remove uneeded columns

library(lubridate)

d4$breeding_entryDOY<-yday(
  d4$breeding_entry) 

d4$SO_startDOY<-yday(
  d4$SO_start) 

d4$SO_endDOY<-yday(
  d4$SO_end) 


# sort to regions
table(d4$country)
library(maps)
plot(SO_median_lat~SO_median_long, d4)
map("world", add=T,  col=3)

d4$region<-"N_Africa_Europe"
d4[d4$country=="United Kingdom",]$region<-"UK"
d4[d4$SO_median_lat<20,]$region<-"tropical_Africa"




# OK now just want last stopover in west africa before bird flew

by_obj<-by(d3[d3$region=="tropical_Africa",], 
           list(d3[d3$region=="tropical_Africa",]$ptt, 
                d3[d3$region=="tropical_Africa",]$year),
           FUN=function(x){x[which(x$SO_endDOY==max(x$SO_endDOY)),]})

d4<-do.call(rbind, by_obj) # whay!
# alternate apprach using ddply from plyr? could get to work


