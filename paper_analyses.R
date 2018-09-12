## 10/09/18
## Re-analyses of cuckoo data for paper as per meeting with James

## Job 1 
## Re-classify departure times from various regions based on bestofday data
## rather than using stopovers to define times

library(rgdal)
library(sp)
library(sf)
library(dplyr)


dmod<-read.csv('C:/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead_attrib_modelready.csv', h=T)

bodpoints<-read.csv('C:/cuckoo_tracking/data/processed_movebank_cuckoos_hybrid_filter_bestofday_2018_clean_stopovers_recalc.csv', h=T)

bodpoints$pttyear<-paste(bodpoints$ptt, bodpoints$year.x)
dmod$pttyear<-paste(dmod$ptt, dmod$year)

#filter to just birds in dmod and spring migration

bodpoints<-bodpoints %>% filter(pttyear %in% unique(dmod$pttyear) & julian<145)


wrld<-st_read('C:/cuckoo_tracking/sourced_data/country_borders/TM_WORLD_BORDERS-0.3.shp')

bodpoints_sp0<-st_as_sf(bodpoints, coords = c("long","lat"),
                 crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

bodpoints_sp0<-select(bodpoints_sp0, ptt, year.x, julian )

wrld$NAME=as.character(wrld$NAME)

bodpoints_sp<-st_join(bodpoints_sp0, wrld[c('NAME', 'REGION', 'SUBREGION')])

pts<-st_intersects(bodpoints_sp0, wrld)

pts_logical = lengths(pts) == 0

sea_pts<-bodpoints_sp0[pts_logical,]

sea_pts$NAME='SEA'
sea_pts$REGION=999
sea_pts$SUBREGION=999

bodpoints_sp<-rbind(bodpoints_sp, sea_pts)

# ok looks good
plot(bodpoints_sp['REGION'])

bodpoints_sp_ext<-bodpoints_sp

bodpoints_sp_ext$geometry<-NULL

## subset and join for North Africa

dp1<-bodpoints_sp_ext %>% as_tibble() %>% filter(REGION!=2) %>%
  group_by(ptt, year.x) %>% summarise_all(first)

dp1<-rename(dp1, DEPnorthAF2=julian, DEPnorthAF2county=NAME)

#drop env data
dmod<-dmod[,c(1:10,20,28)]

dmod<-left_join(dmod, dp1[,1:4], by=c('ptt', 'year'='year.x'))

## subset and join for depart Central Africa (basically arrival in West Africa)

dp1<-bodpoints_sp_ext %>% as_tibble() %>%
  filter(NAME %in% c("Togo","Cote d'Ivoire","Ghana",
                     "Nigeria","Burkina Faso", "Guinea",
                     "Sierra Leone","Benin", "Liberia", "Senegal")) %>%
  group_by(ptt, year.x) %>%
  summarise_all(first)

dp1<-rename(dp1, DEPcentralAF2=julian, DEPcentralAF2county=NAME)

dmod<-left_join(dmod, dp1[,1:4], by=c('ptt', 'year'='year.x'))

## subset and join for depart Central Africa

dp1<-bodpoints_sp_ext %>% as_tibble() %>%
  filter(NAME %in% c("Congo","Cameroon",
                     "Central African Republic",
                     "Democratic Republic of the Congo",
                     "Angola", "Gabon", "Equatorial Guinea")) %>%
  group_by(ptt, year.x) %>%
  summarise_all(last)

dp1<-rename(dp1, DEPcentralAF3=julian, DEPcentralAF3county=NAME)

dmod<-left_join(dmod, dp1[,1:4], by=c('ptt', 'year'='year.x'))

## Now correct departure from wintering ground

d2<-read.csv("C:/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_ALL_mig.csv", h=T)


dat2<-read.csv("C:/cuckoo_tracking/data/stopover_table_bestofday_2018_1daymin_recalc_biomes.csv", h=T)

UK_travel<-read.csv("C:/cuckoo_tracking/data/complete_cycles_UK_all_timing.csv", h=T)

# manually add 4 columns for 2018 spring migration birds. As only best of day 
# data downloaded for 2018 entry datestime is estimated from movebank
# We drop Peckham as he hasnt turned up in 2018
# rows 1-4 are: PJ, Selborne, Victor, Larry

arr_2018<-data.frame(ptt=c(161318, 161321, 161324, 146759), 
                     year=2018, 
                     UK_entry=c('2018-04-17 08:19:09',
                                '2018-04-14 06:15:32',
                                '2018-04-20 11:06:42',
                                '2018-05-08 05:25:51'),
                     UK_exit= NA,
                     deployment_entry=FALSE,
                     unreliable_exit=TRUE,
                     breeding_entry=c('2018-04-17 08:19:09',
                                      '2018-04-14 06:15:32',
                                      '2018-04-20 11:06:42',
                                      '2018-05-08 05:25:51'),
                     breeding_exit= NA, 
                     UK_entryHACK=c('2000-04-17',
                                    '2000-04-14',
                                    '2000-04-20',
                                    '2000-05-08'),
                     breeding_entryHACK=c('2000-04-17',
                                          '2000-04-14',
                                          '2000-04-20',
                                          '2000-05-08'),
                     UK_br_diff_days=0)


# add in the 2018 data

UK_travel<-rbind(UK_travel, arr_2018)


#UK_travel<-UK_travel[UK_travel$deployment_entry==FALSE,]

# Add columns to subset for birds that do a complete cycle

#re-define year so that it is the year that the stopover starts in
dat2$SO_year=as.numeric(substr(dat2$SO_start, 1,4))

dat2<-rename(dat2, year=SO_year)

d2<-left_join(dat2, UK_travel, by=c("ptt", "year"))

# this line subsets birds and years that I want to exclude from the 
# the analyses i.e they do not complete a full cycle
d2<-d2[-which(is.na(d2$deployment_entry)),]

d2$SO_start <- as.POSIXct(strptime(d2$SO_start, "%Y-%m-%d %H:%M:%S"), "UTC")
d2$SO_end <- as.POSIXct(strptime(d2$SO_end, "%Y-%m-%d %H:%M:%S"), "UTC")
d2$UK_entry <- as.POSIXct(strptime(d2$UK_entry, "%Y-%m-%d %H:%M:%S"), "UTC")

d2$SO_month<-NULL



d2$is_spring_mig<-ifelse(d2$SO_start<d2$UK_entry, TRUE, FALSE)

# add the migration_cohort column

d2$mig_cohort<-ifelse(d2$is_spring_mig==FALSE, d2$year, d2$year-1)
#
d2$mig_cohort2<-ifelse(d2$is_spring_mig==TRUE &
                         d2$deployment_entry==TRUE,
                       d2$mig_cohort+1, d2$mig_cohort)
d2$mig_cohort<-NULL
d2<-rename(d2, mig_cohort=mig_cohort2)

# and for dead cucks

dead_cucks=data.frame(ptt=c(62518,62520,62602,11597,11599,128296,128303,161323,134958,128300),
                      year_died=c(2012,2012,2012,2013,2014,2014,2014,2017,2015,2015), 
                      done_dead='yarp')

d3<-left_join(dat2, dead_cucks, by="ptt")

d3<-d3[-which(is.na(d3$done_dead)),]

# all birds except livingstone were tagged the summer before they died
d3<-d3[-which(d3$ptt==128300 & d3$year==2013),]

# edit year died to reflect mig cohort

d3$year_died=d3$year_died-1

d3<-rename(d3,  "mig_cohort"='year_died')

d3$SO_month<-NULL

# join together

d2$dead<-0
d3$dead<-1

dater<-rbind(d2[,c(1:11,22)], d3[,1:12])

library(lubridate)

dater$SO_startDOY<-yday(
  dater$SO_start) 

dater$SO_endDOY<-yday(
  dater$SO_end) 

#write out
#write.csv(dater, "C:/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_ALL_mig_liveNdead.csv", quote=F, row.names=F)


# group by mig-cohort rather than year but 
# can still join back to dmod with year

min_lat<-dater %>% group_by(ptt, mig_cohort) %>%
     arrange(SO_median_lat, .by_group=TRUE) %>%
     summarise_all(first)

mid_wint<-dater %>% group_by(ptt, mig_cohort) %>%
  filter(357-SO_endDOY == min(357-SO_endDOY))

# correct dmod year to match cohort e.g. all dmod refers to
# spring > 1 jan so year is actually +1 above the cohort
dmod$year2=dmod$year-1

dmod<-left_join(dmod, min_lat[,c(1,2,14)],
          by=c('ptt', 'year2'='mig_cohort'))

dmod<-left_join(dmod, mid_wint[,c(1,12,14)],
                by=c('ptt', 'year2'='mig_cohort'))

dmod<-rename(dmod, DEPminlat=SO_endDOY.x, depmidwint=SO_endDOY.y)

dmod$depart_winterSO2<-ifelse(dmod$depmidwint-dmod$DEPminlat>100,
                              dmod$DEPminlat,dmod$depmidwint)

dmod$depart_winterSO2<-ifelse(dmod$depart_winterSO2>100,
                              dmod$depart_winterSO2-365,
                              dmod$depart_winterSO2)

# Clean up depNorthAf2 for birds that first point after 
# North Af is actually back in UK

dmod[which(dmod$DEPnorthAF2county=='United Kingdom'),]$DEPnorthAF2<-NA

#tidy dataset removing test columns and updating originals



dmod$breeding_hab<-'Upland'
dmod[dmod$breeding_loc %in%
       c('Ashdown Forest', 'New Forest', 'Norfolk Broads',
         'Sherwood Forest', 'Thetford Forest'),]$breeding_hab<-'Lowland'

dmod$depart_winterSO<-dmod$depart_winterSO2
dmod$DEPnorthAF<-dmod$DEPnorthAF2
dmod$ARRwestAF<-dmod$DEPcentralAF2
dmod$DEPcentralAF<-dmod$DEPcentralAF3

dmod<-dmod[,c(1:5, 24, 6:11, 23)]

# add mig route back in

temp<-read.csv('C:/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead_attrib_modelready.csv', h=T)

# fix to remove duplicate Livingstone in new dataset

dmod<-dmod[c(1:23, 25:nrow(dmod)),]

# attrib migration
dmod$autumn_mig<-temp$autumn_mig



#write out

write.csv(dmod, "C:/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead_final.csv", quote=F, row.names=F)



##############################
## Job 2 run regressions and 
## compare residuals to explain how different legs of the
## journey incur variance in arrival time

# note need to check NA values and if they are comparable?
# do I need to run analyses on na.omit dataset?

library(lmerTest) # internally calls lme4
library(ggplot2)

dmod<-read.csv("C:/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead_final.csv", h=T)


m1<-lmer(arrive_breeding~arrive_uk+(1|breeding_hab/ptt)+(1|year), data=dmod)
m2<-lmer(arrive_breeding~DEPnorthAF+(1|breeding_hab/ptt)+(1|year), data=dmod)
m3<-lmer(arrive_breeding~DEPwestAF+(1|breeding_hab/ptt)+(1|year), data=dmod)
m4<-lmer(arrive_breeding~ARRwestAF+(1|breeding_hab/ptt)+(1|year), data=dmod)
m5<-lmer(arrive_breeding~DEPcentralAF+(1|breeding_hab/ptt)+(1|year), data=dmod)
m6<-lmer(arrive_breeding~depart_winterSO+(1|breeding_hab/ptt)+(1|year), data=dmod)

anova(m1)


