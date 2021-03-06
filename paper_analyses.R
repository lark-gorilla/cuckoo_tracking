## 10/09/18
## Re-analyses of cuckoo data for paper as per meeting with James

## Job 1 
## Re-classify departure times from various regions based on bestofday data
## rather than using stopovers to define times

library(rgdal)
library(sp)
library(sf)
library(dplyr)


dmod<-read.csv('~/BTO/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead_attrib_modelready.csv', h=T)

bodpoints<-read.csv('~/BTO/cuckoo_tracking/data/processed_movebank_cuckoos_hybrid_filter_bestofday_2018_clean_stopovers_recalc.csv', h=T)

bodpoints$pttyear<-paste(bodpoints$ptt, bodpoints$year.x)
dmod$pttyear<-paste(dmod$ptt, dmod$year)

#filter to just birds in dmod and spring migration

bodpoints<-bodpoints %>% filter(pttyear %in% unique(dmod$pttyear) & julian<145)


wrld<-st_read('~/BTO/cuckoo_tracking/sourced_data/country_borders/TM_WORLD_BORDERS-0.3.shp')

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

d2<-read.csv("~/BTO/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_ALL_mig.csv", h=T)


dat2<-read.csv("~/BTO/cuckoo_tracking/data/stopover_table_bestofday_2018_1daymin_recalc_biomes.csv", h=T)

UK_travel<-read.csv("~/BTO/cuckoo_tracking/data/complete_cycles_UK_all_timing.csv", h=T)

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
#write.csv(dater, "~/BTO/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_ALL_mig_liveNdead.csv", quote=F, row.names=F)


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

# after reviewing the above extraction with best of day points we actually
# revert to using the original data for departure from central Africa
# for most points but some are still leaving erroneously early due to
# recalc parameters attributing next point's time to end of stopver but
# next point still being well within Central Africa. As a comprimise we
# take the greatest value of the original method and new method which
# sorts early and late errors.

dmod$DEPcentralAF<-ifelse(dmod$DEPcentralAF3>dmod$DEPcentralAF,dmod$DEPcentralAF3, dmod$DEPcentralAF)

# and edit to one bird
dmod[28,]$DEPcentralAF<-88


dmod<-dmod[,c(1:5, 24, 6:11, 23)]

# add mig route back in

temp<-read.csv('~/BTO/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead_attrib_modelready.csv', h=T)

# fix to remove duplicate Livingstone in new dataset

dmod<-dmod[c(1:23, 25:nrow(dmod)),]

# attrib migration
dmod$autumn_mig<-temp$autumn_mig

# rm old DEPeurope that slipped thru

dmod$DEPeurope<-NULL

#write out

write.csv(dmod, "~/BTO/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead_final.csv", quote=F, row.names=F)

# use previous modelready dataset to attrib env data to new dmod

dmodA<-cbind(dmod, temp[,c(22:24,31:126)])

write.csv(dmodA, "~/BTO/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead_attrib_modelready_final.csv", quote=F, row.names=F)

### New request (19/10/18): add migration milestones back through Autumn migration
### last day on breeding grounds
### last stopover prior to Sahara crossing
### Departure from the Sahel

dmod<-read.csv('~/BTO/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead_final.csv', h=T)

bodpoints<-read.csv('~/BTO/cuckoo_tracking/data/processed_movebank_cuckoos_hybrid_filter_bestofday_2018_clean_stopovers_recalc.csv', h=T)

bodpoints$pttyear<-paste(bodpoints$ptt, bodpoints$year.x)
dmod$pttyear<-paste(dmod$ptt, (dmod$year-1)) # drop year by 1 to capture autumn mig

#filter to just birds in dmod (year-1) and autumn migration

bodpoints<-bodpoints %>% filter(pttyear %in% unique(dmod$pttyear) & julian>145)


wrld<-st_read('~/BTO/cuckoo_tracking/sourced_data/country_borders/TM_WORLD_BORDERS-0.3.shp')

bodpoints_sp0<-st_as_sf(bodpoints, coords = c("long","lat"),
                        crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

bodpoints_sp0$lat=bodpoints$lat
bodpoints_sp0$long=bodpoints$long
bodpoints_sp0$mtype=bodpoints$mtype

bodpoints_sp0<-select(bodpoints_sp0, ptt, year.x, julian, lat, long, mtype)

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

dp1<-bodpoints_sp_ext %>% as_tibble() %>% filter(NAME=='United Kingdom') %>%
  group_by(ptt, year.x) %>% summarise_all(last)

dp1<-rename(dp1, DEPuk=julian)

#drop env data
dmod$year_1<-dmod$year-1

dmod<-left_join(dmod, dp1[,1:3], by=c('ptt', 'year_1'='year.x'))

## subset and join for depart Europe/Morocco, basically last stopover before sahara crossing

dp1<-bodpoints_sp_ext %>% as_tibble() %>%
  filter(NAME %in% c("United Kingdom","Belgium","Germany",
                     "Italy","France", "Spain",
                     "Netherlands","Luxembourg", "Morocco", "Czech Republic", 
                     "Austria", "Slovakia" , "Montenegro" ,   "Albania"  , 
                     "Bosnia and Herzegovina", "Greece", "Croatia", "Switzerland", 
                     "Hungary", "Poland") & mtype == 'S') %>%
  group_by(ptt, year.x) %>%
  summarise_all(last)

dp1<-rename(dp1, ARRsahara=julian, ARRsaharacountry=NAME)

dmod<-left_join(dmod, dp1[,1:3], by=c('ptt', 'year_1'='year.x'))


## Now for departure from the Sahel

dp1<-bodpoints_sp_ext %>% as_tibble() %>%
  filter(lat>9 & mtype == 'S' & NAME!='SEA') %>%
  group_by(ptt, year.x) %>%
  summarise_all(last)

dp1<-rename(dp1, DEPsahel=julian, DEPsahelcountry=NAME)

dmod<-left_join(dmod, dp1[,c(1:3,7)], by=c('ptt', 'year_1'='year.x'))

dmod1<-select(dmod, ptt, year, dead, breeding_loc, breeding_hab, autumn_mig, DEPuk,
              ARRsahara, DEPsahel, depart_winterSO, DEPcentralAF, DEPwestAF, DEPnorthAF, 
              arrive_uk, arrive_breeding)


write.csv(dmod1, '~/BTO/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_ANNUAL_mig_summary_dead_final.csv', quote=F, row.names=F)

# now calc durations in each region

dmod2=dmod1

dmod2[,10:15]<-dmod2[,10:15]+365

dmod2[,8:15]<-apply(dmod2[,7:15],1, diff)


write.csv(dmod2, '~/BTO/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_ANNUAL_DIFF_mig_summary_dead_final.csv', quote=F, row.names=F)


##############################
## Job 2 run regressions and 
## compare residuals to explain how different legs of the
## journey incur variance in arrival time

# note need to check NA values and if they are comparable?
# do I need to run analyses on na.omit dataset?

library(lmerTest) # internally calls lme4
library(ggplot2)

dmod<-read.csv("~/BTO/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead_final.csv", h=T)

m1<-lmer(arrive_breeding~arrive_uk+(1|breeding_hab/ptt)+(1|year), data=dmod)
m2<-lmer(arrive_breeding~DEPnorthAF+(1|breeding_hab/ptt)+(1|year), data=dmod)
m3<-lmer(arrive_breeding~DEPwestAF+(1|breeding_hab/ptt)+(1|year), data=dmod)

m5<-lmer(arrive_breeding~DEPcentralAF+(1|breeding_hab/ptt)+(1|year), data=dmod)
m6<-lmer(arrive_breeding~depart_winterSO+(1|breeding_hab/ptt)+(1|year), data=dmod)

anova(m1);anova(m2);anova(m3);anova(m4);anova(m5);anova(m6)

dfresid<-rbind(data.frame(residuals=resid(m1, type='pearson'),
                    mig_region='UK arrive'), 
               data.frame(residuals=resid(m2, type='pearson'),
                          mig_region='Europe arrive'), 
               data.frame(residuals=resid(m3, type='pearson'),
                          mig_region='West Africa depart'), 
               data.frame(residuals=resid(m5, type='pearson'),
                          mig_region='Central Africa depart'), 
               data.frame(residuals=resid(m6, type='pearson'),
                          mig_region='Wintering depart'))
               

ggplot(data=dfresid, aes(x=mig_region, y=residuals))+geom_point(shape=1)+
  geom_hline(yintercept=0, colour='red', linetype=2)+
  labs(x='Migration milestone', y='Residuals')+
  scale_y_continuous(breaks=c(-15, -10, -5, 0, 5, 10, 15, 20, 25))

# to compare residuals between regions and attribute variance to region
# I can only use birds that have no NA values throughout the whole migration

dmod_nona<-na.omit(dmod)

m1n<-lmer(arrive_breeding~arrive_uk+(1|breeding_hab/ptt)+(1|year), data=dmod_nona)
m2n<-lmer(arrive_breeding~DEPnorthAF+(1|breeding_hab/ptt)+(1|year), data=dmod_nona)
m3n<-lmer(arrive_breeding~DEPwestAF+(1|breeding_hab/ptt)+(1|year), data=dmod_nona)

m5n<-lmer(arrive_breeding~DEPcentralAF+(1|breeding_hab/ptt)+(1|year), data=dmod_nona)
m6n<-lmer(arrive_breeding~depart_winterSO+(1|breeding_hab/ptt)+(1|year), data=dmod_nona)

#make predictions and plots

newdat<-expand.grid(arrive_uk=min(dmod_nona$arrive_uk):max(dmod_nona$arrive_uk),
                   arrive_breeding=1, 
                   year=2012:2018,
                   ptt=unique(dmod_nona$ptt), 
                   breeding_hab=c('Lowland', 'Upland'))

newdat$p1<-predict(m1, newdata=newdat, re.form=~(1|breeding_hab))
predmat<-model.matrix(arrive_breeding~arrive_uk, data=newdat)
vcv<-vcov(m1)
semod<-sqrt(diag(predmat%*%vcv%*%t(predmat)))
newdat$lc<-newdat$p1-semod*1.96
newdat$uc<-newdat$p1+semod*1.96
newdat$mod='UK_arrive'
m1dat<-newdat

newdat<-expand.grid(DEPnorthAF=min(dmod_nona$DEPnorthAF):max(dmod_nona$DEPnorthAF),
                    arrive_breeding=1, 
                    year=2012:2018,
                    ptt=unique(dmod_nona$ptt), 
                    breeding_hab=c('Lowland', 'Upland'))
newdat$p1<-predict(m2, newdata=newdat, re.form=~(1|breeding_hab))
predmat<-model.matrix(arrive_breeding~DEPnorthAF, data=newdat)
vcv<-vcov(m2)
semod<-sqrt(diag(predmat%*%vcv%*%t(predmat)))
newdat$lc<-newdat$p1-semod*1.96
newdat$uc<-newdat$p1+semod*1.96
newdat$mod='Europe_arrive'
m2dat<-newdat

newdat<-expand.grid(DEPwestAF=min(dmod_nona$DEPwestAF):max(dmod_nona$DEPwestAF),
                    arrive_breeding=1, 
                    year=2012:2018,
                    ptt=unique(dmod_nona$ptt), 
                    breeding_hab=c('Lowland', 'Upland'))
newdat$p1<-predict(m3, newdata=newdat, re.form=~(1|breeding_hab))
predmat<-model.matrix(arrive_breeding~DEPwestAF, data=newdat)
vcv<-vcov(m3)
semod<-sqrt(diag(predmat%*%vcv%*%t(predmat)))
newdat$lc<-newdat$p1-semod*1.96
newdat$uc<-newdat$p1+semod*1.96
newdat$mod='WestAfrica_depart'
m3dat<-newdat

newdat<-expand.grid(DEPcentralAF=min(dmod_nona$DEPcentralAF):max(dmod_nona$DEPcentralAF),
                    arrive_breeding=1, 
                    year=2012:2018,
                    ptt=unique(dmod_nona$ptt), 
                    breeding_hab=c('Lowland', 'Upland'))
newdat$p1<-predict(m5, newdata=newdat, re.form=~(1|breeding_hab))
predmat<-model.matrix(arrive_breeding~DEPcentralAF, data=newdat)
vcv<-vcov(m5)
semod<-sqrt(diag(predmat%*%vcv%*%t(predmat)))
newdat$lc<-newdat$p1-semod*1.96
newdat$uc<-newdat$p1+semod*1.96
newdat$mod='CentralAfrica_depart'
m5dat<-newdat

newdat<-expand.grid(depart_winterSO=min(dmod_nona$depart_winterSO):max(dmod_nona$depart_winterSO),
                    arrive_breeding=1, 
                    year=2012:2018,
                    ptt=unique(dmod_nona$ptt), 
                    breeding_hab=c('Lowland', 'Upland'))
newdat$p1<-predict(m6, newdata=newdat, re.form=~(1|breeding_hab))
predmat<-model.matrix(arrive_breeding~depart_winterSO, data=newdat)
vcv<-vcov(m6)
#semod<-sqrt(diag(predmat%*%vcv%*%t(predmat)))

# edit to remove CIs !!!!
newdat$lc<-newdat$p1
newdat$uc<-newdat$p1
newdat$mod='Winter_depart'
m6dat<-newdat

library(reshape2)
pt_dat<-melt(dmod_nona[,c(1,2,4,5,7:10)], id.vars=c('arrive_breeding', 'ptt', 'year'))

names(m1dat)[1]<-'doy'
names(m2dat)[1]<-'doy'
names(m3dat)[1]<-'doy'

names(m5dat)[1]<-'doy'
names(m6dat)[1]<-'doy'

pred_dat<-rbind(m1dat, m2dat, m3dat, m5dat, m6dat)

pred_dat$mod<-factor(pred_dat$mod, levels=c("Winter_depart", "CentralAfrica_depart",
                                            "WestAfrica_depart",  "Europe_arrive", "UK_arrive" ))  
names(pt_dat)[4]<-'mod'
pt_dat$mod<-as.character(pt_dat$mod)

pt_dat[pt_dat$mod=='depart_winterSO',]$mod<-'Winter_depart'
pt_dat[pt_dat$mod=='DEPcentralAF',]$mod<-'CentralAfrica_depart'

pt_dat[pt_dat$mod=='DEPwestAF',]$mod<-'WestAfrica_depart'
pt_dat[pt_dat$mod=='DEPnorthAF',]$mod<-'Europe_arrive'
pt_dat[pt_dat$mod=='arrive_uk',]$mod<-'UK_arrive'

pt_dat$mod<-factor(pt_dat$mod, levels=c("Winter_depart", "CentralAfrica_depart", 
                                            "WestAfrica_depart",  "Europe_arrive", "UK_arrive" ))  




ggplot()+geom_point(data=pt_dat, aes(x=value, y=arrive_breeding, colour=paste(ptt, year)))+
  geom_line(data=pred_dat, aes(x=doy, y=p1 ))+
  geom_line(data=pred_dat, aes(x=doy, y=lc ), linetype='dashed')+
  geom_line(data=pred_dat, aes(x=doy, y=uc), linetype='dashed')+
  labs(x="Day of Year", y="Arrival at breeding grounds (DOY)", colour='Region')+
  theme_bw()+facet_wrap(~mod, scales='free')


ggplot()+geom_point(data=pt_dat, aes(x=value, y=arrive_breeding))+
  geom_line(data=pred_dat, aes(x=doy, y=p1, colour=breeding_hab))+
  geom_line(data=pred_dat, aes(x=doy, y=lc, colour=breeding_hab), linetype='dashed')+
  geom_line(data=pred_dat, aes(x=doy, y=uc, colour=breeding_hab), linetype='dashed')+
  labs(x="Day of Year", y="Arrival at breeding grounds (DOY)")+
  theme_bw()+facet_wrap(~mod, scales='free')

library(MuMIn)

r.squaredGLMM(m1)
r.squaredGLMM(m2)
r.squaredGLMM(m3)
r.squaredGLMM(m5)
r.squaredGLMM(m6)

#just little idea to see if explanatory power is a simple function of time
# or actually per region

lin_r2<-data.frame(day=1:120, simr2=seq(0,1, length.out=120))

dpred<-rbind(data.frame(t(data.frame(r.squaredGLMM(m1)))),
             data.frame(t(data.frame(r.squaredGLMM(m2)))),
             data.frame(t(data.frame(r.squaredGLMM(m3)))),
             data.frame(t(data.frame(r.squaredGLMM(m5)))),
             data.frame(t(data.frame(r.squaredGLMM(m6)))))
dpred$mod<-c('Arrival at UK border',
             'Arrival in Europe',
             'Depart West Africa',
             'Depart Central Africa',
             'Depart wintering grounds')

dpred$day=c(median(datfinal$arrive_uk, na.rm=T),
            median(datfinal$DEPnorthAF, na.rm=T),
            median(datfinal$DEPwestAF, na.rm=T),
            median(datfinal$DEPcentralAF, na.rm=T),
            median(datfinal$depart_winterSO, na.rm=T))
            

qplot(data=lin_r2, x=day, y=simr2, geom='line')+
  geom_point(data=dpred, aes(x=day, y=R2m), colour='red')+
  geom_text(data=dpred, aes(x=day, y=R2m, label=mod),
            nudge_y=-0.05)

median(dmod$DEPcentralAF, na.rm=T)
lin_r2[70]

median(dmod$DEPwestAF, na.rm=T)
lin_r2[102]

median(dmod$DEPnorthAF, na.rm=T)
lin_r2[107]


## Comparison of residuals to attribute certain regions where variance 
##


dmod_nona$r.arriveUK<-resid(m1, type='pearson')
dmod_nona$r.arriveEU<-resid(m2, type='pearson')
dmod_nona$r.departWA<-resid(m3, type='pearson')
dmod_nona$r.departCA<-resid(m5, type='pearson')
dmod_nona$r.departWI<-resid(m6, type='pearson')

#attrib variance to regions

dmod_nona$var.UK<-dmod_nona$r.arriveUK
dmod_nona$var.europe<-dmod_nona$r.arriveEU-dmod_nona$r.arriveUK
dmod_nona$var.Sahara<-dmod_nona$r.departWA-dmod_nona$r.arriveEU
dmod_nona$var.Wafrica<-dmod_nona$r.departCA-dmod_nona$r.departWA
dmod_nona$var.Cafrica<-dmod_nona$r.departWI-dmod_nona$r.departCA

var_yr<-melt(dmod_nona[,c(2,12,19:23)], id.vars=c('year', 'breeding_hab'))

qplot(data=var_yr, x=variable, y=value, colour=factor(year), geom='boxplot')

qplot(data=var_yr, x=variable, y=value)+facet_wrap(~year)
# but year is in models
# so plot RFEs

### Run investigation of env impacts on W Africa departure date
## as per discussion with James

modenv<-read.csv('~/BTO/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead_attrib_modelready_final.csv', h=T)


m1<-lmer(DEPwestAF~DEPcentralAF+monsTarr_mean+
           (1|breeding_hab/ptt)+(1|year), data=modenv)

plot(m1)
summary(m1)
anova(m1)

qplot(data=modenv, x=monsTarr_mean, y=DEPwestAF)

#better with first rains

m1<-lmer(DEPwestAF~DEPcentralAF+monsTarr_first+
           (1|breeding_hab/ptt)+(1|year), data=modenv)

plot(m1)
summary(m1)
anova(m1)

qplot(data=modenv, x=monsTarr_mean, y=DEPwestAF)


## using dmod_nona data to explain variance in migration speed
## for various migration regions

mm1<-lm(var.Sahara~monsTarr_first, data=dmod_nona)

plot(mm1)
summary(mm1)
anova(mm1)

qplot(data=dmod_nona, y=var.Sahara+var.europe, x=emodisNDVI_mean)


mm1<-lm(var.Sahara+var.europe~emodisNDVI_mean, data=dmod_nona)

summary(mm1)
anova(mm1)
#hmm

### section for chris 09/10/18
# checking difference between r2 for normal models and na.omit models
data.frame(mod=c('Arr UK', 'Arr Europe', 'Dep WA', 'Dep CA', 'Dep winter'),
           m1=c(r.squaredGLMM(m1)[1],r.squaredGLMM(m2)[1],r.squaredGLMM(m3)[1],r.squaredGLMM(m5)[1],r.squaredGLMM(m6)[1]),
           m1NA=c(r.squaredGLMM(m1n)[1],r.squaredGLMM(m2n)[1],r.squaredGLMM(m3n)[1],r.squaredGLMM(m5n)[1],r.squaredGLMM(m6n)[1]),
           c1=c(r.squaredGLMM(m1)[2],r.squaredGLMM(m2)[2],r.squaredGLMM(m3)[2],r.squaredGLMM(m5)[2],r.squaredGLMM(m6)[2]),
           c1NA=c(r.squaredGLMM(m1n)[2],r.squaredGLMM(m2n)[2],r.squaredGLMM(m3n)[2],r.squaredGLMM(m5n)[2],r.squaredGLMM(m6n)[2]))
           

# Checking marginal increase in R2 with adding additional stages sequentially


m1<-lmer(arrive_breeding~depart_winterSO+DEPcentralAF+DEPwestAF+DEPnorthAF+arrive_uk+(1|breeding_hab/ptt)+(1|year), data=dmod)
m2<-lmer(arrive_breeding~depart_winterSO+DEPcentralAF+DEPwestAF+DEPnorthAF+(1|breeding_hab/ptt)+(1|year), data=dmod)
m3<-lmer(arrive_breeding~depart_winterSO+DEPcentralAF+DEPwestAF+(1|breeding_hab/ptt)+(1|year), data=dmod)

m5<-lmer(arrive_breeding~depart_winterSO+DEPcentralAF+(1|breeding_hab/ptt)+(1|year), data=dmod)
m6<-lmer(arrive_breeding~depart_winterSO+(1|breeding_hab/ptt)+(1|year), data=dmod)

r.squaredGLMM(m6);r.squaredGLMM(m5);r.squaredGLMM(m3);r.squaredGLMM(m2);r.squaredGLMM(m1)

# Check effect of different RE specification

r.squaredGLMM(lmer(arrive_breeding~depart_winterSO+(1|breeding_hab/ptt)+(1|year), data=dmod))
r.squaredGLMM(lmer(arrive_breeding~depart_winterSO+(1|breeding_hab)+(1|ptt)+(1|year), data=dmod))
#same
r.squaredGLMM(lmer(arrive_breeding~depart_winterSO+(1|breeding_hab/ptt)+(depart_winterSO|year), data=dmod))
r.squaredGLMM(lmer(arrive_breeding~depart_winterSO+(depart_winterSO|breeding_hab)+(1|ptt)+(depart_winterSO|year), data=dmod))
# var explained by RE shoots up

## Have a look what general picture is

m1<-lmer(arrive_breeding~arrive_uk+(1|breeding_hab)+(1|ptt)+(1|year), data=dmod)
m2<-lmer(arrive_breeding~DEPnorthAF+(1|breeding_hab)+(1|ptt)+(1|year), data=dmod)
m3<-lmer(arrive_breeding~DEPwestAF+(1|breeding_hab)+(1|ptt)+(1|year), data=dmod)
m5<-lmer(arrive_breeding~DEPcentralAF+(1|breeding_hab)+(1|ptt)+(1|year), data=dmod)
m6<-lmer(arrive_breeding~depart_winterSO+(1|breeding_hab)+(1|ptt)+(1|year), data=dmod)

m1a<-lmer(arrive_breeding~arrive_uk+(1|breeding_hab)+(1|ptt)+(arrive_uk|year), data=dmod)
m2a<-lmer(arrive_breeding~DEPnorthAF+(1|breeding_hab)+(1|ptt)+(DEPnorthAF|year), data=dmod)
m3a<-lmer(arrive_breeding~DEPwestAF+(1|breeding_hab)+(1|ptt)+(DEPwestAF|year), data=dmod)
m5a<-lmer(arrive_breeding~DEPcentralAF+(1|breeding_hab)+(1|ptt)+(DEPcentralAF|year), data=dmod)
m6a<-lmer(arrive_breeding~depart_winterSO+(1|breeding_hab)+(1|ptt)+(depart_winterSO|year), data=dmod)

m1b<-lmer(arrive_breeding~arrive_uk+(1|breeding_hab)+(1|ptt)+(1|year), data=dmod)
m2b<-lmer(arrive_breeding~DEPnorthAF+(DEPnorthAF|breeding_hab)+(1|ptt)+(DEPnorthAF|year), data=dmod)
m3b<-lmer(arrive_breeding~DEPwestAF+(DEPwestAF|breeding_hab)+(1|ptt)+(DEPwestAF|year), data=dmod)
m5b<-lmer(arrive_breeding~DEPcentralAF+(DEPcentralAF|breeding_hab)+(1|ptt)+(DEPcentralAF|year), data=dmod)
m6b<-lmer(arrive_breeding~depart_winterSO+(depart_winterSO|breeding_hab)+(1|ptt)+(depart_winterSO|year), data=dmod)


data.frame(mod=c('Arr UK', 'Arr Europe', 'Dep WA', 'Dep CA', 'Dep winter'),
           m1=c(r.squaredGLMM(m1)[1],r.squaredGLMM(m2)[1],r.squaredGLMM(m3)[1],r.squaredGLMM(m5)[1],r.squaredGLMM(m6)[1]),
           c1=c(r.squaredGLMM(m1)[2],r.squaredGLMM(m2)[2],r.squaredGLMM(m3)[2],r.squaredGLMM(m5)[2],r.squaredGLMM(m6)[2]),
           m2=c(r.squaredGLMM(m1a)[1],r.squaredGLMM(m2a)[1],r.squaredGLMM(m3a)[1],r.squaredGLMM(m5a)[1],r.squaredGLMM(m6a)[1]),
           c2=c(r.squaredGLMM(m1a)[2],r.squaredGLMM(m2a)[2],r.squaredGLMM(m3a)[2],r.squaredGLMM(m5a)[2],r.squaredGLMM(m6a)[2]),
           m3=c(r.squaredGLMM(m1b)[1],r.squaredGLMM(m2b)[1],r.squaredGLMM(m3b)[1],r.squaredGLMM(m5b)[1],r.squaredGLMM(m6b)[1]),
           c3=c(r.squaredGLMM(m1b)[2],r.squaredGLMM(m2b)[2],r.squaredGLMM(m3b)[2],r.squaredGLMM(m5b)[2],r.squaredGLMM(m6b)[2]))
           

# Compare effect of REs when considered as fixed effects

r.squaredGLMM(lmer(arrive_breeding~depart_winterSO+(1|breeding_hab)+(1|ptt)+(1|year), data=dmod))
# basically all REs R2c=0.43
summary(lm(arrive_breeding~depart_winterSO+breeding_hab+factor(ptt)+factor(year), data=dmod))
# Adjusted R-squared:  0.5158 
mayte<-lm(arrive_breeding~depart_winterSO+breeding_hab+factor(ptt)+factor(year), data=dmod)
anova(mayte) # looks driven by breeding hab
#library(relaimpo)
#calc.relimp(mayte)

# this should have more var explained by stage

r.squaredGLMM(lmer(arrive_breeding~DEPwestAF+(1|breeding_hab)+(1|ptt)+(1|year), data=dmod))
# R2c= 0.75
summary(lm(arrive_breeding~DEPwestAF+breeding_hab+factor(ptt)+factor(year), data=dmod))
# Adjusted R-squared:  0.65
mayte<-lm(arrive_breeding~DEPwestAF+breeding_hab+factor(ptt)+factor(year), data=dmod)
anova(mayte) 





