### 31/07/18

### Area to explore and develop env (NDVI and rainfall) metrics
### to test against various responses relating to movement withing/ from
### West Africa

library(dplyr)
library(ggplot2)

if(Sys.info()['nodename']=="D9L5812"){
  setwd("C:/cuckoo_tracking")}else{
    setwd("N:/cuckoo_tracking")}

#summary data
dat<- read.csv('C:/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead.csv', h=T)

#stopover coord data (median points) - with ndvi and env data (only WA!!)
soversWA<-read.csv('C:/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_BOTH_weastAF_rfndvistart.csv', h=T)

so2<-read.csv('C:/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_detailcoords_landcov_ext_rivers_precip_ndvi.csv', h=T)

#extra data for filling migratory route

extraz<- read.csv('C:/cuckoo_tracking/data/stopover_bestofday_1daymin_recalc_spring_mig_summary_extras.csv', h=T)

#### Data cleaning

# set NA or manually correct cuckoo depature dates that
# are incorrect, from manual examination

dat[dat$ptt==115586 & dat$year==2014,]$DEPwestAF<-106
dat[dat$ptt==134955 & dat$year==2015,]$DEPwestAF<-NA
dat[dat$ptt==134956 & dat$year==2015,]$DEPwestAF<-NA

# and dead ones
dat[dat$ptt==128296 & dat$year==2014,]$DEPwestAF<-NA

## fill NA in mig route
# fills some
dat<-left_join(dat, select(extraz, ptt, year, autumn_mig), by=c('ptt', 'year'))

# fill 2018 birds
dat[c(47, 44, 42),]$autumn_mig.y='SW'
dat[39,]$autumn_mig.y='SE'
# and dead birds
dat[c(2,20,45),]$autumn_mig.y='SW'
dat[c(1,3,23,25,31),]$autumn_mig.y='SE'

names(dat)[names(dat)=='autumn_mig.y']<-'autumn_mig'
dat$autumn_mig.x<-NULL

### Add departure info from WA e.g. country, lat and long, length of final stopover and 2nd before last

dat<-left_join(dat, soversWA %>% group_by(ptt, year) %>%
                 summarise( dep_country=last(country),
                            dep_long=last(SO_median_long),
                            dep_lat=last(SO_median_lat),
                            st_finalSO=last(SO_startDOY),
                            dur_finalSO_n2=(nth(SO_endDOY, -2)-nth(SO_startDOY, -2))),
                            by=c('ptt', 'year'))
# add departure ground info
dat<-left_join(dat, soversWA %>%
                 filter(country %in% c("Ghana","Cote d'Ivoire",
                                       "Guinea", "Sierra Leone",
                                       ' Burkina Faso', 'Nigeria')) %>%
                 group_by(ptt, year) %>%
                 summarise(firstCdepartureG=first(country),
                           departureGarr=first(SO_startDOY),
                           nSOdepartureG=n(),
                            rainarrDOY=first(rainarrDOY),
                            ndviupDOY=first(ndviupDOY)),
               by=c('ptt', 'year'))


# explaining departure rate using depature staging grounds

# factor 1: arrival time at departure grounds dictates departure 
# doesnt appear to be set amount of time needed to fuel sahara crossingf
#
ggplot(data=dat, aes(x=departureGarr, y=DEPwestAF))+geom_point(shape=1)+facet_wrap(~year)

# or

ggplot(data=dat, aes(x=departureGarr, y=DEPwestAF, colour=DEPwestAF-departureGarr))+geom_point()+
  scale_colour_gradientn(colours=terrain.colors(10))

# class different birds into different groups e.g. some leave
# late cos they arrive late (fine) and same for some early
# roughly 20 day min fuelling? others are 
# 1: early departing cos arrived early = long fuelling
# 2: late departing cos late arrival = short fuelling, 
#this indactes photoperiod importance (but onward conseq?)
# 3: arrive mid time, depart early = good conditions?
# 4: arrive mid time, depart late = bad conditions?
### can use resid model to identify groups and then test NDVI an dprecip and habiata

# factor 2: shifting stopovers, indicates difficulty in locating good area
# more stopovers means you depart later
ggplot(data=dat, aes(x=nSOdepartureG, y=DEPwestAF))+geom_jitter(width=0.2, shape=1)+facet_wrap(~year)

ggplot(data=dat, aes(x=nSOdepartureG, y=DEPwestAF, colour=DEPwestAF-departureGarr))+geom_point()+
  scale_colour_gradientn(colours=terrain.colors(10))

# !!!!! TO REMEBER FOR MODELLING !!!!! #
# !!!!!!!! remember outliers DEPestAF <85 and > 120
# 3 depatures from Nigeria and Burkina Faso
# !!!!! ~~~~~~~~~~~~~~~~ !!!!!!!!!!!! ~~~~~~~~~~~~~#

### arrival into pre-depature stageing grounds per year 
ggplot(data=dat, aes(x=factor(year), y=departureGarr))+geom_jitter(width=0.2)+
  geom_jitter(aes(y=rainarrDOY), col=2)

ggplot(data=dat, aes(x=factor(year), y=departureGarr))+geom_boxplot()+
  geom_boxplot(aes(y=rainarrDOY), col=2)


#calc final stopover length (cos we corrected some values in the summary table)

dat$dur_finalSO=dat$DEPwestAF-dat$st_finalSO

#### AND THE ENV DATA #####

env<-read.csv('C:/cuckoo_tracking/data/spring_rainfall_NDVI_GRIMMS_TAMSAT_emodis_by_stopover_detailcoords_2018_dead.csv', h=T)

names(env)[names(env)=='value']<-'precip'
names(env)[names(env)=='value2']<-'ndvi'
env$cuck_pres<-as.numeric(env$cuck_pres) # abs=1, pres=2

# summarise data for each stopover so that value per date is the mean
# of the detail coords values

dat2<- env[,c(2,3,4,5,10:22)] %>% group_by(year, ptt, SO_startDOY, variable) %>%
  summarise_all(mean,na.rm=T) 

temp<- env %>% group_by(year, ptt, SO_startDOY, variable) %>% summarise(country=first(country))

dat2$country<-temp$country

dat2[which(dat2$precip>500),]$precip<-0 # remove erroneous vals
dat2[which(is.na(dat2$precip)),]$precip<-0 # fix na

dat2<-dat2 %>% group_by(year, ptt, SO_startDOY) %>% dplyr::mutate(cumrf=cumsum(precip))

# for tamsat cumrf
dat2<-dplyr::arrange(dat2, ptt, year, SO_startDOY, variable)
dat2$tamRAIN[which(!(1:21375 %in% seq(5,21375, 5)))]<-0
dat2<-dat2 %>% group_by(year, ptt, SO_startDOY) %>% dplyr::mutate(tamcumrf=cumsum(tamRAIN))


dat2.1<-dat2 %>% subset(cuck_pres==2) %>% group_by(year, ptt, SO_startDOY) %>%
  summarise_all(first)

dat2.1<-filter(dat2.1, country %in% c("Ghana","Cote d'Ivoire",
                      "Guinea", "Sierra Leone",
                      ' Burkina Faso', 'Nigeria'))


dat2.2<-NULL
dat2.3<-NULL
for(i in 1:nrow(dat2.1))
{
  dat2.2<-rbind(dat2.2,
                dat2 %>% filter(ptt==dat2.1[i,]$ptt, year==dat2.1[i,]$year, SO_startDOY==dat2.1[i,]$SO_startDOY,
                  variable==soversWA[soversWA$ptt==dat2.1[i,]$ptt & soversWA$year==dat2.1[i,]$year&
                               soversWA$SO_startDOY==dat2.1[i,]$SO_startDOY,]$rainarrDOY))
  dat2.3<-rbind(dat2.3,
               dat2 %>% filter(ptt==dat2.1[i,]$ptt, year==dat2.1[i,]$year, SO_startDOY==dat2.1[i,]$SO_startDOY,
                                variable==soversWA[soversWA$ptt==dat2.1[i,]$ptt & soversWA$year==dat2.1[i,]$year&
                                                     soversWA$SO_startDOY==dat2.1[i,]$SO_startDOY,]$ndviupDOY)) 
  print(i)
  }


dat2.1$rainTdiff=dat2.1$variable-dat2.2$variable

dat2.1$ndviTdiff=dat2.1$variable-dat2.3$variable

dat2.1$raindiff=dat2.1$cumrf-dat2.2$cumrf
dat2.1$raindiff2=dat2.1$tamcumrf-dat2.2$tamcumrf

dat2.1$ndvidiff=dat2.1$emodisNDVI-dat2.3$emodisNDVI

dat2.4<-dat2.1 %>% group_by(year, ptt) %>%
  summarise_all(last)

dat2.5<-dat2.1 %>% 
  group_by(year, ptt) %>%
  summarise_if(is.numeric, c('mean', 'var'))


dat$year<-as.integer(dat$year)

## join em up 

dat3<-left(dat, dat2.4, by=c('ptt', 'year'))


## check each stopover in West Africa

soversWAenv<-left_join(soversWA, dat2.2, by=c('ptt', 'year', 'SO_startDOY'))

ggplot(data=soversWAenv, aes(x=SO_days, y=aquaNDVI))+geom_point(shape=1)+theme_bw()

## nothing much

## check start and end stopover conditions ##

dat_st<-dat2.1 %>% group_by(year, ptt, SO_startDOY) %>%
  summarise_all(first)

dat_ed<-dat2.1 %>% group_by(year, ptt, SO_startDOY) %>%
  summarise_all(last)

qplot(x=dat_st$cumrf, y=dat_ed$cumrf)

qplot(x=dat_st[dat_st$country=="Cote d'Ivoire" | dat_st$country=="Ghana",]$cumrf,
      y=dat_ed[dat_ed$country=="Cote d'Ivoire" | dat_ed$country=="Ghana",]$cumrf)


ggplot(data=dat3, aes(x=dur_finalSO, y=DEPwestAF, colour=dep_country))+geom_point(shape=1)

# Loop to identify and add start of rains and greening to stopover table
# remember this will calc for all stopovers in WA, not just the subsample
# that have DEPwestAF values

soversWA$rainarrDOY<-NA
soversWA$ndviupDOY<-NA

for( i in 1: nrow(soversWA))
{
    dtemp<-soversWA[i,]
    
    
    t2<-dat2[dat2$ptt==dtemp$ptt & dat2$year==dtemp$year &
               dat2$SO_startDOY==dtemp$SO_startDOY,]
    
    temp=ggplot(data=t2)+
      geom_line( aes(x=variable, y=emodisNDVI*500), colour='dark green')+
      geom_line(aes(x=variable, y=cumrf), col='blue', size=1)+
      geom_line(aes(x=variable, y=tamcumrf), col='cyan', size=1)+
      ylab('cumulative rainfall since 01 Jan (mm)')+ xlab('Day of year')+
      ggtitle(paste("PTT:", dtemp$ptt, "Year:",dtemp$year, "country SO_start:", dtemp$country, dtemp$SO_startDOY))+
      ylim(c(0,500))+scale_x_continuous(breaks=seq(0,125,5))+theme_bw()
    
    print(temp)
    
    soversWA[i,]$rainarrDOY<-readline('enter rain start DOY')
    soversWA[i,]$ndviupDOY<-readline('enter ndvi start DOY')

  print(i)
  
}
# write.csv(soversWA, 'C:/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_BOTH_weastAF_rfndvistart.csv',quote=F,row.names=F)

