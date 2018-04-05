### 13/03/18

### Outputs for Chris H 

### 1) Create summary master table for hypotheses testing

# First we need to use the non-best of day dataset and re-define 
# what we are calling a stopovr. previously we used => 3 days, as per
# the nature paper so need something smaller

#### work around for different laptop/desktop directories ####
#### !!!! RUN THIS !!!! ####
if(Sys.info()['nodename']=="D9L5812"){
  setwd("C:/cuckoo_tracking")}else{
    setwd("N:/cuckoo_tracking")}
#### !!!!!!!!!!!!!!!!!! ####

# Load non best-of-day data which has stopover defined as >= 1 day


dat2<-read.csv("data/stopover_table_bestofday_1daymin_recalc_biomes.csv", h=T)

UK_travel<-read.csv("data/complete_cycles_UK_all_timing.csv", h=T)

#UK_travel<-UK_travel[UK_travel$deployment_entry==FALSE,]

# Add columns to subset for birds that do a complete cycle

library(plyr)

#re-define year so that it is the year that the stopover starts in
dat2$SO_year=as.numeric(substr(dat2$SO_start, 1,4))

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

dt_of_min_lat<-ddply(d2, .(ptt, mig_cohort), .fun =
        function(x){c(x[x$SO_median_lat==min(x$SO_median_lat),]$SO_start,
                      x[x$SO_median_lat==min(x$SO_median_lat),]$SO_end)})
          
d2.5<-join_all(list(d2, dt_of_min_lat), by=c("ptt", "mig_cohort"))

d2.5<-rename(d2.5, c("V1"="winterSO_start",
                 "V2"="winterSO_end"))

d3<-d2.5[d2.5$deployment_entry==FALSE,]
# removes stopovers that are joined to UK deployment
# years i.e. deployment to Autumn migration e.g. 2011

d4<-d3[d3$is_spring_mig==TRUE,]
# only keeps stopovers that are between o1 Jan and UK
# entry i.e. spring migration. Because we have joined on ptt
# and year we basically have the calender year
# as a cutoff point.

d4$key<-NULL
d4$UK_entryHACK<-NULL
d4$breeding_entryHACK<-NULL
d4$is_spring_mig<-NULL
d4$deployment_entry<-NULL
d4$unreliable_exit<-NULL
# write out data
write.csv(d4, "data/stopover_bestofday_1daymin_recalc_spring_mig.csv", quote=F, row.names=F)


# NOW create master file, 1 row per bird and migration 

d4<-read.csv("data/stopover_bestofday_1daymin_recalc_spring_mig.csv", h=T)

d4$SO_start <- as.POSIXct(strptime(d4$SO_start, "%Y-%m-%d %H:%M:%S"), "UTC")
d4$SO_end <- as.POSIXct(strptime(d4$SO_end, "%Y-%m-%d %H:%M:%S"), "UTC")
d4$UK_entry <- as.POSIXct(strptime(d4$UK_entry, "%Y-%m-%d %H:%M:%S"), "UTC")
d4$UK_exit <- as.POSIXct(strptime(d4$UK_exit, "%Y-%m-%d %H:%M:%S"), "UTC")


# classify countries into migratory regions

table(d4$country)
library(maps)
plot(SO_median_lat~SO_median_long, d4)
map("world", add=T,  col=3)

# has to be date as timestamp is out by 1 hr.. UTC vs BST??
d4$region<-ifelse(as.Date(d4$SO_start) >= as.Date(d4$UK_entry) &
                  as.Date(d4$SO_start) <= as.Date(d4$UK_exit),
                  "UK", "Europe" )

d4[d4$country %in% c("Algeria","Morocco"
                     ),]$region<-"North Africa"


d4[d4$country %in% c("Togo","Cote d'Ivoire","Ghana",
                    "Nigeria","Burkina Faso", "Guinea",
                    "Sierra Leone", "Liberia"),]$region<-"West Africa"


d4[d4$country %in% c("Congo","Cameroon",
                     "Central African Republic",
                     "Democratic Republic of the Congo",
                     "Angola", "Gabon"),]$region<-"Central Africa"

plot(SO_median_lat~SO_median_long, d4, col=factor(region))
map("world", add=T,  col="grey")


# finally define the length of a stopover

hist(d4$SO_days)

d4[d4$SO_days<1,] # ok none

nrow(d4)
nrow(d4[d4$SO_days<3,]) # old appraoch lost ~ 25% of stopovers

# OK now just want last stopover in west africa before bird flew

out_tab<-ddply(d4, .(ptt, year, region), summarize,
               depart=max(SO_end), no_SO=length(SO_end),
               sum_SO=sum(SO_days))

out2<-ddply(d4, .(ptt, year), summarize, depart_winterSO=unique(winterSO_end),
            arrive_uk=unique(UK_entry), arrive_breeding=unique(breeding_entry))
            

out2.5<-join_all(list(out2, out_tab[out_tab$region=="Central Africa",]),
               by=c("ptt", "year"))
out2.5$region<-NULL
out2.5<-rename(out2.5, c("depart"="DEPcentralAF",
                     "no_SO"="noSOcentralAF",
                     "sum_SO"="sumSOcentralAF"))

out3<-join_all(list(out2.5, out_tab[out_tab$region=="West Africa",]),
                 by=c("ptt", "year"))

out3$region<-NULL
out3<-rename(out3, c("depart"="DEPwestAF",
                         "no_SO"="noSOwestAF",
                         "sum_SO"="sumSOwestAF"))

out3.5<-join_all(list(out3, out_tab[out_tab$region=="North Africa",]),
               by=c("ptt", "year"))

out3.5$region<-NULL
out3.5<-rename(out3.5, c("depart"="DEPnorthAF",
                     "no_SO"="noSOnorthAF",
                     "sum_SO"="sumSOnorthAF"))

out4<-join_all(list(out3.5, out_tab[out_tab$region=="Europe",]),
                 by=c("ptt", "year"))

out4$region<-NULL
out4<-rename(out4, c("depart"="DEPeurope",
                         "no_SO"="noSOeurope",
                         "sum_SO"="sumSOeurope"))

# Now join in extra metadata to create master file
# tidy to remove uneeded columns

strategy.dat <- read.csv("t_drive/scripts/cuckoo migratory strategy and Sahara crossing success 2014_bird year multiples_NEW1.csv", header=T)


out5<-join(x=out4, y=data.frame(ptt=strategy.dat$tag, 
                                breeding_loc=strategy.dat$capture.location),by="ptt", match="first")

out6<-join_all(list(out5, data.frame(ptt=strategy.dat$tag, year=strategy.dat$year, 
                                    autumn_mig=strategy.dat$migratory.strategy)),
               by=c("ptt", "year"))

# could do with adding more to the mig_strat colum in strategy.dat table

# Finally work out date of Sahara crossing from chat with Chris

allpts<-read.csv("data/processed_movebank_cuckoos_hybrid_filter_bestofday_clean_stopovers_recalc.csv", h=T)
# FORMAT!!
allpts$timestamp <- as.POSIXct(strptime(allpts$timestamp, "%Y-%m-%d %H:%M:%S"), "UTC")

#have a quick look for sahara cutoff

# use just birds were interested in 

allpts<-rename(allpts, c("year.x" = "year"))

jpts<-join(x=allpts, y=UK_travel ,type='inner',  by=c("ptt", "year"))

jpts<-jpts[jpts$month %in% 2:5,]

# this line subsets birds and year

jptSP<-SpatialPointsDataFrame(SpatialPoints(cbind(jpts$long, jpts$lat), 
                                     CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")),
                       data=jpts[,1:3])

jptSP$ptt<-paste0("A",jptSP$ptt)

library(tmap)

tmap_mode('view')

qtm(jptSP, dots.col='ptt')

tm_shape(subset(ras,1), projection=projection(ras), 
         is.master=T, bbox=extent(-17, 33, -17, 26))+tm_raster()+
  tm_shape(World)+tm_borders()

# actually don't need to do the recalc has done this for us!!


## USE DOY as simplar
library(lubridate)

out6$depart_winterSO <- yday(out6$depart_winterSO)
# hack to put previous years yday behind
out6$depart_winterSO<-ifelse(out6$depart_winterSO > 200,
                            out6$depart_winterSO-365, out6$depart_winterSO)

out6$DEPcentralAF <- yday(out6$DEPcentralAF)
out6$DEPwestAF <- yday(out6$DEPwestAF)
out6$DEPnorthAF <- yday(out6$DEPnorthAF)
out6$DEPeurope <- yday(out6$DEPeurope)
out6$arrive_uk <- yday(out6$arrive_uk)
out6$arrive_breeding <- yday(out6$arrive_breeding)


# rearrange with dplyr

library(dplyr)

out6<-out6 %>% select(ptt, year, depart_winterSO, DEPcentralAF,
                DEPwestAF, DEPnorthAF, DEPeurope, arrive_uk, arrive_breeding,
                noSOcentralAF, sumSOcentralAF, noSOwestAF, sumSOwestAF, 
                noSOnorthAF, sumSOnorthAF,noSOeurope, sumSOeurope, breeding_loc,
                autumn_mig)

# make stopovers duration and numer a zero when they don't occur
out6[,10:17][is.na(out6[,10:17])]<-"0"

write.csv(out6, "data/stopover_bestofday_1daymin_recalc_spring_mig_summary.csv", quote=F, row.names=F)


##################################################
###### Now stats
##################################################

dat<-read.csv("data/stopover_bestofday_1daymin_recalc_spring_mig_summary.csv", h=T)

### summarise the data

library(dplyr)

years<-group_by(dat, year)

summarise(years, mean(depart_winterSO, na.rm=T), sd(depart_winterSO, na.rm=T), 
          mean(DEPcentralAF, na.rm=T), sd(DEPcentralAF, na.rm=T), 
          mean(DEPwestAF, na.rm=T), sd(DEPwestAF, na.rm=T), 
          mean(DEPnorthAF, na.rm=T), sd(DEPnorthAF, na.rm=T), 
          mean(DEPeurope, na.rm=T), sd(DEPeurope, na.rm=T), 
          mean(arrive_breeding, na.rm=T), sd(arrive_breeding, na.rm=T))
  
library(reshape2)      

d1<-melt(dat)

d2<-filter(d1, variable=='depart_winterSO'|variable=='DEPcentralAF'|
             variable=='DEPwestAF'|variable=='DEPnorthAF'|
             variable=='DEPeurope'|variable=='arrive_breeding')

#jpeg("outputs/spring_mig_timing_year.jpg",
    width = 12, height =9 , units ="in", res =300)

qplot(data=d2, x=variable, y=value, colour=year, geom='boxplot' )

dev.off()

Anova(lmer(depart_winterSO~year +(1|ptt), data=dat), test.statistic = 'F')

Anova(lmer(DEPcentralAF~year +(1|ptt), data=dat), test.statistic = 'F')

Anova(lmer(DEPwestAF~year +(1|ptt), data=dat), test.statistic = 'F')

Anova(lmer(DEPnorthAF~year +(1|ptt), data=dat), test.statistic = 'F')

Anova(lmer(DEPeurope~year +(1|ptt), data=dat), test.statistic = 'F')


d3<-filter(d1, variable=='sumSOcentralAF'|variable=='sumSOwestAF'|
             variable=='sumSOnorthAF'|variable=='sumSOeurope')

#jpeg("outputs/spring_mig_SOdur_year.jpg",
width = 12, height =9 , units ="in", res =300)

qplot(data=d3, x=variable, y=value, colour=year, geom='boxplot' )

dev.off()


library(GGally)


#jpeg("~/BTO/cuckoo_tracking/outputs/spring_mig_corr.jpg",
#     width = 24, height =12 , units ="in", res =600)
ggpairs(dat)
dev.off()

# ok nice, but too big to visualise in R

 #jpeg("~/BTO/cuckoo_tracking/outputs/spring_mig_corr2.jpg",
 #width = 24, height =12 , units ="in", res =600)
 ggpairs(dat[,11:25])
 dev.off()
                 
 
 #jpeg("outputs/spring_mig_departures.jpg",
      width = 12, height =9 , units ="in", res =300)
 ggpairs(dat[,3:9])
 dev.off()
 
 #jpeg("outputs/spring_mig_stopover_dur.jpg",
 width = 12, height =9 , units ="in", res =300)
ggpairs(dat[,10:17])
dev.off()
 
#########################
######## Modelling

library(lme4)
library(car)

dat$year<pfactor(dat$year)
dat$year<-relevel(dat$year, ref='2014')

#m arrival in the uk by year

library(ggplot2)
qplot(data=dat, x=DEPwestAF, y=arrive_breeding, col=factor(ptt))


qplot(data=dat[dat$DEPwestAF>60,], x=DEPwestAF, y=arrive_breeding)+
  geom_smooth(method='lm')+facet_wrap(~year)

# check multiple year birds

summary(lmer(arrive_breeding~factor(ptt)+
        (1|year), data=dat[dat$ptt %in% names(which(table(dat$ptt)>1)),]))

# Does arrival at breeding location depend on african departures

m4<-lmer(arrive_breeding~depart_winterSO+
           DEPcentralAF+DEPwestAF+
           (1|ptt)+(1|year), data=dat[dat$DEPwestAF>60,])

m41<-lmer(arrive_breeding~depart_winterSO+
           DEPcentralAF+DEPwestAF+
           (1|year), data=dat[dat$DEPwestAF>60,])

anova(m4, m41) # ptt RF doesnt add anything


Anova(m41, test.statistic = 'F')

m4a<-lmer(arrive_breeding~depart_winterSO+
           DEPwestAF+
           (1|year), data=dat[dat$DEPwestAF>60,])

Anova(m4a, test.statistic = 'F')

m4b<-lmer(arrive_breeding~
            DEPwestAF+
            (1|year), data=dat[dat$DEPwestAF>60,])

Anova(m4b, test.statistic = 'F')

m4c<-lm(arrive_breeding~
            DEPwestAF, data=dat[dat$DEPwestAF>60,])

# check if we can drop year randome effect
anova(m4b, m4c)

library(MuMIn)
r.squaredGLMM(m41)
r.squaredGLMM(m4b)
r.squaredGLMM(m4c)
# Nope

summary(m4b)


# is arrival determined by population

bl1<-lmer(arrive_breeding~
            breeding_loc+
            (1|ptt), data=dat)

Anova(bl1, test.statistic = 'F')

# what about by the length of stopovers in various places

bl2<-lmer(arrive_breeding~
            sumSOcentralAF+
            sumSOwestAF+
            sumSOnorthAF+
            sumSOeurope+
            (1|ptt), data=dat)

# nope

# OK so check if departure from various places
# is related to year

dw<-lmer(depart_winterSO~factor(year)+
            (1|ptt), data=dat)

Anova(dw, test.statistic = 'F')
qplot(data=dat, x=depart_winterSO, geom='histogram')+facet_wrap(~year)

dwa<-lmer(DEPwestAF~factor(year)+
           (1|ptt), data=dat)

Anova(dwa, test.statistic = 'F')

qplot(data=dat, x=DEPwestAF, geom='histogram')+facet_wrap(~year)

# Do number no of or tot length of stopovers 
# in west africa vary by year


sWA<-lmer(sumSOwestAF~factor(year)+
           (1|ptt), data=dat)

Anova(sWA, test.statistic = 'F')
qplot(data=dat, x=sumSOwestAF, geom='histogram')+facet_wrap(~year)

# Using number of stopovers in north africa 
# and europe as a metric of condition
# do birds that spend longer in west africa 
# spend less time later on?

SOc<-lmer(sumSOeurope~sumSOwestAF+factor(year)+
            (1|ptt), data=dat)
Anova(SOc, test.statistic = 'F')

SOne<-lmer(sumSOeurope~sumSOnorthAF+factor(year)+
            (1|ptt), data=dat)
Anova(SOne, test.statistic = 'F')
