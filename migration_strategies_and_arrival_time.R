### 13/03/18

### Outputs for Chris H 

### 1) Create summary master table for hypotheses testing

# First we need to use the non-best of day dataset and re-define 
# what we are calling a stopovr. previously we used => 3 days, as per
# the nature paper so need something smaller

# Load non best-of-day data which has stopover defined as >= 1 day


dat2<-read.csv("N:/cuckoo_tracking/data/stopover_table_1daymin_biomes.csv", h=T)

UK_travel<-read.csv("N:/cuckoo_tracking/data/complete_cycles_UK_all_timing.csv", h=T)

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
write.csv(d4, "N:/cuckoo_tracking/data/stopover_1daymin_spring_mig.csv", quote=F, row.names=F)


# NOW create master file, 1 row per bird and migration 

d4<-read.csv("N:/cuckoo_tracking/data/stopover_1daymin_spring_mig.csv", h=T)

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
                    "Sierra Leone"),]$region<-"West Africa"


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

strategy.dat <- read.csv("N:/cuckoo_tracking/t_drive/scripts/cuckoo migratory strategy and Sahara crossing success 2014_bird year multiples_NEW1.csv", header=T)


out5<-join(x=out4, y=data.frame(ptt=strategy.dat$tag, 
                                breeding_loc=strategy.dat$capture.location),by="ptt", match="first")

# rearrange with dplyr
library(dplyr)

out6<-out5 %>% select(ptt, breeding_loc, year, depart_winterSO, DEPcentralAF,
                DEPwestAF, DEPnorthAF, DEPeurope, arrive_uk, arrive_breeding,
                noSOcentralAF, sumSOcentralAF, noSOwestAF, sumSOwestAF, 
                noSOnorthAF, sumSOnorthAF,noSOeurope, sumSOeurope)

# make stopovers duration and numer a zero when they don't occur
out6[,11:18][is.na(out6[,11:18])]<-"0"

write.csv(out6, "N:/cuckoo_tracking/data/stopover_1daymin_spring_mig_summary.csv", quote=F, row.names=F)


##################################################
###### Now stats
##################################################

dat<-read.csv("N:/cuckoo_tracking/data/stopover_1daymin_spring_mig_summary.csv", h=T)

dat$depart_winterSO <- as.POSIXct(strptime(dat$depart_winterSO, "%Y-%m-%d %H:%M:%S"), "UTC")
dat$DEPcentralAF <- as.POSIXct(strptime(dat$DEPcentralAF, "%Y-%m-%d %H:%M:%S"), "UTC")
dat$DEPwestAF <- as.POSIXct(strptime(dat$DEPwestAF, "%Y-%m-%d %H:%M:%S"), "UTC")
dat$DEPnorthAF <- as.POSIXct(strptime(dat$DEPnorthAF, "%Y-%m-%d %H:%M:%S"), "UTC")
dat$DEPeurope <- as.POSIXct(strptime(dat$DEPeurope, "%Y-%m-%d %H:%M:%S"), "UTC")
dat$arrive_uk <- as.POSIXct(strptime(dat$arrive_uk, "%Y-%m-%d %H:%M:%S"), "UTC")
dat$arrive_breeding <- as.POSIXct(strptime(dat$arrive_breeding, "%Y-%m-%d %H:%M:%S"), "UTC")



library(GGally)


#jpeg("~/BTO/cuckoo_tracking/outputs/spring_mig_corr.jpg",
#     width = 24, height =12 , units ="in", res =600)
ggpairs(dat)
dev.off()

# ok nice, but too big to visualise in R

library(lubridate)

dat$depart_winterSO.doy <- yday(dat$depart_winterSO)
# hack to put previous years yday behind
dat$depart_winterSO.doy<-ifelse(dat$depart_winterSO.doy > 200,
                                dat$depart_winterSO.doy-365, dat$depart_winterSO.doy)
                                
dat$DEPcentralAF.doy <- yday(dat$DEPcentralAF)
dat$DEPwestAF.doy <- yday(dat$DEPwestAF)
dat$DEPnorthAF.doy <- yday(dat$DEPnorthAF)
dat$DEPeurope.doy <- yday(dat$DEPeurope)
dat$arrive_uk.doy <- yday(dat$arrive_uk)
dat$arrive_breeding.doy <- yday(dat$arrive_breeding)


 #jpeg("~/BTO/cuckoo_tracking/outputs/spring_mig_corr2.jpg",
 #width = 24, height =12 , units ="in", res =600)
 ggpairs(dat[,11:25])
 dev.off()
                 
 
 #jpeg("~/BTO/cuckoo_tracking/outputs/spring_mig_corr3.jpg",
  #    width = 12, height =6 , units ="in", res =300)
 ggpairs(dat[,19:25])
 dev.off()
 
 jpeg("N:/cuckoo_tracking/outputs/spring_mig_corr4.jpg",
 width = 12, height =9 , units ="in", res =300)
ggpairs(dat[,11:18])
dev.off()
 
#########################
######## Modelling

library(lme4)
library(car)

# Does arrival at breeding location depend on african departures

m1<-lmer(arrive_breeding.doy~factor(year)*depart_winterSO.doy+
           (1|ptt), data=dat)

m2<-lmer(arrive_breeding.doy~factor(year)*DEPcentralAF.doy+
           (1|ptt), data=dat)

m3<-lmer(arrive_breeding.doy~factor(year)*DEPwestAF.doy+
           (1|ptt), data=dat)

anova(m1, m2, m3)

m4<-lmer(arrive_breeding.doy~factor(year)+depart_winterSO.doy+
           DEPcentralAF.doy+DEPwestAF.doy+
           (1|ptt), data=dat)

Anova(m4, test.statistic = 'F')

m4a<-lmer(arrive_breeding.doy~factor(year)+depart_winterSO.doy+
           DEPwestAF.doy+
           (1|ptt), data=dat)

Anova(m4a, test.statistic = 'F')

m4b<-lmer(arrive_breeding.doy~factor(year)+
            DEPwestAF.doy+
            (1|ptt), data=dat)

Anova(m4b, test.statistic = 'F')

m4c<-lmer(arrive_breeding.doy~
            DEPwestAF.doy+
            (1|ptt), data=dat)

anova(m4b, m4c)

library(MuMIn)
r.squaredGLMM(m4)
r.squaredGLMM(m4b)
r.squaredGLMM(m4c)

# is arrival determined by population

bl1<-lmer(arrive_breeding.doy~
            breeding_loc+
            (1|ptt), data=dat)

Anova(bl1, test.statistic = 'F')

# what about by the length of stopovers in various places

bl2<-lmer(arrive_breeding.doy~
            sumSOcentralAF+
            sumSOwestAF+
            sumSOnorthAF+
            sumSOeurope+
            (1|ptt), data=dat)

# nope

# OK so check if departure from various places
# is related to year

dw<-lmer(depart_winterSO.doy~factor(year)+
            (1|ptt), data=dat)

Anova(dw, test.statistic = 'F')
qplot(data=dat, x=depart_winterSO.doy, geom='histogram')+facet_wrap(~year)

dwa<-lmer(DEPwestAF.doy~factor(year)+
           (1|ptt), data=dat)

Anova(dwa, test.statistic = 'F')

qplot(data=dat, x=DEPwestAF.doy, geom='histogram')+facet_wrap(~year)

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
