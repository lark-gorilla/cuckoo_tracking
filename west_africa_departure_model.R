### 07/08/18

### Model factors that determine departure of cuckoos from West Africa

library(dplyr)
library(ggplot2)
library(GGally)
library(lme4)
library(MuMIn)
library(car)

# model vis function

predplot<-function(mod=m1, variable='departureGarr'){
newdat<-data.frame(v1=seq(min(dmod[,which(names(dmod)==variable)]),
                          max(dmod[,which(names(dmod)==variable)]),
                          (max(dmod[,which(names(dmod)==variable)])- 
                             min(dmod[,which(names(dmod)==variable)]))/100),
                   DEPwestAF=1)
names(newdat)[1]<-names(dmod)[which(names(dmod)==variable)]
newdat$p1<-predict(mod, newdata=newdat, re.form=~0)

f1<-as.formula(paste('DEPwestAF~', names(dmod)[which(names(dmod)==variable)]))

predmat<-model.matrix(f1, data=newdat)
vcv<-vcov(mod)
semod<-sqrt(diag(predmat%*%vcv%*%t(predmat)))
newdat$lc<-newdat$p1-semod*1.96
newdat$uc<-newdat$p1+semod*1.96

qplot(data=dmod, x=dmod[,which(names(dmod)==variable)]
, y=DEPwestAF)+
  geom_line(data=newdat, aes(x=newdat[,which(names(newdat)==variable)], y=p1), colour='red')+
  geom_line(data=newdat, aes(x=newdat[,which(names(newdat)==variable)], y=lc), colour='red', linetype='dashed')+
  geom_line(data=newdat, aes(x=newdat[,which(names(newdat)==variable)], y=uc), colour='red', linetype='dashed')+
  ylab("Departure date West Africa (DOY)")+
  theme_bw()}

if(Sys.info()['nodename']=="D9L5812"){
  setwd("C:/cuckoo_tracking")}else{
    setwd("N:/cuckoo_tracking")}


dat<-read.csv('C:/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead_attrib_modelready.csv', h=T)

# check if last info from dat2 joins matches last info from original approach

# do ggally pairs 
# before check with plots how variables behave



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

ggplot(data=dat, aes(x=nSOdepartureG, y=DEPwestAF, colour=DEPwestAF-departureGarr))+geom_jitter( width=0.1, shape=1)+
  scale_colour_gradientn(colours=terrain.colors(10))

# !!!!! TO REMEBER FOR MODELLING !!!!! #
# !!!!!!!! remember outliers DEPestAF <85 and > 120
# 4 depatures from Nigeria, Burkina Faso and Liberia
# !!!!! ~~~~~~~~~~~~~~~~ !!!!!!!!!!!! ~~~~~~~~~~~~~#


#
#ggpairs(dat[,c(6,28,29,31:43)])

ggpairs(dat[,c(6,28,29,31,32)]) # ndvi first

ggpairs(dat[,c(6,28,29,52,53)]) # ndvi last

ggpairs(dat[,c(6,28,29,73,74,94,95)]) # ndvi mean

ggpairs(dat[,c(6,28,29,33,34)]) # rain first

ggpairs(dat[,c(6,28,29,54,55)]) # rain last

ggpairs(dat[,c(6,28,29,75,76,96,97)]) # rain mean

ggpairs(dat[,c(6,28,29,35:42)]) # tdiff first

ggpairs(dat[,c(6,28,29,52,53)]) # tdiff last

ggpairs(dat[,c(6,28,29,73,74,94,95)]) # tdiff mean


# ok so what are main variables:

# departureGarr 
# nSOdepartureG

# monsTdiff_first # does synchronicity with monsoon rains at first stopover

# monsTdiff_first * raindiff2_first # does rate of rainfall at first stopover impact?

# emodisNDVI_last # does greenness of last stopover impact?

# ndviTdiff_last * ndvi_diff # does rate of greening at last stopover impact?

# emodisANOM_mean # does greenness anomaly encountered over all stopovers impact?

# Trees_mean # does forest habitat used throughout impact?

# Croplands_mean # does anthropogenic habitat used throughout impact?

# d_river_mean # does river distance used throughout impact?

# how we looking

ggpairs(dat[,c(6,28,29,37,39,52,57,63,74,85,87,92)]) 

# sort NAs

dmod<-dat[,c(1,2,6,28,29,37,39,52,57,63,74,85,87,92)]

dmod<-na.omit(dmod)

#explore relationships with basic model

# forwards stepwise selection using most mechanistic/simplistic metrics first

m1<-lmer(DEPwestAF~departureGarr+(1|ptt)+(1|year), data=dmod)
             
predplot(mod=m1, variable=departureGarr)             
             
ggplot(data=dmod, aes(x=departureGarr, y=DEPwestAF))+geom_point(shape=1)


ggplot(data=dmod, aes(x=departureGarr, y=DEPwestAF))+geom_point(shape=1)


# only real issue is departureGarr and raindiff2. could just do 
# monsTdiff_first*departureGarr as surrogate
# have a look with dredge

m1<-lmer(DEPwestAF~departureGarr+nSOdepartureG+monsTdiff_first * raindiff2_first+
           emodisNDVI_last+ndviTdiff_last * ndvidiff_last+emodisANOM_mean+
           Trees_mean+Croplands_mean+d_river_mean+(1|ptt)+(1|year), data=dmod,
         na.action = na.fail)

m2<-dredge(m1)


env<-read.csv('C:/cuckoo_tracking/data/spring_rainfall_NDVI_GRIMMS_TAMSAT_emodis_by_stopover_detailcoords_2018_dead.csv', h=T)

names(env)[names(env)=='value']<-'precip'
names(env)[names(env)=='value2']<-'ndvi'
env$cuck_pres<-as.numeric(env$cuck_pres) # abs=1, pres=2

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

temp<- dat2[- which(dat2$country %in% c('Angola', 'Central African Republic','Congo', 'Democratic Republic of the Congo', 'Gabon',  'Cameroon', 'Burkina Faso', 'Liberia')),] 

temp3<-temp %>% group_by(variable) %>%
  summarise(Mcumrf=mean(tamcumrf), rfmin=mean(tamcumrf)-sd(tamcumrf), rfmax=mean(tamcumrf)+sd(tamcumrf),
   MaquaNDVI=mean(emodisNDVI, na.rm=T), ndmin=(mean(emodisNDVI, na.rm=T)-sd(emodisNDVI, na.rm=T)),
   ndmax=(mean(emodisNDVI, na.rm=T)+sd(emodisNDVI, na.rm=T)))

ggplot(data=dmod, aes(x=departureGarr, y=DEPwestAF, colour=DEPwestAF-departureGarr))+
  geom_line(data=temp3,aes(x=variable, y=Mcumrf/2), size=1, colour='blue')+
  geom_line(data=temp3,aes(x=variable, y=rfmin/2), colour='grey', linetype=2)+
  geom_line(data=temp3,aes(x=variable, y=rfmax/2), colour='grey', linetype=2)+ 
  geom_line(data=temp3,aes(x=variable, y=MaquaNDVI*100), size=1, colour='dark green')+
  geom_line(data=temp3,aes(x=variable, y=ndmin*100), colour='orange', linetype=2)+
  geom_line(data=temp3,aes(x=variable, y=ndmax*100), colour='orange', linetype=2)+
  geom_point()+
  scale_colour_gradientn(colours=terrain.colors(10))+
  theme_bw()

#colour graded background:
#https://stackoverflow.com/questions/39466505/diagonal-grading-of-background-color-of-ggplot-graph-in-r

df <- expand.grid(departureGarr=25:100,
                  DEPwestAF=75:130)

df$filly=df$DEPwestAF-df$departureGarr 

df[df$filly<0 | df$filly>80,]$filly<-NA

ggplot() + 
  geom_tile(data=df, aes(x=departureGarr, y=DEPwestAF, fill=filly),alpha = 0.75) +      
  scale_fill_gradient(low='#ffeda0', high='#f03b20', na.value=NA)+
  geom_line(data=temp3,aes(x=variable, y=Mcumrf/2), size=1, colour='blue')+
  geom_line(data=temp3,aes(x=variable, y=rfmin/2), colour='grey', linetype=2)+
  geom_line(data=temp3,aes(x=variable, y=rfmax/2), colour='grey', linetype=2)+ 
  geom_line(data=temp3,aes(x=variable, y=MaquaNDVI*100), size=1, colour='dark green')+
  geom_line(data=temp3,aes(x=variable, y=ndmin*100), colour='orange', linetype=2)+
  geom_line(data=temp3,aes(x=variable, y=ndmax*100), colour='orange', linetype=2)+
  geom_point(data=dmod, aes(x=departureGarr, y=DEPwestAF), shape=1)+
  theme_bw()
  
  
  geom_line(data=data.frame(x=25:100, y=(25:100)+28.73), aes(x=x, y=y))

m1<-lmer(departureGarr~(departureGarr-rainTdiff_first)+(1|ptt)+(1|year), data=dat)

m2<-lmer(departureGarr~departureGarr-monsTdiff_first+(1|ptt)+(1|year), data=dat)

m3<-lmer(departureGarr~departureGarr-ndviTdiff_first+(1|ptt)+(1|year), data=dat)

anova(m1, m2, m3)

r.squaredGLMM(m1)
r.squaredGLMM(m2)
r.squaredGLMM(m3)

qplot(data=dat, x=departureGarr-ndviTdiff_first, y=departureGarr)

