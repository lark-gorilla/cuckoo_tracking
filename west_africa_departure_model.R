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

#lets make variable!

dmod$tWA=dmod$DEPwestAF-dmod$departureGarr

ggplot(data=dmod, aes(x=departureGarr, y=DEPwestAF, colour=tWA))+
  geom_line(data=temp3,aes(x=variable, y=Mcumrf/2), size=1, colour='blue')+
  geom_line(data=temp3,aes(x=variable, y=rfmin/2), colour='grey', linetype=2)+
  geom_line(data=temp3,aes(x=variable, y=rfmax/2), colour='grey', linetype=2)+ 
  geom_line(data=temp3,aes(x=variable, y=MaquaNDVI*100), size=1, colour='dark green')+
  geom_line(data=temp3,aes(x=variable, y=ndmin*100), colour='orange', linetype=2)+
  geom_line(data=temp3,aes(x=variable, y=ndmax*100), colour='orange', linetype=2)+
  geom_point()+scale_y_continuous(breaks=seq(0, 150, 10))+
  scale_x_continuous(breaks=seq(0, 120, 10))+
  scale_colour_gradientn(colours=terrain.colors(10))+
  theme_bw()

#colour graded background:
#https://stackoverflow.com/questions/39466505/diagonal-grading-of-background-color-of-ggplot-graph-in-r

df <- expand.grid(departureGarr=0:130,
                  DEPwestAF=75:130)

df$filly=df$DEPwestAF-df$departureGarr 

df[df$filly<0 | df$filly>80,]$filly<-NA

# add in m.arr line from code below
m.arr<-lmer(tWA~departureGarr+(1|ptt)+(1|year), data=dmod)

tempdf<-data.frame(departureGarr=0:130)
tempdf$predDEPWA<-tempdf$departureGarr+
  predict(m.arr, newdata=tempdf, re.form=~0)

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
  theme_bw()+scale_y_continuous(breaks=seq(0, 130, 10), limits=c(0,130))+
  scale_x_continuous(breaks=seq(0, 130, 10))+
  geom_line(data=tempdf,aes(x=departureGarr, y=predDEPWA))
  

  #geom_line(data=data.frame(x=25:100, y=(25:100)+28.73), aes(x=x, y=y))

# ok so dial it back a step and model duration in west Africa
# as function of arrival time. try poly/aysmptotic
#also wavy gam to see if relationship is there - optimum fuelling?

# two lines of enquiry

#1 what allows early depature/short fuelling around arrival day 70?

#2 late arriving birds leave with shortere and shorter fuelling,
# is this becuase they fuel up faster or because of endogenous
# demands to migrate? If the latter they might not be in peak condition
# but leave anyway. It is important to determine which because
# if they ignore environmental factors in favour of endogenous 
# then they are not constrained per-se by rainfall/ndvi but it 
# could impact onward fitness

# ok so that hump is real
qplot(data=dmod, x=departureGarr, y=DEPwestAF-departureGarr)+
  geom_smooth()

qplot(data=dmod, x=departureGarr, y=DEPwestAF-departureGarr)+
  geom_smooth(method='lm')

#looking at differently, stong pattern based on entry to WA
qplot(data=dmod, x=DEPwestAF-departureGarr, y=departureGarr)+
  labs(x='Days in West Africa', y='Arrival in West Africa')

# no pattern for exit
qplot(data=dmod, x=DEPwestAF-departureGarr, y=DEPwestAF)+
  labs(x='Days in West Africa', y='Departure from West Africa')


m.arr<-lmer(tWA~departureGarr+(1|ptt)+(1|year), data=dmod)

m.dep<-lmer(DEPwestAF~tWA+(1|ptt)+(1|year), data=dmod)

Anova(m.arr, test.statistic='F')
Anova(m.dep, test.statistic='F')

r.squaredGLMM(m.arr)
r.squaredGLMM(m.dep)

newdat<-data.frame(departureGarr=30:100, tWA=1)
newdat$p1<-predict(m.arr, newdata=newdat, re.form=~0)

predmat<-model.matrix(tWA~departureGarr, data=newdat)
vcv<-vcov(m.arr)
semod<-sqrt(diag(predmat%*%vcv%*%t(predmat)))
newdat$lc<-newdat$p1-semod*1.96
newdat$uc<-newdat$p1+semod*1.96

qplot(data=dmod, x=departureGarr, y=tWA)+
  geom_line(data=newdat, aes(x=departureGarr, y=p1), colour='red')+
  geom_line(data=newdat, aes(x=departureGarr, y=lc), colour='red', linetype='dashed')+
  geom_line(data=newdat, aes(x=departureGarr, y=uc), colour='red', linetype='dashed')+
  xlab("Arrival into West Africa (DOY)")+
  ylab("Days spent in West Africa")


# ok so predict back to data

dmod$pred_depWA1<-dmod$departureGarr+
  predict(m.arr, newdata=dmod, re.form=~0)

dmod$pred_depWA2<-dmod$departureGarr+
  predict(m.arr, newdata=dmod) # with random effects

dmod$pred_depWA2a<-dmod$departureGarr+
  predict(m.arr, newdata=dmod, re.form=~(1|ptt)) # with just ptt random effect

dmod$pred_depWA2b<-dmod$departureGarr+
  predict(m.arr, newdata=dmod, re.form=~(1|year)) # with just year random effect


qplot(data=dmod, x=pred_depWA1, y=DEPwestAF) # ok

qplot(data=dmod, x=pred_depWA2, y=DEPwestAF) # good

# okay
ggplot(data=dmod, aes(x=departureGarr))+geom_point(aes(y=DEPwestAF), col=1)+
  geom_point(aes(y=pred_depWA1), col=2)+
  geom_point(aes(y=pred_depWA2), col=3)+
  geom_point(aes(y=pred_depWA2a), col=4)+
  geom_point(aes(y=pred_depWA2b), col=5)
  


## ok so surrogate in time of first rain/mons/ndvi instead of departureGarr

#eg

ggplot(data=dat, aes(x=monsTdiff_first, y=DEPwestAF))+geom_point()

# check > dat$departureGarr+dat$monsTdiff_first to make sure correct|?

# so what is median/mean departure date

summary(m.dep0<-lmer(DEPwestAF~(1|ptt)+(1|year), data=dmod))

median(dmod$DEPwestAF)

# call it day 100

summary(m.arr)

# testing effect on arrival_breeding.

#just to check grouping idea, seems birds that arrive into west africa in the
# 65-75 day speed-fuelling window arrive back in the uk sooner
ggplot(data=dat, aes(x=DEPwestAF, y=arrive_breeding, colour=departureGarr))+
  geom_boxplot(data=dat[dat$departureGarr<65,], aes(group=1), colour=1)+
  geom_boxplot(data=dat[dat$departureGarr%in% 66:75,],  aes(group=1),colour=2)+
  geom_boxplot(data=dat[dat$departureGarr>75,], aes(group=1), colour=3)+
  geom_point(data=dat[dat$departureGarr<65,], colour=1)+
  geom_point(data=dat[dat$departureGarr%in% 66:75,], colour=2)+
  geom_point(data=dat[dat$departureGarr>75,], colour=3)

# so is departureGarr actually better than depWestAF as a predictor

dmod2<-dat[,c(1,2,5,6,10,28,37)]

dmod2<-na.omit(dmod2)

dmod2$rain_arr<-dmod2$departureGarr+dmod2$monsTdiff_first

m1<-lmer(arrive_breeding~DEPwestAF+(1|ptt)+(1|year), data=dmod2)
m2<-lmer(arrive_breeding~departureGarr+(1|ptt)+(1|year), data=dmod2)
m3<-lmer(arrive_breeding~rain_arr+(1|ptt)+(1|year), data=dmod2)
anova(m1, m2,m3)
# ok so no merit in doing first rain against breeding arrival


# ok have a look at all variables


dmod<-read.csv('C:/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead_attrib_modelready.csv', h=T)


#replace NA in sd variables with 0
dtemp<-dmod[,c(94:114)]

dtemp[is.na(dtemp)]<-0

dmod<-dmod[,c(1,2,6,19:25,27:93)]

dmod<-cbind(dmod, dtemp)

# time in west africa
dmod$tWA=dmod$DEPwestAF-dmod$departureGarr

dmod<-na.omit(dmod)


for ( i in 4:ncol(dmod))
{
  
  print(names(dmod)[i])

  print(qplot(data=dmod, y=DEPwestAF, x=dmod[,i])+
          xlab(names(dmod)[i]))
  
  f1<-as.formula(paste('DEPwestAF~', names(dmod)[i], '+(1|ptt)+(1|year)'))


  m1<-lmer(f1, data=dmod[dmod$DEPwestAF%in% 85:120,])
  print(Anova(m1, test.statistic='F'))
  
  readline('detailed plot?')
  
  
  
  if(is.numeric(dmod[,i])){
    
  print(predplot(m1, variable=names(dmod)[i]))
    }else{next}
  
  readline('next?')
} 


#check out collinearity

ggpairs(dmod[,c(21,25,42,46,63,67)])

# basically all the same...
 
m0<-lmer(DEPwestAF~(1|ptt)+(1|year), data=dmod) 

r.squaredGLMM(m0)

m1<-lmer(DEPwestAF~monsTdiff_last+(1|ptt)+(1|year), data=dmod)
m1b<-lmer(DEPwestAF~monsTdiff_first+(1|ptt)+(1|year), data=dmod)
m1c<-lmer(DEPwestAF~monsTdiff_mean+(1|ptt)+(1|year), data=dmod)

anova(m0, m1, m1b, m1c)

r.squaredGLMM(m0)
r.squaredGLMM(m1)
r.squaredGLMM(m1b)
r.squaredGLMM(m1c)


m2<-lmer(DEPwestAF~monsTdiff_last+monsTdiff_last:monsdiff2_last+(1|ptt)+(1|year), data=dmod)

anova(m1, m2)
Anova(m2, test.statistic = 'F')

r.squaredGLMM(m2)

interplot(m2, var1='monsTdiff_last', var2='monsdiff2_last')


m1<-lmer(DEPwestAF~departureGarr+nSOdepartureG+
           monsTdiff_first * raindiff2_first+
           emodisNDVI_last+ndviTdiff_last * ndvidiff_last+
           emodisANOM_mean+Grasslands_mean+Croplands_first+
           d_river_mean+(1|ptt)+(1|year), data=dmod,
         na.action = na.fail)

m2<-dredge(m1)


##### Trying to uncover the simple relationship before diving into a more
##### complex model

qplot(data=dmod, x=departureGarr, y=DEPwestAF, colour=dep_country)
# So early arrving birds leave from Ivory Coast and Ghana, those that arrive
# later leave from multiple countries. some of these recieve rains late eg Sierra Leone
# and Burkina Faso

# Not sure what the story is with monsoon arrival and departure. Ivory coast
# potentially gets rains sooner that could facilitae birds leving earlier from it?
# However there is a faily bit of interannual (and spatial) variation with seeminly littel corresponding
# difference in departure. Mechanism could be that endogenous forcers push birds
# into west africa and those that find good conditions fuel and move faster, rather than
# birds tracking resources and this being the driver of movement.
ggplot(data=dmod, aes(y=departureGarr-monsTdiff_first, x=dep_country))+
  geom_jitter(width=0.03,shape=1)+
  geom_jitter(aes(y=DEPwestAF),width=0.03, colour=2, shape=1)+
  facet_wrap(~year)+scale_y_continuous(breaks=seq(25,130, 10))+
  geom_hline(yintercept=102)+theme_bw()+
  labs(x='Departure Counrty', y='arrival  of monsoon (black) and departure of birds (red)')

#more support for endogenous forcing for departure, ie shorter final
# stopovers if running late
qplot(data=dmod, x=st_finalSO, y=dur_finalSO, colour=dep_country)

## revisiting population drivers for departure
ggplot(data=dmod, aes(x=dep_country, y=DEPwestAF, colour=breeding_loc))+geom_jitter(width=0.06, shape=1)

ggplot(data=dmod, aes(x=dep_country, y=DEPwestAF, colour=breeding_loc))+geom_point()+facet_wrap(~year)

dmod$breeding_hab<-'Upland'
dmod[dmod$breeding_loc %in% c('Ashdown Forest', 'New Forest', 'Norfolk Broads',
                              'Sherwood Forest', 'Thetford Forest'),]$breeding_hab<-'Lowland'

ggplot(data=dmod, aes(x=departureGarr, y=DEPwestAF, colour=breeding_hab))+
  geom_point()+facet_wrap(~year)


## Individual tests for consistent difference between UL/LL

dmod<-read.csv('C:/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead_attrib_modelready.csv', h=T)

dmod$breeding_hab<-'Upland'
dmod[dmod$breeding_loc %in% c('Ashdown Forest', 'New Forest', 'Norfolk Broads',
                              'Sherwood Forest', 'Thetford Forest'),]$breeding_hab<-'Lowland'

library(lmerTest)
library(gridExtra)

m1<-lmer(depart_winterSO~breeding_hab+(1|ptt)+(1|year), data=dmod)
m2<-lmer(DEPcentralAF~breeding_hab+(1|ptt)+(1|year), data=dmod)
m2a<-lmer(departureGarr~breeding_hab+(1|ptt)+(1|year), data=dmod)
m3<-lmer(DEPwestAF~breeding_hab+(1|ptt)+(1|year), data=dmod)
m4<-lmer(arrive_uk~breeding_hab+(1|ptt)+(1|year), data=dmod)
m5<-lmer(arrive_breeding~breeding_hab+(1|ptt)+(1|year), data=dmod)

anova(m1)
anova(m2)
anova(m2a)
anova(m3)
anova(m4)
anova(m5)

p1<-ggplot(data=dmod, aes(x=breeding_hab, y=depart_winterSO))+geom_boxplot()+
  geom_jitter(width=0.08)+
  geom_label(data=data.frame(), 
             aes(label = 'Anova F=4.24, p=0.052', x = -Inf, y = Inf), hjust = -1.5, vjust = 1)+
  labs(x='', y='DOY', title='Depart winter (most southern) stopover')


p2<-ggplot(data=dmod, aes(x=breeding_hab, y=DEPcentralAF))+geom_boxplot()+
  geom_jitter(width=0.08)+
  geom_label(data=data.frame(), 
             aes(label = 'Anova F=2.01, p=0.171', x = -Inf, y = Inf), hjust = -1.5, vjust = 1)+
  labs(x='', y='DOY', title='Depart Central Africa')

  
p3<-ggplot(data=dmod, aes(x=breeding_hab, y=departureGarr))+geom_boxplot()+
  geom_jitter(width=0.08)+
  geom_label(data=data.frame(), 
             aes(label = 'Anova F=5.78, p=0.025', x = -Inf, y = Inf), hjust = -1.5, vjust = 1)+
  labs(x='', y='DOY', title='Arrive West Africa')

  
p4<-ggplot(data=dmod, aes(x=breeding_hab, y=DEPwestAF))+geom_boxplot()+
  geom_jitter(width=0.08)+
  geom_label(data=data.frame(), 
             aes(label = 'Anova F=4.53, p=0.045', x = -Inf, y = Inf), hjust = -1.5, vjust = 1)+
  labs(x='', y='DOY', title='Depart West Africa')

  
p5<-ggplot(data=dmod, aes(x=breeding_hab, y=arrive_uk))+geom_boxplot()+
  geom_jitter(width=0.08)+
  geom_label(data=data.frame(), 
             aes(label = 'Anova F=6.21, p=0.017', x = -Inf, y = Inf), hjust = -1.5, vjust = 1)+
  labs(x='',y='DOY', title='Arrival at UK mainland')

  
p6<-ggplot(data=dmod, aes(x=breeding_hab, y=arrive_breeding))+geom_boxplot()+
  geom_jitter(width=0.08)+
  geom_label(data=data.frame(), 
             aes(label = 'Anova F=7.29, p=0.021', x = -Inf, y = Inf), hjust = -1.5, vjust = 1)+
  labs(x='Breeding habitat', y='DOY', title='Arrival at breeding grounds')

jpeg("C:/cuckoo_tracking/outputs/upland_lowland_phen_diffs.jpg",
     height =   15, width =8.27 , units ="in", res =300)

grid.arrange(p1, p2, p3, p4, p5, p6, ncol=1, nrow=6)

dev.off()

