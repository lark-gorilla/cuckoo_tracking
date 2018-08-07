### 07/08/18

### Model factors that determine departure of cuckoos from West Africa

library(dplyr)
library(ggplot2)
library(GGally)
library(lme4)
library(MuMIn)

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

# monsTdiff_first * raindiff2 # does rate of rainfall at first stopover impact?

# emodisNDVI_last # does greenness of last stopover impact?

# ndviTdiff_last * ndvi_diff # does rate of greening at last stopover impact?

# emodisANOM_mean # does greenness anomaly encountered over all stopovers impact?

# Trees_mean # does forest habitat used throughout impact?

# Croplands_mean # does anthropogenic habitat used throughout impact?

# d_river_mean # does river distance used throughout impact?

# how we looking

ggpairs(dat[,c(6,28,35,39,52,57,63,74,85,87,92)]) 


m1<-lmer(DEPwestAF)



# Not sure I should be doing, all I need to say is that they don't arrive before
# first rain.

## explain arrival into WA in terms of rainfall and ndvi

m1<-lmer(departureGarr~(departureGarr-rainTdiff_first)+(1|ptt)+(1|year), data=dat)

m2<-lmer(departureGarr~departureGarr-monsTdiff_first+(1|ptt)+(1|year), data=dat)

m3<-lmer(departureGarr~departureGarr-ndviTdiff_first+(1|ptt)+(1|year), data=dat)

anova(m1, m2, m3)

r.squaredGLMM(m1)
r.squaredGLMM(m2)
r.squaredGLMM(m3)

qplot(data=dat, x=departureGarr-ndviTdiff_first, y=departureGarr)

