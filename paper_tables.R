# 06/06/18
# Making output tables for cuckoo paper. Initially descriptive tables
# of migration metrics from previously calculated stopover summary table

#### work around for different laptop/desktop directories ####
#### !!!! RUN THIS !!!! ####
if(Sys.info()['nodename']=="D9L5812"){
  setwd("C:/cuckoo_tracking")}else{
    setwd("N:/cuckoo_tracking")}
#### !!!!!!!!!!!!!!!!!! ####

# read in most complete dataset

dat<- read.csv('C:/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead.csv', h=T)

# set NA or manually correct cuckoo depature dates that
# are incorrect, from manual examination

dat[dat$ptt==115586 & dat$year==2014,]$DEPwestAF<-106
dat[dat$ptt==134955 & dat$year==2015,]$DEPwestAF<-NA
dat[dat$ptt==134956 & dat$year==2015,]$DEPwestAF<-NA

# and dead ones
dat[dat$ptt==128296 & dat$year==2014,]$DEPwestAF<-NA

#set up migration length variable
dat$mig_length<-dat$arrive_breeding-dat$DEPwestAF

qplot(data=dat, x=DEPwestAF, y=mig_length)


qplot(data=dat, x=DEPwestAF, y=WA_mig_len, colour=breeding_loc)+facet_wrap(~year)

library(lme4)

m4<-lmer(arrive_breeding~DEPwestAF+
           (1|ptt)+(1|year), data=dat)

summary(m4)

m4c<-lm(arrive_breeding~
          DEPwestAF, data=dat)


# plot

newdat<-data.frame(DEPwestAF=70:130, arrive_breeding=1)
newdat$p1<-predict(m4, newdata=newdat, re.form=~0)

predmat<-model.matrix(arrive_breeding~DEPwestAF, data=newdat)
vcv<-vcov(m4c)
semod<-sqrt(diag(predmat%*%vcv%*%t(predmat)))
newdat$lc<-newdat$p1-semod*1.96
newdat$uc<-newdat$p1+semod*1.96

qplot(data=dat, x=DEPwestAF, y=arrive_breeding)+
  geom_line(data=newdat, aes(x=DEPwestAF, y=p1), colour='red')+
  geom_line(data=newdat, aes(x=DEPwestAF, y=lc), colour='red', linetype='dashed')+
  geom_line(data=newdat, aes(x=DEPwestAF, y=uc), colour='red', linetype='dashed')+
  xlab("Departure date West Africa (DOY)")+
  ylab("Arrival at breeding grounds (DOY)")+
  theme_classic()

library(car)
Anova(m4, test.statistic = 'F')

m4<-lmer(arrive_breeding~DEPwestAF+
           (1|ptt)+(1|year), data=dat)

summary(m4)

m4c<-lm(arrive_breeding~
          DEPwestAF, data=dat)

# check if we can drop year randome effect
anova(m4b, m4c)

# plot

newdat<-data.frame(DEPwestAF=70:130, arrive_breeding=1)
newdat$p1<-predict(m4, newdata=newdat, re.form=~0)

predmat<-model.matrix(arrive_breeding~DEPwestAF, data=newdat)
vcv<-vcov(m4)
semod<-sqrt(diag(predmat%*%vcv%*%t(predmat)))
newdat$lc<-newdat$p1-semod*1.96
newdat$uc<-newdat$p1+semod*1.96

qplot(data=dat, x=DEPwestAF, y=arrive_breeding)+
  geom_line(data=newdat, aes(x=DEPwestAF, y=p1), colour='red')+
  geom_line(data=newdat, aes(x=DEPwestAF, y=lc), colour='red', linetype='dashed')+
  geom_line(data=newdat, aes(x=DEPwestAF, y=uc), colour='red', linetype='dashed')+
  xlab("Departure date West Africa (DOY)")+
  ylab("Arrival at breeding grounds (DOY)")+
  theme_bw()

library(car)
Anova(m4, test.statistic = 'F')

# And for migration length

m5<-lmer(mig_length~DEPwestAF+
           (1|ptt)+(1|year), data=dat)

summary(m5)

m5c<-lm(mig_length~
          DEPwestAF, data=dat)

# plot

newdat<-data.frame(DEPwestAF=70:130, mig_length=1)
newdat$p1<-predict(m5, newdata=newdat, re.form=~0)

predmat<-model.matrix(mig_length~DEPwestAF, data=newdat)
vcv<-vcov(m5)
semod<-sqrt(diag(predmat%*%vcv%*%t(predmat)))
newdat$lc<-newdat$p1-semod*1.96
newdat$uc<-newdat$p1+semod*1.96

qplot(data=dat, x=DEPwestAF, y=mig_length)+
  geom_line(data=newdat, aes(x=DEPwestAF, y=p1), colour='red')+
  geom_line(data=newdat, aes(x=DEPwestAF, y=lc), colour='red', linetype='dashed')+
  geom_line(data=newdat, aes(x=DEPwestAF, y=uc), colour='red', linetype='dashed')+
  xlab("Departure date West Africa (DOY)")+
  ylab("Migration length (days)")+
  theme_bw()

library(car)
Anova(m5, test.statistic = 'F')


# set up duration in WA variable, different to SOdurWA because
# it accounts for time assumed in WA but no stopover recorded.
dat$WA_dur<-dat$DEPwestAF-dat$DEPcentralAF
qplot(data=dat, x=year, y=WA_dur) # some large ones
# hmm there are discrepencies.. use sumSOwestAF as conservative estimate
dat$WA_dur<-dat$DEPcentralAF - dat$sumSOwestAF

# correct em
dat[dat$ptt==115586 & dat$year==2014,]$sumSOwestAF<-22.19065+4
dat[dat$ptt==134955 & dat$year==2015,]$sumSOwestAF<-NA
dat[dat$ptt==134956 & dat$year==2015,]$sumSOwestAF<-NA
# very conservative!

# set year as factor
dat$year<-factor(dat$year)
# relevel to central well sampled year
dat$year<-relevel(dat$year, ref='2015')
# relevel to central well sampled year
dat$breeding_loc<-relevel(dat$breeding_loc, ref='Thetford Forest')

## variable for sahara crossing length - check!

dat$sahara_arr<-dat$DEPnorthAF-dat$sumSOnorthAF

dat$sahara_arr<-ifelse(is.na(dat$sahara_arr),
                       dat$DEPeurope-dat$sumSOeurope,
                       dat$DEPnorthAF-dat$sumSOnorthAF)

dat$sahara_mig<-dat$sahara_arr-dat$DEPwestAF

library(lme4)

m1<-lmer(arrive_breeding~year+breeding_loc+(1|ptt), data=dat)
m2<-lmer(DEPwestAF~year+breeding_loc+(1|ptt), data=dat)
m3<-lmer(mig_length~year+breeding_loc+(1|ptt), data=dat)
m4<-lmer(sumSOwestAF~year+breeding_loc+(1|ptt), data=dat)
m5<-lmer(minlongwestAF~year+breeding_loc+(1|ptt), data=dat)
m6<-lmer(depart_winterSO~year+breeding_loc+(1|ptt), data=dat)

library(stargazer)

stargazer(m1, m3, m2, m4, m5, m6, type='text')

stargazer(m1, m3, m2, m4, m5, m6, type='html',
covariate.labels=c("Constant: Thetford Forest, 2015","2012","2013", "2014", "2016", "2017", "2018", "Ashdown Forest",
                   "Dartmoor", "Lake District", "New Forest", "Norfolk Broads", "North York Moors", 
                   "Scotland", "Sherwood Forest", "Wales"),
dep.var.labels = c("Breeding arrival date", "West Africa - UK migration (days)", 
                  "West Africa depature date", "Duration in West Africa (days)", 
                  "Min West Africa longitude", "Start date of northward migration"),
ci=TRUE, intercept.bottom = FALSE, omit.table.layout = "sn", 
digits=1, model.numbers=FALSE,  report='vcs',
out='C:/cuckoo_tracking/results/summary_table1.htm')

m1<-lmer(arrive_breeding~year+(1|ptt), data=dat)
m2<-lmer(DEPwestAF~year+(1|ptt), data=dat)
m3<-lmer(mig_length~year+(1|ptt), data=dat)
m4<-lmer(sumSOwestAF~year+(1|ptt), data=dat)
m5<-lmer(minlongwestAF~year+(1|ptt), data=dat)
m6<-lmer(depart_winterSO~year+(1|ptt), data=dat)


stargazer(m1, m3, m2, m4, m5, m6, type='html',
          covariate.labels=c("Constant: 2015","2012","2013", "2014", "2016", "2017", "2018")
                             ,
          dep.var.labels = c("Breeding arrival date", "West Africa - UK migration (days)", 
                             "West Africa depature date", "Duration in West Africa (days)", 
                             "Min West Africa longitude", "Start date of northward migration"),
          ci=TRUE, intercept.bottom = FALSE, omit.table.layout = "sn", 
          digits=1, model.numbers=FALSE, report='vcs',
          out='C:/cuckoo_tracking/results/summary_table2.htm')

m1<-lmer(arrive_breeding ~ breeding_loc + (1 | ptt), data = dat)
m2<-lmer(DEPwestAF~breeding_loc+(1|ptt), data=dat)
m3<-lmer(mig_length~breeding_loc+(1|ptt), data=dat)
m4<-lmer(sumSOwestAF~breeding_loc+(1|ptt), data=dat)
m5<-lmer(minlongwestAF~breeding_loc+(1|ptt), data=dat)
m6<-lmer(depart_winterSO~breeding_loc+(1|ptt), data=dat)

stargazer(m1, m3, m2, m4, m5, m6, type='html',
          covariate.labels=c("Constant: Thetford Forest", "Ashdown Forest",
                             "Dartmoor", "Lake District", "New Forest", "Norfolk Broads", "North York Moors", 
                             "Scotland", "Sherwood Forest", "Wales"),
          dep.var.labels = c("Breeding arrival date", "West Africa - UK migration (days)", 
                             "West Africa depature date", "Duration in West Africa (days)", 
                             "Min West Africa longitude", "Start date of northward migration"),
          ci=TRUE, intercept.bottom = FALSE, omit.table.layout = "sn", 
          digits=1, model.numbers=FALSE, report='vcs',
          out='C:/cuckoo_tracking/results/summary_table3.htm')

library(car)

Anova(m1, test.statistic = 'F')
Anova(m2, test.statistic = 'F')
Anova(m3, test.statistic = 'F')
Anova(m4, test.statistic = 'F')
Anova(m5, test.statistic = 'F')
Anova(m6, test.statistic = 'F')
# nothing

library(emmeans)

emmeans(m1, spec=c('year', 'breeding_loc')) # nice but prints loads

# Now per bird that did multiple migrations

mults<-names(which(table(dat$ptt)>1))

dat2<-dat[dat$ptt %in% mults,]

library(dplyr)

out<-dat2 %>% group_by(ptt) %>% 
  dplyr::summarise(population=unique(breeding_loc),
                   n_years=n(),
                   mn_arr=mean(arrive_breeding, na.rm=T),
                   sd_arr=sd(arrive_breeding, na.rm=T),
                   mn_mig=mean(mig_length, na.rm=T),
                   sd_mig=sd(mig_length, na.rm=T),
                   mn_dwa=mean(DEPwestAF, na.rm=T),
                   sd_dwa=sd(DEPwestAF, na.rm=T),
                   mn_sso=mean(sumSOwestAF, na.rm=T),
                   sd_sso=sd(sumSOwestAF, na.rm=T),
                   mn_mlw=mean(minlongwestAF, na.rm=T),
                   sd_mlw=sd(minlongwestAF, na.rm=T),
                   mn_dnb=mean(depart_winterSO, na.rm=T),
                   sd_dnb=sd(depart_winterSO, na.rm=T))
                   
write.csv(out,  'C:/cuckoo_tracking/results/multiyear_ptt_summary.csv', quote=F, row.names=F)                   
                   

## jumping ahead, test NDVI anom

env<-read.csv('data/spring_rainfall_NDVI_GRIMMS_by_stopover_detailcoords_2018_dead.csv', h=T)

# summarise data for each stopover so that value per date is the mean
# of the detail coords values

library(dplyr)

dat2<- env %>% group_by(year, ptt, SO_startDOY, variable) %>%
  summarise(country=last(country), SO_endDOY=first(SO_endDOY),
            cuck_pres=first(cuck_pres), precip=mean(value, na.rm=T),
            NDVI= mean(value2, na.rm=T), aquaNDVI=mean(aquaNDVI, na.rm=T),
            aquaANOM=mean(aquaANOM, na.rm=T),terrNDVI=mean(terrNDVI, na.rm=T),
            terrANOM=mean(terrANOM, na.rm=T))

dat2[which(dat2$precip>500),]$precip<-0 # remove erroneous vals
dat2[which(is.na(dat2$precip)),]$precip<-0 # fix na

dat2<-dat2 %>% group_by(year, ptt, SO_startDOY) %>% dplyr::mutate(cumrf=cumsum(precip))


# rearrange

dat2<- dat2 %>% select(year, ptt, SO_startDOY, SO_endDOY, country, everything())

dat2.1<-subset(dat2, cuck_pres=='Y')

dat2.2<-dat2.1 %>% group_by(year, ptt, SO_startDOY) %>%
  summarise(cumrf=mean(cumrf, na.rm=T),
            NDVI= mean(NDVI, na.rm=T), aquaNDVI=mean(aquaNDVI, na.rm=T),
            aquaANOM=mean(aquaANOM, na.rm=T),terrNDVI=mean(terrNDVI, na.rm=T),
            terrANOM=mean(terrANOM, na.rm=T),
            SOlen=first(SO_endDOY)-first(SO_startDOY))

dat2.3<-dat2.2 %>% group_by(year, ptt) %>%
  summarise(cumrf=last(cumrf),
            NDVI= last(NDVI), 
            aquaNDVI=last(aquaNDVI),
            aquaANOM=last(aquaANOM),
            terrNDVI=last(terrNDVI),
            terrANOM=last(terrANOM),
            last_SOstart=last(SO_startDOY))

dat3<-full_join(dat, dat2.3, by=c('ptt', 'year'))

m6<-lmer(DEPwestAF~aquaANOM+
           (1|ptt)+(1|year), data=dat3)

summary(m6)

# plot

newdat<-data.frame(aquaANOM=seq(-0.15,0.15, 0.005), DEPwestAF=1)
newdat$p1<-predict(m6, newdata=newdat, re.form=~0)

predmat<-model.matrix(DEPwestAF~aquaANOM, data=newdat)
vcv<-vcov(m6)
semod<-sqrt(diag(predmat%*%vcv%*%t(predmat)))
newdat$lc<-newdat$p1-semod*1.96
newdat$uc<-newdat$p1+semod*1.96

qplot(data=dat3, x=aquaANOM, y=DEPwestAF)+
  geom_line(data=newdat, aes(x=aquaANOM, y=p1), colour='red')+
  geom_line(data=newdat, aes(x=aquaANOM, y=lc), colour='red', linetype='dashed')+
  geom_line(data=newdat, aes(x=aquaANOM, y=uc), colour='red', linetype='dashed')+
  xlab("NDVI anomoly")+
  ylab("Departure date West Africa (DOY)")+
  theme_bw()


Anova(m6, test.statistic = 'F')

m7<-lmer(DEPwestAF~cumrf+
           (1|ptt)+(1|year), data=dat3)

newdat<-data.frame(cumrf=seq(0:800), DEPwestAF=1)
newdat$p1<-predict(m7, newdata=newdat, re.form=~0)

predmat<-model.matrix(DEPwestAF~cumrf, data=newdat)
vcv<-vcov(m7)
semod<-sqrt(diag(predmat%*%vcv%*%t(predmat)))
newdat$lc<-newdat$p1-semod*1.96
newdat$uc<-newdat$p1+semod*1.96

qplot(data=dat3, x=cumrf, y=DEPwestAF)+
  geom_line(data=newdat, aes(x=cumrf, y=p1), colour='red')+
  geom_line(data=newdat, aes(x=cumrf, y=lc), colour='red', linetype='dashed')+
  geom_line(data=newdat, aes(x=cumrf, y=uc), colour='red', linetype='dashed')+
  xlab("cumulative rainfall (mm)")+
  ylab("Departure date West Africa (DOY)")+
  theme_bw()

Anova(m7, test.statistic = 'F')

dat2<-dat2 %>% group_by(year, ptt) %>%
  summarise(cumrf=mean(c(last(cumrf),nth(cumrf, -2))),
            NDVI= mean(c(last(NDVI),nth(NDVI, -2))), 
            aquaNDVI=mean(c(last(aquaNDVI),nth(aquaNDVI, -2))),
            aquaANOM=mean(c(last(aquaANOM), nth(aquaANOM, -2))),
            terrNDVI=mean(c(last(terrNDVI),nth(terrNDVI, -2))),
            terrANOM=mean(c(last(terrANOM), nth(terrANOM, -2))))
