# 06/06/18
# Making output tables for cuckoo paper. Initially descriptive tables
# of migration metrics from previously calculated stopover summary table

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
                   


