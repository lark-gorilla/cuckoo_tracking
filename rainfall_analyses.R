#24/04/2018
# Rainfall analyses 
# Firstly pull in stopover data and analyse

#### work around for different laptop/desktop directories ####
#### !!!! RUN THIS !!!! ####
if(Sys.info()['nodename']=="D9L5812"){
  setwd("C:/cuckoo_tracking")}else{
    setwd("N:/cuckoo_tracking")}
#### !!!!!!!!!!!!!!!!!! ####

library(dplyr)
library(ggplot2)

int_out<-read.csv('data/spring_rainfall_by_stopover.csv', h=T)
int_out[is.na(int_out$value),]$value<-0

int_out$country<-factor(int_out$country, levels= c("Angola", "Democratic Republic of the Congo",
                                                   "Congo", "Gabon", "Central African Republic", 
                                                   "Cameroon", "Nigeria", "Togo", "Burkina Faso",
                                                   "Ghana", "Cote d'Ivoire", "Liberia", "Guinea", 
                                                   "Sierra Leone"))

t2<-int_out %>% group_by(ID) %>% dplyr::mutate(cumrf=cumsum(value))

t2$cuckbin<-0
t2[t2$cuck_pres=='Y',]$cuckbin<-1

library(lme4)

m1<-glmer(cuckbin~poly(cumrf, 2)+country+(1|ID), family=binomial, data=t2)

pred.df=expand.grid(country=unique(t2$country), 
                    cumrf=0:1000, ptt=unique(t2$ptt))

pred.df$p1<-predict(m1, newdata=pred.df, re.form=~0)


qplot(data=t2, x=cumrf, y=cuckbin)+
  geom_line(data=pred.df, (aes(x=cumrf, y=plogis(p1), colour=country)))

# looks like the curve needs more flexability

m2<-glmer(cuckbin~poly(cumrf, 2):country+(1|ID), family=binomial, data=t2)
pred.df$p2<-predict(m2, newdata=pred.df, re.form=~0)

qplot(data=t2, x=cumrf, y=cuckbin)+
  geom_line(data=pred.df, (aes(x=cumrf, y=plogis(p2), colour=country)))

m3<-glmer(cuckbin~poly(cumrf, 2):factor(ptt)+(1|year), family=binomial, data=t2)

pred.df$p3<-predict(m3, newdata=pred.df, re.form=~0)

qplot(data=t2, x=cumrf, y=cuckbin)+
  geom_line(data=pred.df, (aes(x=cumrf, y=plogis(p3), colour=ptt)))


anova(m1, m2, m3)

library(mgcv)

out=NULL
for( i in unique(t2$country))
{
  tempdat<-t2[t2$country==i,]
  if(length(unique(tempdat$year))>1){
  
  minc<-glmer(cuckbin~poly(cumrf, 2):factor(year)+(1|ID), family=binomial, data=tempdat)
  gminc<-gam(cuckbin~s(cumrf, k=5, by=year), family=binomial, data=tempdat)
  
  }else{
  
  minc<-glm(cuckbin~poly(cumrf, 2), family=binomial, data=tempdat) 
  gminc<-gam(cuckbin~s(cumrf, k=5), family=binomial, data=tempdat)
     
  }

  pred.df=expand.grid(cumrf=0:1500, country=i, year=unique(tempdat$year))
  
  pred.df$p1<-predict(minc, newdata=pred.df, re.form=~0)
  pred.df$p2<-predict(gminc, newdata=pred.df)
  
  out<-rbind(out, pred.df)
}
  
qplot(data=out, x=cumrf, y=plogis(p1), colour=factor(year), geom='line')+facet_wrap(~country) 



## try some kind of proportion of rainfall within cuckoo stopover to
# proportion before / after stopover