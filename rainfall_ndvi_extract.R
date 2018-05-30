## remote sensing data Rainfall and NDVI extract
## 30/05/2018

#### work around for different laptop/desktop directories ####
#### !!!! RUN THIS !!!! ####
if(Sys.info()['nodename']=="D9L5812"){
  setwd("C:/cuckoo_tracking")}else{
    setwd("N:/cuckoo_tracking")}
#### !!!!!!!!!!!!!!!!!! ####

# Get cuckoo data

dat<-read.csv('data/stopover_bestofday_2018_1daymin_recalc_spring_mig.csv', h=T)
dat$dead<-0
dat1<-read.csv('data/stopover_bestofday_2018_1daymin_recalc_spring_mig_dead.csv', h=T)
dat1$dead<-1
dat1$biome1<-NULL
dat1$biome2<-NULL

dat<-rbind(dat,dat1)
#remove non- west african rows

dat<-dat[which( ! dat$country%in% c('Algeria', 'France', 'Italy', 'Morocco' ,'Spain',
                                    'United Kingdom', 'Portugal')),]


##### %%%%%%%%% RAINFALL DATA %%%%%%%%%%%%###

# read in monthly data in netcdf format ok

library(raster) # needs ncdf4 installed

# stacks up all the layers
#ras<-stack("sourced_data/PERSIANN_rainfall/monthly/PERSIANN_2018-03-26035525am.nc")
ras<-stack("sourced_data/PERSIANN_rainfall/daily_inc2018/PERSIANN_2018-05-25054150am.nc")
# warning in projection 

print(raster(ras, layer=1))

# basically it is specifcying WGS but CRS gets it a bit wrong

ras@crs
#fix 
ras@crs<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


plot(raster(ras, layer=5))

library(maps)
map('world', add=T)

##### check data

names(ras)
# name is 'units: days since 2011-06-01'

print(raster(ras, layer=1))

# get annual rasters
# 124 comes from 1;124 max DOY in west africa (4th May)

r2012<-subset(ras, 213:(213+124))
r2013<-subset(ras, 579:(579+124))
r2014<-subset(ras, 944:(944+123))
r2015<-subset(ras, 1309:(1309+124))
r2016<-subset(ras, 1674:(1674+124))
r2017<-subset(ras, 2040:(2040+124))
r2018<-subset(ras, 2405:(2405+124))

# With each annual raster, the position [x] of the layer is the DOY

# I THINK that it is actually pulling the rainfall DOY before the one in
# question, so 24hr time lag.. which is kinda what we want

##### %%%%%%%%% NDVI DATA %%%%%%%%%%%%###

tifz<-list.files("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/NDVI_WGS")

#order them by date rather than satellite (Terra and Aqua)
tifz<-tifz[order(substr(tifz, 14,21))]

for(i in tifz)
{
  if(which(i==tifz)==1){
    rastack<-raster(paste0("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/NDVI_WGS/",i))
  }else{
    rastack<-stack(rastack, paste0("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/NDVI_WGS/",i))  
  }
  print(i)  
}

projection(rastack)


library(reshape2)
library(dplyr)


int_out<-NULL
for( i in 2012:2018)
{
  
  indat<-dat[dat$year==i,]
  
  ## Rainfall extract
  
  # cool the 'get' command grabs the collect raster set from the memory
  ras_temp<-get(paste0('r', i))
  
  # add 50 km buffer as per stopover definition. buffer=50000
  ext<-extract(ras_temp, indat[,7:8], buffer=50000, fun=median)
  
  ext[ext<0]<-NA # remove negative values '-99'
  
  dimnames(ext)[[2]]<-1:(length(dimnames(ext)[[2]]))
  
  out<-data.frame(ptt=indat$ptt, country=indat$country, year=i, 
                  SO_startDOY=indat$SO_startDOY, 
                  SO_endDOY=indat$SO_endDOY, ext)
  
  ## NDVI  extract
  
  ndvi_temp<-subset(rastack, which(substr(tifz, 14,17)==i))
  
  ext2<-extract(ndvi_temp, indat[,7:8], buffer=50000, fun=median, na.rm=T)
  
  ext2<-ext2*0.0001 # apply scaling to get NDVI from -1 to 1
  
  # loop to rep ndvi values over each of the 8 days they represent
  extrep<-NULL
  for(i in 1:(ncol(ext2)-1))
  {
   extrep<-cbind(extrep, matrix(rep(ext2[,i], 8), ncol=8)) 
  }
  extrep<-cbind(extrep, matrix(rep(ext2[,16], 5), ncol=5))
  

  
  
  
  out$ID<-with(out, paste(ptt, year, SO_startDOY, sep="_"))
  
  outm<-melt(out, id.vars=c('ID', 'ptt', 'country', 'year',
                            'SO_startDOY', 'SO_endDOY'))
  
  # reconstruct data.frame to have rainfall and ndvi
  outndvi<-outm[((nrow(out)*125)+1):((nrow(out)*125)*2),]$value
  outm<-outm[1:(nrow(out)*125),]
  outm$value2<-outndvi
  
  outm$variable<-as.numeric(substr(outm$variable,
                                   2,nchar(as.character(outm$variable))))
  
  outm<-outm[order(outm$ID),]
  
  
  out2<-outm %>% group_by(ID) %>%
    mutate(cuck_pres=ifelse(variable %in% 
                              (SO_startDOY:SO_endDOY),"Y", "N"))
  # warning comes from all the DOY values in the group. not an issue
  
  int_out<-rbind(int_out, out2)
}

# write out

write.csv(int_out, 'data/spring_rainfall_by_stopover_2018.csv', row.names = F, quote=F)


### Spatial extraction and analyses.
### comparison between rainfall at start of stopover n
### against rainfall of all other stopovers extract for n's date

int_out2<-NULL
for( i in 1:nrow(dat)) # or should I do per row
{
  
  indat<-dat[i,]
  
  # cool the 'get' command grabs the collect raster set from the memory
  ras_temp<-get(paste0('r', indat$year))
  
  ras_temp<-subset(ras_temp, indat$SO_startDOY)
  
  # add 50 km buffer as per stopover definition. buffer=50000
  ext<-extract(ras_temp, dat[,7:8], buffer=50000, fun=median)
  
  ext[ext<0]<-NA # remove negative values '-99'
  
  out<-data.frame(pa=0,ptt=dat$ptt, year=dat$year, 
                  SO_startDOY=indat$SO_startDOY, ext)
  out[i,]$pa<-1
  
  int_out2<-rbind(int_out2, out)
}

# write out

write.csv(int_out2, 'data/spring_rainfall_stopover_comparison.csv', row.names = F, quote=F)

