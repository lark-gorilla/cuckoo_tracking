## remote sensing data Rainfall and NDVI extract
## 30/05/2018

#### work around for different laptop/desktop directories ####
#### !!!! RUN THIS !!!! ####
if(Sys.info()['nodename']=="D9L5812"){
  setwd("C:/cuckoo_tracking")}else{
    setwd("N:/cuckoo_tracking")}
#### !!!!!!!!!!!!!!!!!! ####

# Get cuckoo data

#dat<-read.csv('data/stopover_bestofday_2018_1daymin_recalc_spring_mig.csv', h=T)
#dat$dead<-0
#dat1<-read.csv('data/stopover_bestofday_2018_1daymin_recalc_spring_mig_dead.csv', h=T)
#dat1$dead<-1
#dat1$biome1<-NULL
#dat1$biome2<-NULL

#dat<-rbind(dat,dat1)
#remove non- west african rows

# Use detailed coord data now for extract

dat<-read.csv('data/stopover_bestofday_2018_1daymin_recalc_spring_mig_detailcoords.csv', h=T)


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
r2014<-subset(ras, 944:(944+124))
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

tifz<-tifz[-grep("xml", tifz)] # remove qgus xml additional files

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
#MODIS naming convention means that the date on the product name is the start
# of the 16 day period iver which data is averaged see here for more:
#https://gis.stackexchange.com/questions/218564/how-do-modis-products-naming-conventions-work


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
  #ext<-extract(ras_temp, indat[,7:8], buffer=50000, fun=median)
  
  ext<-extract(ras_temp, indat[,23:24]) # no buffer for detailedcoord points as most
  # are lc class 0-3 so < 1500m
  
  ext[ext<0]<-NA # remove negative values '-99'
  
  dimnames(ext)[[2]]<-1:(length(dimnames(ext)[[2]]))
  
  out<-data.frame(ptt=indat$ptt, country=indat$country, year=i, 
                  SO_startDOY=indat$SO_startDOY, 
                  SO_endDOY=indat$SO_endDOY, timestamp=indat$timestamp, ext)
  
  ## NDVI  extract
  
  ndvi_temp<-subset(rastack, which(substr(tifz, 14,17)==i))
  
  #ext2<-extract(ndvi_temp, indat[,7:8], buffer=50000, fun=median, na.rm=T)
  
  ext2<-extract(ndvi_temp, indat[,23:24], na.rm=T) # no buffer
  
  ext2<-ext2*0.0001 # apply scaling to get NDVI from -1 to 1
  
  # loop to rep ndvi values over each of the 8 days they represent
  extrep<-NULL
  for(j in 1:15) # limits to day 121
  {
   extrep<-cbind(extrep, matrix(rep(ext2[,j], 8), ncol=8)) 
  }
  extrep<-cbind(extrep, matrix(rep(ext2[,16], 5), ncol=5))
  

  out<-data.frame(out, extrep) # add the ndvi data to rainfall df
  
  out$ID<-with(out, paste(ptt, year, SO_startDOY, sep="_"))
  
  outm<-melt(out, id.vars=c('ID', 'ptt', 'country', 'year',
                            'SO_startDOY', 'SO_endDOY', 'timestamp'))
  
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

write.csv(int_out, 'data/spring_rainfall_NDVI_by_stopover_detailcoords_2018_dead.csv', row.names = F, quote=F)

### Add in GRIMMS NDVI to compare

int_out<-read.csv('data/spring_rainfall_NDVI_by_stopover_detailcoords_2018_dead.csv', h=T)

grimms<-read.csv('data/GRIMMS_0.25deg_NDVI_climatology.csv',h=T)
grimms$LIS<-as.character(grimms$LIS)

# need to join stopover ID to correct LIS square ..
library(raster)

# remake a long and lat raster and then join values to make key

rs<-stack(raster(nrows=(140*4), ncols=(360*4), xmn=-180, xmx=180, ymn=-60, ymx=80, 
           crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs',
           vals=rep((1:1440), 560)),

raster(nrows=(140*4), ncols=(360*4), xmn=-180, xmx=180, ymn=-60, ymx=80, 
           crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs',
           vals=sort(rep((1:560), 1440), decreasing=T)))


#ext<-extract(rs, dat[,7:8])
ext<-extract(rs, dat[,23:24]) # detailed coords

# correct code nuances
dat$LIS=paste0('0', ext[,1], "_", ext[,2])

dat$ID<-paste(dat$ptt, dat$year, dat$SO_startDOY, dat$timestamp,sep="_")

grimms$year<-substr(grimms$ORDINAL.DATE,1,4)

dat_join<-dat[,c(9,25,26)]
dat_join$year<-as.character(dat_join$year)


# subsets grimms to only LIS squares that match in specific years
#and adds corresponding ID column to grimms
grimms<-inner_join(grimms, dat_join, by=c('LIS', 'year'))

# add day of year variable to match int_out
grimms$variable<-substr(grimms$ORDINAL.DATE,6,8)
grimms$variable<-as.integer(grimms$variable)

# split and rename/drop columns before merge 
grimms$year<-NULL
grimms$LIS<-NULL
grimms$SAMPLE.COUNT<-NULL
grimms$MEAN.COUNT<-NULL
grimms$MEAN.VALUE<-NULL
grimms$START.DATE <-NULL
grimms$END.DATE <-NULL
grimms$ORDINAL.DATE <-NULL

## extra code to fill the relicates of 'variable' ahead of the join 
grimms2<-do.call('rbind', list(grimms,grimms,grimms,grimms,
                              grimms,grimms,grimms,grimms))

grimms2<-grimms2[order(grimms2$ID, grimms2$variable),]

grimms2<-grimms2[grimms2$variable!=145,] # remove dataset 145 (d145-153)
# as not needed, actually above 125 but anyways....

# repeat sequence 1:144 for each ID (times 2 cos both satellites)
grimms2$v2<-rep((sort(rep(1:144, 2))), length(unique(grimms2$ID)))

#set grimms2 as grimms and v2 as variable
grimms<-grimms2
grimms$variable<-grimms$v2
grimms$v2<-NULL


# split into seperate satellites
gaqua<-grimms[grimms$sat=='AQUA',]

gaqua$sat<-NULL
names(gaqua)[1]<-'aquaNDVI'
names(gaqua)[2]<-'aquaANOM'

gterr<-grimms[grimms$sat=='TERR',]

gterr$sat<-NULL
names(gterr)[1]<-'terrNDVI'
names(gterr)[2]<-'terrANOM'


# join both aqua and terra to int_out, for the variable (days)
# that are not matched in the grimms datast we get NA, need to fill down

#redo ID to include timestamp
int_out$ID<-paste(int_out$ptt, int_out$year,
                  int_out$SO_startDOY, int_out$timestamp,sep="_")

int2<-left_join(int_out, gaqua, by=c('ID', 'variable'))
int2<-left_join(int2, gterr, by=c('ID', 'variable'))

# legit NAs from GIMMS dataset
which(!is.na(int2$aquaNDVI))

write.csv(int2, 'data/spring_rainfall_NDVI_GRIMMS_by_stopover_detailcoords_2018_dead.csv', row.names = F, quote=F)


#################
# extra code to extract TAMSAT rainfall and anomoly data also and
# add to existing file

library(raster)
library(dplyr)

int2<-read.csv('data/spring_rainfall_NDVI_GRIMMS_by_stopover_detailcoords_2018_dead.csv', h=T)

# get coord data
dat<-read.csv('data/stopover_bestofday_2018_1daymin_recalc_spring_mig_detailcoords.csv', h=T)

# join lat long

dat$ID=paste(dat$ptt, dat$year, dat$SO_startDOY, dat$timestamp, sep='_')

int2$ID <-as.character(int2$ID)

int2<-left_join(int2, select(dat, ID, long, lat), by='ID')

# rearrange
int2<- int2 %>% select(ID, ptt, year, SO_startDOY, SO_endDOY, country,
                       timestamp, long, lat, everything())

int2$tamRAIN<-NA

int2$tamRAINanom<-NA

# set up key to get data

int2$key<-'01-pt1'
int2[int2$variable %in% 6:10,]$key <-'01-pt2'
int2[int2$variable %in% 11:15,]$key <-'01-pt3'
int2[int2$variable %in% 16:20,]$key <-'01-pt4'
int2[int2$variable %in% 21:25,]$key <-'01-pt5'
int2[int2$variable %in% 26:30,]$key <-'01-pt6'

int2[int2$variable %in% 31:35,]$key <-'02-pt1'
int2[int2$variable %in% 36:40,]$key <-'02-pt2'
int2[int2$variable %in% 41:45,]$key <-'02-pt3'
int2[int2$variable %in% 46:50,]$key <-'02-pt4'
int2[int2$variable %in% 51:55,]$key <-'02-pt5'
int2[int2$variable %in% 56:60,]$key <-'02-pt6'

int2[int2$variable %in% 61:65,]$key <-'03-pt1'
int2[int2$variable %in% 66:70,]$key <-'03-pt2'
int2[int2$variable %in% 71:75,]$key <-'03-pt3'
int2[int2$variable %in% 76:80,]$key <-'03-pt4'
int2[int2$variable %in% 81:85,]$key <-'03-pt5'
int2[int2$variable %in% 86:90,]$key <-'03-pt6'

int2[int2$variable %in% 91:95,]$key <-'04-pt1'
int2[int2$variable %in% 96:100,]$key <-'04-pt2'
int2[int2$variable %in% 101:105,]$key <-'04-pt3'
int2[int2$variable %in% 106:110,]$key <-'04-pt4'
int2[int2$variable %in% 111:115,]$key <-'04-pt5'
int2[int2$variable %in% 116:120,]$key <-'04-pt6'

int2[int2$variable %in% 121:125,]$key <-'05-pt1'


for(i in unique(int2$year))
{
  for(j in unique(int2[int2$year==i,]$key))
  {
  ras_rf<-raster(paste('C:/cuckoo_tracking/sourced_data/TAMSAT/rainfall/', 
                       i, '/', substr(j, 1,2), '/rfe',i, '_', j, '.v3.nc',sep=""))  
  ras_anom<-raster(paste('C:/cuckoo_tracking/sourced_data/TAMSAT/rainfall_anom/', 
                       i, '/', substr(j, 1,2), '/rfe',i, '_', j, '_anom.v3.nc',sep=""))  

  int2[int2$year==i & int2$key==j,]$tamRAIN<-
  extract(ras_rf, int2[int2$year==i & int2$key==j,8:9], na.rm=T)  
  
  int2[int2$year==i & int2$key==j,]$tamRAINanom<-
    extract(ras_anom, int2[int2$year==i & int2$key==j,8:9], na.rm=T) 
  
  print(paste(i, j))
  }
  
}

write.csv(int2, 'data/spring_rainfall_NDVI_GRIMMS_TAMSAT_by_stopover_detailcoords_2018_dead.csv', row.names = F, quote=F)



## OLD

### Spatial extraction and analyses.
### comparison between rainfall at start of stopover n
### against rainfall of all other stopovers extract for n's date

##for spatial apporach calc square centre coords

int_out2<-NULL
for( i in 1:nrow(dat)) 
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



