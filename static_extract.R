## remote sensing data Rainfall and NDVI extract
## 30/05/2018

#### work around for different laptop/desktop directories ####
#### !!!! RUN THIS !!!! ####
if(Sys.info()['nodename']=="D9L5812"){
  setwd("C:/cuckoo_tracking")}else{
    setwd("N:/cuckoo_tracking")}
#### !!!!!!!!!!!!!!!!!! ####


# Use detailed coord data now for extract

dat<-read.csv('data/stopover_bestofday_2018_1daymin_recalc_spring_mig_detailcoords.csv', h=T)


dat<-dat[which( ! dat$country%in% c('Algeria', 'France', 'Italy', 'Morocco' ,'Spain',
                                    'United Kingdom', 'Portugal')),]


# data accessed via the portal: http://maps.elie.ucl.ac.be/CCI/viewer/index.php



library(raster) 

ras<-raster("sourced_data/ESA_CCI_landcover/ESACCI-LC-L4-LC10-Map-20m-P1Y-2016-v1.0/ESACCI-LC-L4-LC10-Map-20m-P1Y-2016-v1.0.tif")

#landcover<-extract(ras, dat[, 23:24], buffer=150, df=T) # add 1500 m buffer as landcover is c. 250 m resolution
## only use 150m cos 1500 grabs too many!

#landcover2<-extract(ras, dat[, 23:24], buffer=1500, fun=function(x){table(x)}) # add 1500 m buffer as landcover is c. 250 m resolution
## only use 150m cos 1500 grabs too many!

landcover3<-extract(ras, dat[, 23:24], buffer=1500) # add 1500 m buffer as landcover is c. 20 m!! resolution
## on
lc3<-lapply(landcover3, FUN=function(x)
{
  data.frame(matrix(table(x), ncol=length(table(x)),
                    dimnames=list(NULL,unlist(dimnames(table(x))))))
})

library(dplyr)

lc3a<-bind_rows(lc3) # binds up rows if columns don't match gives an NA - sweet!

dat<-cbind(dat, lc3a)

names(dat)[names(dat)=='X1']<-"Trees"
names(dat)[names(dat)=='X2']<-"Shrubs"
names(dat)[names(dat)=='X3']<-"Grasslands"
names(dat)[names(dat)=='X4']<-"Croplands"
names(dat)[names(dat)=='X5']<-"Aquatic veg reg flooded"
names(dat)[names(dat)=='X7']<-"Bare areas"
names(dat)[names(dat)=='X8']<-"Built up areas"
names(dat)[names(dat)=='X10']<-"Open water"
names(dat)[names(dat)=='X200']<-"NA"


write.csv(dat, "data/stopover_bestofday_2018_1daymin_recalc_spring_mig_detailcoords_landcov_ext.csv", quote=F, row.names=F)

## read in rivers layer

library(rgdal)
library(raster)

setwd("C:/cuckoo_tracking/sourced_data/rivers")
rivers<-readOGR(layer="rivers_africa_37333",
                dsn="rivers_africa_37333") # different linux/windows

spdat<-SpatialPoints(dat[,23:24], proj4string = CRS(projection(rivers)))

#crop rivre layer

library(sf)

riversSF <- st_as_sf(rivers)
spdatSF <- st_as_sf(spdat)
rectangleSF <- st_as_sf(as(raster::extent(spdat)+3, "SpatialPolygons"))
st_crs(rectangleSF)<-st_crs(spdatSF)


plot(st_geometry(rectangleSF), border=2)
plot(st_geometry(spdatSF), add=T)

riversCROP <- st_intersection(riversSF, rectangleSF)

plot(st_geometry(riversCROP['Strahler']))
plot(st_geometry(spdatSF), add=T, col=2)


# coerce back to sp
riversCROP <- as(riversCROP, 'Spatial')

#reporject both

spdatproj<-spTransform(spdat, CRS=CRS("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))


riversproj<-spTransform(riversCROP, CRS=CRS("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

# byid creaes matrix of min distances from every river to point
rivers2<-gDistance(spdatproj, riversproj, byid=T)

# apply function to find the nearest river from the matrix to each point
dat$d_river<-apply(rivers2, 2, min)

# do sma efor Strahler order => 2 indicating larger rivers

rivers2<-gDistance(spdatproj, riversproj[riversproj$Strahler>1,], byid=T)

# apply function to find the nearest river from the matrix to each point
dat$d_river_str2<-apply(rivers2, 2, min)

write.csv(dat, "data/stopover_bestofday_2018_1daymin_recalc_spring_mig_detailcoords_landcov_ext_rivers.csv", quote=F, row.names=F)

### code to attribute stopover bestofday locations with env data and depature form 
### West Africa details for vis in GIS

library(dplyr)

soversWA<-read.csv("data/stopover_bestofday_2018_1daymin_recalc_spring_mig_detailcoords_landcov_ext_rivers.csv", h=T)

dat<- read.csv('C:/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead.csv', h=T)

# set NA or manually correct cuckoo depature dates that
# are incorrect, from manual examination

dat[dat$ptt==115586 & dat$year==2014,]$DEPwestAF<-106
dat[dat$ptt==134955 & dat$year==2015,]$DEPwestAF<-NA
dat[dat$ptt==134956 & dat$year==2015,]$DEPwestAF<-NA

# and dead ones
dat[dat$ptt==128296 & dat$year==2014,]$DEPwestAF<-NA

env<-read.csv('C:/cuckoo_tracking/data/spring_rainfall_NDVI_GRIMMS_TAMSAT_emodis_by_stopover_detailcoords_2018_dead.csv', h=T)

names(env)[names(env)=='value']<-'precip'
names(env)[names(env)=='value2']<-'ndvi'

soversWA1<-left_join(soversWA, dat[,c(1,2,6)], by=c('ptt', 'year'))

soversWA1[,25:31]<-round(soversWA1[,25:31]/rowSums(soversWA1[,25:31], na.rm=T) *100)                      

env[which(env$precip>500),]$precip<-0 # remove erroneous vals
env[which(is.na(env$precip)),]$precip<-0 # fix na

dat2<-env %>% group_by(year, ptt, timestamp) %>% dplyr::mutate(cumrf=cumsum(precip))

dat2<-dplyr::arrange(dat2, ptt, year, timestamp, variable)
dat2$tamRAIN[which(!(1:118625 %in% seq(5,118625, 5)))]<-0
dat2<-dat2 %>% group_by(year, ptt, timestamp) %>% dplyr::mutate(tamcumrf=cumsum(tamRAIN))

dat2.1<-subset(dat2, cuck_pres=='Y')

dat2.2<- dat2.1 %>% group_by(year, ptt, timestamp) %>%
  summarise_all(mean, na.rm=T) # warnings cos of factors

soversWA2<-left_join(soversWA1, dat2.2[,c(1,2,3,14:24)], by=c('ptt', 'year', 'timestamp'))

write.csv(soversWA2, "data/stopover_bestofday_2018_1daymin_recalc_spring_mig_detailcoords_landcov_ext_rivers_precip_ndvi.csv", quote=F, row.names=F)


