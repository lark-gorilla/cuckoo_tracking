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

