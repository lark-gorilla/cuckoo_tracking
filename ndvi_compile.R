## 25/05/18
## Downloading, processing and extracting MODIS NDVI data

# Following tutorial:
#https://cran.r-project.org/web/packages/MODIStsp/vignettes/MODIStsp.pdf

#install.packages("gWidgetsRGtk2")
library(gWidgetsRGtk2)

library(devtools)
install_github("lbusett/MODIStsp")

library(MODIStsp)
MODIStsp()
# Thats all you need to use this tool! nice!

# OK so I ran the MODIStsp tool for 1 km NDVI from Terra and 
# Aqua see here for run parameters:
#C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/MODIStsp_parameters.png

## OK now explore and compile the data

library(raster)

tifz<-list.files("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/NDVI")

#order them by date rather than satellite (Terra and Aqua)
tifz<-tifz[order(substr(tifz, 14,21))]

for(i in tifz)
{
if(which(i==tifz)==1){
  rastack<-raster(paste0("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/NDVI/",i))
  }else{
  rastack<-stack(rastack, paste0("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/NDVI/",i))  
  }
print(i)  
}


plot(raster::subset(rastack, 1))

# data needs scaling factor (0.0001) applied to adjust to wothin
# NDVI bounds (-1 to 1)
# Will do at later stage

# # OK so thats all great but I need them in WGS format for visualising
# raster's 'repojectRaster' seems to take ages so try gith GDAL

library(gdalUtils)

tifz<-list.files("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/NDVI")

#order them by date rather than satellite (Terra and Aqua)
tifz<-tifz[order(substr(tifz, 14,21))]

for(i in tifz)
{
  gdalwarp(srcfile=paste0("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/NDVI/",i),
           dstfile=paste0("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/NDVI_WGS/",i),
           s_srs=projection(rastack),
           t_srs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs',
           output_Raster=TRUE,
           overwrite=TRUE,verbose=TRUE)
  print(i)  
}
# much faster!


##### Different method !
### Using GRIMMS cleaned MODIS NDVI product here: https://glam1.gsfc.nasa.gov/
### Make Spatial grid that matches extractable one on website (LIS 0.25 degree)
### Index grid to match website then assign each stopover to a 0.25 cell and 
### pull these from the website. This provides a nifty csv with NDVI and anomaly
### data in the cell from 2003 until present.
library(raster)
library(sp)
library(rgdal)

# LIS grid is full longitude but only -60 to 80 latitude
# these are index values 
exp1<-(expand.grid(1:1440, 560:1))
v2<-as.numeric(paste(exp1[,1], exp1[,2], sep='.'))

r<-raster(nrows=(140*4), ncols=(360*4), xmn=-180, xmx=180, ymn=-60, ymx=80, 
       crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs', vals=v2)

writeRaster(r, "C:/cuckoo_tracking/data/spatial/marks_LIS_ras.tif")

# nice to have a grid but huge file for whole world, 
# so just subset to WA

r2<-crop(r, extent(c(-18, 20, -10, 12)))
plot(r2)

map('world', add=T)
r.poly <- rasterToPolygons(r2, dissolve=F )

setwd("C:/cuckoo_tracking/data")
writeOGR(obj=r.poly, dsn="spatial",
         layer="marks_LIS_grid", driver="ESRI Shapefile",
         overwrite_layer = T)

## too big just leave as ras
#sgdf<-as(r, 'SpatialGridDataFrame')

# OK have had a look in QGIS all is correct, now simply bring in stopover data and
# extract the LIS cell ID elow each stopover. There are some issues here: the cells are 
# 0.25 degree so the grid won't always have stopover points in the centre, so which
# cell should be extract from? Could average from neighbours i.g. 4 nearest cells or
# just take the one over which the point falls. The important thing to rememebr is stopover
# points are the median location of the stopover (which can cover up to 50km between points)



https://glam1.gsfc.nasa.gov/wui/4.1.0/chart.html?sat=MYD&mean=2003-2015&layer=NDVI&crop=NONE&type=LIS&n=1&numIds=1&level=null&initYr=2018&id0=0718_269


download.file(url='https://glam1.gsfc.nasa.gov/_/csv_2.1?src=https%3A%2F%2Fglam1.gsfc.nasa.gov%2Ftbl%2Fv4%2FMYD%2FGMYD09Q1.6v1.NDVI.MOD44W_2009_land.LIS%2Ftxt%2FGMYD09Q1.6v1.NDVI.MOD44W_2009_land.LIS.0718_269.tbl.txt',
              destfile = 'C:/cuckoo_tracking/sourced_data/GRIMMS_NDVI/test.csv')



