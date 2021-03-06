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

#tifz<-list.files("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/NDVI")

# And for quality fields
tifz<-list.files("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/QA_qual")

#order them by date rather than satellite (Terra and Aqua)
#tifz<-tifz[order(substr(tifz, 14,21))]
#
tifz<-tifz[order(substr(tifz, 17, 24))]

p<-projection(raster(paste0("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/QA_qual/",tifz[1])))

for(i in tifz)
{
  gdalwarp(srcfile=paste0("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/QA_qual/",i),
           dstfile=paste0("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/QA_qual_WGS/",i),
           s_srs=p,
           t_srs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs',
           output_Raster=TRUE,
           overwrite=TRUE,verbose=TRUE)
  print(i)  
}
# much faster!

# Ok now we apply a savitzky-golay-filter to smooth the NDVI timeseries
# we will try two methods:
#1) using raw ndvi data and
#2) after masking out poor quality pixels
#https://matinbrandt.wordpress.com/2014/12/02/smoothingfiltering-a-ndvi-time-series-using-a-savitzky-golay-filter-and-r/

#stack up data
# make quality data list first
tifz<-list.files("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/QA_qual")

tifz<-tifz[order(substr(tifz, 17, 24))]
#remove last one
tifz<-tifz[1:124]

# then make ndvi list from it (cos no 2011 quality data but is ndvi data)
ndvitifz<-gsub("QA_qual", "NDVI", tifz)

for(i in tifz)
{
  if(which(i==tifz)==1){
    qastack<-raster(paste0("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/QA_qual_WGS/",i))
    ndvistack<-raster(paste0("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/NDVI_WGS/",ndvitifz[which(i==tifz)]))
    
  }else{
   
    qastack<-stack(qastack, paste0("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/QA_qual_WGS/",i))  
    ndvistack<-stack(ndvistack, paste0("C:/cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/NDVI_WGS/",ndvitifz[which(i==tifz)])) 
  }
  print(i)  
}

# recalss # too slow to apply on the stack so 
#qastack[qastack>1]<-NA
#qastack[qastack==0]<-1

# finally achieved using gdal_calc from osgeo4w cmd line

#cd /cuckoo_tracking/sourced_data/MODIStsp_NDVI/VI_16Days_1Km_v6/QA_qual_WGS

#dir

#for  %i in (*.tif) do gdal_calc -A  "C:\cuckoo_tracking\sourced_data\MODIStsp_NDVI\VI_16Days_1Km_v6\QA_qual_WGS\%i" --outfile="C:\cuckoo_tracking\sourced_data\MODIStsp_NDVI\VI_16Days_1Km_v6\QA_qual_WGS_mask\%i" --calc="1*(A==0)+1*(A==1)" --NoDataValue=0 --overwrite



# mask out areas that were quality level 2 or 3
maskedndvi<-ndvistack*qastack

# run filter per year on raw and masked ndvi datasets

ndvi_yr<-subset(ndvistack, which(substr(tifz, 14,17)==2012))


library(signal)
library(zoo)

fun <- function(x) {
  v=as.vector(x)
  z=substituteNA(v, type=”mean”)
  MODIS.ts2 = ts(z, start=c(2005,1), end=c(2010,23), frequency=23)
  x=sgolayfilt(MODIS.ts2, p=1, n=3, ts=30)
}

MODIS.filtered <- calc(MODIS, fun)


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

writeRaster(r, "C:/cuckoo_tracking/data/spatial/marks_LIS_ras.tif", overwrite=T)

#make individual rasters as there is some issue with storing 
# combined values as a decimal

r1<-raster(nrows=(140*4), ncols=(360*4), xmn=-180, xmx=180, ymn=-60, ymx=80, 
          crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs',
          vals=rep((1:1440), 560))

writeRaster(r1, "C:/cuckoo_tracking/data/spatial/marks_LIS_ras_lon.tif", overwrite=T)

r2<-raster(nrows=(140*4), ncols=(360*4), xmn=-180, xmx=180, ymn=-60, ymx=80, 
          crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs',
          vals=sort(rep((1:560), 1440), decreasing=T))

writeRaster(r2, "C:/cuckoo_tracking/data/spatial/marks_LIS_ras_lat.tif", overwrite=T)

#stack em
rs<-stack(r, r1, r2)


# nice to have a grid but huge file for whole world, 
# so just subset to WA

r3<-crop(rs, extent(c(-18, 20, -10, 12)))
plot(r3)

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

# Update, actually I'm gonna pull data for all the cells in the WA region incase needed for 
# different analyses later

library(rgdal)

setwd("C:/cuckoo_tracking/data")
r.poly<-readOGR(dsn="spatial",
         layer="marks_LIS_grid")

data("wrld_simpl", package = 'maptools')

plot(r.poly)
plot(wrld_simpl, add=T, border=2)
r.poly2<-r.poly[wrld_simpl,] # simple clip
plot(r.poly2, add=T, border=3) # cool

indexes<-r.poly2@data$layer_1
# replace . with _ so to match website 
indexes<-gsub(pattern="\\.", x=as.character(indexes), replacement="_")

# fix to get include indexes that have 0 at end
indexes<-ifelse(nchar(indexes)==6, paste0(indexes, '0'), indexes)

# ok so now we pull the csv for each of these 0.25degree cells from the GRIMMS
# server there are ~ 6000 indexes so 6000 files to download. IF a LIS square I have constructed
# does not exist on the server ( near coast maybe) it downloads a blank csv, these
# should be identifiable after as they are smaller (bytes rather than Kb) so cen be deleated)

for(i in indexes)
{
download.file(url=paste0('https://glam1.gsfc.nasa.gov/_/csv_2.1?src=https%3A%2F%2Fglam1.gsfc.nasa.gov%2Ftbl%2Fv4%2FMYD%2FGMYD09Q1.6v1.NDVI.MOD44W_2009_land.LIS%2Ftxt%2FGMYD09Q1.6v1.NDVI.MOD44W_2009_land.LIS.',
                         '0',i,'.tbl.txt'),
              destfile = paste0('C:/cuckoo_tracking/sourced_data/GRIMMS_NDVI/AQUA_','0',i,'.csv'))
}
# note the 0 before index i in the url to match the website

# same for TERRA

for(i in indexes)
{
  download.file(url=paste0('https://glam1.gsfc.nasa.gov/_/csv_2.1?src=https%3A%2F%2Fglam1.gsfc.nasa.gov%2Ftbl%2Fv4%2FMOD%2FGMOD09Q1.6v1.NDVI.MOD44W_2009_land.LIS%2Ftxt%2FGMOD09Q1.6v1.NDVI.MOD44W_2009_land.LIS.',
                           '0',i,'.tbl.txt'),
                destfile = paste0('C:/cuckoo_tracking/sourced_data/GRIMMS_NDVI/TERRA_','0',i,'.csv'))
}
# note the 0 before index i in the url to match the website

# now comile into one dataset

filez<-list.files('C:/cuckoo_tracking/sourced_data/GRIMMS_NDVI')

out<-NULL
for(i in filez)
{
temp2<-read.csv(paste0('C:/cuckoo_tracking/sourced_data/GRIMMS_NDVI/',i), h=T, skip=12, na.strings='#N/A')

temp2$sat=substr(i, 1, 4)
temp2$LIS=substr(i, 6, 13)

temp2$SOURCE<-NULL
temp2$year<-as.numeric(substr(temp2$ORDINAL.DATE,1,4))
temp2<-temp2[temp2$year>2011,]
temp2$year<-NULL
temp2$ord<-as.numeric(substr(temp2$ORDINAL.DATE,6,8))
temp2<-temp2[temp2$ord<153,]
temp2$ord<-NULL
out<-rbind(out, temp2)
print(i)
}

# quick fix cos of AQUA nchar=4, TERRA nchar=5 error cock up
out[out$sat=='TERR',]$LIS<-out[out$sat=='AQUA',]$LIS

write.csv(out, 'C:/cuckoo_tracking/data/GRIMMS_0.25deg_NDVI_climatology.csv', quote=F, row.names=F)

# A final extract from the http://maps.elie.ucl.ac.be/CCI/viewer/index.php viewer
# this is mainly to get landcover but also has NDVI and fire stuff

# alternate attempt to grab huge raster and subset to west africa to 
# reduce size


## code to grab modis ndvi anomaly data

for(i in c(1201:1214, 1301:1314, 1401:1414, 1501:1514,
           1601:1614, 1701:1714, 1801:1813))
{
  download.file(url=paste0('https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/fews/web/africa/west/dekadal/emodis/ndvi_c6/mediananomaly/downloads/dekadal/wa',
                           i,'stmdn.zip'),
                destfile = paste0('F:/NDVIanom/ndvianom', i, '.zip'))
  
  download.file(url=paste0('https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/fews/web/africa/west/dekadal/emodis/ndvi_c6/temporallysmoothedndvi/downloads/dekadal/wa',
                           i,'.zip'),
                destfile = paste0('F:/NDVIsmooth/ndvismooth', i, '.zip'))
}


# to grab rfe rainfall and long term mean data

expg<-expand.grid(1801:1805, 1:3)
idz<-paste0(expg[,1], expg[,2])
for(j in idz)
{
  download.file(url=paste0('https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/fews/web/africa/west/dekadal/rfe/dekadrainfall/downloads/dekadal/w',
                           j,'rb.zip'),
                destfile = paste0('F:/RFErainfall/RFE', j, '.zip'))
  
  download.file(url=paste0('https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/fews/web/africa/west/dekadal/rfe/longtermanom/downloads/dekadal/wlm',
                           substr(j, 3,5),'rb.zip'),
                destfile = paste0('F:/RFEltm/RFEltm', substr(j, 3,5), '.zip'))
  
}