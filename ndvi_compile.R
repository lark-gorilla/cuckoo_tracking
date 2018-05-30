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
