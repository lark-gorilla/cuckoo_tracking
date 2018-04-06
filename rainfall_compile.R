## 26/03/18
## Downloading rainfall data
## Using the 'Climate Data Record (CDR) of Precipitation Estimation from Remotely
## Sensed Information using Artificial Neural Networks (PERSIANN-CDR)' dataset
# data is avaiable at daily 0.25 degree resolution, from 2000 until preseant
# There is also PERSIANN-CCS  a hi-res (4km) product avilable daily from 2003
# There is also PERSIANN_CDR a long term dataset a(0.25 degree) back until 1983

#
# data accessed via the portal: http://chrsdata.eng.uci.edu

#### work around for different laptop/desktop directories ####
#### !!!! RUN THIS !!!! ####
 if(Sys.info()['nodename']=="D9L5812"){
   setwd("C:/cuckoo_tracking")}else{
    setwd("N:/cuckoo_tracking")}
#### !!!!!!!!!!!!!!!!!! ####

# read in monthly data in netcdf format ok

library(raster) # needs ncdf4 installed

# stacks up all the layers
#ras<-stack("sourced_data/PERSIANN_rainfall/monthly/PERSIANN_2018-03-26035525am.nc")
ras<-stack("sourced_data/PERSIANN_rainfall/daily/PERSIANN_2018-03-26040045am.nc")


# warning in projection 

print(raster(ras, layer=1))

# basically it is specifcying WGS but CRS gets it a bit wrong

ras@crs
#fix 
ras@crs<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


plot(raster(ras, layer=5))

library(maps)
map('world', add=T)

# Get cuckoo data

dat<-read.csv('data/stopover_bestofday_1daymin_recalc_spring_mig.csv', h=T)

# Make are of WA extraction

data("wrld_simpl", package = 'maptools')

library('sf')

world <- st_as_sf(wrld_simpl)
rectangle <- st_as_sf(as(raster::extent(-18, 14, 4, 12), "SpatialPolygons"))
st_crs(rectangle)<-st_crs(world)

# difference between world polygons and the rectangle
inty <- st_intersection(rectangle, st_union(world))

# coerce back to sp
inty <- as(inty, 'Spatial')

# plot the result
plot(inty); map('world', add=T, col=3)

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

# With each annual raster, the position [x] of the layer is the DOY

# I THINK that it is actually pulling the rainfall DOY before the one in
# question, so 24hr time lag.. which is kinda what we want

for( i in 1:124) # or should I do per row
{
 
  indat<-which(by(dat, 1:nrow(dat), FUN=function(x){i %in% x$SO_startDOY:x$SO_endDOY}))
  
  if(length(indat)==0){print(paste0("no cuckos for date ",i)); next}
    
  d_temp<-dat[indat,]
  
  int_out<-NULL
  for(j in unique(d_temp$year))
  {
    d2_temp<-d_temp[d_temp$year==j,] 
    
    # cool the 'get' command grabs the collect raster set from the memory
    ras_temp<-subset(get(paste0('r', j)), i)
    
    # add 50 km buffer as per stopover definition. buffer=50000
    ext<-extract(ras_temp, d2_temp[,7:8])
    
    out<-data.frame(type='cuckooIS', ptt=d2_temp$ptt, year=j, day=i, rain=ext)
    
    ext2<-extract(ras_temp, dat[,7:8])
    
    out2<-data.frame(type='cuckooALL', ptt=dat$ptt, year=dat$year, day=i, rain=ext2)
    
    #ext2<-extract(ras_temp, as(inty, 'SpatialPolygons'))
    
    #out2<-data.frame(type='WA', ptt=999999, year=j, day=i, rain=ext2)
    #names(out2)[5]<-'rain'
    
    out3<-rbind(out, out2)
    
    int_out<-rbind(int_out, out3)
  }
   
  
      
  
}


library(ggplot2)
library(scales)
p<-ggplot(data=int_out, aes(x=rain, colour=type))
p+geom_density(aes(y=..scaled..))+facet_wrap(~year)

for (i in 1:length(r2015)){
   plot(subset(r2015, i), main=i)
   map('world', add=T)
   readline('')
   }

# ok really wanna do Chris' approach per stopover over time and see when cuckoos arrive
# also for NDVI



# name dat is 'units: days since 2000-03-01' I took up until June 2017
names(ras)<-paste(c("03", "04", "05", "06", "07", "08", "09", "10", "11", "12", 
  rep(c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), 16),
  "01", "02", "03", "04", "05", "06"),

c(rep("2000", 10), rep("2001", 12), rep("2002", 12),
  rep("2003", 12), rep("2004", 12),rep("2005", 12),
  rep("2006", 12),rep("2007", 12),rep("2008", 12), 
  rep("2009", 12),rep("2010", 12),rep("2011", 12),
  rep("2012", 12),rep("2013", 12),rep("2014", 12),
  rep("2015", 12),rep("2016", 12),rep("2017", 6)), sep="_")

plot(subset(ras, grep("X03",names(ras))))

# ok looks fine


############# VISUALISING rainfall in West Africa ###############

#crop rasters to ROI
wa_ras<-crop(ras, extent(-17, 33, -17, 26))

# I'm not sure how tmap can facet rasterstacks.. dont think it can so we 
# convert to SpatialPixelsDataFrame and try using sf

# not sure what I'm doing here

# got excited about the tm_animate thing
# but am struggling to facet with raster
# think im making it difficult

# aims: to learn sf, tmap
# explore how birds and rainfall move
# lets actually do saome spatial analyses
# using tidyverse power. also make nice map
# finally animation is not the be and end all..

c2<-rasterToContourPoly(subset(wa_ras, 1), nclass=10)


c1<-cut(subset(wa_ras, 1), breaks=c(0, 200, 400, 600, 800, 1000))
p1<-rasterToPolygons(c1, dissolve=T)

wa_pix<-as(wa_ras, 'SpatialPixelsDataFrame') # as() very powerful from Raster

# into a 'sf' object

pix_sf<-st_as_sf(wa_pix) # does not accept SpatialGridDataFrame so did Pixels instead

# some plotting using tmap
library(tmap)

tmap_mode('plot')

d4<-read.csv("data/stopover_1daymin_spring_mig.csv", h=T)





tm_shape(subset(ras,1), projection=projection(ras), 
         is.master=T, bbox=extent(-17, 33, -17, 26))+tm_raster()+
          tm_shape(World)+tm_borders()
