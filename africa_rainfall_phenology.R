# 30/08/2018

#Code to compile annual rainfall rasters stacks and extract day
# at which rainfall is > 100ml for each cell

library(raster) # needs ncdf4 installed

# stacks up all the layers
#ras<-stack("sourced_data/PERSIANN_rainfall/monthly/PERSIANN_2018-03-26035525am.nc")
ras<-stack("C:/cuckoo_tracking/sourced_data/PERSIANN_rainfall/daily_inc2018/PERSIANN_2018-05-25054150am.nc")
# warning in projection 

print(raster(ras, layer=1))

# basically it is specifcying WGS but CRS gets it a bit wrong

ras@crs
#fix 
ras@crs<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


##### check data

names(ras)
# name is 'units: days since 2011-06-01'

print(raster(ras, layer=1))

# set negative vals to 0

ras[ras<0]<-0

# get annual rasters
# 124 comes from 1;124 max DOY in west africa (4th May)

r2012<-subset(ras, 213:(213+124))
r2013<-subset(ras, 579:(579+124))
r2014<-subset(ras, 944:(944+124))
r2015<-subset(ras, 1309:(1309+124))
r2016<-subset(ras, 1674:(1674+124))
r2017<-subset(ras, 2040:(2040+124))
r2018<-subset(ras, 2405:(2405+124))

fn1<-function(x)
{
  x[x<0]<-0 # remove negative vals
  x[is.na(x)]<-0
  x[x>200]<-0
out<-subset(x, 1)
for(i in 2:nlayers(x))
  {
  out<-stack(out, subset(out,(i-1))+ subset(x, i))
  print(i)
  } 
# the 'out[]' coerces stack into data.frame
v1<-apply(out[], 1, FUN=function(x){min(which(x > 100))})

out2<-subset(x, 1)
values(out2)<-v1
return(out2)
}
#gives warnings for Inf values from never rained on places, ok as 
# turned to NA in resultant raster

r2012_mon<-fn1(r2012)
r2013_mon<-fn1(r2013)
r2014_mon<-fn1(r2014)
r2015_mon<-fn1(r2015)
r2016_mon<-fn1(r2016)
r2017_mon<-fn1(r2017)
r2018_mon<-fn1(r2018)

#quick plot
library(maps)

plot(stack(r2012_mon, r2013_mon, r2014_mon, r2015_mon, r2016_mon,
               r2017_mon, r2018_mon)) 

#write out results

writeRaster(r2012_mon, 'C:/cuckoo_tracking/sourced_data/PERSIANN_rainfall/mons_arr2012.tif')
writeRaster(r2013_mon, 'C:/cuckoo_tracking/sourced_data/PERSIANN_rainfall/mons_arr2013.tif')
writeRaster(r2014_mon, 'C:/cuckoo_tracking/sourced_data/PERSIANN_rainfall/mons_arr2014.tif')
writeRaster(r2015_mon, 'C:/cuckoo_tracking/sourced_data/PERSIANN_rainfall/mons_arr2015.tif')
writeRaster(r2016_mon, 'C:/cuckoo_tracking/sourced_data/PERSIANN_rainfall/mons_arr2016.tif')
writeRaster(r2017_mon, 'C:/cuckoo_tracking/sourced_data/PERSIANN_rainfall/mons_arr2017.tif')
writeRaster(r2018_mon, 'C:/cuckoo_tracking/sourced_data/PERSIANN_rainfall/mons_arr2018.tif')
