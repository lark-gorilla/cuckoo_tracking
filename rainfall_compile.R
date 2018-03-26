## 26/03/18
## Downloading rainfall data
## Using the 'Climate Data Record (CDR) of Precipitation Estimation from Remotely
## Sensed Information using Artificial Neural Networks (PERSIANN-CDR)' dataset
# data is avaiable at daily 0.25 degree resolution, from 2000 until preseant
# There is also PERSIANN-CCS  a hi-res (4km) product avilable daily from 2003
# There is also PERSIANN_CDR a long term dataset a(0.25 degree) back until 1983

#
# data accessed via the portal: http://chrsdata.eng.uci.edu

# read in monthly data in netcdf format

library(raster)

# stacks up all the layers
ras<-stack("N:/cuckoo_tracking/sourced_data/PERSIANN_rainfall/monthly/PERSIANN_2018-03-26035525am.nc")

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

print(raster(ras, layer=1))
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


# some plotting using tmap
library(tmap)

tmap_mode('plot')

d4<-read.csv("N:/cuckoo_tracking/data/stopover_1daymin_spring_mig.csv", h=T)




tm_shape(subset(ras,1), projection=projection(ras), 
         is.master=T, bbox=extent(-17, 33, -17, 26))+tm_raster()+
          tm_shape(World)+tm_borders()