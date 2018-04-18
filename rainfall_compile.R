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

#remove non- west african rows

dat<-dat[which( ! dat$country%in% c('Algeria', 'France', 'Italy', 'Morocco' ,'Spain',
                            'United Kingdom', 'Portugal')),]

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


# Remove - values from rasters? should do !!! 

library(reshape2)
library(dplyr)


int_out<-NULL
for( i in 2012:2017) # or should I do per row
{
 
  indat<-dat[dat$year==i,]
  
    # cool the 'get' command grabs the collect raster set from the memory
    ras_temp<-get(paste0('r', i))
    
    # add 50 km buffer as per stopover definition. buffer=50000
    ext<-extract(ras_temp, indat[,7:8], buffer=50000, fun=median)
    
    ext[ext<0]<-NA # remove negative values '-99'
    
    dimnames(ext)[[2]]<-1:(length(dimnames(ext)[[2]]))
    
    out<-data.frame(ptt=indat$ptt, country=indat$country, year=i, 
                    SO_startDOY=indat$SO_startDOY, 
                    SO_endDOY=indat$SO_endDOY, ext)
    
    out$ID<-with(out, paste(ptt, year, SO_startDOY, sep="_"))
    
    outm<-melt(out, id.vars=c('ID', 'ptt', 'country', 'year',
                              'SO_startDOY', 'SO_endDOY'))
    
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

write.csv(int_out, 'data/spring_rainfall_by_stopover.csv', row.names = F, quote=F)




#### animation of rainfall #####

data("wrld_simpl", package = 'maptools')

library('sf')

world <- st_as_sf(wrld_simpl)

dat<-read.csv('data/stopover_bestofday_1daymin_recalc_spring_mig.csv', h=T)

# remove negative values

r2012[r2012<0]<-NA
r2013[r2013<0]<-NA
r2014[r2014<0]<-NA
r2015[r2015<0]<-NA
r2016[r2016<0]<-NA
r2017[r2017<0]<-NA

d2012<-dat[dat$year=='2012',]
d2013<-dat[dat$year=='2013',]
d2014<-dat[dat$year=='2014',]
d2015<-dat[dat$year=='2015',]
d2016<-dat[dat$year=='2016',]
d2017<-dat[dat$year=='2017',]


library(tmap)
library(animation)

tmaptools::palette_explorer() 


tmap_mode('plot')

saveGIF({
for(i in 1:123)
{
 
  indat<-which(by(d2012, 1:nrow(d2012), FUN=function(x){i %in% x$SO_startDOY:x$SO_endDOY}))
   
  d_day<-d2012[indat,]
  
  d_day$name<-factor(d_day$name)
  
  if(nrow(d_day)==0){
    d_day<-SpatialPointsDataFrame(SpatialPoints(cbind(8, 55), 
            proj4string=CRS(projection(ras))), data=d_day[1,])
            }else{
            d_day<-SpatialPointsDataFrame(SpatialPoints(d_day[,7:8], 
            proj4string=CRS(projection(ras))), data=d_day)
            }
  
    ras1<-subset(r2012,i)
    
    if(i==1){cumra<-ras1}else{cumra<-cumra+ras1}
    cumra[cumra<1]<-NA
    
    ras1[ras1>10]<-10
    
    p1<-rasterToPolygons(ras1, dissolve=T, fun=function(x){x==10})
    if(is.null(p1)){p1<-rasterToPolygons(crop(ras1, extent(c(20,21, 55, 56))), dissolve=T)}
    
    
    print(
      tm_shape(cumra, projection=projection(ras), 
               is.master=T, bbox=extent(-17, 33, -17, 26))+
        tm_raster(breaks=c(100,500,1000,5000,10000,12000), palette = "Blues", n = 5,
                  colorNA=NULL, title='rainfall (mm)')+
        tm_shape(world)+tm_borders()+tm_shape(p1)+tm_fill(col='red', alpha=0.5)+
        tm_layout(legend.position=c('right', 'top'), main.title=paste('Day', i))
        )

}
}, interval=1, "N:/cuckoo_tracking/outputs/rainfall_stopovers2012.gif")




















### visualisastion  
  
library(ggplot2)
library(scales)


# order country factors by longitude

int_out$country<-factor(int_out$country)
int_out$country<-factor(int_out$country, levels= c("Angola", "Democratic Republic of the Congo",
                                                   "Congo", "Gabon", "Central African Republic", 
                                                   "Cameroon", "Nigeria", "Togo", "Burkina Faso",
                                                   "Ghana", "Cote d'Ivoire", "Liberia", "Guinea", 
                                                   "Sierra Leone"))


p<-ggplot(data=int_out[int_out$ptt=='62608' & int_out$year=='2012',], 
          aes(x=variable, y=value))

p+geom_rect(aes(xmin=SO_startDOY, xmax=SO_endDOY, ymin=0, ymax=100), fill='grey')+
  geom_bar(stat='identity', colour='black')+
  geom_smooth(span=0.5, se=FALSE, linetype='dashed', col='red')+
  facet_wrap(~SO_startDOY+country)+theme_classic()+
  ylab('daily rainfall (mm)')+ xlab('Day of year')


p<-ggplot(data=int_out[int_out$year=='2013',], 
          aes(x=variable, y=value))

p+geom_rect(aes(xmin=SO_startDOY, xmax=SO_endDOY, ymin=0, ymax=100, colour=factor(ptt)), fill=NA)+
  geom_bar(stat='identity', colour='black')+
  geom_smooth(span=0.5, se=FALSE, linetype='dashed', col='red')+
  facet_wrap(~country)+theme_classic()+
  ylab('daily rainfall (mm)')+ xlab('Day of year')


p<-ggplot(data=int_out, 
          aes(x=variable, y=value))

p+geom_rect(aes(xmin=SO_startDOY, xmax=SO_endDOY, ymin=0, ymax=100, colour=factor(ptt)), fill=NA)+
  geom_bar(stat = "summary", fun.y = "mean", colour='black')+
  geom_smooth(span=0.5, se=FALSE, linetype='dashed', col='red')+
  facet_wrap(~country)+theme_classic()+
  ylab('daily rainfall (mm)')+ xlab('Day of year')


p<-ggplot(data=int_out[int_out$country='Ghana',], 
          aes(x=variable, y=value))

p+geom_rect(aes(xmin=SO_startDOY, xmax=SO_endDOY, ymin=0, ymax=100, colour=factor(ptt)), fill=NA)+
  geom_bar(stat = "summary", fun.y = "mean", colour='black')+
  geom_smooth(span=0.5, se=FALSE, linetype='dashed', col='red')+
  facet_wrap(~year)+theme_classic()+
  ylab('daily rainfall (mm)')+ xlab('Day of year')




##### old #####
  
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
