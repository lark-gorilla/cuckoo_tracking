## 10/09/18
## Re-analyses of cuckoo data for paper as per meeting with James

## Job 1 
## Re-classify departure times from various regions based on bestofday data
## rather than using stopovers to define times

library(rgdal)
library(sp)
library(sf)
library(dplyr)


dmod<-read.csv('C:/cuckoo_tracking/data/stopover_bestofday_2018_1daymin_recalc_spring_mig_summary_dead_attrib_modelready.csv', h=T)

bodpoints<-read.csv('C:/cuckoo_tracking/data/processed_movebank_cuckoos_hybrid_filter_2018_clean_stopovers_recalc.csv', h=T)

bodpoints$pttyear<-paste(bodpoints$ptt, bodpoints$year.x)
dmod$pttyear<-paste(dmod$ptt, dmod$year)

#filter to just birds in dmod and spring migration

bodpoints<-bodpoints %>% filter(pttyear %in% unique(dmod$pttyear) & julian<145)


wrld<-st_read('C:/cuckoo_tracking/sourced_data/country_borders/TM_WORLD_BORDERS-0.3.shp')

bodpoints_sp0<-st_as_sf(bodpoints, coords = c("long","lat"),
                 crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

bodpoints_sp0<-select(bodpoints_sp0, ptt, year.x, julian )

wrld$NAME=as.character(wrld$NAME)

bodpoints_sp<-st_join(bodpoints_sp0, wrld[c('NAME', 'REGION', 'SUBREGION')])

pts<-st_intersects(bodpoints_sp0, wrld)

pts_logical = lengths(pts) == 0

sea_pts<-bodpoints_sp0[pts_logical,]

sea_pts$NAME='SEA'
sea_pts$REGION=999
sea_pts$SUBREGION=999

bodpoints_sp<-rbind(bodpoints_sp, sea_pts)

# ok looks good
plot(bodpoints_sp['REGION'])

bodpoints_sp$geometry<-NULL

bodpoints_sp %>% as_tibble() %>% filter(REGION!=2) %>%
  group_by(ptt, year.x) %>% summarise_all(first)