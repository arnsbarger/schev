# written by Aaron Schroeder 10/2017
## LIBRARIES ----------------------------------------------

library(rgdal)
library(rgeos)
library(stringr)
library(dplyr)
library(tmaptools)
library(sp)

setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)")

## SCHOOL BOUNDARIES ----------------------------------------------

# import school crosswalk
sch_cw<-read.csv("Code/Bianica/school_crosswalk.csv")
# GIS data
boundaries <- read_shape("Data/School Attendance Boundaries/SABS_1314_SchoolLevels/SABS_1314_High.shp")
# filter for schools in virginia
va_boundaries <- boundaries[which(str_sub(as.character(boundaries$leaid),1,2)=="51"),] # Virginia
# get school names we're interested in
sch_names<-left_join(sch_cw,va_boundaries@data,by=c("sch_names"="schnam"))
sch_names<-sch_names$sch_names
# filter for schools in study area
sch_boundaries <- va_boundaries[which(va_boundaries$schnam %in% sch_names),] # study area
# filter out high schools not in study area
sch_boundaries <- sch_boundaries[which(sch_boundaries$ncessch!=510390001749),]
sch_boundaries <- sch_boundaries[which(sch_boundaries$ncessch!=510183000776),]
sch_boundaries <- sch_boundaries[which(sch_boundaries$ncessch!=510411001801),]
sch_boundaries <- sch_boundaries[which(sch_boundaries$ncessch!=510126002034),]
sch_boundaries <- sch_boundaries[which(sch_boundaries$schnam!="ROCKY GAP HIGH"),]
# add clean school name to shapefile data
boundary_data<-left_join(sch_boundaries@data,sch_cw,by=c("schnam"="sch_names"))
sch_boundaries@data<-boundary_data


## COUNTY BORDERS ----------------------------------------------

# GIS data
usa <- read_shape("~/Downloads/cb_2016_us_county_5m/cb_2016_us_county_5m.shp")
# filter for Virginia
virginia <-usa[usa@data$STATEFP==51,]
# transform CRS of virginia to that of sch_boundaries
virginia_t <- spTransform(virginia, CRS(proj4string(sch_boundaries)))


## PLOT ----------------------------------------------

plot(virginia_t)
plot(sch_boundaries, col="red", add=T)