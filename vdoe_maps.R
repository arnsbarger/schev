# make maps for SCHEV meeting 10/13/2017
library(rgdal)
library(maptools)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(reshape2)
library(scales)
#library(OpenStreetMap)
library(stplanr)
library(rgeos)
library(stringr)

setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)")

#source("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Bianica/SchoolBoundaries.R") # doesn't work :( ... copy/paste below

# run Bianica's code to clean school attendance boundaries ####
# import school crosswalk
sch_cw<-read.csv("Code/Bianica/school_crosswalk.csv")

# GIS data
boundaries <- readShapePoly("Data/School Attendance Boundaries/SABS_1314_SchoolLevels/SABS_1314_High.shp")

# filter for schools in virginia
va_boundaries <- boundaries[which(str_sub(as.character(boundaries$leaid),1,2)=="51"),] # Virginia

# get school names we're interested in
sch_names<-left_join(sch_cw,va_boundaries@data,by=c("sch_names"="schnam"))
sch_names<-sch_names$sch_names

# filter for schools in study area
sch_boundaries <- va_boundaries[which(va_boundaries$schnam %in% sch_names),] # study area

# check what schools are missing
boundary_data<-sch_boundaries@data
anti_join(boundary_data,sch_cw,by=c("schnam"="sch_names"))
anti_join(sch_cw,boundary_data,by=c("sch_names"="schnam"))
# use Bland High polygon for Bland County High, there is no polygon for Frankly Military Academy in Richmond City
#bland<-sch_boundaries[which(sch_boundaries$schnam=="BLAND HIGH"),] # plot bland high polygon
#rockyGap<-sch_boundaries[which(sch_boundaries$schnam=="ROCKY GAP HIGH"),] # plot rocky gap polygon

# keep the Patrick Henry High in Roanoke
# all ids: 510183000776, 510330001430, 510390001749
# Correct NCES ID: 510330001430

# keep George Wythe High in Richmond
# all ids: 510324002081, 510411001801
# Correct NCES ID: 510324002081

# keey Thomas Jefferson High in Richmond
# all ids: 510126002034, 510324002070 
# Correct NCES ID: 510324002070

# filter out high schools not in study area
sch_boundaries <- sch_boundaries[which(sch_boundaries$ncessch!=510390001749),]
sch_boundaries <- sch_boundaries[which(sch_boundaries$ncessch!=510183000776),]
sch_boundaries <- sch_boundaries[which(sch_boundaries$ncessch!=510411001801),]
sch_boundaries <- sch_boundaries[which(sch_boundaries$ncessch!=510126002034),]
sch_boundaries <- sch_boundaries[which(sch_boundaries$schnam!="ROCKY GAP HIGH"),]

# add clean school name to shapefile data
boundary_data<-left_join(sch_boundaries@data,sch_cw,by=c("schnam"="sch_names"))
sch_boundaries@data<-boundary_data

### There are a total of 21 school zones for the study area (instead of 22 because Franklin Military 
#Academy does not have a school zone polygon)
plot(sch_boundaries)

# maps ####
vdoe <- read.csv("Code/Bianica/vdoe_psEnrollmentDataForMaps.csv")
usa <- maptools::readShapePoly("~/Downloads/cb_2016_us_county_5m/cb_2016_us_county_5m.shp")
virginia <- fortify(usa[usa@data$STATEFP==51,])

# convert sch_boundaries block groups to a dataframe
sch_boundaries@data$id = rownames(sch_boundaries@data)
sch_boundaries.points = fortify(sch_boundaries, region="id")
sch_boundaries.df = left_join(sch_boundaries.points, sch_boundaries@data, by="id")

map_data <- left_join(vdoe, sch_boundaries.df, by = "sch_name_clean")

names(map_data)
which(is.na(map_data$long))
map_data <- map_data[!map_data$sch_name_clean == "Franklin Military Academy",] # remove NA coordinate

# convert 
coords <- data.frame(x=c(map_data$long), y=c(map_data$lat))
nad83_coords <- coords
coordinates(nad83_coords) <- c('x', 'y')
proj4string(nad83_coords)=CRS("+init=epsg:32047") #32047 ... http://spatialreference.org/ref/?search=virginia&srtext=Search
spTransform(nad83_coords,CRS("+init=epsg:4326"))
coords <- as.data.frame(spTransform(nad83_coords,CRS("+init=epsg:4326")))

head(coords)

map_data$lat <- coords$y
map_data$long <- coords$x

# total proportion of postsecondary enrollment by high school zone for 2-year colleges

ggplot() +
    geom_polygon(data = virginia, aes(x=long,y=lat,group=group), fill = NA, color = 'black') +
    geom_polygon(data = map_data, aes(x = long, y = lat, group = group, fill = totalProp_2YearCollegeEnrollment)) +
    scale_fill_continuous(trans = 'reverse') +
    labs(fill = "Proportion of students enrolling in 2-year colleges")+
    coord_map()


