# school boundary shapefile is missing several school districts...
# 1/17/2018

setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)")
library(tmaptools)
library(stringr)
library(sp)
library(ggplot2)
library(plyr)
library(dplyr)
library(rgeos)
library("rgdal")


boundaries <- read_shape("Data/School Attendance Boundaries/SABS_1314_SchoolLevels/SABS_1314_High.shp")
boundaries$schnam <- gsub("PULASKI HIGH SCHOOL", "PULASKI COUNTY SENIOR HIGH", boundaries$schnam)
# filter for schools in virginia
va_boundaries <- boundaries[which(str_sub(as.character(boundaries$leaid),1,2)=="51"),] # Virginia

# import county shapefile
usa <- read_shape("~/Downloads/cb_2016_us_county_5m 2/cb_2016_us_county_5m.shp")
#usa <- readOGR("Data/GIS/", "cb_2016_us_county_5m.shp")
# filter for Virginia
virginia <-usa[usa@data$STATEFP==51,]
# transform CRS of virginia to that of sch_boundaries
virginia_t <- spTransform(virginia, CRS(proj4string(va_boundaries)))

plot(virginia_t)
plot(va_boundaries, col="purple", add=T)

source("~/git/schev/pulaski/splitting_giles.R")

# pulaski co - 1 high school ####
pulaski_t <- virginia_t[virginia_t$NAME=="Pulaski",]
pulaski_t@data <- data.frame(SrcName = NA, ncessch = NA, schnam = "PULASKI COUNTY SENIOR HIGH", leaid = "5100060",updateDate=NA,gslo=NA,gshi=NA,defacto=NA,stAbbrev="VA",sLevel=NA,openEnroll=0,MultiBdy=0,Shape_Leng=NA,Shape_Area=828438966)
va_boundaries <- rbind(va_boundaries, pulaski_t)

# giles co - 2 high schools ####
giles_t <- virginia_t[virginia_t$NAME=="Giles",]
plot(giles_t)
parts = chop_half(giles_t) 

plot(parts[[1]], add=TRUE, col=1) 
plot(parts[[2]], add=TRUE, col=2) 

giles_hs <- parts[[1]]
narrows_hs <- parts[[2]]

plot(giles_hs)
plot(narrows_hs)

narrows_df <- data.frame(STATEFP=51,COUNTYFP=as.character(071), COUNTYNS=NA, AFFGEOID=NA, GEOID=NA, NAME = "Narrows High", LSAD = NA, ALAND = 466307920, AWATER = NA)
narrows_poly <- SpatialPolygonsDataFrame(narrows_hs, narrows_df)
plot(narrows_poly)

giles_df <- data.frame(STATEFP=51,COUNTYFP=as.character(071), COUNTYNS=NA, AFFGEOID=NA, GEOID=NA, NAME = "Giles High", LSAD = NA, ALAND = 466307920, AWATER = NA)
giles_poly <- SpatialPolygonsDataFrame(giles_hs, giles_df)
plot(giles_poly)

narrows_poly@data <- data.frame(SrcName = NA, ncessch = NA, schnam = "NARROWS HIGH", leaid = "5100060",updateDate=NA,gslo=NA,gshi=NA,defacto=NA,stAbbrev="VA",sLevel=NA,openEnroll=0,MultiBdy=0,Shape_Leng=NA,Shape_Area=466307920)
giles_poly@data <- data.frame(SrcName = NA, ncessch = NA, schnam = "GILES HIGH", leaid = "5100060",updateDate=NA,gslo=NA,gshi=NA,defacto=NA,stAbbrev="VA",sLevel=NA,openEnroll=0,MultiBdy=0,Shape_Leng=NA,Shape_Area=466307920)
va_boundaries <- rbind(va_boundaries, pulaski_t, narrows_poly, giles_poly)

# lee co - 2 high schools #### 
lee_t <- virginia_t[virginia_t$NAME=="Lee",]

parts <- chop_half(lee_t)
plot(parts[[1]], add=TRUE, col=1) 
plot(parts[[2]], add=TRUE, col=2) 

thomaswalker_hs <- parts[[1]]
lee_hs <- parts[[2]]

thomaswalker_df <- data.frame(STATEFP=51,COUNTYFP=as.character(lee_t@data$COUNTYFP), COUNTYNS=NA, AFFGEOID=NA, GEOID=NA, NAME = "Thomas Walker High", LSAD = NA, ALAND = NA, AWATER = NA)
thomaswalker_poly <- SpatialPolygonsDataFrame(thomaswalker_hs, thomaswalker_df)
plot(thomaswalker_poly)
thomaswalker_poly@data <- data.frame(SrcName = NA, ncessch = NA, schnam = "THOMAS WALKER HIGH", leaid = "5100060",updateDate=NA,gslo=NA,gshi=NA,defacto=NA,stAbbrev="VA",sLevel=NA,openEnroll=0,MultiBdy=0,Shape_Leng=NA,Shape_Area=NA)

lee_df <- data.frame(STATEFP=51,COUNTYFP=as.character(lee_t@data$COUNTYFP), COUNTYNS=NA, AFFGEOID=NA, GEOID=NA, NAME = "Lee High", LSAD = NA, ALAND = NA, AWATER = NA)
lee_poly <- SpatialPolygonsDataFrame(lee_hs, lee_df)
plot(lee_poly)
lee_poly@data <- data.frame(SrcName = NA, ncessch = NA, schnam = "LEE HIGH", leaid = "5100060",updateDate=NA,gslo=NA,gshi=NA,defacto=NA,stAbbrev="VA",sLevel=NA,openEnroll=0,MultiBdy=0,Shape_Leng=NA,Shape_Area=NA)

va_boundaries <- rbind(va_boundaries, thomaswalker_poly, lee_poly)

# tazewell co - 3 high schools ####
tazewell_t <- virginia_t[virginia_t$NAME=="Tazewell",]

parts <- chop_thirds(tazewell_t)
plot(parts[[1]], add=TRUE, col=1) 
plot(parts[[2]], add=TRUE, col=2) 
plot(parts[[3]], add=TRUE, col=3) 

richlands_hs <- parts[[1]]
tazewell_hs <- parts[[2]]
graham_hs <- parts[[3]]

richlands_df <- data.frame(STATEFP=51,COUNTYFP=as.character(lee_t@data$COUNTYFP), COUNTYNS=NA, AFFGEOID=NA, GEOID=NA, NAME = "Richlands High", LSAD = NA, ALAND = NA, AWATER = NA)
richlands_poly <- SpatialPolygonsDataFrame(richlands_hs, richlands_df)
plot(richlands_poly)
richlands_poly@data <- data.frame(SrcName = NA, ncessch = NA, schnam = "RICHLANDS HIGH", leaid = "5100060",updateDate=NA,gslo=NA,gshi=NA,defacto=NA,stAbbrev="VA",sLevel=NA,openEnroll=0,MultiBdy=0,Shape_Leng=NA,Shape_Area=NA)

tazewell_df <- data.frame(STATEFP=51,COUNTYFP=as.character(lee_t@data$COUNTYFP), COUNTYNS=NA, AFFGEOID=NA, GEOID=NA, NAME = "Tazewell High", LSAD = NA, ALAND = NA, AWATER = NA)
tazewell_poly <- SpatialPolygonsDataFrame(tazewell_hs, tazewell_df)
plot(tazewell_poly)
tazewell_poly@data <- data.frame(SrcName = NA, ncessch = NA, schnam = "TAZEWELL HIGH", leaid = "5100060",updateDate=NA,gslo=NA,gshi=NA,defacto=NA,stAbbrev="VA",sLevel=NA,openEnroll=0,MultiBdy=0,Shape_Leng=NA,Shape_Area=NA)

graham_df <- data.frame(STATEFP=51,COUNTYFP=as.character(lee_t@data$COUNTYFP), COUNTYNS=NA, AFFGEOID=NA, GEOID=NA, NAME = "Graham High", LSAD = NA, ALAND = NA, AWATER = NA)
graham_poly <- SpatialPolygonsDataFrame(graham_hs, graham_df)
plot(graham_poly)
graham_poly@data <- data.frame(SrcName = NA, ncessch = NA, schnam = "GRAHAM HIGH", leaid = "5100060",updateDate=NA,gslo=NA,gshi=NA,defacto=NA,stAbbrev="VA",sLevel=NA,openEnroll=0,MultiBdy=0,Shape_Leng=NA,Shape_Area=NA)

va_boundaries <- rbind(va_boundaries, richlands_poly, tazewell_poly, graham_poly)

# craig co - 1 high school ####
craig_t <- virginia_t[virginia_t$NAME=="Craig",]
craig_t@data <- data.frame(SrcName = NA, ncessch = NA, schnam = "CRAIG COUNTY HIGH", leaid = "5100060",updateDate=NA,gslo=NA,gshi=NA,defacto=NA,stAbbrev="VA",sLevel=NA,openEnroll=0,MultiBdy=0,Shape_Leng=NA,Shape_Area=828438966)
va_boundaries <- rbind(va_boundaries, craig_t)

# highland co - 1 high school ####
highland_t <- virginia_t[virginia_t$NAME=="Highland",]
highland_t@data <- data.frame(SrcName = NA, ncessch = NA, schnam = "HIGHLAND HIGH", leaid = "5100060",updateDate=NA,gslo=NA,gshi=NA,defacto=NA,stAbbrev="VA",sLevel=NA,openEnroll=0,MultiBdy=0,Shape_Leng=NA,Shape_Area=828438966)
va_boundaries <- rbind(va_boundaries, highland_t)

# grayson co - 1 high school ####
grayson_t <- virginia_t[virginia_t$NAME=="Grayson",]
grayson_t@data <- data.frame(SrcName = NA, ncessch = NA, schnam = "GRAYSON COUNTY HIGH", leaid = "5100060",updateDate=NA,gslo=NA,gshi=NA,defacto=NA,stAbbrev="VA",sLevel=NA,openEnroll=0,MultiBdy=0,Shape_Leng=NA,Shape_Area=828438966)
va_boundaries <- rbind(va_boundaries, grayson_t)

# patrick co - 1 high school ####
patrick_t <- virginia_t[virginia_t$NAME=="Patrick",]
patrick_t@data <- data.frame(SrcName = NA, ncessch = NA, schnam = "PATRICK COUNTY HIGH", leaid = "5100060",updateDate=NA,gslo=NA,gshi=NA,defacto=NA,stAbbrev="VA",sLevel=NA,openEnroll=0,MultiBdy=0,Shape_Leng=NA,Shape_Area=828438966)
va_boundaries <- rbind(va_boundaries, patrick_t)

# franklin co - 1 high school ####
franklin_t <- virginia_t[virginia_t$NAME=="Franklin",]
franklin_t@data <- data.frame(SrcName = NA, ncessch = NA, schnam = "FRANKLIN COUNTY HIGH", leaid = "5100060",updateDate=NA,gslo=NA,gshi=NA,defacto=NA,stAbbrev="VA",sLevel=NA,openEnroll=0,MultiBdy=0,Shape_Leng=NA,Shape_Area=828438966)
franklin_t@data <- rbind(franklin_t@data, 
                         data.frame(SrcName = NA, ncessch = NA, schnam = "UNKNOWN SHAPE", leaid = "5100060",updateDate=NA,gslo=NA,gshi=NA,defacto=NA,stAbbrev="VA",sLevel=NA,openEnroll=0,MultiBdy=0,Shape_Leng=NA,Shape_Area=828438966))
franklin_t@polygons <- franklin_t@polygons[[2]]
va_boundaries <- rbind(va_boundaries, franklin_t)









