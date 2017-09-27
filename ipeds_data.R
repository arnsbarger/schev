# Aggregating IPEDS data for SCHEV Fall 2017 project:
# Number of vocational schools within 60 minutes
# Number of 4-year colleges/universities within 60 minutes
# Number of community colleges within 60 minutes

library(gdata)
library(dplyr)
library(raster)
library(maptools)

setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Data/IPEDS/Institutional Characteristics/")

# load hd2015 ipeds table - only relevant variables (see hd2015 dictionary) ####
hd2015 <- read.csv("hd2015.csv")[,c(c("UNITID","INSTNM","ADDR","CITY","STABBR","ZIP","FIPS","OBEREG","OPEID","OPEFLAG","ICLEVEL","CONTROL",
                                      "SECTOR","HLOFFER","HBCU","TRIBAL","LOCALE","C15SZSET","CBSA","CSA","COUNTYCD","COUNTYNM","CNGDSTCD","LONGITUD","LATITUDE"))]
# keep only VA schools
hd2015 <- hd2015 %>% filter(STABBR == "VA")

# load ic2015_ay ipeds table ####
ic2015_ay <- read.csv("ic2015_ay.csv")
# exclude tuition for chiropractic, dentistry, medicine, optometry, osteopathic, pharmacy, podiatry, and veterinary programs.
ic2015_ay <- ic2015_ay[ , !names(ic2015_ay) %in% c("ISPROF1","ISPFEE1","OSPROF1","OSPFEE1","ISPROF2","ISPFEE2","OSPROF2","OSPFEE2",
                      "ISPROF3","ISPFEE3","OSPROF3","OSPFEE3","ISPROF4","ISPFEE4","OSPROF4","OSPFEE4",
                      "ISPROF5","ISPFEE5","OSPROF5","OSPFEE5","ISPROF6","ISPFEE6","OSPROF6","OSPFEE6",
                      "ISPROF7","ISPFEE7","OSPROF7","OSPFEE7","ISPROF8","ISPFEE8","OSPROF8","OSPFEE8")]

# merge hd2015 and ic2015_ay ####
intersect(names(hd2015), names(ic2015_ay))

hd_icay2015 <- merge(hd2015, ic2015_ay, by = "UNITID", all.x = TRUE, all.y = FALSE)

# map of virginia schools by various characteristics ####
usa <- maptools::readShapePoly("~/Downloads/cb_2016_us_county_5m/cb_2016_us_county_5m.shp")
virginia <- fortify(usa[usa@data$STATEFP==51,])

hd_icay2015$LONGITUD <- as.numeric(as.character(hd_icay2015$LONGITUD))
hd_icay2015$LATITUDE <- as.numeric(as.character(hd_icay2015$LATITUDE))
hd_icay2015$TUITION1 <- as.numeric(as.character(hd_icay2015$TUITION1))

ggplot() +
    geom_polygon(data = virginia, aes(x=long,y=lat,group=group), fill = NA, color = 'black') +
    geom_point(data = hd_icay2015, aes(x = LONGITUD, y = LATITUDE, size = TUITION1, color = as.factor(CONTROL)))

# get lat/longs of each high school ####
highschools <- read.csv("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Bianica/school_crosswalk.csv")
highschools$google_query <- paste(highschools$sch_names, highschools$div_name)

library(ggmap)

for(i in 1:nrow(highschools)){    
    location <- highschools$google[i]
    coords <- ggmap::geocode(location, output = "latlon", source = "google")
    highschools$long[i] <- coords$lon
    highschools$lat[i] <- coords$lat
    
}

# calculate distance from each high school to each university ####

for(i in 1:nrow(highschools)){
    
    stationid <- fire_nooutliers$station[i]
    
    stationcoord <- as.matrix(stationgis[which(stationgis$Name == stationid), c("Longitude", "Latitude")], ncol = 2)
    
    firecoord <- as.matrix(fire_nooutliers[i, c("longitude","latitude")], ncol = 2)
    
    fire_nooutliers$distance_hav[i] <- distHaversine(stationcoord, firecoord)
    
    fire_nooutliers$distance_mi <- (fire_nooutliers$distance_hav)/(1609.344)
}

