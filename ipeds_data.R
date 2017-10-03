# Aggregating IPEDS data for SCHEV Fall 2017 project to create the following variables:
# Number of vocational schools within 60 minutes
# Number of 4-year colleges/universities within 60 minutes
# Number of community colleges within 60 minutes

library(ggplot2)
library(gdata)
library(dplyr)
library(raster)
library(maptools)

setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Data/IPEDS/Institutional Characteristics/")

# load hd2015 ipeds table - only relevant variables (see hd2015 dictionary) ####
hd2015 <- read.csv("hd2015.csv")[,c(c("UNITID","INSTNM","ADDR","CITY","STABBR","ZIP","FIPS","OBEREG","OPEID","OPEFLAG","ICLEVEL","CONTROL","C15IPUG",
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
    geom_point(data = hd_icay2015, aes(x = LONGITUD, y = LATITUDE, size = TUITION1, color = as.factor(CONTROL))) +
    coord_map()

# get lat/longs of each high school ####
highschools <- read.csv("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Bianica/school_crosswalk.csv") # wrote the csv below - now this file has the coordinates already
#highschools$google_query <- paste(highschools$sch_names, highschools$div_name)

# library(ggmap)
# 
# for(i in 1:nrow(highschools)){
#     location <- highschools$google[i]
#     coords <- ggmap::geocode(location, output = "latlon", source = "google")
#     highschools$long[i] <- coords$lon
#     highschools$lat[i] <- coords$lat
# 
# }
# 
# write.csv(highschools, "~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Bianica/school_crosswalk.csv")

# assign surrounding counties ####
bland_county <- c("Tazewell County", "Smyth County", "Wythe County", "Pulaski County", "Giles County", "Bland County")
buchanan_county <- c("Dickenson County", "Russell County", "Tazewell County", "Buchanan County")
roanoke_city <- c("Roanoke County", "Salem County", "Roanoke City")
roanoke_county <- c("Roanoke City","Montgomery County", "Floyd County", "Franklin County", "Bedford County", "Botetourt County", "Craig County", "Roanoke County")
richmond_city <- c("Chesterfield County", "Henrico County", "Richmond City")
sussex_county <- c("Prince George County", "Dinwiddie County", "Greensville County", "Southampton County", "Isle of Wight County", "Surry County")
powhatan <- c("Goochland County", "Cumberland County", "Amelia County", "Chesterfield County")


counties <- list(bland_county, buchanan_county, roanoke_city, roanoke_county, richmond_city,sussex_county,powhatan)

# calculate nearby colleges (number in county + number in surrounding counties)
# all schools ####
for(i in 1:7){
    highschools$count_all_colleges[highschools$div_name==unique(highschools$div_name)[i]] <- sum(as.character(hd_icay2015$COUNTYNM) %in% counties[[i]])
}
# 4-year schools ####
hd_icay2015$INSTNM[hd_icay2015$C15IPUG %in% 6:17]

hd_ic2015_4year <- hd_icay2015 %>% filter(C15IPUG %in% 6:17)

for(i in 1:7){
    highschools$count_4year[highschools$div_name==unique(highschools$div_name)[i]] <- sum(as.character(hd_ic2015_4year$COUNTYNM) %in% counties[[i]])
}
# community colleges ####
hd_icay2015$INSTNM[hd_icay2015$C15IPUG %in% 1:2]

hd_ic2015_cc <- hd_icay2015 %>% filter(C15IPUG %in% 1:2)

for(i in 1:7){
    highschools$count_comm_college[highschools$div_name==unique(highschools$div_name)[i]] <- sum(as.character(hd_ic2015_cc$COUNTYNM) %in% counties[[i]])
}
# career schools ####
hd_icay2015$INSTNM[hd_icay2015$C15IPUG %in% c(3:5,18:20)]

hd_ic2015_career <- hd_icay2015 %>% filter(C15IPUG %in% c(3:5,18:20))

for(i in 1:7){
    highschools$count_career_schools[highschools$div_name==unique(highschools$div_name)[i]] <- sum(as.character(hd_ic2015_career$COUNTYNM) %in% counties[[i]])
}
# vocational/cte schools ####
hd_icay2015$INSTNM[hd_icay2015$C15IPUG == -2]

hd_ic2015_cte <- hd_icay2015 %>% filter(C15IPUG  == -2)

for(i in 1:7){
    highschools$count_cte_schools[highschools$div_name==unique(highschools$div_name)[i]] <- sum(as.character(hd_ic2015_cte$COUNTYNM) %in% counties[[i]])
}


