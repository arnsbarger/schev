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


## PLOT GENERAL MAPS ----------------------------------------------

plot(virginia_t)
plot(sch_boundaries, col="red", add=T)

## PREPARE VDOE DATA FOR MAPS ----------
vdoe <- read.csv("Code/Bianica/vdoe_psEnrollmentDataForMaps.csv")
#sch_boundaries@data <- left_join(vdoe, sch_boundaries@data, by = "sch_name_clean")

sch_boundaries@data$id = rownames(sch_boundaries@data)
sch_boundaries.points = fortify(sch_boundaries, region="id")
sch_boundaries.df = left_join(sch_boundaries.points, sch_boundaries@data, by="id")

map_data <- left_join(vdoe, sch_boundaries.df, by = "sch_name_clean")

## PLOT MAPS ------
library(ggmap)

# two year
ggplot() + 
    geom_polygon(data = virginia_t, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data, aes(x=long, y=lat, group=group, fill = totalProp_2YearCollegeEnrollment), color = "darkgrey") +
    scale_fill_continuous(limits = c(0,1)) +
    labs(title = "Proportion of students enrolling in 2-year colleges", fill = "Proportion") 
ggsave("Code/Maddie/output/map_prop_2year.png", device = "png", width = 11, height = 6, units = "in")

# four year
ggplot() + 
    geom_polygon(data = virginia_t, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data, aes(x=long, y=lat, group=group, fill = totalProp_4YearCollegeEnrollment), color = "darkgrey") +
    scale_fill_continuous(limits = c(0,1)) +
    labs(title = "Proportion of students enrolling in 4-year colleges", fill = "Proportion")
ggsave("Code/Maddie/output/map_prop_4year.png", device = "png", width = 11, height = 6, units = "in")

# two year disadvantaged
ggplot() + 
    geom_polygon(data = virginia_t, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data, aes(x=long, y=lat, group=group, fill = disadvY_2_year_ps_prop), color = "darkgrey") +
    scale_fill_continuous(limits = c(0,1)) +
    labs(title = "Proportion of disadvantaged students enrolling in 2-year colleges", fill = "Proportion")
ggsave("Code/Maddie/output/map_prop_disadv2year.png", device = "png", width = 11, height = 6, units = "in")

# four year disadvantaged
ggplot() + 
    geom_polygon(data = virginia_t, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data, aes(x=long, y=lat, group=group, fill = disadvY_4_year_ps_prop), color = "darkgrey") +
    scale_fill_continuous(limits = c(0,1)) +
    labs(title = "Proportion of disadvantaged students enrolling in 4-year colleges", fill = "Proportion")
ggsave("Code/Maddie/output/map_prop_disadv4year.png", device = "png", width = 11, height = 6, units = "in")

# ZOOM OF ROANOKE CO
bland_county <- c("Tazewell County", "Smyth County", "Wythe County", "Pulaski County", "Giles County", "Bland County")
buchanan_county <- c("Dickenson County", "Russell County", "Tazewell County", "Buchanan County")
roanoke_city <- c("Roanoke County", "Salem County", "Roanoke City")
roanoke_county <- c("Roanoke City","Montgomery County", "Floyd County", "Franklin County", "Bedford County", "Botetourt County", "Craig County", "Roanoke County")
richmond_city <- c("Chesterfield County", "Henrico County", "Richmond City")
sussex_county <- c("Prince George County", "Dinwiddie County", "Greensville County", "Southampton County", "Isle of Wight County", "Surry County", "Sussex County")
powhatan <- c("Goochland County", "Cumberland County", "Amelia County", "Chesterfield County", "Powhatan County")
    # two year
    ggplot() + 
        #geom_polygon(data = virginia_t %>% filter(NAME == "Bland"), aes(x = long, y = lat, group = group), fill=NA,color='black') +
        geom_polygon(data = map_data %>% filter(county_name %in% roanoke_county), aes(x=long, y=lat, group=group, fill = totalProp_2YearCollegeEnrollment), color = "darkgrey") +
        #scale_fill_continuous(limits = c(0,1)) +
        labs(title = "Proportion of students enrolling in 2-year colleges", fill = "Proportion") 
    ggsave("Code/Maddie/output/ROANOKECOmap_prop_2year.png", device = "png", width = 11, height = 6, units = "in")
    
    # four year
    ggplot() + 
        #geom_polygon(data = virginia_t, aes(x = long, y = lat, group = group), fill=NA,color='black') +
        geom_polygon(data = map_data %>% filter(county_name %in% roanoke_county), aes(x=long, y=lat, group=group, fill = totalProp_4YearCollegeEnrollment), color = "darkgrey") +
        #scale_fill_continuous(limits = c(0,1)) +
        labs(title = "Proportion of students enrolling in 4-year colleges", fill = "Proportion")
    ggsave("Code/Maddie/output/ROANOKECOmap_prop_4year.png", device = "png", width = 11, height = 6, units = "in")
    
    # two year disadvantaged
    ggplot() + 
        #geom_polygon(data = virginia_t, aes(x = long, y = lat, group = group), fill=NA,color='black') +
        geom_polygon(data = map_data %>% filter(county_name %in% roanoke_county), aes(x=long, y=lat, group=group, fill = disadvY_2_year_ps_prop), color = "darkgrey") +
        #scale_fill_continuous(limits = c(0,1)) +
        labs(title = "Proportion of disadvantaged students enrolling in 2-year colleges", fill = "Proportion")
    ggsave("Code/Maddie/output/ROANOKECOmap_prop_disadv2year.png", device = "png", width = 11, height = 6, units = "in")
    
    # four year disadvantaged
    ggplot() + 
        #geom_polygon(data = virginia_t, aes(x = long, y = lat, group = group), fill=NA,color='black') +
        geom_polygon(data = map_data %>% filter(county_name %in% roanoke_county), aes(x=long, y=lat, group=group, fill = disadvY_4_year_ps_prop), color = "darkgrey") +
        #scale_fill_continuous(limits = c(0,1)) +
        labs(title = "Proportion of disadvantaged students enrolling in 4-year colleges", fill = "Proportion")
    ggsave("Code/Maddie/output/ROANOKECOmap_prop_disadv4year.png", device = "png", width = 11, height = 6, units = "in")
    

