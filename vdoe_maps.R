## LIBRARIES ----------------------------------------------

library(rgdal)
library(rgeos)
library(stringr)
library(dplyr)
library(tmaptools)
library(sp)
library(ggplot2)
library(plyr)

setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)")

## PREPARE COUNTY & SCHOOL BOUNDARY SHAPEFILES ----------------------------------------------
# import school crosswalk
sch_cw<-read.csv("Code/Bianica/school_crosswalk.csv")
# import school boundary shapefile 
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
# import county shapefile
usa <- read_shape("~/Downloads/cb_2016_us_county_5m/cb_2016_us_county_5m.shp")
# filter for Virginia
virginia <-usa[usa@data$STATEFP==51,]
# transform CRS of virginia to that of sch_boundaries
virginia_t <- spTransform(virginia, CRS(proj4string(sch_boundaries)))

# plot
plot(virginia_t)
plot(sch_boundaries, col="red", add=T)

# prepare for ggplot presentation
sch_boundaries@data$id = rownames(sch_boundaries@data)
sch_boundaries.points = fortify(sch_boundaries, region="id")
sch_boundaries.df = join(sch_boundaries.points, sch_boundaries@data, by="id")

virginia_t@data$id = rownames(virginia_t@data)
virginia_t.points = fortify(virginia_t, region="id")
virginia_t.df = join(virginia_t.points, virginia_t@data, by="id")

## PREPARE VDOE DATA FOR MAPS ----------
vdoe <- read.csv("Code/Bianica/vdoe_psEnrollmentDataForMaps.csv")

map_data <- left_join(x=vdoe, y=sch_boundaries.df, by = "sch_name_clean")

## VIRGINIA MAPS ------

# two year
ggplot() + 
    geom_polygon(data = virginia_t, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data, aes(x=long, y=lat, group=group, fill = totalProp_2YearCollegeEnrollment), color = "black", size = .1) +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    #labs(title = "Proportion of students enrolling in 2-year colleges", fill = "Proportion") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) + #labels
    labs(title="Proportion of students enrolling in 2-year colleges", x="", y="", fill = "Proportion") +
    coord_equal(ratio=1)
ggsave("Code/Maddie/output/map_prop_2year.png", device = "png", width = 11, height = 6, units = "in")

# four year
ggplot() + 
    geom_polygon(data = virginia_t, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data, aes(x=long, y=lat, group=group, fill = totalProp_4YearCollegeEnrollment), color = "black", size = .1) +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    labs(title = "Proportion of students enrolling in 4-year colleges", fill = "Proportion", x="",y="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0))  +
    coord_equal(ratio=1)
ggsave("Code/Maddie/output/map_prop_4year.png", device = "png", width = 11, height = 6, units = "in")

# two year disadvantaged
ggplot() + 
    geom_polygon(data = virginia_t, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data, aes(x=long, y=lat, group=group, fill = disadvY_2_year_ps_prop), color = "black", size = .1) +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    labs(title = "Proportion of disadvantaged students enrolling in 2-year colleges", fill = "Proportion", x="",y="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) +
    coord_equal(ratio=1)
ggsave("Code/Maddie/output/map_prop_disadv2year.png", device = "png", width = 11, height = 6, units = "in")

# four year disadvantaged
ggplot() + 
    geom_polygon(data = virginia_t, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data, aes(x=long, y=lat, group=group, fill = disadvY_4_year_ps_prop), color = "black", size = .1) +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    labs(title = "Proportion of disadvantaged students enrolling in 4-year colleges", fill = "Proportion", x="",y="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) +
    coord_equal(ratio=1)
ggsave("Code/Maddie/output/map_prop_disadv4year.png", device = "png", width = 11, height = 6, units = "in")

# ASSIGN NEARBY COUNTIES TO STUDY REGIONS  -----
# surrounding counties/areas
bland_county <- c("Tazewell County", "Smyth County", "Wythe County", "Pulaski County", "Giles County", "Bland County")
buchanan_county <- c("Dickenson County", "Russell County", "Tazewell County", "Buchanan County")
roanoke_city <- c("Roanoke County", "Salem County", "Roanoke City")
roanoke_county <- c("Roanoke City","Montgomery County", "Floyd County", "Franklin County", "Bedford County", "Botetourt County", "Craig County", "Roanoke County")
richmond_city <- c("Chesterfield County", "Henrico County", "Richmond City")
sussex_county <- c("Prince George County", "Dinwiddie County", "Greensville County", "Southampton County", "Isle of Wight County", "Surry County", "Sussex County")
powhatan <- c("Goochland County", "Cumberland County", "Amelia County", "Chesterfield County", "Powhatan County")

# categorize counties into appalachia or richmond (for zoomed in maps)
# appalachia
appalachia.sch <- c(bland_county, buchanan_county, roanoke_county, roanoke_city) # use to subset school boundary polygon
#appalachia <- gsub(" .*", "", c(bland_county, buchanan_county, roanoke_county, roanoke_city)) # use to subset county polygon
appalachia <- virginia_t.df %>% filter(NAME %in% gsub(" .*", "", c(bland_county, buchanan_county, roanoke_county, roanoke_city))) %>% filter(long < 1500000)
#appalachia <- map_data %>% filter(county_name %in% appalachia.sch)

# richmond
eastern.sch <- c(richmond_city, sussex_county, powhatan)
eastern <-  gsub(" County", "", c(richmond_city, sussex_county, powhatan))
eastern <- gsub(" City", "", eastern)

# find centroids of each polygon for labeling schools
sch_boundaries.fort <- fortify(sch_boundaries, region = "sch_name_clean")
idList <- sch_boundaries@data$sch_name_clean
centroids.df <- as.data.frame(coordinates(sch_boundaries))
names(centroids.df) <- c("Longitude", "Latitude")
text.labels.df <- left_join(data.frame(id = idList, centroids.df), sch_cw, by=c("id"="sch_name_clean"))

appalachia.labels <- text.labels.df %>% filter(county_name %in% appalachia.sch)
appalachia.labels$id2 <- gsub(" High", "", appalachia.labels$id)
eastern.labels <- text.labels.df %>% filter(county_name %in% eastern.sch)
east.nonrichmond.labels <- eastern.labels%>% filter(id %in% c("Sussex Central High", "Powhatan High"))
richmond.labels <- eastern.labels %>% filter(!id %in% c("Open High", "Richmond Community High"))

eastern_data <- map_data %>% filter(!sch_name_clean %in% c("Open High", "Richmond Community High"))
richmond_data <- map_data %>% filter(county_name == "Richmond City") %>% filter(!sch_name_clean %in% c("Open High", "Richmond Community High"))
richmond_t.df <- virginia_t.df %>% filter(NAME %in% eastern) %>% filter(!id == 1700)


# APPALACHIA ------
# two year
ggplot() + 
    geom_polygon(data = appalachia, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data %>% filter(county_name %in% appalachia.sch), aes(x=long, y=lat, group=group, fill = totalProp_2YearCollegeEnrollment), color = "black", size = .1) +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    geom_text(data = appalachia.labels, aes(label = gsub("High", "",id), x = Longitude, y = Latitude), size = 2) +
    labs(title = "Proportion of students enrolling in 2-year colleges", fill = "Proportion", x="",y="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) +
    coord_equal(ratio=1)
ggsave("Code/Maddie/output/APPALACHIAmap_prop_2year.png", device = "png", width = 11, height = 6, units = "in")

# four year
ggplot() + 
    geom_polygon(data = appalachia, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data %>% filter(county_name %in% appalachia.sch), aes(x=long, y=lat, group=group, fill = totalProp_4YearCollegeEnrollment), color = "black", size = .1) +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    geom_text(data = appalachia.labels, aes(label = gsub("High", "",id), x = Longitude, y = Latitude), size = 2) +
    labs(title = "Proportion of students enrolling in 4-year colleges", fill = "Proportion", x="",y="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) +
    coord_equal(ratio=1)
ggsave("Code/Maddie/output/APPALACHIAmap_prop_4year.png", device = "png", width = 11, height = 6, units = "in")

# disadvantaged two year
ggplot() + 
    geom_polygon(data = appalachia, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data %>% filter(county_name %in% appalachia.sch), aes(x=long, y=lat, group=group, fill = disadvY_2_year_ps_prop), color = "black", size = .1) +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    geom_text(data = appalachia.labels, aes(label = gsub("High", "",id), x = Longitude, y = Latitude), size = 2) +
    labs(title = "Proportion of disadvantaged students enrolling in 2-year colleges", fill = "Proportion", x="",y="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) +
    coord_equal(ratio=1)
ggsave("Code/Maddie/output/APPALACHIAmap_prop_disadv2year.png", device = "png", width = 11, height = 6, units = "in")

# disadvantaged four year
ggplot() + 
    geom_polygon(data = appalachia, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data %>% filter(county_name %in% appalachia.sch), aes(x=long, y=lat, group=group, fill = disadvY_4_year_ps_prop), color = "black", size = .1) +
    geom_text(data = appalachia.labels, aes(label = gsub("High", "",id), x = Longitude, y = Latitude), size = 2) +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    geom_text(data = appalachia.labels, aes(label = gsub("High", "",id), x = Longitude, y = Latitude), size = 2) +
    labs(title = "Proportion of disadvantaged students enrolling in 4-year colleges", fill = "Proportion", x="",y="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) +
    coord_equal(ratio=1)
ggsave("Code/Maddie/output/APPALACHIAmap_prop_disadv4year.png", device = "png", width = 11, height = 6, units = "in")
# ROANOKE ZOOM ---------
# two year
ggplot() + 
    #geom_polygon(data = virginia_t %>% filter(NAME == "Bland"), aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data %>% filter(county_name %in% roanoke_county), aes(x=long, y=lat, group=group, fill = totalProp_2YearCollegeEnrollment), color = "black") +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    labs(title = "Proportion of students enrolling in 2-year colleges", fill = "Proportion", x="",y="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) +
    coord_equal(ratio=1)+
    geom_text(data = appalachia.labels %>% filter(county_name %in% roanoke_county), aes(x=Longitude,y=Latitude, label=id2))
ggsave("Code/Maddie/output/ROANOKE-ZOOMmap_prop_2year.png", device = "png", width = 11, height = 6, units = "in")

# four year
ggplot() + 
    #geom_polygon(data = virginia_t, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data %>% filter(county_name %in% roanoke_county), aes(x=long, y=lat, group=group, fill = totalProp_4YearCollegeEnrollment), color = "black") +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    labs(title = "Proportion of students enrolling in 4-year colleges", fill = "Proportion", x="",y="")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) +
    coord_equal(ratio=1)+
    geom_text(data = appalachia.labels %>% filter(county_name %in% roanoke_county), aes(x=Longitude,y=Latitude, label=id2))
ggsave("Code/Maddie/output/ROANOKE-ZOOMmap_prop_4year.png", device = "png", width = 11, height = 6, units = "in")

# two year disadvantaged
ggplot() + 
    #geom_polygon(data = virginia_t, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data %>% filter(county_name %in% roanoke_county), aes(x=long, y=lat, group=group, fill = disadvY_2_year_ps_prop), color = "black") +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    labs(title = "Proportion of disadvantaged students enrolling in 2-year colleges", fill = "Proportion", x="",y="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) +
    coord_equal(ratio=1)+
    geom_text(data = appalachia.labels %>% filter(county_name %in% roanoke_county), aes(x=Longitude,y=Latitude, label=id2))
ggsave("Code/Maddie/output/ROANOKE-ZOOMmap_prop_disadv2year.png", device = "png", width = 11, height = 6, units = "in")

# four year disadvantaged
ggplot() + 
    #geom_polygon(data = virginia_t, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data %>% filter(county_name %in% roanoke_county), aes(x=long, y=lat, group=group, fill = disadvY_4_year_ps_prop), color = "black") +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) +
    labs(title = "Proportion of disadvantaged students enrolling in 4-year colleges", fill = "Proportion", x="",y="") +
    coord_equal(ratio=1)+
    geom_text(data = appalachia.labels %>% filter(county_name %in% roanoke_county), aes(x=Longitude,y=Latitude, label=id2))
ggsave("Code/Maddie/output/ROANOKE-ZOOMmap_prop_disadv4year.png", device = "png", width = 11, height = 6, units = "in")

# RICHMOND/EASTERN ------

# EASTERN (all non-appalachia schools, excluding open high and richmond community high, not labeling richmond schools) -----
# two year
ggplot() + 
    geom_polygon(data = richmond_t.df %>% filter(NAME %in% eastern), aes(x = long, y = lat, group = group),fill=NA, color='black') +
    geom_polygon(data = eastern_data %>% filter(county_name %in% eastern.sch), aes(x=long, y=lat, group=group, fill = totalProp_2YearCollegeEnrollment), color = "black", size = .1) +
    geom_text(data = east.nonrichmond.labels, aes(label = gsub("High", "",id), x = Longitude, y = Latitude), size = 4) +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    labs(title = "Proportion of students enrolling in 2-year colleges", fill = "Proportion", x="",y="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) +
    coord_equal(ratio=1)
ggsave("Code/Maddie/output/EASTERNmap_prop_2year.png", device = "png", width = 11, height = 6, units = "in")

# four year
ggplot() + 
    geom_polygon(data = richmond_t.df %>% filter(NAME %in% eastern), aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = eastern_data %>% filter(county_name %in% eastern.sch), aes(x=long, y=lat, group=group, fill = totalProp_4YearCollegeEnrollment), color = "black", size = .1) +
    geom_text(data = east.nonrichmond.labels, aes(label = gsub("High", "",id), x = Longitude, y = Latitude), size = 4) +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    labs(title = "Proportion of students enrolling in 2-year colleges", fill = "Proportion", x="",y="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) +
    coord_equal(ratio=1)
ggsave("Code/Maddie/output/EASTERNmap_prop_4year.png", device = "png", width = 11, height = 6, units = "in")

# disadvantaged two year
ggplot() + 
    geom_polygon(data = richmond_t.df %>% filter(NAME %in% eastern), aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = eastern_data %>% filter(county_name %in% eastern.sch), aes(x=long, y=lat, group=group, fill = disadvY_2_year_ps_prop), color = "black", size = .1) +
    geom_text(data = east.nonrichmond.labels, aes(label = gsub("High", "",id), x = Longitude, y = Latitude), size = 4) +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    labs(title = "Proportion of disadvantaged students enrolling in 2-year colleges", fill = "Proportion", x="",y="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) +
    coord_equal(ratio=1)
ggsave("Code/Maddie/output/EASTERNmap_prop_disadv2year.png", device = "png", width = 11, height = 6, units = "in")

# disadvantaged four year
ggplot() + 
    geom_polygon(data = richmond_t.df %>% filter(NAME %in% eastern), aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = eastern_data %>% filter(county_name %in% eastern.sch), aes(x=long, y=lat, group=group, fill = disadvY_4_year_ps_prop), color = "black", size = .1) +
    geom_text(data = east.nonrichmond.labels, aes(label = gsub("High", "",id), x = Longitude, y = Latitude), size = 4) +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    labs(title = "Proportion of disadvantaged students enrolling in 2-year colleges", fill = "Proportion", x="",y="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) +
    coord_equal(ratio=1)
ggsave("Code/Maddie/output/EASTERNmap_prop_disadv4year.png", device = "png", width = 11, height = 6, units = "in")
# FOCUS ON RICHMOND CITY ONLY (excluding open high and richmond community high) -------
# two year
ggplot() + 
    geom_polygon(data = virginia_t.df %>% filter(NAME %in% richmond_city), aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = richmond_data %>% filter(county_name %in% eastern.sch), aes(x=long, y=lat, group=group, fill = totalProp_2YearCollegeEnrollment), color = "black", size = .1) +
    geom_text(data = richmond.labels %>% filter(county_name %in% richmond_city), aes(label = gsub("High", "",id), x = Longitude, y = Latitude), size = 4) +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    labs(title = "Proportion of students enrolling in 2-year colleges", fill = "Proportion", x="",y="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) +
    coord_equal(ratio=1)
ggsave("Code/Maddie/output/RICHMOND-ZOOMmap_prop_2year.png", device = "png", width = 11, height = 6, units = "in")

# four year
ggplot() + 
    geom_polygon(data = virginia_t.df %>% filter(NAME %in% richmond_city), aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = richmond_data %>% filter(county_name %in% eastern.sch), aes(x=long, y=lat, group=group, fill = totalProp_4YearCollegeEnrollment), color = "black", size = .1) +
    geom_text(data = richmond.labels %>% filter(county_name %in% richmond_city), aes(label = gsub("High", "",id), x = Longitude, y = Latitude), size = 4) +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    labs(title = "Proportion of students enrolling in 4-year colleges", fill = "Proportion", x="",y="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) +
    coord_equal(ratio=1)
ggsave("Code/Maddie/output/RICHMOND-ZOOMmap_prop_4year.png", device = "png", width = 11, height = 6, units = "in")

# disadvantaged two year
ggplot() + 
    geom_polygon(data = virginia_t.df %>% filter(NAME %in% richmond_city), aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = richmond_data %>% filter(county_name %in% eastern.sch), aes(x=long, y=lat, group=group, fill = disadvY_2_year_ps_prop), color = "black", size = .1) +
    geom_text(data = richmond.labels %>% filter(county_name %in% richmond_city), aes(label = gsub("High", "",id), x = Longitude, y = Latitude), size = 4) +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    labs(title = "Proportion of disadvantaged students enrolling in 2-year colleges", fill = "Proportion", x="",y="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) +
    coord_equal(ratio=1)
ggsave("Code/Maddie/output/RICHMOND-ZOOMmap_prop_disadv2year.png", device = "png", width = 11, height = 6, units = "in")

# disadvantaged four year
ggplot() + 
    geom_polygon(data = virginia_t.df %>% filter(NAME %in% richmond_city), aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = richmond_data %>% filter(county_name %in% eastern.sch), aes(x=long, y=lat, group=group, fill = disadvY_4_year_ps_prop), color = "black", size = .1) +
    geom_text(data = richmond.labels %>% filter(county_name %in% richmond_city), aes(label = gsub("High", "",id), x = Longitude, y = Latitude), size = 4) +
    scale_fill_gradient(limits = c(0,1), low = "white", high = "navyblue") +
    labs(title = "Proportion of disadvantaged students enrolling in 4-year colleges", fill = "Proportion", x="",y="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) +
    coord_equal(ratio=1)
ggsave("Code/Maddie/output/RICHMOND-ZOOMmap_prop_disadv4year.png", device = "png", width = 11, height = 6, units = "in")

