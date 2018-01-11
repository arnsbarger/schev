# pulaski vis
# 1/10/2017
setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)")

sch_cw <- read.csv("Code/Maddie/pulaski/VDOE_wide_format_merge-allVA.csv")
NRV <- list("Pulaski County","Radford City","Montgomery County","Giles County","Floyd County") 

library(tmaptools)
library(stringr)
library(sp)
library(ggplot2)
library(plyr)

# PREPARE COUNTY & SCHOOL BOUNDARY SHAPEFILES ####
# import school boundary shapefile 
boundaries <- read_shape("Data/School Attendance Boundaries/SABS_1314_SchoolLevels/SABS_1314_High.shp")
boundaries$schnam <- gsub("PULASKI HIGH SCHOOL", "PULASKI COUNTY SENIOR HIGH", boundaries$schnam)
# filter for schools in virginia
va_boundaries <- boundaries[which(str_sub(as.character(boundaries$leaid),1,2)=="51"),] # Virginia
# get school names we're interested in
sch_names<-left_join(sch_cw,va_boundaries@data,by=c("sch_names"="schnam"))
sch_names<- sch_names[which(sch_names$county_name %in% NRV),] # study area (NRV = New River Valley)
sch_names<-sch_names$sch_names
# filter for schools in study area
sch_boundaries <- va_boundaries[which(va_boundaries$schnam %in% sch_names),] # study area 

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
plot(sch_boundaries, col="orange", add=T)

# prepare for ggplot presentation
sch_boundaries@data$id = rownames(sch_boundaries@data)
sch_boundaries.points = fortify(sch_boundaries, region="id")
sch_boundaries.df = join(sch_boundaries.points, sch_boundaries@data, by="id")

virginia_t@data$id = rownames(virginia_t@data)
virginia_t.points = fortify(virginia_t, region="id")
virginia_t.df = join(virginia_t.points, virginia_t@data, by="id")

sch_boundaries.fort <- fortify(sch_boundaries, region = "sch_name_clean")
idList <- sch_boundaries@data$sch_name_clean
centroids.df <- as.data.frame(coordinates(sch_boundaries))
names(centroids.df) <- c("Longitude", "Latitude")
text.labels.df <- left_join(data.frame(id = idList, centroids.df), sch_cw, by=c("id"="sch_name_clean"))


# PLOT MAP ####
map_data <- sch_boundaries.df
map_data$female_dropout_rate2011 <- map_data$female_dropouts2011 / map_data$sch_total2011

ggplot() + 
    geom_polygon(data = virginia_t, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data, aes(x=long, y=lat, group=group, fill = female_dropout_rate2011), color = "black", size = .1) +
    scale_fill_gradient(limits = c(0,0.015), low = "white", high = "red") +
    geom_text(data = text.labels.df, aes(label = gsub("High", "",id), x = Longitude, y = Latitude), size = 3) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) + #labels
    # labs(title="Proportion of students enrolling in 2-year colleges", x="", y="", fill = "Proportion") +
    coord_equal(ratio=1)
ggsave("Code/Maddie/pulaski/vis/discipline_rate.png", device = "png", width = 11, height = 6, units = "in")
