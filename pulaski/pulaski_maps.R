# pulaski vis
# 1/10/2017
setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)")

sch_cw <- read.csv("Code/Maddie/pulaski/VDOE_long_format_merge-allVA.csv")
NRV <- list("Pulaski County","Radford City","Montgomery County","Giles County","Floyd County") 

# maddie's stuff
library(tmaptools)
library(stringr)
library(sp)
library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)
# map themes
library(tidycensus)
library(tidyverse)
library(tigris)
library(viridis)
#SDAL map theme
theme_map <- function(...) {
    theme_minimal() +
        theme(
            text=element_text(family="sans", color="#22211d"),
            plot.title = element_text(size=18),
            plot.subtitle = element_text(size=15),
            plot.caption = element_text(size=15),
            axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major=element_blank(),
            #panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
            panel.grid.minor=element_blank(),
            #panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
            plot.background=element_rect(fill="#f5f5f2", color = NA), 
            panel.background=element_rect(fill="#f5f5f2", color = NA), 
            legend.background=element_rect(fill="#f5f5f2", color = NA),
            panel.border=element_blank(),
            ...
        )
}


# PREPARE COUNTY & SCHOOL BOUNDARY SHAPEFILES ####
# import school boundary shapefile 
boundaries <- read_shape("Data/School Attendance Boundaries/SABS_1314_SchoolLevels/SABS_1314_High.shp")
boundaries <- spTransform(boundaries,CRS("+init=epsg:4326"))

boundaries$schnam <- gsub("PULASKI HIGH SCHOOL", "PULASKI COUNTY SENIOR HIGH", boundaries$schnam)
# filter for schools in virginia
va_boundaries <- boundaries[which(str_sub(as.character(boundaries$leaid),1,2)=="51"),] # Virginia

# import county shapefile
usa <- read_shape("Data/GIS/cb_2016_us_county_5m/cb_2016_us_county_5m.shp")
# filter for Virginia
virginia <-usa[usa@data$STATEFP==51,]
# transform CRS of virginia to that of sch_boundaries
virginia_t <- spTransform(virginia, CRS(proj4string(va_boundaries)))

pulaski_t <- virginia_t[virginia_t$NAME=="Pulaski",]
giles_t <- virginia_t[virginia_t$NAME=="Giles",]

# add pulaski and giles to school va_boundaries shp [GILES HAS 2 HIGH SCHOOLS - SHAPEFILES UNAVAILABLE]
pulaski_t@data <- data.frame(SrcName = NA, ncessch = NA, schnam = "PULASKI COUNTY SENIOR HIGH", leaid = "5100060",updateDate=NA,gslo=NA,gshi=NA,defacto=NA,stAbbrev="VA",sLevel=NA,openEnroll=0,MultiBdy=0,Shape_Leng=NA,Shape_Area=828438966)
new_shp <- rbind(va_boundaries, pulaski_t)

load("Code/Maddie/pulaski/giles_county_highschool_boundaries_polygons.RData")
narrows_poly <- spTransform(narrows_poly, CRS(proj4string(va_boundaries)))
giles_poly <- spTransform(giles_poly, CRS(proj4string(va_boundaries)))


# plot
plot(virginia_t)
plot(new_shp, col="limegreen", add=T)
plot(narrows_poly, col="orange", add=T)
plot(giles_poly, col="pink", add=T)

narrows_poly@data <- data.frame(SrcName = NA, ncessch = NA, schnam = "NARROWS HIGH", leaid = "5100060",updateDate=NA,gslo=NA,gshi=NA,defacto=NA,stAbbrev="VA",sLevel=NA,openEnroll=0,MultiBdy=0,Shape_Leng=NA,Shape_Area=466307920)
giles_poly@data <- data.frame(SrcName = NA, ncessch = NA, schnam = "GILES HIGH", leaid = "5100060",updateDate=NA,gslo=NA,gshi=NA,defacto=NA,stAbbrev="VA",sLevel=NA,openEnroll=0,MultiBdy=0,Shape_Leng=NA,Shape_Area=466307920)
sch_boundaries <- rbind(va_boundaries, pulaski_t, narrows_poly, giles_poly)

plot(virginia_t)
plot(sch_boundaries, col="purple", add=T)

# prepare for ggplot presentation
sch_boundaries@data$id = rownames(sch_boundaries@data)
sch_boundaries.points = fortify(sch_boundaries, region="id")
sch_boundaries.df = join(sch_boundaries.points, sch_boundaries@data, by="id")

virginia_t@data$id = rownames(virginia_t@data)
virginia_t.points = fortify(virginia_t, region="id")
virginia_t.df = join(virginia_t.points, virginia_t@data, by="id")

sch_boundaries.fort <- fortify(sch_boundaries, region = "schnam")
idList <- sch_boundaries@data$schnam
centroids.df <- as.data.frame(coordinates(sch_boundaries))
names(centroids.df) <- c("Longitude", "Latitude")
text.labels.df <- left_join(data.frame(id = idList, centroids.df), sch_cw, by=c("id"="sch_name_clean"))
text.labels.df <- text.labels.df[,1:3]
text.labels.df$id_short <- gsub("([A-Za-z]+).*", "\\1", text.labels.df$id)


map_data <- left_join(x=sch_cw, y=sch_boundaries.df, by = c("sch_names"= "schnam"))
map_data$cohort_dropout_rate <- map_data$cohort_dropout_cnt / map_data$total_students_sch
map_data$total_dropout_rate <- map_data$dropoutTotal / map_data$total_students_sch
map_data$percent_grads_earned_ged <- map_data$ged_certificate / map_data$total_grads
map_data$percent_grads_employment <- map_data$employment / map_data$total_grads
map_data$num_grads_cont_ed <- map_data$attending_two_year_college + map_data$attending_four_year_college + map_data$other_continuing_ed_plans
map_data$percent_grads_cont_ed <- map_data$num_grads_cont_ed / map_data$total_grads

map_data$student_offenses_ratio <- map_data$numStudentOffenses / map_data$total_students_sch


map_data.f <- map_data %>% filter(GENDER=="F")

# VIRGINIA MAPS ####


# 
# summary(map_data.f$cohort_dropout_rate)[6]
# summary(map_data$total_dropout_rate)[6] # REALLY high because of alternative/adult high schools...
# # female drop out rate
# ggplot() +
#     geom_polygon(data = virginia_t, aes(x = long, y = lat, group = group), fill=NA,color='black') +
#     geom_polygon(data = map_data.f %>% filter(year == "2014"), aes(x=long, y=lat, group=group, fill = cohort_dropout_rate), color = "black", size = .1) +
#     scale_fill_gradient(limits = c(0,0.1592), low = "white", high = "orangered") +
#     coord_equal() +
#     theme_map() +
#     labs(title="Female student dropout rate (2014-2015)", x="", y="", fill = "Dropout Rate") +
#     geom_text(data = text.labels.df, aes(label = id_short, x = Longitude, y = Latitude), size = 3)
# ggsave("Code/Maddie/pulaski/vis/female_dropout_rate-VA.png", device = "png", width = 11, height = 6, units = "in")
# # total drop out rate
# ggplot() +
#     geom_polygon(data = virginia_t, aes(x = long, y = lat, group = group), fill=NA,color='black') +
#     geom_polygon(data = map_data, aes(x=long, y=lat, group=group, fill = total_dropout_rate), color = "black", size = .1) +
#     scale_fill_gradient(limits = c(0,0.1), low = "white", high = "orangered") +
#     coord_equal() +
#     theme_map() +
#     labs(title="Total student dropout rate", x="", y="", fill = "Dropout Rate") +
#     geom_text(data = text.labels.df, aes(label = gsub(c("HIGH","COUNTY","SENIOR"), "",id), x = Longitude, y = Latitude), size = 3)
# ggsave("Code/Maddie/pulaski/vis/total_dropout_rate-VA.png", device = "png", width = 11, height = 6, units = "in")

# NRV MAPS ####
nrv_t.df <- virginia_t.df %>% filter(NAME %in% gsub(" .*", "", c("Giles", "Montgomery", "Pulaski", "Floyd"))) #%>% filter(long < 1500000)
map_data.nrv <- map_data %>% filter(county_name %in% NRV)
text.labels.df2 <- text.labels.df %>% filter(id %in% c("NARROWS HIGH", "GILES HIGH", "PULASKI COUNTY SENIOR HIGH", "AUBURN HIGH","BLACKSBURG HIGH","CHRISTIANSBURG HIGH","EASTERN MONTGOMERY HIGH","RADFORD HIGH","FLOYD COUNTY HIGH"))

# text.labels.df2$google <- paste(text.labels.df2$id, "virginia public school")
# 
# for(i in 1:nrow(text.labels.df2)){    
#     location <- text.labels.df2$google[i]
#     coords <- ggmap::geocode(location, output = "latlon", source = "google")
#     text.labels.df2$long_hs[i] <- coords$lon
#     text.labels.df2$lat_hs[i] <- coords$lat
#     
# }
# 
# save(text.labels.df2, file = "Code/Maddie/pulaski/text.labels2.RData")

load("Code/Maddie/pulaski/text.labels2.RData")
library(ggmap)
al1 = get_map(location = c(lon = -80.44353, lat = 37.05961), zoom = 9, source = "google", color ="bw")
al2 = get_map(location = c(lon = -80.44353, lat = 37.05961), zoom = 9, source = "google")

al1MAP = ggmap::ggmap(al1)
al1MAP

al2MAP = ggmap::ggmap(al2)

pal <- rev(viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1,
                       option = "A")(20))[c(1,3,6,8,10,13,15,17,20)] 

show_col(pal)


# boundary maps
# 1
png("Code/Maddie/pulaski/vis/sch_boundary_map1.png", height = 900, width = 900)   
al1MAP +
   # geom_polygon(data = nrv_t.df, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data.nrv, aes(x=long, y=lat, group=group, fill=sch_name_clean), color = "black", size = .1, alpha=.5) +
    geom_point(data = text.labels.df2, aes(x= long_hs, y = lat_hs), size = 4) +
    #geom_label(data = text.labels.df2, aes(label = str_to_title(id), x = long_hs, y = lat_hs), vjust=-1, size = 5) +
    scale_fill_manual(values=pal, name="School") + 
    coord_fixed(ratio=1.2,xlim=range(map_data.nrv$long)+c(-.05,.05),ylim=range(map_data.nrv$lat)+c(-.05,.05)) +
    theme(legend.text = element_text(size=15), legend.title = element_text(size=20), 
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
dev.off()
# 2
pdf("Code/Maddie/pulaski/vis/sch_boundary_map2.pdf", height = 9, width = 12)   
al2MAP +
    geom_polygon(data = map_data.nrv, aes(x=long, y=lat, group=group), fill="white",color = "black", size = .1, alpha=.9) +
    geom_point(data = text.labels.df2, aes(x= long_hs, y = lat_hs), size = 4) +
    geom_label(data = text.labels.df2, aes(label = str_to_title(id), x = long_hs, y = lat_hs), vjust=-1, size = 5) +
    coord_fixed(ratio=1.2,xlim=range(map_data.nrv$long)+c(-.05,.05),ylim=range(map_data.nrv$lat)+c(-.05,.05)) +
    theme(legend.text = element_text(size=15), legend.title = element_text(size=20), 
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
dev.off()

png("Code/Maddie/pulaski/vis/sch_boundary_map19.1.png", height = 900, width = 900)   
al1MAP +
    # geom_polygon(data = nrv_t.df, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data.nrv, aes(x=long, y=lat, group=group, fill=sch_name_clean), color = "black", size = .1, alpha=.9) +
    geom_point(data = text.labels.df2, aes(x= long_hs, y = lat_hs), size = 4) +
    geom_point(data = text.labels.df2, aes(x= long_hs, y = lat_hs), size = 4, shape=1, color="white") +
    #geom_label(data = text.labels.df2, aes(label = str_to_title(id), x = long_hs, y = lat_hs), vjust=-1, size = 5) +
    scale_fill_manual(values=pal, name="School") + 
    coord_fixed(ratio=1.2,xlim=range(map_data.nrv$long)+c(-.05,.05),ylim=range(map_data.nrv$lat)+c(-.05,.05)) +
    theme(legend.text = element_text(size=15), legend.title = element_text(size=20), 
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
dev.off()

# total drop out rate
summary(map_data.nrv$cohort_dropout_rate)[6]

# total_dropout <- ggplot() +
#     geom_polygon(data = nrv_t.df, aes(x = long, y = lat, group = group), fill=NA,color='black') +
#     geom_polygon(data = map_data.nrv %>% filter(year == "2014"), aes(x=long, y=lat, group=group, fill = cohort_dropout_rate), color = "black", size = .1) +
#     scale_fill_gradient(limits = c(0,0.05), low = "white", high = "orangered") +
#     coord_equal() +
#     theme_map() +
#     labs(title="Total student dropout rate (New River Valley, 2014-2015)", x="", y="", fill = "Dropout Rate") +
#     geom_text(data = text.labels.df2, aes(label = id_short, x = Longitude, y = Latitude), size = 3)

# female drop out rate
female_dropout <- ggplot() +
    geom_polygon(data = nrv_t.df, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data.nrv %>% filter(GENDER =="F"), aes(x=long, y=lat, group=group, fill = cohort_dropout_rate), color = "black", size = .1) +
    scale_fill_gradient(limits = c(0,0.05), low = "white", high = "orangered") +
    coord_equal() +
    theme_map() +
    labs(title="Female student dropout rate (New River Valley, 2014-2015)", x="", y="", fill = "Dropout Rate") +
    geom_text(data = text.labels.df2, aes(label = id_short, x = Longitude, y = Latitude), size = 3)

male_dropout <- ggplot() +
    geom_polygon(data = nrv_t.df, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data.nrv %>% filter(GENDER =="M"), aes(x=long, y=lat, group=group, fill = cohort_dropout_rate), color = "black", size = .1) +
    scale_fill_gradient(limits = c(0,0.05), low = "white", high = "orangered") +
    coord_equal() +
    theme_map() +
    labs(title="Male student dropout rate (New River Valley, 2014-2015)", x="", y="", fill = "Dropout Rate") +
    geom_text(data = text.labels.df2, aes(label = id_short, x = Longitude, y = Latitude), size = 3)


# # percent of graduates earning a GED (instead of standard diploma)
# percent_grads_earned_ged <- ggplot() +
#     geom_polygon(data = nrv_t.df, aes(x = long, y = lat, group = group), fill=NA,color='black') +
#     geom_polygon(data = map_data.nrv %>% filter(year == "2013"), aes(x=long, y=lat, group=group, fill = percent_grads_earned_ged), color = "black", size = .1) +
#     #scale_fill_gradient(limits = c(0,0.05), low = "white", high = "orangered") +
#     coord_equal() +
#     theme_map() +
#     labs(title="Percent of graduates earning a GED (New River Valley, 2011)", x="", y="", fill = "Percent") +
#     geom_text(data = text.labels.df2, aes(label = id_short, x = Longitude, y = Latitude), size = 3)

text.labels.df2 <- text.labels.df %>% filter(id %in% c("NARROWS HIGH", "GILES HIGH", "PULASKI COUNTY SENIOR HIGH", "AUBURN HIGH","BLACKSBURG HIGH","CHRISTIANSBURG HIGH","EASTERN MONTGOMERY HIGH","RADFORD HIGH","FLOYD COUNTY HIGH"))
# 
# percent_grads_cont_ed <- ggplot() +
#     geom_polygon(data = nrv_t.df, aes(x = long, y = lat, group = group), fill=NA,color='black') +
#     geom_polygon(data = map_data.nrv %>% filter(year == "2014"), aes(x=long, y=lat, group=group, fill = percent_grads_cont_ed), color = "black", size = .1) +
#     scale_fill_gradient(limits = c(0.5,1), low = "white", high = "darkgreen") +
#     coord_equal() +
#     theme_map() +
#     labs(title="Percent of graduates continuing education (NRV, 2014-2015)", x="", y="", fill = "Percent") +
#     geom_text(data = text.labels.df2, aes(label = id_short, x = Longitude, y = Latitude), size = 3)
# 
# percent_grads_employment <- ggplot() +
#     geom_polygon(data = nrv_t.df, aes(x = long, y = lat, group = group), fill=NA,color='black') +
#     geom_polygon(data = map_data.nrv %>% filter(year == "2014"), aes(x=long, y=lat, group=group, fill = percent_grads_employment), color = "black", size = .1) +
#     scale_fill_gradient(c(.5,1),low = "white", high = "seagreen4") +
#     coord_equal() +
#     theme_map() +
#     labs(title="Percent of graduates entering workforce (NRV, 2014-2015)", x="", y="", fill = "Percent") +
#     geom_text(data = text.labels.df2, aes(label = id_short, x = Longitude, y = Latitude), size = 3)

# student_offenses_ratio <- ggplot() +
library(ggrepel)
# map_data$student_offenses_ratio <- map_data$numStudentOffenses / map_data$total_students_sch

png("Code/Maddie/pulaski/vis/purple_NRV_disciplinary_map.png", 900, 900)
    al1MAP +
    geom_polygon(data = nrv_t.df, aes(x = long, y = lat, group = group), fill=NA,color='black') +
    geom_polygon(data = map_data.nrv %>% filter(year == "2014" & disciplineType == "SHORT-TER M SUSPENSION (OUT OF SCHOOL)"), aes(x=long, y=lat, group=group, fill = student_offenses_ratio), color = "grey50", size = .1, alpha=.9) +
        scale_fill_viridis(option="magma") +
        guides(fill = guide_colorbar(barwidth = 2, barheight = 30)) +
    coord_equal() +
    coord_fixed(ratio=1.2,xlim=range(map_data.nrv$long)+c(-.05,.05),ylim=range(map_data.nrv$lat)+c(-.05,.05)) +
        theme(legend.text = element_text(size=20), legend.title = element_text(size=25),
              axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
              title = element_text(size=25)) +
    labs(title=paste(str_to_title("SHORT-TERM SUSPENSION (OUT OF SCHOOL) "),"(NRV, 2014-2015)"), fill = "Percent") +
    geom_label_repel(data = text.labels.df2, aes(label = id_short, x = Longitude, y = Latitude), size = 7)
dev.off()
