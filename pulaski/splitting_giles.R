# functions from https://stat.ethz.ch/pipermail/r-help//2016-February/436631.html

library(tmaptools)
library(stringr)
library(sp)
library(ggplot2)
library(plyr)
# 
# # PREPARE COUNTY & SCHOOL BOUNDARY SHAPEFILES ####
# # import school boundary shapefile 
# boundaries <- read_shape("Data/School Attendance Boundaries/SABS_1314_SchoolLevels/SABS_1314_High.shp")
# boundaries$schnam <- gsub("PULASKI HIGH SCHOOL", "PULASKI COUNTY SENIOR HIGH", boundaries$schnam)
# # filter for schools in virginia
# va_boundaries <- boundaries[which(str_sub(as.character(boundaries$leaid),1,2)=="51"),] # Virginia
# 
# # import county shapefile
# usa <- read_shape("~/Downloads/cb_2016_us_county_5m/cb_2016_us_county_5m.shp")
# # filter for Virginia
# virginia <-usa[usa@data$STATEFP==51,]
# # transform CRS of virginia to that of sch_boundaries
# virginia_t <- spTransform(virginia, CRS(proj4string(va_boundaries)))
# 
# giles_t <- virginia_t[virginia_t$NAME=="Giles",]
# 
# 
# 
# 
# pol <- giles_t

makeVchopper <- function(pol){ 
    bb = bbox(pol) 
    delta = (bb[2,2] - bb[2,1])/10 
    xmin = bb[1,1]-delta 
    ymin = bb[2,1]-delta 
    ymax = bb[2,2]+delta 
    
    choppoly = function(xmax){ 
        readWKT(sprintf("POLYGON((%s %s, %s %s, %s %s, %s %s, %s %s))", 
                        xmin,ymin, xmin,ymax, xmax,ymax, xmax,ymin, xmin,ymin)) 
    } 
    choppoly 
} 

slicer <- function(pol, xmin, xmax){ 
    bb = bbox(pol) 
    delta = (bb[2,2] - bb[2,1])/10 
    ymax = bb[2,2] + delta 
    ymin = bb[2,1] - delta 
    r = readWKT(sprintf("POLYGON((%s %s, %s %s, %s %s, %s %s, %s %s))", 
                        xmin,ymin, xmin,ymax, xmax,ymax, xmax,ymin, xmin,ymin)) 
    gIntersection(pol,r) 
} 

chop_thirds <- function(pol, fractions=c(1/3, 2/3)){
    chopper = makeVchopper(pol)
    bb = bbox(pol)
    xmin = bb[1,1]
    xmax = bb[1,2]
    
    totalArea = gArea(pol)
    
    chopped_area = function(x){
        gArea(gIntersection(chopper(x),pol))
    }
    
    edges = lapply(fractions, function(fraction){
        target = totalArea * fraction
        target_function = function(x){
            chopped_area(x) - target
        }
        uniroot(target_function, lower=xmin, upper=xmax)$root
    })
    
    xdelta = (xmax-xmin)/10
    chops = matrix(c(xmin-xdelta, rep(edges,rep(2,length(edges))),
                     xmax+xdelta), ncol=2, byrow=TRUE)
    apply(chops, 1, function(edges){
        slicer(pol, edges[1], edges[2])
    })
    
}

chop_half <- function(pol, fractions=c(1/2)){ 
    chopper = makeVchopper(pol) 
    bb = bbox(pol) 
    xmin = bb[1,1] 
    xmax = bb[1,2] 
    
    totalArea = gArea(pol) 
    
    chopped_area = function(x){ 
        gArea(gIntersection(chopper(x),pol)) 
    } 
    
    edges = lapply(fractions, function(fraction){ 
        target = totalArea * fraction 
        target_function = function(x){ 
            chopped_area(x) - target 
        } 
        uniroot(target_function, lower=xmin, upper=xmax)$root 
    }) 
    
    xdelta = (xmax-xmin)/10 
    chops = matrix(c(xmin-xdelta, rep(edges,rep(2,length(edges))), 
                     xmax+xdelta), ncol=2, byrow=TRUE) 
    apply(chops, 1, function(edges){ 
        slicer(pol, edges[1], edges[2]) 
    }) 
    
} 

    
# library(rgeos) 
# library(sp) 
# # sample data 
# plot(pol) 
# 
# # now split 
# 
# parts = chop_thirds(pol) 
# plot(pol) 
# plot(parts[[1]], add=TRUE, col=1) 
# plot(parts[[2]], add=TRUE, col=2) 
# 
# giles_hs <- parts[[1]]
# narrows_hs <- parts[[2]]
# 
# plot(giles_hs)
# plot(narrows_hs)
# 
# # convert giles_hs and narrows_hs back to SpatialPolygonsDataFrame
# library("rgdal")
# # do some staff with "poly_df" that doesn't support SpatialPolygonsDataFrame
# narrows_df <- data.frame(STATEFP=51,COUNTYFP=as.character(071), COUNTYNS=NA, AFFGEOID=NA, GEOID=NA, NAME = "Narrows High", LSAD = NA, ALAND = 466307920, AWATER = NA)
# narrows_poly <- SpatialPolygonsDataFrame(narrows_hs, narrows_df)
# plot(narrows_poly)
# 
# giles_df <- data.frame(STATEFP=51,COUNTYFP=as.character(071), COUNTYNS=NA, AFFGEOID=NA, GEOID=NA, NAME = "Giles High", LSAD = NA, ALAND = 466307920, AWATER = NA)
# giles_poly <- SpatialPolygonsDataFrame(giles_hs, giles_df)
# plot(giles_poly)
# 
# save(giles_poly, narrows_poly, file = "Code/Maddie/pulaski/giles_county_highschool_boundaries_polygons.RData")
