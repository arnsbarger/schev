# Aggregating IPEDS data for SCHEV Fall 2017 project to create the following variables:

library(ggplot2)
library(gdata)
library(dplyr)
library(raster)
library(maptools)

setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Data/IPEDS/")

ipeds1516 <- read.csv("output/ipeds_subset1516.csv")

# get lat/longs of each high school ####
highschools <- read.csv("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Bianica/school_crosswalk.csv") # wrote the csv below - now this file has the coordinates already
highschools$google_query <- paste(highschools$sch_names, highschools$div_name)

# library(ggmap)
# 
# for(i in 1:nrow(highschools)){
#     location <- highschools$google[i]
#     coords <- ggmap::geocode(location, output = "latlon", source = "google")
#     highschools$long[i] <- coords$lon
#     highschools$lat[i] <- coords$lat
#     
# }

# assign surrounding counties ####
bland_county <- c("Tazewell County", "Smyth County", "Wythe County", "Pulaski County", "Giles County", "Bland County")
buchanan_county <- c("Dickenson County", "Russell County", "Tazewell County", "Buchanan County")
roanoke_county <- c("Roanoke City","Montgomery County", "Floyd County", "Franklin County", "Bedford County", "Botetourt County", "Craig County", "Roanoke County")
roanoke_city <- c("Roanoke County", "Salem County", "Roanoke City", roanoke_county)
richmond_city <- c("Chesterfield County", "Henrico County", "Richmond City")
sussex_county <- c("Prince George County", "Dinwiddie County", "Greensville County", "Southampton County", "Isle of Wight County", "Surry County", "Sussex County")
powhatan <- c("Goochland County", "Cumberland County", "Amelia County", "Chesterfield County", "Powhatan County")


counties <- list(bland_county, buchanan_county, roanoke_city, roanoke_county, richmond_city,sussex_county,powhatan)

# calculate nearby colleges (number in county + number in surrounding counties)
# all schools ####
for(i in 1:7){
    highschools$count_all_colleges[highschools$div_name==unique(highschools$div_name)[i]] <- sum(as.character(ipeds1516$COUNTYNM1516) %in% counties[[i]])
}
# 4-year schools ####
ipeds1516$INSTNM1516[ipeds1516$CCIPUG1516 %in% 6:17]

hd_ic2015_4year <- ipeds1516 %>% filter(CCIPUG1516 %in% 6:17)

for(i in 1:7){
    highschools$count_4year[highschools$div_name==unique(highschools$div_name)[i]] <- sum(as.character(hd_ic2015_4year$COUNTYNM1516) %in% counties[[i]])
}
# community colleges ####
ipeds1516$INSTNM1516[ipeds1516$CCIPUG1516 %in% 1:2]

hd_ic2015_cc <- ipeds1516 %>% filter(CCIPUG1516 %in% 1:2)

for(i in 1:7){
    highschools$count_comm_college[highschools$div_name==unique(highschools$div_name)[i]] <- sum(as.character(hd_ic2015_cc$COUNTYNM1516) %in% counties[[i]])
}
# career schools ####
ipeds1516$INSTNM1516[ipeds1516$CCIPUG1516 %in% c(3:5,18:20)]

hd_ic2015_career <- ipeds1516 %>% filter(CCIPUG1516 %in% c(3:5,18:20))

for(i in 1:7){
    highschools$count_career_schools[highschools$div_name==unique(highschools$div_name)[i]] <- sum(as.character(hd_ic2015_career$COUNTYNM1516) %in% counties[[i]])
}
# vocational/cte schools ####
ipeds1516$INSTNM1516[ipeds1516$CCIPUG1516 == -2]

hd_ic2015_cte <- ipeds1516 %>% filter(CCIPUG1516  == -2)

for(i in 1:7){
    highschools$count_cte_schools[highschools$div_name==unique(highschools$div_name)[i]] <- sum(hd_ic2015_cte$COUNTYNM1516 %in% counties[[i]])
}

# finance/admissions variables ####
# fanp <- read.csv("Student Financial Aid and Net Price/sfa1516.csv", na.strings=c("",".","NA"))
# admissions <- read.csv("Admissions and Test Scores/adm2015.csv")
# 
# hd_icay_fanp2015 <- merge(ipeds1516, fanp, by = "UNITID", all.x = TRUE)
# hd_icay_fanp2015 <- merge(hd_icay_fanp2015, admissions, by = "UNITID", all.x = TRUE)


for(i in 1:length(counties)){
    
    colleges <- ipeds1516 %>% filter(COUNTYNM1516 %in% counties[[i]])
    
    # # financial aid / net price
    # # percent of students to receive aid of any type 2015
    # highschools$mean_percent_receive_aid2015[highschools$div_name==unique(highschools$div_name)[i]] <- mean(colleges$UAGRNTP1516, na.rm=TRUE)
    # highschools$median_percent_receive_aid2015[highschools$div_name==unique(highschools$div_name)[i]] <- median(colleges$UAGRNTP1516, na.rm=TRUE)
    # highschools$min_percent_receive_aid2015[highschools$div_name==unique(highschools$div_name)[i]] <- min(colleges$UAGRNTP1516, na.rm=TRUE)
    # highschools$max_percent_receive_aid2015[highschools$div_name==unique(highschools$div_name)[i]] <- max(colleges$UAGRNTP1516, na.rm=TRUE)
    # # sum of aid of any type given to students 2015
    # highschools$mean_total_aid2015[highschools$div_name==unique(highschools$div_name)[i]] <- mean(colleges$UAGRNTT1516, na.rm=TRUE)
    # highschools$median_total_aid2015[highschools$div_name==unique(highschools$div_name)[i]] <- median(colleges$UAGRNTT1516, na.rm=TRUE)
    # highschools$min_total_aid2015[highschools$div_name==unique(highschools$div_name)[i]] <- min(colleges$UAGRNTT1516, na.rm=TRUE)
    # highschools$max_total_aid2015[highschools$div_name==unique(highschools$div_name)[i]] <- max(colleges$UAGRNTT1516, na.rm=TRUE)
    # # average amount of aid of any type given to students 2015
    # highschools$mean_average_aid2015[highschools$div_name==unique(highschools$div_name)[i]] <- mean(colleges$UAGRNTA1516, na.rm=TRUE)
    # highschools$median_average_aid2015[highschools$div_name==unique(highschools$div_name)[i]] <- median(colleges$UAGRNTA1516, na.rm=TRUE)
    # highschools$min_average_aid2015[highschools$div_name==unique(highschools$div_name)[i]] <- min(colleges$UAGRNTA1516, na.rm=TRUE)
    # highschools$max_average_aid2015[highschools$div_name==unique(highschools$div_name)[i]] <- max(colleges$UAGRNTA1516, na.rm=TRUE)
    # 
    # # average net price (definition below) 2015
    # highschools$mean_net_price2015[highschools$div_name==unique(highschools$div_name)[i]] <- mean(colleges$NPIST11516, na.rm=TRUE)
    # highschools$median_net_price2015[highschools$div_name==unique(highschools$div_name)[i]] <- median(colleges$NPIST11516, na.rm=TRUE)
    # highschools$min_net_price2015[highschools$div_name==unique(highschools$div_name)[i]] <- min(colleges$NPIST11516, na.rm=TRUE)
    # highschools$max_net_price2015[highschools$div_name==unique(highschools$div_name)[i]] <- max(colleges$NPIST11516, na.rm=TRUE)
    # 
    # ---------SAT---------------------------------
    
    # percent of students submitting SAT scores 2015
    highschools$mean_percent_submit_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- mean(colleges$SATPCT1516, na.rm=TRUE)
    highschools$median_percent_submit_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- median(colleges$SATPCT1516, na.rm=TRUE)
    highschools$min_percent_submit_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- min(colleges$SATPCT1516, na.rm=TRUE)
    highschools$max_percent_submit_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- max(colleges$SATPCT1516, na.rm=TRUE)
    
    # SAT 25th verbal percentile 2015
    highschools$mean_verbal25_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- mean(colleges$SATVR251516, na.rm=TRUE)
    highschools$median_verbal25_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- median(colleges$SATVR251516, na.rm=TRUE)
    highschools$min_verbal25_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- min(colleges$SATVR251516, na.rm=TRUE)
    highschools$max_verbal25_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- max(colleges$SATVR251516, na.rm=TRUE)
    
    # SAT 75th verbal percentile 2015
    highschools$mean_verbal75_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- mean(colleges$SATVR751516, na.rm=TRUE)
    highschools$median_verbal75_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- median(colleges$SATVR751516, na.rm=TRUE)
    highschools$min_verbal75_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- min(colleges$SATVR751516, na.rm=TRUE)
    highschools$max_verbal75_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- max(colleges$SATVR751516, na.rm=TRUE)
    
    # SAT 25th math percentile 2015
    highschools$mean_math25_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- mean(colleges$SATMT251516, na.rm=TRUE)
    highschools$median_math25_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- median(colleges$SATMT251516, na.rm=TRUE)
    highschools$min_math25_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- min(colleges$SATMT251516, na.rm=TRUE)
    highschools$max_math25_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- max(colleges$SATMT251516, na.rm=TRUE)
    
    # SAT 75th math percentile 2015
    highschools$mean_math75_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- mean(colleges$SATMT751516, na.rm=TRUE)
    highschools$median_math75_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- median(colleges$SATMT751516, na.rm=TRUE)
    highschools$min_math75_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- min(colleges$SATMT751516, na.rm=TRUE)
    highschools$max_math75_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- max(colleges$SATMT751516, na.rm=TRUE)
    
    # # SAT 25th writing percentile 2015
    # highschools$mean_writing25_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- mean(colleges$SATWR251516, na.rm=TRUE)
    # highschools$median_writing25_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- median(colleges$SATWR251516, na.rm=TRUE)
    # highschools$min_writing25_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- min(colleges$SATWR251516, na.rm=TRUE)
    # highschools$max_writing25_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- max(colleges$SATWR251516, na.rm=TRUE)
    # 
    # # SAT 75th writing percentile 2015
    # highschools$mean_writing75_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- mean(colleges$SATWR751516, na.rm=TRUE)
    # highschools$median_writing75_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- median(colleges$SATWR751516, na.rm=TRUE)
    # highschools$min_writing75_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- min(colleges$SATWR751516, na.rm=TRUE)
    # highschools$max_writing75_SAT2015[highschools$div_name==unique(highschools$div_name)[i]] <- max(colleges$SATWR751516, na.rm=TRUE)
    # 
    # ------ ACT ------------------------------------
    
    # percent of students submitting ACT scores 2015
    highschools$mean_percent_submit_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- mean(colleges$ACTPCT1516, na.rm=TRUE)
    highschools$median_percent_submit_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- median(colleges$ACTPCT1516, na.rm=TRUE)
    highschools$min_percent_submit_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- min(colleges$ACTPCT1516, na.rm=TRUE)
    highschools$max_percent_submit_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- max(colleges$ACTPCT1516, na.rm=TRUE)
    
    # ACT 25th verbal percentile 2015
    highschools$mean_english25_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- mean(colleges$ACTEN251516, na.rm=TRUE)
    highschools$median_english25_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- median(colleges$ACTEN251516, na.rm=TRUE)
    highschools$min_english25_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- min(colleges$ACTEN251516, na.rm=TRUE)
    highschools$max_english25_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- max(colleges$ACTEN251516, na.rm=TRUE)
    
    # ACT 75th verbal percentile 2015
    highschools$mean_english75_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- mean(colleges$ACTEN751516, na.rm=TRUE)
    highschools$median_english75_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- median(colleges$ACTEN751516, na.rm=TRUE)
    highschools$min_english75_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- min(colleges$ACTEN751516, na.rm=TRUE)
    highschools$max_english75_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- max(colleges$ACTEN751516, na.rm=TRUE)
    
    # ACT 25th math percentile 2015
    highschools$mean_math25_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- mean(colleges$ACTMT251516, na.rm=TRUE)
    highschools$median_math25_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- median(colleges$ACTMT251516, na.rm=TRUE)
    highschools$min_math25_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- min(colleges$ACTMT251516, na.rm=TRUE)
    highschools$max_math25_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- max(colleges$ACTMT251516, na.rm=TRUE)
    
    # ACT 75th math percentile 2015
    highschools$mean_math75_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- mean(colleges$ACTMT751516, na.rm=TRUE)
    highschools$median_math75_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- median(colleges$ACTMT751516, na.rm=TRUE)
    highschools$min_math75_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- min(colleges$ACTMT751516, na.rm=TRUE)
    highschools$max_math75_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- max(colleges$ACTMT751516, na.rm=TRUE)
    # 
    # # ACT 25th writing percentile 2015
    # highschools$mean_writing25_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- mean(colleges$ACTWR251516, na.rm=TRUE)
    # highschools$median_writing25_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- median(colleges$ACTWR251516, na.rm=TRUE)
    # highschools$min_writing25_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- min(colleges$ACTWR251516, na.rm=TRUE)
    # highschools$max_writing25_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- max(colleges$ACTWR251516, na.rm=TRUE)
    # 
    # # ACT 75th writing percentile 2015
    # highschools$mean_writing75_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- mean(colleges$ACTWR751516, na.rm=TRUE)
    # highschools$median_writing75_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- median(colleges$ACTWR751516, na.rm=TRUE)
    # highschools$min_writing75_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- min(colleges$ACTWR751516, na.rm=TRUE)
    # highschools$max_writing75_ACT2015[highschools$div_name==unique(highschools$div_name)[i]] <- max(colleges$ACTWR751516, na.rm=TRUE)
    # 
}

highschools[highschools=='Inf' | highschools=='-Inf' | highschools=="NaN"] <- NA


write.csv(highschools[,-c(1:2)], "output/select_ipeds_vars1516.csv")

# net cost median, average, min, and max ####
# Average net price for full-time, first-time degree/certificate-seeking undergraduates paying the in-state or in-district tuition rate who were awarded 
# grant or scholarship aid from federal, state or local governments, or the institution. Other sources of grant aid are excluded. Aid awarded anytime during 
# the full aid year is included. Average net price is generated by subtracting the average amount of federal, state or local government, or institutional 
# grant and scholarship aid from the total cost of attendance. Total cost of attendance is the sum of published tuition and required fees (lower of in-district
# or in-state), books and supplies and the weighted average room and board and other expenses. The weighted average for room and board and other expenses is 
# generated as follows: (amount for on-campus room, board and other expenses * # of students living on-campus. +  amount for off-campus (with family) room, board and other expenses * # of students living off-campus with family +  amount for off-campus (not with family) room, board and other expenses * # of students living off-campus not with family) divided by the total # of students. 
# Students whose living arrangements are unknown are excluded from the calculation.  For some institutions the # of students by living arrangement will be known, 
# but dollar amounts will not be known.  In this case the # of students with no corresponding dollar amount will be excluded from the denominator.
