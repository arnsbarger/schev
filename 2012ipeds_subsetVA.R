# subset IPEDS data for VA only
library(dplyr)

setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Data/IPEDS/")

# load hd2012 ipeds table - only relevant variables (see hd2012 dictionary) ####
hd2012 <- read.csv("Institutional Characteristics/hd2012.csv")   

# keep only VA schools
hd2012 <- hd2012 %>% filter(STABBR == "VA")

# add ic2012_ay
ic2012_ay <- read.csv("Institutional Characteristics/ic2012_ay.csv")
hd_icay2012 <- merge(hd2012, ic2012_ay, by = "UNITID", all.x = TRUE, all.y = FALSE)

# add financial aid data
fanp <- read.csv("Student Financial Aid and Net Price/SFA1213/sfa1213.csv", na.strings=c("",".","NA"))
hd_icay_fanp2012 <- merge(hd_icay2012, fanp, by = "UNITID", all.x = TRUE)

# add admissions data
admissions <- read.csv("Admissions and Test Scores/IC2012/ic2012.csv")
ipeds_final <- merge(hd_icay_fanp2012, admissions, by = "UNITID", all.x = TRUE)

# subset - keep only requested variables
ipeds_final <- ipeds_final[, c("INSTNM", # school name
                               "UNITID", # institution id
                               "COUNTYNM", # county name
                               "CCIPUG", # school type
                               "UAGRNTP", # percent of students to receive aid of any type
                               "UAGRNTT", # sum of aid of any type given to students
                               "UAGRNTA", # average amount of aid of any type given to students
                               "NPIST1", # average net price
                               "SATPCT", # percent of students submitting SAT scores
                               "ACTPCT")] # percent of students submitting ACT scores

ipeds_final$coded_sch_type <- NA
ipeds_final$coded_sch_type[ipeds_final$CCIPUG %in% 6:17] <- "4 Year"
ipeds_final$coded_sch_type[ipeds_final$CCIPUG %in% 1:2] <- "2 Year"
ipeds_final$coded_sch_type[ipeds_final$CCIPUG %in% c(3:5,18:20)] <- "Career"
ipeds_final$coded_sch_type[ipeds_final$CCIPUG %in% c(-2,-3)] <- "Vocational"
ipeds_final$coded_sch_type[ipeds_final$CCIPUG == 0] <- "Not classified"

names(ipeds_final) <- c("INSTNM","UNITID","COUNTYNM","ipeds_sch_type","percent_receive_any_aid", "sum_total_aid","mean_aid_awarded","average_net_price","percent_submit_SAT","percent_submit_ACT","coded_sch_type")
colnames(ipeds_final) <- paste(colnames(ipeds_final), "1213", sep = "")

ipeds_final[ipeds_final=='.'] <- NA

write.csv(ipeds_final, "output/ipeds_subset1213.csv", row.names = FALSE)

