# subset IPEDS data for VA only
library(dplyr)

setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Data/IPEDS/")

# load hd2015 ipeds table - only relevant variables (see hd2015 dictionary) ####
hd2015 <- read.csv("Institutional Characteristics/hd2015.csv")   

# keep only VA schools
hd2015 <- hd2015 %>% filter(STABBR == "VA")

# add ic2015_ay
ic2015_ay <- read.csv("Institutional Characteristics/ic2015_ay.csv")
hd_icay2015 <- merge(hd2015, ic2015_ay, by = "UNITID", all.x = TRUE, all.y = FALSE)

# add financial aid data
fanp <- read.csv("Student Financial Aid and Net Price/sfa1415.csv", na.strings=c("",".","NA"))
hd_icay_fanp2015 <- merge(hd_icay2015, fanp, by = "UNITID", all.x = TRUE)

# add admissions data
admissions <- read.csv("Admissions and Test Scores/adm2015.csv")
ipeds_final <- merge(hd_icay_fanp2015, admissions, by = "UNITID", all.x = TRUE)

# subset - keep only requested variables
ipeds_final <- ipeds_final[, c("UAGRNTP", # percent of students to receive aid of any type
                               "UAGRNTT", # sum of aid of any type given to students
                               "UAGRNTA", # average amount of aid of any type given to students
                               "NPIST1", # average net price
                               "SATPCT", # percent of students submitting SAT scores
                               "ACTPCT")] # percent of students submitting ACT scores

names(ipeds_final) <- c("percent_receive_any_aid", "sum_total_aid","mean_aid_awarded","average_net_price","percent_submit_SAT","percent_submit_ACT")
