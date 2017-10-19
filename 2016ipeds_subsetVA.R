# subset IPEDS data for VA only
library(dplyr)

setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Data/IPEDS/")

# load hd2016 ipeds table - only relevant variables (see hd2016 dictionary) ####
hd2016 <- read.csv("Institutional Characteristics/hd2016.csv")   

# keep only VA schools
hd2016 <- hd2016 %>% filter(STABBR == "VA")

# add ic2016_ay
ic2016_ay <- read.csv("Institutional Characteristics/ic2016_ay.csv")
hd_icay2016 <- merge(hd2016, ic2016_ay, by = "UNITID", all.x = TRUE, all.y = FALSE)

# 2016-2017 financial aid data unavailable as of 10/18/2017 -- add in later?
#fanp <- read.csv("Student Financial Aid and Net Price/sfa1617.csv", na.strings=c("",".","NA"))
#hd_icay_fanp2016 <- merge(hd_icay2016, fanp, by = "UNITID", all.x = TRUE)

# placeholder until 2016 financial aid data comes through
hd_icay_fanp2016 <- hd_icay2016

# add admissions data
admissions <- read.csv("Admissions and Test Scores/adm2016.csv")
ipeds_final <- merge(hd_icay_fanp2016, admissions, by = "UNITID", all.x = TRUE)

# subset - keep only requested variables
ipeds_final <- ipeds_final[, c("INSTNM", # school name
                               "UNITID", # institution id
                               "COUNTYNM", # county name
                               "C15IPUG", # school type
                               #"UAGRNTP", # percent of students to receive aid of any type
                               #"UAGRNTT", # sum of aid of any type given to students
                               #"UAGRNTA", # average amount of aid of any type given to students
                               #"NPIST1", # average net price
                               "SATPCT", # percent of students submitting SAT scores
                               "ACTPCT")] # percent of students submitting ACT scores

ipeds_final$coded_sch_type <- NA
ipeds_final$coded_sch_type[ipeds_final$C15IPUG %in% 6:17] <- "4 Year"
ipeds_final$coded_sch_type[ipeds_final$C15IPUG %in% 1:2] <- "2 Year"
ipeds_final$coded_sch_type[ipeds_final$C15IPUG %in% c(3:5,18:20)] <- "Career"
ipeds_final$coded_sch_type[ipeds_final$C15IPUG == -2] <- "Vocational"
ipeds_final$coded_sch_type[ipeds_final$C15IPUG == 0] <- "Not classified"

names(ipeds_final) <- c("INSTNM","UNITID","COUNTYNM","ipeds_sch_type",
                        #"percent_receive_any_aid", "sum_total_aid","mean_aid_awarded","average_net_price",
                        "percent_submit_SAT","percent_submit_ACT","coded_sch_type")
colnames(ipeds_final) <- paste(colnames(ipeds_final), "1617", sep = "")

ipeds_final[ipeds_final=='.'] <- NA

write.csv(ipeds_final, "output/ipeds_subset1617.csv", row.names = FALSE)
