# subset IPEDS data for VA only
library(dplyr)

setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Data/IPEDS/")

# load hd2014 ipeds table - only relevant variables (see hd2014 dictionary) ####
hd2014 <- read.csv("Institutional Characteristics/hd2014.csv")   

# keep only VA schools
hd2014 <- hd2014 %>% filter(STABBR == "VA")

# add ic2014_ay
ic2014_ay <- read.csv("Institutional Characteristics/ic2014_ay.csv")
hd_icay2014 <- merge(hd2014, ic2014_ay, by = "UNITID", all.x = TRUE, all.y = FALSE)

# add financial aid data
fanp <- read.csv("Student Financial Aid and Net Price/sfa1415.csv", na.strings=c("",".","NA"))
hd_icay_fanp2014 <- merge(hd_icay2014, fanp, by = "UNITID", all.x = TRUE)

# add admissions data
admissions <- read.csv("Admissions and Test Scores/adm2014.csv")
ipeds_final <- merge(hd_icay_fanp2014, admissions, by = "UNITID", all.x = TRUE)

# subset - keep only requested variables
ipeds_final <- ipeds_final[, c("INSTNM", # school name
                               "UNITID", # institution id
                               "COUNTYNM", # county name
                               "CCIPUG", # school type
                               "UAGRNTP", # percent of students to receive aid of any type
                               "UAGRNTT", # sum of aid of any type given to students
                               "UAGRNTA", # average amount of aid of any type given to students
                               "NPIST1", # average net price
                               "APPLCN", # number of applicants
                               "ADMSSN", # number admitted
                               "ENRLT", # number enrolled
                               "SATPCT", # percent of applicants submitting SAT
                               "ACTPCT", # percent of applicants submitting ACT
                               "SATVR25", # SAT verbal 25 percentile
                               "SATVR75", # SAT verbal 75 percentile
                               "SATMT25", # SAT math 25 percentile
                               "SATMT75", # SAT math 75 percentile
                               "SATWR25", # SAT writing 25 percentile
                               "SATWR75", # SAT writing 75 percentile
                               "ACTCM25", # ACT math 25 percentile
                               "ACTCM75", # ACT math 75 percentile
                               "ACTEN25", # ACT english 25 percentile
                               "ACTEN75", # ACT english 75 percentile
                               "ACTMT25", # ACT math 25 percentile
                               "ACTMT75", # ACT math 75 percentile
                               "ACTWR25", # ACT writing 25 percentile
                               "ACTWR75")] # ACT writing 75 percentile

ipeds_final$coded_sch_type <- NA
ipeds_final$coded_sch_type[ipeds_final$CCIPUG %in% 6:17] <- "4 Year"
ipeds_final$coded_sch_type[ipeds_final$CCIPUG %in% 1:2] <- "2 Year"
ipeds_final$coded_sch_type[ipeds_final$CCIPUG %in% c(3:5,18:20)] <- "Career"
ipeds_final$coded_sch_type[ipeds_final$CCIPUG %in% c(-2,-3)] <- "Vocational"
ipeds_final$coded_sch_type[ipeds_final$CCIPUG == 0] <- "Not classified"

#names(ipeds_final) <- c("INSTNM","UNITID","COUNTYNM","ipeds_sch_type","percent_receive_any_aid", "sum_total_aid","mean_aid_awarded","average_net_price","total_applicants","total_admit","total_enroll","pct_submitSAT","pct_submitACT","SATread25pctl","SATread75pctl","SATmath25pctl","SATmath75pctl","SATwrite25pctl","SATwrite75pctl","ACT25pctl","ACT75pctl","ACTenglish25pctl","ACTenglish75pctl","ACTmath25pctl","ACTmath75pctl","ACTwrite25pctl","ACTwrite75pctl","coded_sch_type")
colnames(ipeds_final) <- paste(colnames(ipeds_final), "1415", sep = "")

ipeds_final[ipeds_final=='.'] <- NA

write.csv(ipeds_final, "output/ipeds_subset1415.csv", row.names = FALSE)
