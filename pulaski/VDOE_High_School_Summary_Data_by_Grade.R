# ADAPTED FROM "~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Bianica/VDOE_High_School_Summary_Data_by_Grade.R"
# 1/8/2017

library(dplyr)
library(stringr)
library(reshape2)
library(gdata)

## get Virginia school data
setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Data/VDOE/Fall Membership/")

filenames<-list.files(pattern="school_summaries_ethnicity-", full.names=TRUE)

for (i in 1: length(filenames)) {
  
  tmp<-read.xls(filenames[i],sheet=1,pattern="Division No.")
  tmp$year<-substr(filenames[i],nchar(filenames[i])-7,nchar(filenames[i])-4)
  
  if (i==1) {
    data1<-tmp
  } else {
    data1<-rbind(data1,tmp)
  }
}

data2 <- data1[, c("Division.No.","Division.Name","School.No.","School.Name","Grade")]
data2$Division.Name <- gsub("CHARLOTTESVILLE CTY PBLC SCHS", "CHARLOTTESVILLE CITY PBLC SCHS", data2$Division.Name) # correct incorrect data entry
data2$School.Name <- gsub("PULASKI COUNTY SR. HIGH", "PULASKI COUNTY SENIOR HIGH", data2$School.Name)

# PK = PreKindergarten
# JK = Junior Kindergarten
# KA = Half-Day Kindergarten – AM
# KP = Half-Day Kindergarten – PM
# KG = Kindergarten
# T1 = Transitional First Grade
# 01 = Grade 1
# 02 = Grade 2
# 03 = Grade 3
# 04 = Grade 4
# 05 = Grade 5
# 06 = Grade 6
# 07 = Grade 7
# 08 = Grade 8
# 09 = Grade 9
# 10 = Grade 10
# 11 = Grade 11
# 12 = Grade 12
# PG = Post Graduate
# TT = Test Taker

# recreate old school_crosswalk csv, but for all of VA (sch_cw<-read.csv("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Bianica/school_crosswalk.csv",stringsAsFactors = F))
data3 <- data2 %>% filter(Grade %in% c(9,10,11,12)) # subset high schools only
data4 <- unique(data3[, c("Division.No.","Division.Name","School.No.","School.Name")])

names(data4) <- c("div_num","div_name","sch_num","sch_names") # use same column names



library(stringr)

data4$sch_name_clean <- str_to_title(data4$sch_names)
data4$county_name <- str_to_title(gsub(" PBLC SCHS", "", data4$div_name))
data4$county_name <- gsub(" Co", " County", data4$county_name)

write.csv(data4, "~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Maddie/school_crosswalk-allVA.csv", row.names = FALSE)
