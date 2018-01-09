# adapted from "~/Google Drive/SDAL Google Drive Folders/SCHEV (Peter Blake - Wendy Kang)/Code/Bianica/VDOE Student Disciplinary Data.R"
# 1/9/2017

### Student Disciplinary Data
library(dplyr)
library(stringr)
library(gdata)

# school crosswalk
setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)")
sch_cw <- read_csv("Code/Maddie/school_crosswalk-allVA.csv")

offenses<-read.csv("Data/VDOE/Safe School Information Resource/Student Offender Report.csv",stringsAsFactors = F)
discOutcome<-read.csv("Data/VDOE/Safe School Information Resource/Disciplinary Outcome Report.csv",stringsAsFactors = F)

# reformat data
my.names <- offenses[16,]
colnames(offenses) <- my.names
my.names <- discOutcome[17,]
colnames(discOutcome) <- my.names

# remove NA columns
offenses<-offenses[,c(1:17)]
discOutcome<-discOutcome[,c(1:18)]

# remove blank rows
offenses<-filter(offenses,`School Name`!="")
discOutcome<-filter(discOutcome,`School Name`!="")
# rmeove first row, which has column names
offenses<-offenses[-1,]
discOutcome<-discOutcome[-1,]

# keep total offenses by school
offenses<-offenses[,c(1:2,4:8)]
discOutcome<-discOutcome[,c(1:2,4:9)]

# # filter for schools in study area
# offenses<-filter(offenses, `Division Name`=="Sussex County Public Schools" | `Division Name`=="Powhatan County Public Schools" | 
#                    `Division Name`=="Richmond City Public Schools" | `Division Name`=="Buchanan County Public Schools" |
#                    `Division Name`=="Bland County Public Schools" | `Division Name`=="Roanoke County Public Schools" | `Division Name`=="Roanoke City Public Schools")

offenses$sch_name2<-toupper(offenses$`School Name`)
offenses$sch_name2<-trim(offenses$sch_name2)
offenses1<-left_join(sch_cw,offenses,by=c("sch_names"="sch_name2"))

# discOutcome<-filter(discOutcome, `Division Name`=="Sussex County Public Schools" | `Division Name`=="Powhatan County Public Schools" | 
#                    `Division Name`=="Richmond City Public Schools" | `Division Name`=="Buchanan County Public Schools" |
#                    `Division Name`=="Bland County Public Schools" | `Division Name`=="Roanoke County Public Schools" | `Division Name`=="Roanoke City Public Schools")

discOutcome$sch_name2<-toupper(discOutcome$`School Name`)
discOutcome$sch_name2<-trim(discOutcome$sch_name2)
discOutcome1<-left_join(sch_cw,discOutcome,by=c("sch_names"="sch_name2"))

# remove na rows
offenses1<-filter(offenses1,!is.na(Population))
discOutcome1<-filter(discOutcome1,!is.na(Population))
# if count is between 1-9, make 5
offenses1$numStudentOffenses<-ifelse(offenses1$`Individual Student Offenders`=="<",5,offenses1$`Individual Student Offenders`)
offenses1$numStudentOffenses<-as.numeric(offenses1$numStudentOffenses)
discOutcome1$numStudentOffenses<-ifelse(discOutcome1$`Individual Student Offenders`=="<",5,discOutcome1$`Individual Student Offenders`)
discOutcome1$numStudentOffenses<-as.numeric(discOutcome1$numStudentOffenses)

# sum by school name clean
offenses2<-group_by(offenses1,div_num,div_name,sch_name_clean,schoolYear=`School Year`) %>%
  summarise(numStudentOffenses=sum(numStudentOffenses))
discOutcome2<-group_by(discOutcome1,div_num,div_name,sch_name_clean,disciplineType=`Discipline Type`,schoolYear=`School Year`) %>%
  summarise(numStudentOffenses=sum(numStudentOffenses))

write.csv(offenses2,"Code/Maddie/pulaski/vdoe_student_offenses-allVA.csv")
write.csv(discOutcome2,"Code/Maddie/pulaski/vdoe_disciplinary_outcome-allVA.csv")
