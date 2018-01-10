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

# student counts by grade and subpopulations (race/ethnicity,gender)
membM<-melt(data1,id=c("Division.No.","Division.Name","School.No.","School.Name","Grade","year"))
membM<-filter(membM,variable!="Total.Full.time.Students" & variable!="Part.time.Students" & variable!="Total..Full.time...Part.time.Students")

membM$gender<-ifelse(str_sub(membM$variable,nchar(as.character(membM$variable))-4,nchar(as.character(membM$variable))-4)==".","M","F")

# add race column
# import race cw
race_cw<-read.csv("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Bianica/race_cw.csv")
membM$race<-ifelse(str_sub(membM$variable,nchar(as.character(membM$variable))-4,nchar(as.character(membM$variable))-4)==".",
                   str_sub(membM$variable,1,nchar(as.character(membM$variable))-5),
                   str_sub(membM$variable,1,nchar(as.character(membM$variable))-5))
membM<-left_join(membM,select(race_cw,race_desc,federal_race_desc_clean,federal_race_code),by=c("race"="race_desc"))

#import school crosswalk
sch_cw<-read_csv("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Maddie/school_crosswalk-allVA.csv")



### descriptive statistics of virginia fall membership data
## total students by grade
grade<-select(data1,year,DIV_NUM=Division.No.,DIV_NAME=Division.Name,SCH_NUM=School.No.,SCH_NAME=School.Name,grade=Grade,total_students_grade=Total..Full.time...Part.time.Students)
grade$total_students_grade<-as.numeric(as.character(grade$total_students_grade))
grade$grade <- as.numeric(as.character(grade$grade))

grade <- grade[grade$grade %in% c(9,10,11,12),]
grade$div_sch_name <- paste(grade$DIV_NAME, grade$SCH_NAME, grade$year)

total <- ddply(grade,~div_sch_name,summarise,total_students_sch=sum(total_students_grade))

new_sch_summ <- left_join(grade, total, by="div_sch_name")
new_sch_summ$SCH_NAME <- gsub("PULASKI COUNTY SR. HIGH", "PULASKI COUNTY SENIOR HIGH", new_sch_summ$SCH_NAME)

write.csv(new_sch_summ[,-8],"~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Maddie/pulaski/2011-2016student_counts-allVA.csv")

# 
# ## total students in school
# total<-group_by(grade,year,DIV_NUM,DIV_NAME,SCH_NUM,SCH_NAME) %>%
#   summarise(value=sum(value))
# total$variable<-"total_students"
# total$variable_category<-"total_students"
# total<-total[,c(1:5,8,6:7)]
# total<-as.data.frame(total)
# 
# # total students by race/ethnicity
# race<-data1[,c(1:4,6:19,23)]
# race<-melt(race,id=c("Division.No.","Division.Name","School.No.","School.Name","year"))
# 
# race_data<-unique(race$variable)
# race_cat<-c("Hispanic of any race","Hispanic of any race","American Indian or Alaska Native","American Indian or Alaska Native",
#             "Asian","Asian","Black or African American","Black or African American","Native Hawaiian or Pacific Islander","Native Hawaiian or Pacific Islander",
#             "White","White","Two or more races","Two or more races")
# race_cw<-data.frame(race_data,race_cat)
# 
# race<-left_join(race,race_cw,by=c("variable"="race_data"))
# race<-group_by(race,year,DIV_NUM=Division.No.,DIV_NAME=Division.Name,SCH_NUM=School.No.,SCH_NAME=School.Name,race_cat) %>%
#   summarise(value=sum(as.numeric(value)))
# race<-filter(race,value>0)
# race<-filter(race,!is.na(value))
# race<-rename(race,variable_category=race_cat)
# race$variable<-"race"
# race<-as.data.frame(race)
# 
# df<-rbind(total,grade,race)
# 
# # filter for schools of interest
# sch_cw$div_num<-as.numeric(sch_cw$div_num)
# df2<-left_join(sch_cw,df,by=c("div_num"="DIV_NUM","sch_names"="SCH_NAME"))
# 
# # group schools
# df_summ<-group_by(df2,div_num,div_name,sch_name_clean,year,variable_category,variable) %>%
#   summarise(value=sum(value))
# 
# write.csv(df_summ,"~/Google Drive/SDAL Google Drive Folders/SCHEV (Peter Blake - Wendy Kang)/Code/Bianica/vdoe_hs_total_grade_race.csv")
# 
# 
# #grades<-filter(df_summ,variable=="grade" & year==2015)
# #write.csv(grades,"~/Google Drive/SDAL Google Drive Folders/SCHEV (Peter Blake - Wendy Kang)/Code/Bianica/vdoe_studentsbygrade.csv")
