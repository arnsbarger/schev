library(dplyr)
library(stringr)
library(reshape2)
library(gdata)

## get Virginia school data
setwd("Data/VDOE/Fall Membership/")

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
race_cw<-read.csv("~/Google Drive/SDAL Google Drive Folders/SCHEV (Peter Blake - Wendy Kang)/Code/Bianica/race_cw.csv")
membM$race<-ifelse(str_sub(membM$variable,nchar(as.character(membM$variable))-4,nchar(as.character(membM$variable))-4)==".",
                   str_sub(membM$variable,1,nchar(as.character(membM$variable))-5),
                   str_sub(membM$variable,1,nchar(as.character(membM$variable))-7))
membM<-left_join(membM,select(race_cw,race_desc,federal_race_desc_clean,federal_race_code),by=c("race"="race_desc"))

sch_cw<-read.csv("~/Google Drive/SDAL Google Drive Folders/SCHEV (Peter Blake - Wendy Kang)/Code/Bianica/school_crosswalk.csv",stringsAsFactors = F)

# filter for schools of interest
sch_cw$div_num<-as.numeric(sch_cw$div_num)
df1<-left_join(sch_cw,membM,by=c("div_num"="Division.No.","sch_names"="School.Name"))

# group schools
df_summ<-group_by(df1,div_num,div_name,sch_name_clean,year,Grade,gender,federal_race_desc_clean,federal_race_code) %>%
  summarise(value=sum(as.numeric(value)))

# remove totals of zero
df_summ<-filter(df_summ,value>0)

write.csv(df_summ,"~/Google Drive/SDAL Google Drive Folders/SCHEV (Peter Blake - Wendy Kang)/Code/Bianica/vdoe_hs_fall_membership_by_subpopulation.csv")


#grades<-filter(df_summ,variable=="grade" & year==2015)
#write.csv(grades,"~/Google Drive/SDAL Google Drive Folders/SCHEV (Peter Blake - Wendy Kang)/Code/Bianica/vdoe_studentsbygrade.csv")
