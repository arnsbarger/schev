### Grad diplomas by type
library(dplyr)
library(stringr)
library(reshape2)
library(gdata)

## get Virginia school data
setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Data/VDOE/Annual High School Gradulates and Completers/")

## school crosswalk
sch_cw<-read.csv("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Maddie/school_crosswalk-allVA.csv",stringsAsFactors = F)

# postsec enrollment - 2011-2013
filenames<-list.files(pattern="hs_grad.csv", full.names=TRUE)

for (i in 1: length(filenames)) {
  
  tmp<-read.csv(filenames[i],stringsAsFactors = F)
  
  if (i==1) {
    data1<-tmp
  } else {
    data1<-rbind(data1,tmp)
  }
}

data1$DIV_NAME <- str_trim(data1$DIV_NAME)
data1$SCH_NAME <- str_trim(data1$SCH_NAME)

# grad<-filter(data1, DIV_NAME=="Sussex County" | DIV_NAME=="Powhatan County" | DIV_NAME=="Richmond City" | DIV_NAME=="Buchanan County" |
#                DIV_NAME=="Bland County" | DIV_NAME=="Roanoke County" | DIV_NAME=="Roanoke City")
# 
# grad<-filter(grad,SCH_NAME!="")

grad <- filter(data1, SCH_NAME!="")

# get clean school names
grad$SCH_NAME2<-toupper(grad$SCH_NAME)
grad<-left_join(grad,sch_cw,by=c("SCH_NAME2"="sch_names"),ignore.case=TRUE)

# sum using clean school name
grad1<-group_by(grad,SCHOOL_YEAR,div_num,div_name,sch_name_clean,GENDER,LEP_FLAG,
                DISADVANTAGED_FLAG,DISABILITY_FLAG,FEDERAL_RACE_CODE,HS_COMPLETION_NAME) %>%
  summarise(hs_completer_cnt=sum(HS_COMPLETER_CNT))

## get total grads across all subpopulations
gradAll<-filter(grad1, GENDER=="" & DISABILITY_FLAG=="" & LEP_FLAG=="" & DISADVANTAGED_FLAG=="" 
                & is.na(FEDERAL_RACE_CODE))
gradAll<-group_by(gradAll,SCHOOL_YEAR,div_num,div_name,sch_name_clean) %>%
  summarise(hsCompletersTotal=sum(hs_completer_cnt))

########## DISADVANTAGED ###############
# get on time graduation and dropout by disadvantaged
gradDisadv <- grad1 %>%
  filter(GENDER=="" & DISABILITY_FLAG=="" & LEP_FLAG=="" & DISADVANTAGED_FLAG!=""
         & is.na(FEDERAL_RACE_CODE))

# add total
gradDisadv<-left_join(gradDisadv,gradAll,by=c("SCHOOL_YEAR","div_num","div_name","sch_name_clean"))

## check for drop offs
check1<-group_by(gradDisadv,SCHOOL_YEAR,div_num,div_name,sch_name_clean,hsCompletersTotal) %>%
  summarise(hs_completer_cnt=sum(hs_completer_cnt))
check1$diff<-check1$hsCompletersTotal-check1$hs_completer_cnt
sum(check1$diff)/sum(check1$hsCompletersTotal)
#[1] 0.06033349

# get columns
gradDisadv<-gradDisadv[,c(1:4,7,10,11:12)]
write.csv(gradDisadv,"~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Maddie/pulaski/vdoe_hs_completer_dimploma_by_disadv-allVA.csv")

########## RACE/ETHNICITY ###############
# get on time graduation and dropout by disadvantaged
gradRace <- grad1 %>%
  filter(GENDER=="" & DISABILITY_FLAG=="" & LEP_FLAG=="" & DISADVANTAGED_FLAG==""
         & !is.na(FEDERAL_RACE_CODE))

# add total
gradRace<-left_join(gradRace,gradAll,by=c("SCHOOL_YEAR","div_num","div_name","sch_name_clean"))

## check for drop offs
check1<-group_by(gradRace,SCHOOL_YEAR,div_num,div_name,sch_name_clean,hsCompletersTotal) %>%
  summarise(hs_completer_cnt=sum(hs_completer_cnt))
check1$diff<-check1$hsCompletersTotal-check1$hs_completer_cnt
sum(check1$diff)/sum(check1$hsCompletersTotal)
#[1] 0.1019874

# get columns
gradRace<-gradRace[,c(1:4,9,10,11:12)]
write.csv(gradRace,"~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Maddie/pulaski/vdoe_hs_completer_dimploma_by_race-allVA.csv")

########## GENDER ###############
# get on time graduation and dropout by disadvantaged
gradGender <- grad1 %>%
  filter(GENDER!="" & DISABILITY_FLAG=="" & LEP_FLAG=="" & DISADVANTAGED_FLAG==""
         & is.na(FEDERAL_RACE_CODE))

# add total
gradGender<-left_join(gradGender,gradAll,by=c("SCHOOL_YEAR","div_num","div_name","sch_name_clean"))

## check for drop offs
check1<-group_by(gradGender,SCHOOL_YEAR,div_num,div_name,sch_name_clean,hsCompletersTotal) %>%
  summarise(hs_completer_cnt=sum(hs_completer_cnt))
check1$diff<-check1$hsCompletersTotal-check1$hs_completer_cnt
sum(check1$diff)/sum(check1$hsCompletersTotal)
#[1] 0.0590402

# get columns
gradGender<-gradGender[,c(1:4,5,10,11:12)]
write.csv(gradGender,"~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Maddie/pulaski/vdoe_hs_completer_dimploma_by_gender-allVA.csv")
