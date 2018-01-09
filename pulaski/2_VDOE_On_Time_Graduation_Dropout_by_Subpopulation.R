library(dplyr)
library(stringr)
library(reshape2)
library(gdata)

## get Virginia school data
setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Data/VDOE/On-Time Graduation Rate and Cohort Dropout Rate/")

## school crosswalk
sch_cw<-read.csv("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Maddie/school_crosswalk-allVA.csv",stringsAsFactors = F)

# postsec enrollment - 2011-2013
filenames <- list.files(pattern="_cohort.csv", full.names=TRUE)

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

write.csv(data1,"~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Maddie/pulaski/vdoe_on_time_grad_all_schools.csv")

# get postsec enrollment by subpopulations and by 2-year and 4-year
grad<-filter(data1, DIV_NAME %in% sch_cw$county_name)

grad<-filter(grad,DIV_NAME!="" & SCH_NAME=="")

# get clean school names
grad$SCH_NAME2<-toupper(grad$SCH_NAME)
grad<-left_join(grad,sch_cw,by=c("SCH_NAME2"="sch_names"),ignore.case=TRUE)

# sum using clean school name
grad1<-group_by(grad,SCHOOL_YEAR,div_num,div_name,sch_name_clean,GENDER,LEP_FLAG,
                    DISADVANTAGED_FLAG,DISABILITY_FLAG,FEDERAL_RACE_CODE) %>%
  summarise(cohort_graduate_cnt=sum(COHORT_CNT),diploma_rate=mean(DIPLOMA_RATE),dropout_rate=mean(DROPOUT_RATE))

# convert ontime grade rate and dropout rate to counts
grad1$ontime_grad_cnt<-as.integer(grad1$cohort_graduate_cnt*(grad1$diploma_rate/100))
grad1$cohort_dropout_cnt<-as.integer(grad1$cohort_graduate_cnt*(grad1$dropout_rate/100))

## get total on time grad and dropout across all subpopulations
gradAll<-filter(grad1, GENDER=="" & DISABILITY_FLAG=="" & LEP_FLAG=="" & DISADVANTAGED_FLAG=="" 
              & is.na(FEDERAL_RACE_CODE))
gradAll<-group_by(gradAll,SCHOOL_YEAR,div_num,div_name,sch_name_clean) %>%
  summarise(onTimeTotal=sum(ontime_grad_cnt),dropoutTotal=sum(cohort_dropout_cnt))

########## DISADVANTAGED ###############
# get on time graduation and dropout by disadvantaged
gradDisadv <- grad1 %>%
  filter(GENDER=="" & DISABILITY_FLAG=="" & LEP_FLAG=="" & DISADVANTAGED_FLAG!=""
         & is.na(FEDERAL_RACE_CODE))

# add total
gradDisadv<-left_join(gradDisadv,gradAll,by=c("SCHOOL_YEAR","div_num","div_name","sch_name_clean"))

## check for drop offs
checkDisadv1<-group_by(gradDisadv,SCHOOL_YEAR,div_num,div_name,sch_name_clean,onTimeTotal,dropoutTotal) %>%
  summarise(grad_cnt=sum(ontime_grad_cnt),do_cnt=sum(cohort_dropout_cnt))
checkDisadv1$diff<-checkDisadv1$onTimeTotal-checkDisadv1$grad_cnt
sum(checkDisadv1$diff)/sum(checkDisadv1$onTimeTotal)
#[1] -0.002258542
checkDisadv1$diff<-checkDisadv1$dropoutTotal-checkDisadv1$do_cnt
sum(checkDisadv1$diff)/sum(checkDisadv1$dropoutTotal)
#[1] 0.008523168

# sum using clean school name
gradDisadv<-gradDisadv[,c(1:4,7,10,13:16)]
write.csv(gradDisadv,"~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Maddie/pulaski/vdoe_ontimegrad_dropout_by_disadv-allVA.csv")

########## RACE/ETHNICITY ###############
# get on time graduation and dropout by disadvantaged
gradRace <- grad1 %>%
  filter(GENDER=="" & DISABILITY_FLAG=="" & LEP_FLAG=="" & DISADVANTAGED_FLAG==""
         & !is.na(FEDERAL_RACE_CODE))

# add total
gradRace<-left_join(gradRace,gradAll,by=c("SCHOOL_YEAR","div_num","div_name","sch_name_clean"))

## check for drop offs
check1<-group_by(gradRace,SCHOOL_YEAR,div_num,div_name,sch_name_clean,onTimeTotal,dropoutTotal) %>%
  summarise(grad_cnt=sum(ontime_grad_cnt),do_cnt=sum(cohort_dropout_cnt))
check1$diff<-check1$onTimeTotal-check1$grad_cnt
sum(check1$diff)/sum(check1$onTimeTotal)
#[1] 0.009955737
check1$diff<-check1$dropoutTotal-check1$do_cnt
sum(check1$diff)/sum(check1$dropoutTotal)
#[1] 0.01978408

# sum using clean school name
gradRace<-gradRace[,c(1:4,9,10,13:16)]
write.csv(gradRace,"~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Maddie/pulaski/vdoe_ontimegrad_dropout_by_race-allVA.csv")

########## GENDER ###############
# get on time graduation and dropout by disadvantaged
gradGender <- grad1 %>%
  filter(GENDER!="" & DISABILITY_FLAG=="" & LEP_FLAG=="" & DISADVANTAGED_FLAG==""
         & is.na(FEDERAL_RACE_CODE))

# add total
gradGender<-left_join(gradGender,gradAll,by=c("SCHOOL_YEAR","div_num","div_name","sch_name_clean"))

## check for drop offs
check1<-group_by(gradGender,SCHOOL_YEAR,div_num,div_name,sch_name_clean,onTimeTotal,dropoutTotal) %>%
  summarise(grad_cnt=sum(ontime_grad_cnt),do_cnt=sum(cohort_dropout_cnt))
check1$diff<-check1$onTimeTotal-check1$grad_cnt
sum(check1$diff)/sum(check1$onTimeTotal)
#[1] 0.000167558
check1$diff<-check1$dropoutTotal-check1$do_cnt
sum(check1$diff)/sum(check1$dropoutTotal)
#[1] 0.0007748334

# sum using clean school name
gradGender<-gradGender[,c(1:5,10,13:16)]
write.csv(gradGender,"~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Maddie/pulaski/vdoe_ontimegrad_dropout_by_gender-allVA.csv")

### UPDATE DATA ON MORE RECENT YEARS FROM SCHOOL QUALITY REPORTS
