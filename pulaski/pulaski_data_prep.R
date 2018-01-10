# pulaski data prep
# 1/10/2017
setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)")
load("Code/Maddie/pulaski/pulaski_vis_data.RData")

library(reshape)

# COUNT OF STUDENTS
total_students_sch <- unique(student_counts_allVA[,c("DIV_NAME", "SCH_NAME", "year","total_students_sch")])

sch_total2011 <- total_students_sch[total_students_sch$year=="2011", c("DIV_NAME", "SCH_NAME","total_students_sch")]
sch_cw <- merge(sch_cw, sch_total2011, by.x = c("div_name","sch_names"), by.y = c("DIV_NAME","SCH_NAME"), all.x = TRUE)
sch_total2012 <- total_students_sch[total_students_sch$year=="2012", c("DIV_NAME", "SCH_NAME","total_students_sch")]
sch_cw <- merge(sch_cw, sch_total2012, by.x = c("div_name","sch_names"), by.y = c("DIV_NAME","SCH_NAME"), all.x = TRUE)
sch_total2013 <- total_students_sch[total_students_sch$year=="2013", c("DIV_NAME", "SCH_NAME","total_students_sch")]
sch_cw <- merge(sch_cw, sch_total2013, by.x = c("div_name","sch_names"), by.y = c("DIV_NAME","SCH_NAME"), all.x = TRUE)
sch_total2014 <- total_students_sch[total_students_sch$year=="2014", c("DIV_NAME", "SCH_NAME","total_students_sch")]
sch_cw <- merge(sch_cw, sch_total2014, by.x = c("div_name","sch_names"), by.y = c("DIV_NAME","SCH_NAME"), all.x = TRUE)
sch_total2015 <- total_students_sch[total_students_sch$year=="2015", c("DIV_NAME", "SCH_NAME","total_students_sch")]
sch_cw <- merge(sch_cw, sch_total2015, by.x = c("div_name","sch_names"), by.y = c("DIV_NAME","SCH_NAME"), all.x = TRUE)
sch_total2016 <- total_students_sch[total_students_sch$year=="2016", c("DIV_NAME", "SCH_NAME","total_students_sch")]
sch_cw <- merge(sch_cw, sch_total2016, by.x = c("div_name","sch_names"), by.y = c("DIV_NAME","SCH_NAME"), all.x = TRUE)
colnames(sch_cw)[7:ncol(sch_cw)] <- c("sch_total2011","sch_total2012","sch_total2013","sch_total2014","sch_total2015","sch_total2016")

# POST HS PLANS & DIPLOMA TYPE COUNTS
vdoe_diploma_and_posths_plans_all_schools <- data.frame(vdoe_diploma_and_posths_plans_all_schools)[,-c(1,2,4,22)]

sch_cw <- merge(sch_cw, vdoe_diploma_and_posths_plans_all_schools2011, by.x = c("county_name","sch_name_clean"), by.y = c("div_name2011","school_name2011"), all.x = TRUE)
sch_cw <- merge(sch_cw, vdoe_diploma_and_posths_plans_all_schools2012, by.x = c("county_name","sch_name_clean"), by.y = c("div_name2012","school_name2012"), all.x = TRUE)
sch_cw <- merge(sch_cw, vdoe_diploma_and_posths_plans_all_schools2013, by.x = c("county_name","sch_name_clean"), by.y = c("div_name2013","school_name2013"), all.x = TRUE)
sch_cw <- merge(sch_cw, vdoe_diploma_and_posths_plans_all_schools2014, by.x = c("county_name","sch_name_clean"), by.y = c("div_name2014","school_name2014"), all.x = TRUE)
sch_cw <- merge(sch_cw, vdoe_diploma_and_posths_plans_all_schools2015, by.x = c("county_name","sch_name_clean"), by.y = c("div_name2015","school_name2015"), all.x = TRUE)

# DROPOUTS
dropouts <- cast(vdoe_ontimegrad_dropout_by_gender_allVA, div_name + sch_name_clean + year_fall ~ GENDER, value = "cohort_dropout_cnt")
dropouts$total_dropouts <- dropouts$`F` + dropouts$M

dropouts2011 <- dropouts[dropouts$year_fall=="2011",-3]
dropouts2012 <- dropouts[dropouts$year_fall=="2012",-3]
dropouts2013 <- dropouts[dropouts$year_fall=="2013",-3]
dropouts2014 <- dropouts[dropouts$year_fall=="2014",-3]

colnames(dropouts2011)[3:5] <- c("female_dropouts2011", "male_dropouts2011", "total_dropouts2011")
colnames(dropouts2012)[3:5] <- c("female_dropouts2012", "male_dropouts2012", "total_dropouts2012")
colnames(dropouts2013)[3:5] <- c("female_dropouts2013", "male_dropouts2013", "total_dropouts2013")
colnames(dropouts2014)[3:5] <- c("female_dropouts2014", "male_dropouts2014", "total_dropouts2014")

sch_cw <- merge(sch_cw, dropouts2011, by = c("div_name","sch_name_clean"), all.x = TRUE)
sch_cw <- merge(sch_cw, dropouts2012, by = c("div_name","sch_name_clean"), all.x = TRUE)
sch_cw <- merge(sch_cw, dropouts2013, by = c("div_name","sch_name_clean"), all.x = TRUE)
sch_cw <- merge(sch_cw, dropouts2014, by = c("div_name","sch_name_clean"), all.x = TRUE)



