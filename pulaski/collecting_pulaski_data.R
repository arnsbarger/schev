# combining data for Pulaski (attempting for all of VA...)
# cleaning/preparing for data vis
# 1/9/2017
library(readr)
library(dplyr)

setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)")
NRV <- list("Pulaski County","Radford City","Montgomery County","Giles County","Floyd County") 

sch_cw <- read_csv("Code/Maddie/school_crosswalk-allVA.csv")

# POST-HS PLANS AND DIPLOMA TYPE #######
vdoe_diploma_and_posths_plans_all_schools <- read_csv("Code/Bianica/vdoe_diploma_and_posths_plans_all_schools.csv")


vdoe_ontimegrad_dropout_by_gender_allVA <- read_csv("Code/Maddie/pulaski/vdoe_ontimegrad_dropout_by_gender-allVA.csv")
vdoe_ontimegrad_dropout_by_gender_allVA$year_fall <- as.numeric(gsub("\\-.*","", vdoe_ontimegrad_dropout_by_gender_allVA$SCHOOL_YEAR))


# DISCIPLINARY OUTCOMES #####
vdoe_disciplinary_outcome_allVA <- read_csv("Code/Maddie/pulaski/vdoe_disciplinary_outcome-allVA.csv")
vdoe_disciplinary_outcome_allVA$year_fall <- as.numeric(gsub("\\-.*","", vdoe_disciplinary_outcome_allVA$schoolYear))

# STUDENT OFFENSES #####
vdoe_student_offenses_allVA <- read_csv("Code/Maddie/pulaski/vdoe_student_offenses-allVA.csv")
vdoe_student_offenses_allVA$year_fall <- as.numeric(gsub("\\-.*","", vdoe_student_offenses_allVA$schoolYear))


# DIPLOMA COMPLETION
vdoe_hs_completer_by_disadv_allVA <- read_csv("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Maddie/pulaski/vdoe_hs_completer_dimploma_by_disadv-allVA.csv")
vdoe_hs_completer_by_disadv_allVA$year_fall <- as.numeric(gsub("\\-.*","", vdoe_hs_completer_by_disadv_allVA$SCHOOL_YEAR))


vdoe_hs_completer_by_gender_allVA <- read_csv("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Maddie/pulaski/vdoe_hs_completer_dimploma_by_gender-allVA.csv")
vdoe_hs_completer_by_gender_allVA$year_fall <- as.numeric(gsub("\\-.*","", vdoe_hs_completer_by_gender_allVA$SCHOOL_YEAR))

# SCHOOL SUMMARY
vdoe_hs_total_grade_race <- read_csv("Code/Bianica/vdoe_hs_total_grade_race.csv")


# CTE ####
cte2014 <- read_csv("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Data/VDOE/Career & Technical Education Program Graduates and Completers/2014-cte-comp.csv")

# STUDENT COUNTS ####
student_counts_allVA <- read.csv("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Maddie/pulaski/2011-2016student_counts-allVA.csv")
student_counts_allVA$sch_name_clean <- str_to_title(student_counts_allVA$SCH_NAME)

save(list = ls(all.names = TRUE), file = "Code/Maddie/pulaski/pulaski_vis_data.RData", envir = .GlobalEnv)


