# combining data for Pulaski (attempting for all of VA...)
# 1/9/2017
library(readr)
library(dplyr)

setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)")
NRV <- list("Pulaski County","Radford City","Montgomery County","Giles County","Floyd County") 

sch_cw <- read_csv("Code/Maddie/school_crosswalk-allVA.csv")

# educational_attainment_school_districts_va <- read_csv("Code/Bianica/education_attainment_school_disctricts.csv")
# # dropout rate by DIVISION
# # [7] "attending_two_year_college"                "attending_four_year_college"               "other_continuing_ed_plans"
# # [10] "employment"                                "military"                                  "no_plans"
# rm(educational_attainment_school_districts_va)
# 
# vdoe_on_time_grad_all_schools <- read_csv("Code/Bianica/vdoe_on_time_grad_all_schools.csv")
# # dropout rate by SCHOOL
# # any colums we could match with are empty...
# rm(vdoe_on_time_grad_all_schools)
# 

# POST-HS PLANS AND DIPLOMA TYPE #######
vdoe_diploma_and_posths_plans_all_schools <- read_csv("Code/Bianica/vdoe_diploma_and_posths_plans_all_schools.csv")
# 2011

vdoe_ontimegrad_dropout_by_gender_allVA <- read_csv("Code/Maddie/pulaski/vdoe_ontimegrad_dropout_by_gender-allVA.csv")


# vdoe_ontimegrad_by_subpopulation <- read_csv("Code/Bianica/archive/vdoe_ontimegrad_by_subpopulation.csv")
# # 2011-2012
# # only appalachia counties from SCHEV
# rm(vdoe_ontimegrad_by_subpopulation)
# 
# vdoe_ontimegrad_by_subpopulation <- read_csv("Code/Bianica/vdoe_diploma_and_posths_plans_all_schools.csv")
# # 2011-2015!
# pulaski <- vdoe_ontimegrad_by_subpopulation %>% filter(sch_name2 == "PULASKI COUNTY SENIOR HIGH")
# plot(as.numeric(pulaski$year), pulaski$no_plans, col="blue")
# plot(as.numeric(pulaski$year), pulaski$employment, col="red")
# rm(pulaski)

# vdoe_hs_completer_dimploma_by_gender <- read_csv("Code/Bianica/vdoe_hs_completer_dimploma_by_gender.csv")
# # only schev counties :( 

# DISCIPLINARY OUTCOMES #####
vdoe_disciplinary_outcome_allVA <- read_csv("Code/Maddie/pulaski/vdoe_disciplinary_outcome-allVA.csv")
# type of disciplinary action (suspension, expulsion, etc)
# clean year
vdoe_disciplinary_outcome_allVA$MKAyear_fall <- as.numeric(gsub("\\-.*","", vdoe_disciplinary_outcome_allVA$schoolYear))

# STUDENT OFFENSES #####
vdoe_student_offenses_allVA <- read_csv("Code/Maddie/pulaski/vdoe_student_offenses-allVA.csv")

# COMPLETER DIPLOMA BY DISADVANTAGED STATUS ####
vdoe_hs_completer_dimploma_by_disadv_allVA <- read_csv("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Maddie/pulaski/vdoe_hs_completer_dimploma_by_disadv-allVA.csv")

# DIPLOMA COMPELTER BY GENDER ####
vdoe_hs_completer_dimploma_by_gender_allVA <- read_csv("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)/Code/Maddie/pulaski/vdoe_hs_completer_dimploma_by_gender-allVA.csv")

vdoe_hs_completer_dimploma_by_gender_allVA$MKAyear_fall <- as.numeric(gsub("\\-.*","", vdoe_hs_completer_dimploma_by_gender_allVA$SCHOOL_YEAR))

