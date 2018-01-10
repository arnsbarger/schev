# pulaski data prep
# 1/10/2017
setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)")
load("Code/Maddie/pulaski/pulaski_vis_data.RData")

total_students_sch <- unique(student_counts_allVA[,c("DIV_NAME", "SCH_NAME", "year","total_students_sch")])

sch_total2011 <- total_students_sch[total_students_sch$year=="2011", c("DIV_NAME", "SCH_NAME","total_students_sch")]
sch_cw <- merge(sch_cw, sch_total2011, by.x = c("div_name","sch_names"), by.y = c("DIV_NAME","SCH_NAME"))
sch_total2012 <- total_students_sch[total_students_sch$year=="2012", c("DIV_NAME", "SCH_NAME","total_students_sch")]
sch_cw <- merge(sch_cw, sch_total2012, by.x = c("div_name","sch_names"), by.y = c("DIV_NAME","SCH_NAME"))
sch_total2013 <- total_students_sch[total_students_sch$year=="2013", c("DIV_NAME", "SCH_NAME","total_students_sch")]
sch_cw <- merge(sch_cw, sch_total2013, by.x = c("div_name","sch_names"), by.y = c("DIV_NAME","SCH_NAME"))
sch_total2014 <- total_students_sch[total_students_sch$year=="2014", c("DIV_NAME", "SCH_NAME","total_students_sch")]
sch_cw <- merge(sch_cw, sch_total2014, by.x = c("div_name","sch_names"), by.y = c("DIV_NAME","SCH_NAME"))
sch_total2015 <- total_students_sch[total_students_sch$year=="2015", c("DIV_NAME", "SCH_NAME","total_students_sch")]
sch_cw <- merge(sch_cw, sch_total2015, by.x = c("div_name","sch_names"), by.y = c("DIV_NAME","SCH_NAME"))
sch_total2016 <- total_students_sch[total_students_sch$year=="2016", c("DIV_NAME", "SCH_NAME","total_students_sch")]
sch_cw <- merge(sch_cw, sch_total2016, by.x = c("div_name","sch_names"), by.y = c("DIV_NAME","SCH_NAME"))
colnames(sch_cw)[7:ncol(sch_cw)] <- c("sch_total2011","sch_total2012","sch_total2013","sch_total2014","sch_total2015","sch_total2016")
