# pulaski long data formatting
# 1/10/2017
library(reshape2)
library(scales)

setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)")

sch_cw <- read.csv("Code/Maddie/pulaski/VDOE_long_format_merge-allVA.csv")
NRV <- list("Pulaski County","Radford City","Montgomery County","Giles County","Floyd County") 

sch_cw.nrv <- sch_cw %>% filter(county_name %in% NRV & disciplineType == "SHORT-TER M SUSPENSION (OUT OF SCHOOL)")
colnames(sch_cw.nrv)[38] <- "numShortTermOutOfSchSuspensions"

wide_table.nrv <- sch_cw.nrv[, c("div_name","sch_name_clean","year","schoolYear","county_name","sch_names","total_students_sch", # school info
                                 "standard_diploma","adv_studies_diploma","other_diploma","certificate_of_program_completion","ged_certificate", # diploma types
                                 "total_grads","attending_two_year_college","attending_four_year_college","other_continuing_ed_plans","employment","military","no_plans", # grads/post-grad plans
                                 "numShortTermOutOfSchSuspensions" # discipline
                                 #"GENDER","cohort_graduate_cnt","ontime_grad_cnt","cohort_dropout_cnt","onTimeTotal","dropoutTotal" # dropouts
                                 )]

dropouts <- sch_cw.nrv[, c("div_name","sch_name_clean","year","schoolYear","county_name","sch_names","total_students_sch",
                           "GENDER","cohort_graduate_cnt","ontime_grad_cnt","cohort_dropout_cnt","onTimeTotal","dropoutTotal")] # dropouts

dropouts.f <- dropouts %>% filter(GENDER == "F")
colnames(dropouts.f) <- c("div_name","sch_name_clean","year","schoolYear","county_name","sch_names","total_students_sch",
                          "GENDER", "num_female_graduates", "num_ontime_female_graduates","num_female_dropouts","onTimeTotal","dropoutTotal")

dropouts.m <- dropouts %>% filter(GENDER == "M")
colnames(dropouts.m) <- c("div_name","sch_name_clean","year","schoolYear","county_name","sch_names","total_students_sch",
                          "GENDER", "num_male_graduates", "num_ontime_male_graduates","num_male_dropouts","onTimeTotal","dropoutTotal")
dropouts2 <- merge(dropouts.f[,-8], dropouts.m[,-8])

wide_table.nrv <- merge(wide_table.nrv, dropouts2)

wide_table.nrv$total_cohort_count <- wide_table.nrv$num_female_dropouts +wide_table.nrv$num_female_graduates + wide_table.nrv$num_male_dropouts + wide_table.nrv$num_male_graduates
wide_table.nrv$female_cohort_count<-wide_table.nrv$num_female_dropouts+wide_table.nrv$num_female_graduates
wide_table.nrv$male_cohort_count<-wide_table.nrv$num_male_dropouts+wide_table.nrv$num_male_graduates

long_table<-melt(wide_table.nrv,id=c("div_name","county_name","sch_name_clean","sch_names","year","schoolYear","total_grads","total_students_sch","total_cohort_count","female_cohort_count","male_cohort_count","onTimeTotal","dropoutTotal"))

long_table$category<-ifelse(long_table$variable %in% c("attending_two_year_college","attending_four_year_college","other_continuing_ed_plans","employment","military","no_plans"),"grad_outcomes","")
long_table$category<-ifelse(long_table$variable %in% c("standard_diploma","adv_studies_diploma","other_diploma","certificate_of_program_completion","ged_certificate"),"diploma_type",long_table$category)
long_table$category<-ifelse(long_table$variable %in% c("dropoutTotal","num_ontime_female_graduates","num_female_dropouts","num_ontime_male_graduates","num_male_dropouts","num_male_graduates","num_female_graduates"),"dropout",long_table$category)
long_table$category<-ifelse(long_table$variable %in% c("numShortTermOutOfSchSuspensions"),"disciplinaryOutcome",long_table$category)

wide_table.nrv <- unique(wide_table.nrv)
    
sallie_table <- wide_table.nrv[, c(2,3,7,20,29,28,25)]  
# sallie_table$percent_2year_edu <- wide_table.nrv$attending_two_year_college / wide_table.nrv$total_cohort_count
# sallie_table$percent_4year_edu <- wide_table.nrv$attending_four_year_college / wide_table.nrv$total_cohort_count
# sallie_table$percent_other_edu <- wide_table.nrv$other_continuing_ed_plans / wide_table.nrv$total_cohort_count
# sallie_table$percent_military <- wide_table.nrv$military / wide_table.nrv$total_cohort_count
# sallie_table$percent_workforce <- wide_table.nrv$employment / wide_table.nrv$total_cohort_count
# sallie_table$percent_male_dropouts <- wide_table.nrv$num_male_dropouts / wide_table.nrv$total_cohort_count
# sallie_table$percent_female_dropouts <- wide_table.nrv$num_female_dropouts / wide_table.nrv$total_cohort_count
# sallie_table$percent_insch_suspension <- wide_table.nrv$numShortTermOutOfSchSuspensions / wide_table.nrv$total_students_sch
# 
# sallie_table <- sallie_table %>% filter(year == 2014)
# 
# sallie_table$number_female_dropouts <- wide_table.nrv$num_female_dropouts
# 
# sallie_table2 <- sallie_table[, c("sch_name_clean","percent_female_dropouts","percent_male_dropouts")]
# 
# write.csv(sallie_table, "Code/Maddie/pulaski/nrv_wide_table2.csv")

#long_table$percent <- long_table$value / long_table$total_cohort_count

long_table <- unique(long_table)

diploma_type <- long_table %>% filter(category %in% c("diploma_type"))
grad_outcomes <- long_table %>% filter(category %in% c("grad_outcomes"))
# grad_outcomes <- grad_outcomes %>% filter(!variable %in% c("num_female_graduates","num_ontime_female_graduates","num_male_graduates","num_ontime_male_graduates"))

grad_outcomes_total <- group_by(grad_outcomes,sch_name_clean,schoolYear) %>% summarise(total=sum(value))
grad_outcomes<-left_join(grad_outcomes,grad_outcomes_total,by=c("sch_name_clean","schoolYear"))

grad_outcomes$percent <- grad_outcomes$value / grad_outcomes$total


pal <- rev(viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1,
                       option = "A")(20))[c(1,5,7,10,14,20)] 

show_col(pal)


grad_outcomes$sch_name_clean2 <- gsub(" High", "",grad_outcomes$sch_name_clean)
grad_outcomes$sch_name_clean2 <- factor(grad_outcomes$sch_name_clean2, levels = c('Pulaski County Senior','Floyd County','Giles','Narrows','Auburn','Blacksburg','Christiansburg','Eastern Montgomery','Radford'))

#pdf("Code/Maddie/pulaski/vis/cohort_outcomes_class2014.pdf", height = 9, width = 12)
#png("Code/Maddie/pulaski/vis/cohort_outcomes_class2014.png", height = 900, width = 1200)
png("~/Desktop/cohort_outcomes_class2014.png")
ggplot(data=grad_outcomes %>% filter(year==2014), aes(x=sch_name_clean2, y=percent, fill=factor(variable))) +
    geom_bar(colour="grey50", stat="identity") +
    scale_fill_manual(values=pal,
                       name="Cohort Outcome",
                       labels=c("Attending 2-year institution", "Attending 4-year institution", "Other continuing education", "Employment","Military","No plans")) +
    labs(title = "Cohort outcomes by school (Class of 2014)",x="School",y="Proportion") +
    theme_bw() +
    theme(axis.text.x = element_text(size=15, angle=90,hjust=0.95,vjust=0.2),
          axis.text.y = element_text(size=15),
          axis.title = element_text(size=20),
          title = element_text(size=20),
          legend.text = element_text(size=15))
dev.off()
    

    