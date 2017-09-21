# compile dataframe for lasso model
library(dplyr)

# initialize data frame
lasso.df <- data.frame(school = as.character(),
                       county = as.character(),
                       var_name = as.character(),
                       var_value = as.character(),
                       year = as.numeric(), 
                       stringsAsFactors=FALSE)

# vdoe data
#### graduation rate ####
gradrate <- read.csv("~/Documents/SDAL/SCHEV/data/VDOE/2015_hs_grad.csv")

gradrate <- gradrate %>% filter(GENDER=="" & DISABILITY_FLAG=="" & LEP_FLAG=="" & DISADVANTAGED_FLAG=="" & is.na(FEDERAL_RACE_CODE))
summer <- group_by(gradrate,SCHOOL_YEAR,SCH_NAME,SCH_NUM, DIV_NAME, DIV_NUM,HS_COMPLETION_NAME) %>% summarise(count=sum(HS_COMPLETER_CNT))

count_all_grads <- aggregate(summer$count, by=list(summer$SCH_NAME, summer$DIV_NAME), FUN=sum)
lasso.df <- rbind(lasso.df, data.frame(school = count_all_grads$Group.1, 
                                       county = count_all_grads$Group.2,
                                       var_name="count_all_graduates", 
                                       var_value = count_all_grads$x, 
                                       year = 2016))

count_grads_by_degreetype <- aggregate(summer$count, by=list(summer$SCH_NAME, summer$DIV_NAME, summer$HS_COMPLETION_NAME), FUN=sum)
lasso.df <- rbind(lasso.df, data.frame(school = count_grads_by_degreetype$Group.1, 
                                       county = count_grads_by_degreetype$Group.2,
                                       var_name= paste("count of", count_grads_by_degreetype$Group.3), 
                                       var_value = count_grads_by_degreetype$x, 
                                       year = 2016))

rm("gradrate", "count_all_grads", "count_grads_by_degreetype")

#### career & technical completers ####
cte <- read.csv("~/Documents/SDAL/SCHEV/data/VDOE/2014-cte-comp.csv")
unique(cte$SCHOOL_YEAR)
cte <- cte[cte$SCHOOL_YEAR=="2014-2015", ]

cte <- cte %>% filter(GENDER=="" & DISABILITY_FLAG=="" & LEP_FLAG=="" & DISADVANTAGED_FLAG=="" & is.na(FEDERAL_RACE_CODE)) # why is SCH_NAME empty for so many values, but there are still counts?
summer <- group_by(cte,SCH_NAME,SCH_NUM, DIV_NAME, DIV_NUM) %>% summarise(count=sum(as.numeric(as.character(COMPLETER_CNT))))

count_all_cte_completers <- aggregate(summer$count, by=list(summer$SCH_NAME, summer$DIV_NAME), FUN=sum)
lasso.df <- rbind(lasso.df, data.frame(school = count_all_cte_completers$Group.1, 
                                       county = count_all_cte_completers$Group.2,
                                       var_name= "count_all_cte_completers", 
                                       var_value = count_all_cte_completers$x, 
                                       year = 2015))

rm("count_all_cte_completers", "cte")

#### home school or religious exempt ####
hsre <- read.csv("~/Documents/SDAL/SCHEV/data/VDOE/religious_exempt/2015_2016.csv")

lasso.df <- rbind(lasso.df, data.frame(school = "", 
                                       county = hsre$Division.Name,
                                       var_name= "count_home_instruction9.12", 
                                       var_value = hsre$Home.Instruction.9.12, 
                                       year = 2016))

