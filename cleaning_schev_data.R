schev <- readRDS("Documents/SDAL/SCHEV/data/schev_col_cos.RDS") # list - 79 colleges

final_product <- data.frame(college_fips = character(),
                            fall_term = numeric(),
                            num_applications = numeric(),
                            num_admitted = numeric(),
                            num_enrolled = numeric(),
                            admit_rate = numeric(),
                            yield_rate = numeric(),
                            stringsAsFactors = FALSE)

# i in 79 college
i <- 1
college <- schev[i]

# j of 133 counties
j <- 2
for (j in 1:133){
    county <- college[[j]]
    
    final_product[(9*i-8):(9*i),"college_fips"] <- names(county[j])
    
    # number of applications - store in final_product DataTable_apps231420.Count
    college_fips <- as.data.frame(county[j])
    final_product[(9*i-8):(9*i),"fall_term"] <- college_fips[1:9,2]
    #final_product[(9*i-8):(9*i),"num_applications"] <- college_fips[1:9,grep("apps", colnames(college_fips))] # apps Count
    final_product[(9*i-8):(9*i),"num_applications"] <- college_fips[1:9,3]
    final_product[(9*i-8):(9*i),"num_admitted"] <- college_fips[1:9,6]
    final_product[(9*i-8):(9*i),"num_enrolled"] <- college_fips[1:9,9]
    final_product[(9*i-8):(9*i),"admit_rate"] <- college_fips[1:9,12]
    final_product[(9*i-8):(9*i),"yield_rate"] <- college_fips[1:9,15]
    
    print(j)
}

