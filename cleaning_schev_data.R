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
for (i in 1:79){
    college <- schev[i]
    
    # j of 133 counties
    for (j in 1:133){
        county <- college[[1]]
        
        final_product[(9*j-8):(9*j),"college_fips"] <- names(county[j])
        
        college_fips <- as.data.frame(county[j])
        final_product[(9*j-8):(9*j),"fall_term"] <- college_fips[1:9,2]
        final_product[(9*j-8):(9*j),"num_applications"] <- college_fips[1:9,3]
        final_product[(9*j-8):(9*j),"num_admitted"] <- college_fips[1:9,6]
        final_product[(9*j-8):(9*j),"num_enrolled"] <- college_fips[1:9,9]
        final_product[(9*j-8):(9*j),"admit_rate"] <- college_fips[1:9,12]
        final_product[(9*j-8):(9*j),"yield_rate"] <- college_fips[1:9,15]
        
        print(j)
    }
    
    print(i)
}




# j <- 1
# college <- schev[i]
# county <- college[[j]]
# 
names(county[j])
names(county[5])
names(county[100])

