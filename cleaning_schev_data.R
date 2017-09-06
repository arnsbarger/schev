schev <- readRDS("Documents/SDAL/SCHEV/data/schev_col_cos.RDS") # list - 79 colleges

final_product <- data.frame(college_fips = character(),
                            fall_term = numeric(),
                            num_applications = numeric(),
                            num_admitted = numeric(),
                            num_enrolled = numeric(),
                            admit_rate = numeric(),
                            yield_rate = numeric(),
                            stringsAsFactors = FALSE)

# i in 79 colleges
for (i in 1:79){
    college <- schev[i]
    
    # j of 133 counties
    for (j in 1:133){
        county <- college[[1]]
        
        college_fips <- as.data.frame(county[(j*i)])
        
        # a usually = 9 (and b = 8) because there are 9 years recorded (2007-2016)
        # some empty data frames (no data collected for any years) cause errors
        # therefore we make a, b dependent on the inputs
        a <- as.numeric(nrow(as.data.frame(county[(j*i)]))) 
        b <- (a - 1)
        
        final_product[(a*(j*i)-b):(a*(j*i)),"college_fips"] <- names(county[(j*i)])
        
        final_product[(a*(j*i)-b):(a*(j*i)),"fall_term"] <- college_fips[1:a,2]
        final_product[(a*(j*i)-b):(a*(j*i)),"num_applications"] <- college_fips[1:a,3]
        final_product[(a*(j*i)-b):(a*(j*i)),"num_admitted"] <- college_fips[1:a,6]
        final_product[(a*(j*i)-b):(a*(j*i)),"num_enrolled"] <- college_fips[1:a,9]
        final_product[(a*(j*i)-b):(a*(j*i)),"admit_rate"] <- college_fips[1:a,12]
        final_product[(a*(j*i)-b):(a*(j*i)),"yield_rate"] <- college_fips[1:a,15]
        
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

