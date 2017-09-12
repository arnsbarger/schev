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
        
        # some colleges contain lists of 5 empty data frames.
        # we exclude these because they appear as rows of na's in the final product anyways.
        if (nrow(college_fips) > 0){ 
            
            # a usually = 9 (and b usually = 8) because there are 9 years recorded (2007-2016).
            # some empty data frames (no data collected for any years) cause errors.
            # therefore we make a, b (the dimensions of the df we rbind to final product) dependent on the inputs.
            a <- as.numeric(nrow(as.data.frame(county[(j*i)]))) 
            b <- (a - 1)
            
            school_df <- data.frame(college_fips = names(county[(j*i)]),
                                    fall_term = college_fips[1:a,2],
                                    num_applications = college_fips[1:a,3],
                                    num_admitted = college_fips[1:a,6],
                                    num_enrolled = college_fips[1:a,9],
                                    admit_rate = college_fips[1:a,12],
                                    yield_rate = college_fips[1:a,15])
            
            final_product <- rbind(final_product, school_df)
            
            print(j)
        }
        
    }
    
    print(i)
}

# separeate first column of form "College_###" into two separate columns:
# (1) college name, (2) 3-digit county fips code
library(tidyr)
final_product <- separate(data = final_product, col = "college_fips", into = c("college", "fips"), sep = "_", remove = T)

# save data
write.csv(final_product, "~/Documents/SDAL/SCHEV/data/schev_website.csv", row.names = FALSE, quote = 2)

test <- read.csv("~/Documents/SDAL/SCHEV/data/schev_website.csv")
