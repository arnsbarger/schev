# pulaski vis

setwd("~/Google Drive/SCHEV (Peter Blake - Wendy Kang)")
vis_data <- read.csv("Code/Maddie/pulaski/VDOE_long_format_merge-allVA.csv")
NRV <- list("Pulaski County","Radford City","Montgomery County","Giles County","Floyd County") 

library(dplyr)
library(ggplot2)
library(gridExtra)

nrv <- vis_data %>% filter(county_name %in% NRV)

# FEMALE DROPOUT RATE
nrv$cohort_dropout_rate <- nrv$cohort_dropout_cnt / nrv$total_students_sch
nrv$total_drop_rate <- nrv$dropoutTotal / nrv$total_students_sch

dropout.f <- ggplot() +
    geom_line(data = nrv %>% filter(GENDER == "F"), aes(x = year, y = cohort_dropout_rate, group = sch_name_clean, color = sch_name_clean)) +
    labs(title = "Female drop out rates in NRV high schools", x = "Year", y = "Dropout Rate")

dropout.m <- ggplot() +
    geom_line(data = nrv %>% filter(GENDER == "M"), aes(x = year, y = cohort_dropout_rate, group = sch_name_clean, color = sch_name_clean)) +
    labs(title = "Male drop out rates in NRV high schools", x = "Year", y = "Dropout Rate")

grid.arrange(dropout.f, dropout.m)


