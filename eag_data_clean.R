

## This file will create the master data that each figure will pull from 

setwd("C:/Users/g38f293/Desktop/eags/data")

j2jOD <- read.csv("j2jod_mt_all.csv", header = TRUE, stringsAsFactors = FALSE)

j2jOD2 <- subset(j2jOD, (j2jOD$geography == 30 &
                       (j2jOD$agg_level == 197633
                        | 
                        j2jOD$agg_level == 197635
                       |
                         j2jOD$agg_level == 197649
                       | 
                         j2jOD$agg_level == 197889
                       )))

j2jOD2$totin = j2jOD2$EE + j2jOD2$AQHire

write.csv(j2jOD2, file = "j2jod_mt_subset.csv")




