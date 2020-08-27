
wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)

source("./SizeAggregTend_code/Habitat_Information/Function_Habitat_information.R")

northsea <- read.csv("./SizeAggregTend_data/compiled/CPUE_per_length_per_subarea_clean.csv", header = T, sep = ",")
tot_sta <-as.data.frame(xtabs(~SubArea,data = northsea))

# make the subarea symbol become true latitude and longtitude
lat <-seq(49.75,61.75,0.5)
long <- seq(-3.5,12.5,1)

subarea_symb <- data.frame(subarea=tot_sta[,1],
                           lat = lat[as.integer(factor(substr(tot_sta[,1],1,2)))],
                           long = long[as.integer(factor(substr(tot_sta[,1],3,4)))])



write.csv(subarea_symb,"./SizeAggregTend_data/compiled/subarea_symb.csv")
