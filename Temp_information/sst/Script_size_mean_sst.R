
wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)

# biological data #############################################################
northsea <- read.csv("./SizeAggregTend_data/compiled/CPUE_per_length_per_subarea_clean.csv", header = T, sep = ",")
allsp_length_range.s <- as.matrix(read.table("./SizeAggregTend_data/compiled/allsp_length_range_16class.txt"))
sp_name <- c("Clupea harengus","Gadus morhua","Melanogrammus aeglefinus",
             "Merlangius merlangus","Pleuronectes platessa", 
             "Pollachius virens","Scomber scombrus",
             "Sprattus sprattus", "Trisopterus esmarkii")

#### spatial information #####
subarea_symb <- read.csv("./SizeAggregTend_data/compiled/subarea_symb.csv")
subarea_symb <- subarea_symb[,-1]


# sst data        #############################################################
SST.Q1.yrmean <- read.csv("./SizeAggregTend_data/compiled/sst/Q1sst_yr_mean.csv",header = T)
SST.Q3.yrmean <- read.csv("./SizeAggregTend_data/compiled/sst/Q3sst_yr_mean.csv",header = T)


# pretreatment of SST data
# Q1
SST.Q1.yrmean <- SST.Q1.yrmean[,-1]
SST.Q1.yrmean$subarea <- "a"

for(i in 1:length(subarea_symb$subarea)){
  sta_lab = which(SST.Q1.yrmean$lon==subarea_symb$long[i] & SST.Q1.yrmean$lat==subarea_symb$lat[i])
  SST.Q1.yrmean$subarea[sta_lab]= as.character(subarea_symb$subarea[i])
} 

subarea_symb$subarea <- as.character(subarea_symb$subarea)
SST.Q1.yrmean.order <- merge(subarea_symb,SST.Q1.yrmean,by="subarea",all.x=T)
SST.Q1.yrmean.order <- SST.Q1.yrmean.order[,c(1,4:6)]

# Q3
SST.Q3.yrmean <- SST.Q3.yrmean[,-1]
SST.Q3.yrmean$subarea <- "a"
for(i in 1:length(subarea_symb$subarea)){
  sta_lab = which(SST.Q3.yrmean$lon==subarea_symb$long[i] & SST.Q3.yrmean$lat==subarea_symb$lat[i])
  SST.Q3.yrmean$subarea[sta_lab]= as.character(subarea_symb$subarea[i])
} 
SST.Q3.yrmean.order <- merge(subarea_symb,SST.Q3.yrmean,by="subarea",all.x=T)
SST.Q3.yrmean.order <- SST.Q3.yrmean.order[,c(1,4:6)]




########################################################
# each sp
########################################################

source("./SizeAggregTend_code/Habitat_Information/Function_Habitat_information.R")
source("./SizeAggregTend_code/Temp_information/sst/Function_size_mean_sst.R")


pela_sp_SST <- NULL

for(i in c(1,7,8)){
  SST_1 <- size_mean_SST_fuc(SST.Q1.yrmean.order,
                            sp_name[i],
                            1,
                            allsp_length_range.s[i,])
  SST_3 <- size_mean_SST_fuc(SST.Q3.yrmean.order,
                             sp_name[i],
                             3,
                             allsp_length_range.s[i+9,])
  pela_sp_SST <- c(pela_sp_SST,SST_1,SST_3)
}

write.csv(pela_sp_SST,"./SizeAggregTend_data/compiled/sst/size_mean_sst.csv")
