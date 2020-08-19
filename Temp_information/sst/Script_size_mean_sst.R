
# biological data #############################################################
# tot_sta --> from "Script_Size_based_TL_16classes.R"
northsea <- read.csv("D:\\Ruo_data\\2019_Paper_Publish\\Data\\CPUE per length per subarea_2016-09-21_1965to2016_q1q3.csv", header = T, sep = ",")
tot_sta <-as.data.frame(xtabs(~Subarea,data = northsea))
allsp_length_range.s <- as.matrix(read.table("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\allsp_length_range_16class.txt"))

lat <-seq(49.75,61.75,0.5)
long <- seq(-3.5,12.5,1)
subarea_symb <- data.frame(subarea=tot_sta[,1],
                           lat = lat[as.integer(factor(substr(tot_sta[,1],1,2)))],
                           long = long[as.integer(factor(substr(tot_sta[,1],3,4)))])



# sst data        #############################################################
SST.Q1.yrmean <- read.csv("D:\\Ruo_data\\2019_Paper_Publish\\Data\\environment_data\\Temperature\\surface\\Q1sst_yr_mean.csv",header = T)
SST.Q3.yrmean <- read.csv("D:\\Ruo_data\\2019_Paper_Publish\\Data\\environment_data\\Temperature\\surface\\Q3sst_yr_mean.csv",header = T)

# pretreatment of SST data

SST.Q1.yrmean <- SST.Q1.yrmean[,-1]
SST.Q1.yrmean$subarea <- "a"
for(i in 1:length(subarea_symb$subarea)){
  sta_lab = which(SST.Q1.yrmean$lon==subarea_symb$long[i] & SST.Q1.yrmean$lat==subarea_symb$lat[i])
  SST.Q1.yrmean$subarea[sta_lab]= as.character(subarea_symb$subarea[i])
} 
subarea_symb$subarea <- as.character(subarea_symb$subarea)
SST.Q1.yrmean.order <- merge(subarea_symb,SST.Q1.yrmean,by="subarea",all.x=T)
SST.Q1.yrmean.order <- SST.Q1.yrmean.order[,c(1,4:6)]

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

source("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Function_Habitat_information.R")
source("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Code_for_bodysize_and_temperature_After20200310\\Surface_temperature\\Function_size_mean_sst.R")

sp_name <- c("Clupea harengus","Gadus morhua","Melanogrammus aeglefinus",
             "Merlangius merlangus","Pleuronectes platessa", 
             "Pollachius virens","Scomber scombrus",
             "Sprattus sprattus", "Trisopterus esmarkii")

sp_common_name <- c("Herring","Cod","Haddock","Whiting","Plaice","Saithe",
                    "Mackerel","Sprat","Norwaypout")



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

write.csv(pela_sp_SST,"D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\mean_surface_Temperature_pela.csv")
