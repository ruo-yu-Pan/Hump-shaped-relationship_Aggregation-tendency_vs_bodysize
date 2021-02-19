
library("dplyr")


windowsFonts(newrom = windowsFont("Times New Roman"))

######################################################################
# Calculate the mean bottom temperature across size-specific habitat
######################################################################
wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)

#### import northsea data and bio information
northsea <- read.csv("./SizeAggregTend_data/compiled/CPUE_per_length_per_subarea_clean.csv", header = T, sep = ",")
allsp_length_range.s <- as.matrix(read.table("./SizeAggregTend_data/compiled/allsp_length_range_16class.txt"))
sp_name <- c("Clupea harengus","Gadus morhua","Melanogrammus aeglefinus",
             "Merlangius merlangus","Pleuronectes platessa", 
             "Pollachius virens","Scomber scombrus",
             "Sprattus sprattus", "Trisopterus esmarkii")

#### import temperature
Temp_data <- read.csv("./SizeAggregTend_data/compiled/bottomT/Temp_1991_2015.csv",header = T,stringsAsFactors = F)
Temp.Q1 <- Temp_data[which(substr(Temp_data$yyyy.mm.ddThh.mm,6,7)=="02"),]
Temp.Q3 <- Temp_data[which(substr(Temp_data$yyyy.mm.ddThh.mm,6,7)=="08"),]


##################################################################
#### spatial information #########################################
##################################################################
subarea_symb <- read.csv("./SizeAggregTend_data/compiled/subarea_symb.csv")
subarea_symb$lat_lab <- as.integer(factor(subarea_symb$lat))
subarea_symb$long_lab <- as.integer(factor(subarea_symb$long))
subarea_symb <- subarea_symb[,-1]

#
min_lat =min(subarea_symb$lat)
max_lat =max(subarea_symb$lat)
min_lon =min(subarea_symb$long)
max_lon =max(subarea_symb$long)

# grid line
grid_size_lat = 0.5
grid_size_lon = 1
lat_vec =  seq(min_lat-0.25,max_lat+0.25,grid_size_lat)
lon_vec =  seq(min_lon-0.5,max_lon+0.5,grid_size_lon)


########################################################
# calculate mean and cv of temperature 
########################################################
source("./SizeAggregTend_code/Temp_information/Function_SST_in_size_specific_habitat.R")
source("./SizeAggregTend_code/Temp_information/Function_bottomT_in_size_specific_habitat.R")


# each sp
source("./SizeAggregTend_code/Habitat_Information/Function_Habitat_information.R")


allsp_Temp <- NULL

for(i in 1:9){
  if(i %in% c(1,7,8)){
    SST_1 <- size_SST_fuc(Temp.Q1,sp_name[i],1,allsp_length_range.s[i,])
    allsp_Temp <- rbind(allsp_Temp,SST_1)
  }else{
    bT_1 <- size_bottom_T_fuc(Temp.Q1,sp_name[i],1,allsp_length_range.s[i,])
    allsp_Temp <- rbind(allsp_Temp,bT_1)
  }
}

for(i in 1:9){
  if(i %in% c(1,7,8)){
    SST_3 <- size_SST_fuc(Temp.Q3,sp_name[i],1,allsp_length_range.s[i,])
    allsp_Temp <- rbind(allsp_Temp,SST_3)
  }else{
    bT_3 <- size_bottom_T_fuc(Temp.Q3,sp_name[i],3,allsp_length_range.s[i+9,])
    allsp_Temp <- rbind(allsp_Temp,bT_3)
  }
}

allsp_Temp <- as.data.frame(allsp_Temp)
allsp_Temp$species <- rep(rep(c(1:9),each=16),2)
allsp_Temp$quarter <- rep(c(1,3),each=144)
allsp_Temp <- allsp_Temp %>% arrange(species,quarter)

write.csv(allsp_Temp,"./SizeAggregTend_data/compiled/Temperature_info.csv")
