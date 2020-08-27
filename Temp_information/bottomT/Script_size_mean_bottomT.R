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
# calculate mean bottom temperature 
########################################################
source("./SizeAggregTend_code/Temp_information/bottomT/Function_mean_bottomT_in_size_specific_habitat.R")

# each sp
source("./SizeAggregTend_code/Habitat_Information/Function_Habitat_information.R")


allsp_bT <- NULL

for(i in 1:9){
  bT_1 <- size_mean_bottom_T_fuc(Temp.Q1,sp_name[i],1,allsp_length_range.s[i,])
  allsp_bT <- rbind(allsp_bT,bT_1)
}

for(i in 1:9){
  bT_3 <- size_mean_bottom_T_fuc(Temp.Q3,sp_name[i],3,allsp_length_range.s[i+9,])
  allsp_bT <- rbind(allsp_bT,bT_3)
}

write.csv(allsp_bT,"./SizeAggregTend_data/compiled/bottomT/mean_bottom_Temperature.csv")


# analysis ########################################################
allsp_bT_tras <- NULL
for(j in 1:9){
  allsp_bT_tras <- c(allsp_bT_tras,allsp_bT[j,],allsp_bT[j+9,])
}

write.csv(allsp_bT_tras,"./SizeAggregTend_data/compiled/bottomT/mean_bottom_Temperature_trans.csv")



