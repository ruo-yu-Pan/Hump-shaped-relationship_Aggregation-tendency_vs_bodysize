
setwd("D:\\Ruo_data\\2019_Paper_Publish\\Data\\environment_data\\Temperature")
windowsFonts(newrom = windowsFont("Times New Roman"))

#Temp_data <- read.csv("1991.csv",header = T)
#for(year in 1:24){
#  temp <- read.csv(paste(as.character(year+1991),".csv",sep=""),header = T)
#  Temp_data <- rbind(Temp_data,temp)
#  print("save")
#}


Temp_data <- read.csv("Temp_1991_2015.csv",header = T)

Temp.Q1 <- Temp_data[which(substr(Temp_data$yyyy.mm.ddThh.mm,6,7)=="02"),]

Temp.Q3 <- Temp_data[which(substr(Temp_data$yyyy.mm.ddThh.mm,6,7)=="08"),]
#Temp_need <- rbind(Temp.Q1,Temp.Q3)
#write.csv(Temp_need,"Temp_1991_2015.csv")


##################################################################
#### check which station have temperature data ###################
##################################################################

northsea <- read.csv("D:\\Ruo_data\\2019_Paper_Publish\\Data\\CPUE per length per subarea_2016-09-21_1965to2016_q1q3.csv", header = T, sep = ",")
tot_sta <-as.data.frame(xtabs(~Subarea,data = northsea))
allsp_length_range.s <- as.matrix(read.table("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\allsp_length_range_16class.txt"))

lat <-seq(49.75,61.75,0.5)
long <- seq(-3.5,12.5,1)

# tot_sta --> from "Script_Size_based_TL_16classes.R"
subarea_symb <- data.frame(subarea=tot_sta[,1],
                           lat = lat[as.integer(factor(substr(tot_sta[,1],1,2)))],
                           long = long[as.integer(factor(substr(tot_sta[,1],3,4)))])
subarea_symb$lat_lab <- as.integer(factor(subarea_symb$lat))
subarea_symb$long_lab <- as.integer(factor(subarea_symb$long))

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

# the function for plot the station that have temp data
source("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Code_for_bodysize_and_temperature_After20200310\\Bottom_temperature_effect\\Function_station_have_temperature.R")

########################################################
# get bottom temperature function
########################################################

source("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Code_for_bodysize_and_temperature_After20200310\\Bottom_temperature_effect\\Function_bottom_temperature_in_size_specific_habitat.R")

########################################################
# each sp
########################################################

source("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Function_Habitat_information.R")

sp_name <- c("Clupea harengus","Gadus morhua","Melanogrammus aeglefinus",
             "Merlangius merlangus","Pleuronectes platessa", 
             "Pollachius virens","Scomber scombrus",
             "Sprattus sprattus", "Trisopterus esmarkii")

sp_common_name <- c("Herring","Cod","Haddock","Whiting","Plaice","Saithe",
                    "Mackerel","Sprat","Norwaypout")


allsp_bT <- NULL

for(i in 1:9){
  bT_1 <- size_mean_bottom_T_fuc(Temp.Q1,sp_name[i],1,allsp_length_range.s[i,],i)[[1]]
  allsp_bT <- rbind(allsp_bT,bT_1)
}

for(i in 1:9){
  bT_3 <- size_mean_bottom_T_fuc(Temp.Q3,sp_name[i],3,allsp_length_range.s[i+9,],i)[[1]]
  allsp_bT <- rbind(allsp_bT,bT_3)
}



# analysis ########################################################
allsp_bT_tras <- NULL
for(j in 1:9){
  allsp_bT_tras <- c(allsp_bT_tras,allsp_bT[j,],allsp_bT[j+9,])
}

write.csv(allsp_bT_tras,"D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\mean_bottom_Temperature.csv")



