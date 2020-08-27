
# function for calculating the weighted prefer bottom temperature for species
library("dplyr")
library("raster")
source("D:/Ruo_data/2019_Paper_Publish/Publish/SizeAggregTend_code/Habitat_Information/Function_Habitat_information.R")

# data: Compiled temperature data
# sp_s_name: species science name
# quarter: 1 or 3
# sp_length_range.s: the length range for specific species and specific quarter


size_mean_bottom_T_fuc <- function(data=Temp.Q1,
                         sp_s_name=sp_name[i],
                         quarter=1,
                         sp_length_range.s=allsp_length_range.s[i,]){
  
  data$C.lat <- cut(data$Latitude..degrees_north.,breaks = lat_vec,include.lowest = TRUE,labels = seq(1, length(lat_vec)-1))
  data$C.lon <- cut(data$Longitude..degrees_east.,breaks = lon_vec,include.lowest = TRUE,labels = seq(1, length(lon_vec)-1))
  data$temp_sta <- "a"
  
  for(i in 1:length(subarea_symb$subarea)){
    data$temp_sta [
      which(data$C.lat==subarea_symb$lat_lab[i] & data$C.lon==subarea_symb$long_lab[i])]=as.character(subarea_symb$subarea[i])
  }   
  
  data$year <- substr(data$yyyy.mm.ddThh.mm,1,4)
  
  sta_yr_data <-
    data %>% 
    group_by(year, temp_sta, Bot..Depth..m.) %>%
    mutate(max_pres=max(PRES..db.))
  
  bottom_T_data <- 
    sta_yr_data %>%
    filter(PRES..db.== max_pres)
  bottom_T_data$temp_sta <- factor(bottom_T_data$temp_sta)
  
  bottom_T_0 <- matrix(NA,nrow = 193,ncol=25)
  
  bottom_T <- tapply(bottom_T_data$TEMP..deg.C.,
                     list(bottom_T_data$temp_sta,bottom_T_data$year),
                     function(x)mean(x,na.rm=T))
  bottom_T <- bottom_T[-nrow(bottom_T),]
  
  bottom_T_0[which(subarea_symb$subarea %in% rownames(bottom_T)),]<-bottom_T 
  
  
  ## define size range #######################################################
  species <- northsea[which(northsea$Species== sp_s_name & northsea$Year>=1991 & northsea$Year<=2015 & northsea$Quarter ==quarter),]
  
  #size_cv_bT <- NULL
  size_mean_bT <- NULL
  for(j in 1:16){
    fish_stage <- species[which(species$LngtClass < sp_length_range.s[j+1] & 
                                  species$LngtClass >= sp_length_range.s[j]),]
    
    stage_hab_info <- habitat_info_fuc(fish_stage)
    
    # exclude the non-habitat data
    
    bottom_T_in_habitat <- bottom_T_0
    bottom_T_in_habitat[which(is.nan(bottom_T_in_habitat)==T)] <- NA
    bottom_T_in_habitat[as.numeric(stage_hab_info[[3]][["subarea"]]),] <- NA 
    #bottom_T_in_habitat_cv <- mean(apply(bottom_T_in_habitat,1,cv,na.rm=T),na.rm=T)
    size_mean_bT <- c(size_mean_bT,mean(bottom_T_in_habitat,na.rm = T))
    #size_cv_bT <- c(size_cv_bT,bottom_T_in_habitat_cv)
  }
  return(size_mean_bT)
}

