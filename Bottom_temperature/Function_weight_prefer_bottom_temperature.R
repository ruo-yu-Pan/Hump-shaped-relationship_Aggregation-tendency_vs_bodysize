
# function for calculating the weighted prefer bottom temperature for species
library("dplyr")

prefer_bottom_T_fuc <- function(data=Temp.Q1,
                         sp_s_name=sp_name[i],
                         quarter=1,
                         sp_length_range.s=allsp_length_range.s[i,],
                         s=i){
  
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
    
  bottom_T <- tapply(bottom_T_data$TEMP..deg.C.,
                           list(bottom_T_data$temp_sta,bottom_T_data$year),
                           function(x)mean(x,na.rm=T))

  
  # size part
  spe_data_filt0 <- northsea[which(northsea$Species== sp_s_name & northsea$Year>=1991 & northsea$Year<=2015 & northsea$Quarter ==quarter),]
  # exclude the subarea data which is not in habitat
  sp.hab.info <- habitat_info_fuc(spe_data_filt0)
  for(i in 1:length(sp.hab.info[[3]]$subarea)){
    if(length(which(spe_data_filt0$Subarea==sp.hab.info[[3]]$subarea[i]))!=0){
      spe_data_filt0 <- spe_data_filt0[-which(spe_data_filt0$Subarea==sp.hab.info[[3]]$subarea[i]),] 
    }
  }
  
  Cut_study_size <- cut(as.numeric(as.character(spe_data_filt0$LngtClas)),
                        breaks = sp_length_range.s,right = F,labels = seq(1,16),include.lowest = T)
  Cut_study_size <- as.character(Cut_study_size)
  Cut_study_size[which(is.na(Cut_study_size)==T)] <- "99"
  
  sta_size_totalcount <- list()
  for(i in 1:16){
    lab <- which(Cut_study_size==as.character(i))
    spe <- spe_data_filt0[lab,]
    
    sta_size_totalcount[[i]] <- tapply(spe$CPUE_number_per_hour,
                                       list(spe$Year,spe$Subarea),
                                       sum,na.rm=T)#calculate the density of fish whose size is within certain length for each year and station.
    if(nrow(sta_size_totalcount[[i]])!=25){
      lab2 <- which(as.character(c(1991:2015)) %in% rownames(sta_size_totalcount[[i]]))
      df <- matrix(NA,nrow=25,ncol=194)
      for(k in 1:length(lab2)){
        df[lab2[k],]=sta_size_totalcount[[i]][k,]
      }
      sta_size_totalcount[[i]] <- df
    }
  }
  
  
  sta_weight_temp_pre <- lapply(sta_size_totalcount,function(x)x * t(bottom_T[-195,]))
  sta_weight_temp <- NULL
  for(i in 1:16){
    sta_weight_temp_sum <- apply(sta_weight_temp_pre[[i]],1,sum,na.rm=T)/apply(sta_weight_temp_pre[[i]]/t(bottom_T[-195,]),1,sum,na.rm=T)
    sta_weight_temp <- rbind(sta_weight_temp,sta_weight_temp_sum)
  }
  
  size_specific_weight_temp <- apply(sta_weight_temp,1,function(x)mean(x,na.rm = T))
  print(s)
  return(size_specific_weight_temp)
}
