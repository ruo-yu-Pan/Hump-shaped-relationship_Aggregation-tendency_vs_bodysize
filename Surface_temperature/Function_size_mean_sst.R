
size_mean_SST_fuc <- function(data=SST.Q3.yrmean.order,
                              sp_s_name=sp_name[i],
                              quarter=3,
                              sp_length_range.s=allsp_length_range.s[i+9,]
                              ){
  
  
  ## define size range #######################################################
  species <- northsea[which(northsea$Species== sp_s_name & northsea$Year>=1991 & northsea$Year<=2015 & northsea$Quarter ==quarter),]
  
  #size_cv_bT <- NULL
  size_mean_ssT <- NULL
  for(j in 1:16){
    fish_stage <- species[which(species$LngtClas < sp_length_range.s[j+1] & 
                                  species$LngtClas >= sp_length_range.s[j]),]
    
    stage_hab_info <- habitat_info_fuc(fish_stage)
    
    # exclude the non-habitat data
    
    SST_in_habitat <- data
    colnames(SST_in_habitat)[4] <- "sst"
    n_hab_num <- match(stage_hab_info[[3]][["subarea"]],data$subarea)
    SST_in_habitat$sst[n_hab_num] <- NA 
    
    #bottom_T_in_habitat_cv <- mean(apply(bottom_T_in_habitat,1,cv,na.rm=T),na.rm=T)
    size_mean_ssT <- c(size_mean_ssT,mean(SST_in_habitat$sst,na.rm = T))
    #size_cv_bT <- c(size_cv_bT,bottom_T_in_habitat_cv)
  }
  #result <- list(size_mean_bT,size_cv_bT)
  return(size_mean_ssT)
}

