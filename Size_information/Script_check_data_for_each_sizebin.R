
wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)

source("./SizeAggregTend_code/Habitat_Information/Function_Habitat_information.R")

northsea <- read.csv("./SizeAggregTend_data/compiled/CPUE_per_length_per_subarea_clean.csv", header = T, sep = ",")
tot_sta <-as.data.frame(xtabs(~Subarea,data = northsea))

# make the subarea symbol become true latitude and longtitude
lat <-seq(49.75,61.75,0.5)
long <- seq(-3.5,12.5,1)

subarea_symb <- data.frame(subarea=tot_sta[,1],
                           lat = lat[as.integer(factor(substr(tot_sta[,1],1,2)))],
                           long = long[as.integer(factor(substr(tot_sta[,1],3,4)))])



sp_name <- c("Clupea harengus","Gadus morhua","Melanogrammus aeglefinus",
             "Merlangius merlangus","Pleuronectes platessa", 
             "Pollachius virens","Scomber scombrus",
             "Sprattus sprattus", "Trisopterus esmarkii")


sizebin_datacount <- list()

for(i in 1:9){ 
  sp_s_name <- sp_name[i]
  sp.2 <- northsea[which(northsea$Species== sp_s_name & northsea$Year>=1991 & northsea$Year<=2015),]
  sp.hab.info <- habitat_info_fuc(sp.2)
  
  sp.2.Q1 <- northsea[which(northsea$Species== sp_s_name & northsea$Year>=1991 & 
                              northsea$Year<=2015 & northsea$Quarter ==1),]
  sp.2.Q3 <- northsea[which(northsea$Species== sp_s_name & northsea$Year>=1991 & 
                              northsea$Year<=2015 & northsea$Quarter ==3),]
  
  # exclude the subarea data which is not in habitat
  for(j in 1:length(sp.hab.info[[3]]$subarea)){
    if(length(which(sp.2.Q1$Subarea==sp.hab.info[[3]]$subarea[j]))!=0){
      sp.2.Q1 <- sp.2.Q1[-which(sp.2.Q1$Subarea==sp.hab.info[[3]]$subarea[j]),] 
    }
    if(length(which(sp.2.Q3$Subarea==sp.hab.info[[3]]$subarea[j]))!=0){
      sp.2.Q3 <- sp.2.Q3[-which(sp.2.Q3$Subarea==sp.hab.info[[3]]$subarea[j]),] 
    }
  }
  
  
  sp_size_selec <- xtabs(~LngtClass+Year,data = sp.2.Q1)
  sp_size_selec <- ifelse(sp_size_selec>=5,1,0)
  sp_size_selec <- rowSums(as.data.frame(sp_size_selec))
  sizebin_datacount[[i]] <- sp_size_selec
  
  sp_size_selec <- xtabs(~LngtClass+Year,data = sp.2.Q3)
  sp_size_selec <- ifelse(sp_size_selec>=5,1,0)
  sp_size_selec <- rowSums(as.data.frame(sp_size_selec))
#  Lngth_selec <- sort(unique(as.numeric(as.character(sp.2.Q3$LngtClass))))
#  Lngth_selec <- Lngth_selec[which(sp_size_selec>=3)]
  sizebin_datacount[[i+9]] <- sp_size_selec
}

###############################################################
