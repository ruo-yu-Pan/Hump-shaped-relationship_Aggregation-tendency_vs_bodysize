windowsFonts(newrom = windowsFont("Times New Roman"))

setwd("D:\\Ruo_data\\2017_Master\\Master_Research\\Data\\NorthSea\\Analysis")
northsea <- read.csv("CPUE per length per subarea_2016-09-21_1965to2016_q1q3.csv", header = T, sep = ",")
tot_sta <-as.data.frame(xtabs(~Subarea,data = northsea))
allsp_length_range.s <- as.matrix(read.table("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\allsp_length_range_16class.txt"))

source("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Function_Habitat_information.R")
source("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Code_for_bodysize_and_temperature_After20200310\\16_size_class\\Function_SizeBased_TL_16.R")

# make the subarea symbol become true latitude and longtitude
lat <-seq(49.75,61.75,0.5)
long <- seq(-3.5,12.5,1)

subarea_symb <- data.frame(subarea=tot_sta[,1],
                           lat = lat[as.integer(factor(substr(tot_sta[,1],1,2)))],
                           long = long[as.integer(factor(substr(tot_sta[,1],3,4)))])


############################################################## species analysis
# each sp #############################
sp_name <- c("Clupea harengus","Gadus morhua","Melanogrammus aeglefinus",
             "Merlangius merlangus","Pleuronectes platessa", 
             "Pollachius virens","Scomber scombrus",
             "Sprattus sprattus", "Trisopterus esmarkii")

sp_common_name <- c("Herring","Cod","Haddock","Whiting","Plaice","Saithe",
                    "Mackerel","Sprat","Norwaypout")

all_TL_result <- NULL

for(i in 1:9){ 
  sp_s_name <- sp_name[i]
  sp_c_name <- sp_common_name[i]
  sp.2 <- northsea[which(northsea$Species== sp_s_name & northsea$Year>=1991 & northsea$Year<=2015),]
  sp.hab.info <- habitat_info_fuc(sp.2)
  
  sp.2.Q1 <- northsea[which(northsea$Species== sp_s_name & northsea$Year>=1991 & 
                              northsea$Year<=2015 & northsea$Quarter ==1),]
  sp.2.Q3 <- northsea[which(northsea$Species== sp_s_name & northsea$Year>=1991 & 
                              northsea$Year<=2015 & northsea$Quarter ==3),]
  
  
  sp_length_range.1 <- allsp_length_range.s[i,]
  sp_length_range.3 <- allsp_length_range.s[i+9,]
  
  Path="D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\sp_sizeTL\\"
  
  png(paste(Path,sp_c_name,"Q1.png",sep=""), width=6, height=5, units = "in",res=300)
  sp_TL.Q1 <- sb_TL_fuc(sp.2.Q1,sp_length_range.1,sp.hab.info,spe.name=paste(sp_c_name," Q1"))
  dev.off()
  
  png(paste(Path,sp_c_name,"Q3.png",sep=""), width=6, height=5, units = "in",res=300)
  sp_TL.Q3 <- sb_TL_fuc(sp.2.Q3,sp_length_range.3,sp.hab.info,spe.name=paste(sp_c_name," Q3"))
  dev.off()
  
  all_TL_result <- rbind(all_TL_result,sp_TL.Q1,sp_TL.Q3)
}

write.csv(all_TL_result,"D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\all_TL_result_16class.csv")
