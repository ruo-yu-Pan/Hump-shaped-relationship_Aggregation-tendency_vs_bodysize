windowsFonts(newrom = windowsFont("Times New Roman"))

wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)

northsea <- read.csv("./SizeAggregTend_data/compiled/CPUE_per_length_per_subarea_clean.csv", header = T, sep = ",")
allsp_length_range.s <- as.matrix(read.table("./SizeAggregTend_data/compiled/allsp_length_range_16class.txt"))

#### spatial information #####
subarea_symb <- read.csv("./SizeAggregTend_data/compiled/subarea_symb.csv")
subarea_symb <- subarea_symb[,-1]

#### Function needed
source("./SizeAggregTend_code/Habitat_Information/Function_Habitat_information.R")
source("./SizeAggregTend_code/Analysis/Function_SizeBased_TL_16.R")


############################################################## 
### Size-based TL
############################################################## 

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
  
  Path="./SizeAggregTend_data/output/1_size_TL/sizeTL_plot/"
  
  png(paste0(Path,sp_c_name,"Q1.png"), width=6, height=5, units = "in",res=300)
  sp_TL.Q1 <- sb_TL_fuc(sp.2.Q1,sp_length_range.1,sp.hab.info,spe.name=paste(sp_c_name," Q1"),"./SizeAggregTend_data/output/1_size_TL/TL999boot/")
  dev.off()
  
  png(paste0(Path,sp_c_name,"Q3.png"), width=6, height=5, units = "in",res=300)
  sp_TL.Q3 <- sb_TL_fuc(sp.2.Q3,sp_length_range.3,sp.hab.info,spe.name=paste(sp_c_name," Q3"),"./SizeAggregTend_data/output/1_size_TL/TL999boot/")
  dev.off()
  
  all_TL_result <- rbind(all_TL_result,sp_TL.Q1,sp_TL.Q3)
}

write.csv(all_TL_result,"./SizeAggregTend_data/output/1_size_TL/all_TL_result_16class.csv")
