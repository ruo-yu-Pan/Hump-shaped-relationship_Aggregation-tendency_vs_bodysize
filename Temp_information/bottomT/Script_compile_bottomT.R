
# Compile bottom temperature data

wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)


Temp_data = read.csv("./SizeAggregTend_data/raw_data/bottomT/1991.csv",header = T,stringsAsFactors = F)

for(year in 1:24){
  temp = read.csv(paste0("./SizeAggregTend_data/raw_data/bottomT/",as.character(year+1991),".csv"),header = T,stringsAsFactors = F)
  Temp_data = rbind(Temp_data,temp)
  print("save")
}

write.csv(Temp_data,"./SizeAggregTend_data/compiled/bottomT/Temp_1991_2015.csv")
