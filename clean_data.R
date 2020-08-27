wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)

northsea = read.csv("./SizeAggregTend_data/raw_data/cpue_per_length_per_subarea/CPUE_per_length_per_subarea.csv", header=T)
northsea = subset(northsea, select=c(Year, Quarter,Area, SubArea, Species, LngtClass, CPUE_number_per_hour))
colnames(northsea)[4] = "Subarea"

write.csv(northsea,"./SizeAggregTend_data/compiled/CPUE_per_length_per_subarea_clean.csv")
