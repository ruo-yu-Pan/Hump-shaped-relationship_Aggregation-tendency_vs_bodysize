windowsFonts(NR = "Times New Roman")

wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)
fig_path = "D:/Ruo_data/2019_Paper_Publish/Publish/SizeAggregTend_data/output/fig/Appendix/"

northsea <- read.csv("./SizeAggregTend_data/compiled/CPUE_per_length_per_subarea_clean.csv", header = T, sep = ",")
tot_sta<-as.data.frame(xtabs(~Subarea,data = northsea))
subarea_symb <- read.csv("./SizeAggregTend_data/compiled/subarea_symb.csv")


### North Sea map ############################

library(maps)
library(mapdata)

min_lat =min(subarea_symb$lat)
max_lat =max(subarea_symb$lat)
min_lon =min(subarea_symb$long)
max_lon =max(subarea_symb$long)

# grid line

grid_size_lat = 0.5
grid_size_lon = 1

lat_vec =  seq(min_lat-2.25,max_lat+2.25,grid_size_lat)
lon_vec =  seq(min_lon-5.5,max_lon+5.5,grid_size_lon)

# plot
jpeg(paste0(fig_path,"Fig_NorthSea_subarea_map.jpeg"), width=1400, height=1600, units = "px",res=300)

plot(c(min_lon,min_lat),xlim=c(min_lon-5,max_lon+5),
     ylim=c(min_lat-2,max_lat+2),type="n",
     xlab = "Longitude",ylab="Latitude",
     cex.axis=0.6,cex.lab=1.2)

points(subarea_symb$long[-c(1:6)],subarea_symb$lat[-c(1:6)],pch=15,col="grey",cex=1.6)
abline(v=lon_vec)
abline(h=lat_vec)

maps::map('worldHires',xlim=c(min_lon-5,max_lon+5),
          ylim=c(min_lat-2,max_lat+2),fill=T,col="white",add=T)

dev.off()
