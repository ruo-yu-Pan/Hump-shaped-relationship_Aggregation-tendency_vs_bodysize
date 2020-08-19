windowsFonts(NR = "Times New Roman")

#2019/06/18 Plotting
wd = "D:/Ruo_data/2019_Paper_Publish/Code/Git/"
setwd(wd)
fig_path = "D:/Ruo_data/2019_Paper_Publish/Figure/"

northsea <- read.csv("./Data/CPUE per length per subarea_2016-09-21_1965to2016_q1q3.csv", header = T, sep = ",")
tot_sta<-as.data.frame(xtabs(~Subarea,data = northsea))

# make the subarea symbol become true latitude and longtitude
lat <-seq(49.75,61.75,0.5)
long <- seq(-3.5,12.5,1)

subarea_symb <- data.frame(subarea=tot_sta[,1],
                           lat = lat[as.integer(factor(substr(tot_sta[,1],1,2)))],
                           long = long[as.integer(factor(substr(tot_sta[,1],3,4)))])

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

jpeg(paste(fig_path,"NorthSea_subarea_map.jpeg",sep=""), width=7, height=9, units = "in",res=300)

plot(c(min_lon,min_lat),xlim=c(min_lon-5,max_lon+5),
     ylim=c(min_lat-2,max_lat+2),type="n",xlab = "Longitude",ylab="Latitude",family = "NR",cex.lab=2)

points(subarea_symb$long[-c(1:6)],subarea_symb$lat[-c(1:6)],pch=15,col="grey",cex=2.5)
abline(v=lon_vec)
abline(h=lat_vec)

maps::map('worldHires',xlim=c(min_lon-5,max_lon+5),
          ylim=c(min_lat-2,max_lat+2),fill=T,col="white",add=T)

dev.off()
