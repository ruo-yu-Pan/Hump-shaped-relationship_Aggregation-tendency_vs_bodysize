windowsFonts(NR = "Times New Roman")

#2019/06/18 Plotting
northsea <- read.csv("D:\\Ruo_data\\2017_Master\\Master_Research\\Data\\NorthSea\\Analysis\\CPUE per length per subarea_2016-09-21_1965to2016_q1q3.csv", header = T, sep = ",")
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
library(ggplot2)

min_lat =min(subarea_symb$lat)
max_lat =max(subarea_symb$lat)
min_lon =min(subarea_symb$long)
max_lon =max(subarea_symb$long)

# grid line

grid_size_lat = 0.5
grid_size_lon = 1

lat_vec =  seq(min_lat-2.25,max_lat+2.25,grid_size_lat)
lon_vec =  seq(min_lon-5.5,max_lon+5.5,grid_size_lon)

jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\NorthSea_subarea_map.jpeg", width=3.5, height=4.5, units = "in",res=300)

plot(c(min_lon,min_lat),xlim=c(min_lon-5,max_lon+5),
     ylim=c(min_lat-2,max_lat+2),type="n",xlab = "Longtitude",ylab="Latitude",family = "NR")

points(subarea_symb$long[-c(1:6)],subarea_symb$lat[-c(1:6)],pch=15,col="grey",cex=1.2)
abline(v=lon_vec)
abline(h=lat_vec)

maps::map('worldHires',xlim=c(min_lon-5,max_lon+5),
          ylim=c(min_lat-2,max_lat+2),fill=T,col="white",add=T)

dev.off()

# plot the station

plot(c(min_lon,min_lat),xlim=c(min_lon-5,max_lon+5),
     ylim=c(min_lat-2,max_lat+2),type="n",xlab = "longtitude",ylab="latitude")
maps::map('worldHires',xlim=c(min_lon-5,max_lon+5),
          ylim=c(min_lat-2,max_lat+2),fill=T,col="grey90",add=T,border="grey80")
points(subarea_symb$long[-c(1:6)],subarea_symb$lat[-c(1:6)],pch=1,cex=0.8)

#####################################################################################
####  function for ploting habitat map information and non-habitat station   ########
#####################################################################################

library(sp)
habitat_plot_fuc <- function(species){
  
  if(length(which(species$Area==10))>0){
    species <- species[-which(species$Area==10),]
  }
  habitat_pre<-as.data.frame(xtabs(~Subarea,data = species))
  
  # the station has less than 3 data through whole station
  less.3.habitat <-subarea_symb[which(habitat_pre[,2]<=3),]
  
  # the subarea that has at least 3 data through whole station
  habitat.hul <- subarea_symb[-which(habitat_pre[,2]<=3),]
  
  # calculation for polygon
  hull <- chull(as.matrix(habitat.hul[,-1]))
  hull <- c(hull, hull[1])
  
  # pick out the station which is inside the polygon from less.3.habitat
  n.habitat <- less.3.habitat[-which(
    point.in.polygon(less.3.habitat[,2],less.3.habitat[,3],
                     habitat.hul[hull,2],habitat.hul[hull,3])!=0),]
  
  output <- list(habitat.hul,hull,n.habitat)
  return(output)
}


#####################################################################################
###################  function for ploting habitat map   #############################
#####################################################################################
habitat_plot <- function(sp_name,sp_common_name){
  sp <- northsea[which(northsea$Species== sp_name & northsea$Year>=1991 & northsea$Year<=2015),]
  sp.name <- c(sp_common_name)
  sp.hab.info <- habitat_plot_fuc(sp)
  plot(c(min_lon,min_lat),xlim=c(min_lon-5,max_lon+5),
       ylim=c(min_lat-2,max_lat+2),type="n",
       xlab = "longtitude",ylab="latitude",xaxt="n",yaxt="n",
       family="newrom")

  maps::map('worldHires',xlim=c(min_lon-4.8,max_lon+4.8),
            ylim=c(min_lat-1.8,max_lat+1.8),fill=T,col="grey90",add=T,border="grey80")
  points(subarea_symb$long[-c(1:6)],subarea_symb$lat[-c(1:6)],pch=1,cex=0.3,lwd=0.3)
  points(sp.hab.info[[1]][,3],sp.hab.info[[1]][,2],pch=19,cex=0.3)
  polygon(sp.hab.info[[1]][sp.hab.info[[2]],3],
          sp.hab.info[[1]][sp.hab.info[[2]],2],col=rgb(0,0,0,alpha=0.3))
  text(pos=4,x=-9,y=49,label=sp_common_name,family="newrom",cex=1.4)
}

