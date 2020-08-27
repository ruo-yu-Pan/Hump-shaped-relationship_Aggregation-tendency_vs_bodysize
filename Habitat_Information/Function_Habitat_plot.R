windowsFonts(NR = "Times New Roman")


wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)

source("./SizeAggregTend_code/Habitat_Information/Function_Habitat_information.R")


### map information ############################

library(maps)
library(mapdata)

min_lat =min(subarea_symb$lat)
max_lat =max(subarea_symb$lat)
min_lon =min(subarea_symb$long)
max_lon =max(subarea_symb$long)

#####################################################################################
###################  function for ploting habitat map   #############################
#####################################################################################

habitat_plot_func <- function(sp_name="Gadus morhua",sp_common_name){
  sp <- northsea[which(northsea$Species== sp_name & northsea$Year>=1991 & northsea$Year<=2015),]
  sp.name <- c(sp_common_name)
  sp.hab.info <- habitat_info_fuc(sp)
  plot(c(min_lon,min_lat),xlim=c(min_lon-5,max_lon+5),
       ylim=c(min_lat-2,max_lat+2),type="n",
       xlab = "longtitude",ylab="latitude",xaxt="n",yaxt="n",
       family="newrom")

  maps::map('worldHires',xlim=c(min_lon-4.8,max_lon+4.8),
            ylim=c(min_lat-1.8,max_lat+1.8),fill=T,col="grey90",add=T,border="grey80")
  points(subarea_symb$long[-c(1:6)],subarea_symb$lat[-c(1:6)],pch=1,cex=0.5,lwd=0.3)
  points(sp.hab.info[[1]][,2],sp.hab.info[[1]][,1],pch=19,cex=0.5)
  polygon(sp.hab.info[[1]][sp.hab.info[[2]],2],
          sp.hab.info[[1]][sp.hab.info[[2]],1],col=rgb(0,0,0,alpha=0.3))
  text(pos=4,x=-9,y=49,label=sp_common_name,family="newrom",cex=1.4)
}

