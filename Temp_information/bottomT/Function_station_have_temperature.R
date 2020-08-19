library(maps)
library(mapdata)
library(ggplot2)

sta_hav_temp_fuc <- function(data){
  C.lat <- as.matrix(cut(data$Latitude..degrees_north.,breaks = lat_vec,include.lowest = TRUE,labels = seq(1, length(lat_vec)-1)))
  C.lon <- cut(data$Longitude..degrees_east.,breaks = lon_vec,include.lowest = TRUE,labels = seq(1, length(lon_vec)-1))
  C <- cbind(C.lat,C.lon)
  nna <- apply(C,1,function(x)any(is.na(x)==T))
  C <- C[-which(nna==TRUE),]
  
  temp_sta <- rep("a",length(C[,1]))
  
  for(i in 1:length(subarea_symb$subarea)){
    temp_sta[
      which(C[,1]==subarea_symb$lat_lab[i] & C[,2]==subarea_symb$long_lab[i])]=as.character(subarea_symb$subarea[i])
  }
  
  temp_sta <- as.matrix(temp_sta)
  temp_sta_tru <- temp_sta[which(temp_sta!="a"),]
  
  temp_hav_sta <- unique(temp_sta_tru)
  temp_hav_sta_loc <- subarea_symb[which(as.character(subarea_symb$subarea)%in%temp_hav_sta),]
  
  #visualization
  plot(c(min_lon,min_lat),xlim=c(min_lon-5,max_lon+5),
       ylim=c(min_lat-2,max_lat+2),type="n",xlab = "longtitude",ylab="latitude")
  
  maps::map('worldHires',xlim=c(min_lon-4.8,max_lon+4.8),
            ylim=c(min_lat-1.8,max_lat+1.8),fill=T,col="grey90",add=,border="grey80")
  points(subarea_symb$long[-c(1:6)],subarea_symb$lat[-c(1:6)],pch=1,cex=0.8,lwd=0.3)
  points(temp_hav_sta_loc$long,temp_hav_sta_loc$lat,pch=19,cex=0.8)
  
  return(temp_sta)
}