

bottom_T_fuc <- function(data=Temp.Q3
                                   ){
  
  data$C.lat <- cut(data$Latitude..degrees_north.,breaks = lat_vec,include.lowest = TRUE,labels = seq(1, length(lat_vec)-1))
  data$C.lon <- cut(data$Longitude..degrees_east.,breaks = lon_vec,include.lowest = TRUE,labels = seq(1, length(lon_vec)-1))
  data$temp_sta <- "a"
  
  for(i in 1:length(subarea_symb$subarea)){
    data$temp_sta [
      which(data$C.lat==subarea_symb$lat_lab[i] & data$C.lon==subarea_symb$long_lab[i])]=as.character(subarea_symb$subarea[i])
  }   
  
  data$year <- substr(data$yyyy.mm.ddThh.mm,1,4)
  
  sta_yr_data <-
    data %>% 
    group_by(year, temp_sta, Bot..Depth..m.) %>%
    mutate(max_pres=max(PRES..db.))
  
  bottom_T_data <- 
    sta_yr_data %>%
    filter(PRES..db.== max_pres)
  
  bottom_T <- tapply(bottom_T_data$TEMP..deg.C.,
                     list(bottom_T_data$temp_sta,bottom_T_data$year),
                     function(x)mean(x,na.rm=T))
  
  bottom_T <- bottom_T[-195,]
}

library(maps)
library(mapdata)
library(ggplot2)

#Q1
bT_Q1 <- bottom_T_fuc(Temp.Q1)
Year25_mean_bottomT.1 <- apply(bT_Q1,1,mean,na.rm=T)
Year25_mean_bottomT.1 <- Year25_mean_bottomT.1[-c(1:6)]
plot(c(min_lon,min_lat),xlim=c(min_lon-5,max_lon+5),
     ylim=c(min_lat-2,max_lat+2),type="n",xlab = "longtitude",ylab="latitude",xaxt="n",yaxt="n",family="newrom",main="bottom temperature in Q1")
maps::map('worldHires',xlim=c(min_lon-4.8,max_lon+4.8),
          ylim=c(min_lat-1.8,max_lat+1.8),fill=T,col="grey90",add=T,border="grey80")
points(subarea_symb$long[-c(1:6)],subarea_symb$lat[-c(1:6)],pch=19,cex=1,lwd=0.3,col=viridis(7)[ceiling(Year25_mean_bottomT.1-2)])


#Q3
bT_Q3 <- bottom_T_fuc(Temp.Q3)
Year25_mean_bottomT.3 <- apply(bT_Q3,1,mean,na.rm=T)
Year25_mean_bottomT.3 <- Year25_mean_bottomT.3[-c(1:6)]
plot(c(min_lon,min_lat),xlim=c(min_lon-5,max_lon+5),
     ylim=c(min_lat-2,max_lat+2),type="n",xlab = "longtitude",ylab="latitude",xaxt="n",yaxt="n",family="newrom",main="bottom temperature in Q3")

maps::map('worldHires',xlim=c(min_lon-4.8,max_lon+4.8),
          ylim=c(min_lat-1.8,max_lat+1.8),fill=T,col="grey90",add=T,border="grey80")
points(subarea_symb$long[-c(1:6)],subarea_symb$lat[-c(1:6)],pch=19,cex=1,lwd=0.3,col=viridis(13)[ceiling(Year25_mean_bottomT.3-6)])
