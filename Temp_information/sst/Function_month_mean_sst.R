library(ncdf4)
library(tidyr)


  # load data information

month_mean_sst <- function(yr_data){
  sst = ncvar_get(yr_data)  # get variable in the data
  lon = yr_data$dim$lon$vals  # longitude (starts from 0.5E with interval 1)
  lat = yr_data$dim$lat$vals  # latitude (starts from 89.5N with interval 1)
  time = yr_data$dim$time$vals  # 
  
  # restrict SST in February and August
  time.date = as.Date(time, origin = '1800-1-1')  #convert days to yyyy-mm-dd
  time.date = substr(time.date, start=6, stop=7)  #extract month
  sst = sst[, , which(time.date %in% c("02","08"))]
  
  lenghQ1 = length(which(time.date %in% c("02")))
  n_t = length(sst[1,1,])
  
  # restrict SST in the North Sea (IBTS: 5W-13E, 49.5N-61.5N)
  # since the grid size is different, here we limit the satellite data in
  # 3.5W-12.5E, 49.5N-61.5N
  NS_lat = which(lat<62&lat>49.5)
  NS_lon = which(lon<13|lon>355.9)
  
  sst = sst[NS_lon, NS_lat, ]
  
  ### Statistics of SST data
  month_mean = apply(sst,3,"c")
  
  Q1_mean = as.matrix(apply(month_mean[,1:lenghQ1]      ,1,mean,na.rm=T))
  Q3_mean = as.matrix(apply(month_mean[,(lenghQ1+1):n_t],1,mean,na.rm=T))
  
  latlon_lab = cbind(rep(lat[NS_lat],each=68),rep(lon[NS_lon],50))
  Q1_mean = as.data.frame(cbind(latlon_lab,Q1_mean))
  Q3_mean = as.data.frame(cbind(latlon_lab,Q3_mean))
  
  colnames(Q1_mean) = c("lat","lon","sst")
  colnames(Q3_mean) = c("lat","lon","sst")
  
  Q1_mean_wide = spread(Q1_mean, lon, sst)
  Q3_mean_wide = spread(Q3_mean, lon, sst)
  
  
  #Q1
  Q1_mean_wide = Q1_mean_wide[,-1]
  Q1_mean_fitNS1 = matrix(NA,nrow = 25,ncol=68)
  
  for(la in 0:24){
    Q1.lat.deg5 = apply(Q1_mean_wide[(la*2+1):(la*2+2),],2,mean,na.rm=T)
    Q1_mean_fitNS1[la+1,] = Q1.lat.deg5
  }
  
  Q1_mean_fitNS1 [which(is.nan(Q1_mean_fitNS1))] = NA
  Q1_mean_fitNS2 = matrix(NA,nrow = 25,ncol=17)
  for(lo in 0:16){
    Q1.lon.deg1 = apply(Q1_mean_fitNS1[,(lo*4+1):(lo*4+4)],1,mean,na.rm=T)
    Q1_mean_fitNS2[,lo+1] = Q1.lon.deg1
  }
  
  #Q3
  Q3_mean_wide = Q3_mean_wide[,-1]
  Q3_mean_fitNS1 = matrix(NA,nrow = 25,ncol=68)
  
  for(la in 0:24){
    Q3.lat.deg5 = apply(Q3_mean_wide[(la*2+1):(la*2+2),],2,mean,na.rm=T)
    Q3_mean_fitNS1[la+1,] = Q3.lat.deg5
  }
  
  Q3_mean_fitNS1 [which(is.nan(Q1_mean_fitNS1))] = NA
  Q3_mean_fitNS2 = matrix(NA,nrow = 25,ncol=17)
  for(lo in 0:16){
    Q3.lon.deg1 = apply(Q3_mean_fitNS1[,(lo*4+1):(lo*4+4)],1,mean,na.rm=T)
    Q3_mean_fitNS2[,lo+1] = Q3.lon.deg1
  }
  
  lat = seq(49.75,61.75,0.5)
  long = c(seq(0.5,12.5,1),seq(-3.5,-0.5,1))
  
  rownames(Q1_mean_fitNS2) = lat
  rownames(Q3_mean_fitNS2) = lat
  colnames(Q1_mean_fitNS2) = long
  colnames(Q3_mean_fitNS2) = long
  
  Q1_mean_fitNS2 = as.data.frame(Q1_mean_fitNS2)
  Q1_mean_fitNS2.long = gather(Q1_mean_fitNS2,lon,sst)
  Q1_mean_fitNS2.long$lat = rep(lat,17)
  Q3_mean_fitNS2 = as.data.frame(Q3_mean_fitNS2)
  Q3_mean_fitNS2.long = gather(Q3_mean_fitNS2,lon,sst)
  Q3_mean_fitNS2.long$lat = rep(lat,17)
  
  mean_sst_q = list(Q1_mean_fitNS2.long,Q3_mean_fitNS2.long)
  return(mean_sst_q)
}


