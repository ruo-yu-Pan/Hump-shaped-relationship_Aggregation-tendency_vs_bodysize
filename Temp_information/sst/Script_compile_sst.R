
wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)

ncpath = "./SizeAggregTend_data/raw_data/sst/"
ncfilesn = list.files( path = ncpath, pattern="*.nc")

source("./SizeAggregTend_code/Temp_information/sst/Function_month_mean_sst.R")


Q1sst <- NULL
Q3sst <- NULL

for(n in 1:25){
  nc_name = paste0(ncpath,ncfilesn[n])
  nc = nc_open(nc_name)
  yrsst = month_mean_sst(nc)
  Q1sst = cbind(Q1sst,yrsst[[1]][,2])
  Q3sst = cbind(Q3sst,yrsst[[2]][,2])
  print(n)
}

Q1sst_year_mean <- apply(Q1sst,1,mean,na.rm=T)
Q3sst_year_mean <- apply(Q3sst,1,mean,na.rm=T)

lat_lon_lab <- yrsst[[1]][,c(1,3)]
Q1sst_year_mean <- cbind(lat_lon_lab,Q1sst_year_mean)
Q3sst_year_mean <- cbind(lat_lon_lab,Q3sst_year_mean)

write.csv(Q1sst,"./SizeAggregTend_data/compiled/sst/Q1sst.csv")
write.csv(Q3sst,"./SizeAggregTend_data/compiled/sst/Q3sst.csv")
write.csv(Q1sst_year_mean,"./SizeAggregTend_data/compiled/sst/Q1sst_yr_mean.csv")
write.csv(Q3sst_year_mean,"./SizeAggregTend_data/compiled/sst/Q3sst_yr_mean.csv")
