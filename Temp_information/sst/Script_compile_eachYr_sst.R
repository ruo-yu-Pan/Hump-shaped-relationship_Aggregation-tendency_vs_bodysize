
setwd("D:\\Ruo_data\\2019_Paper_Publish\\Data\\environment_data\\Temperature\\surface")

ncpath = "D:\\Ruo_data\\2019_Paper_Publish\\Data\\environment_data\\Temperature\\surface\\"

ncfilesn = list.files( path = ncpath, pattern="*.nc")

source("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Code_for_bodysize_and_temperature_After20200310\\Surface_temperature\\Function_sst_data_pretreatment.R")



Q1sst <- NULL
Q3sst <- NULL

for(n in 1:25){
  nc_name = paste(ncpath,ncfilesn[n], sep = "")
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

write.csv(Q1sst,"Q1sst.csv")
write.csv(Q3sst,"Q3sst.csv")
write.csv(Q1sst_year_mean,"Q1sst_yr_mean.csv")
write.csv(Q3sst_year_mean,"Q3sst_yr_mean.csv")
