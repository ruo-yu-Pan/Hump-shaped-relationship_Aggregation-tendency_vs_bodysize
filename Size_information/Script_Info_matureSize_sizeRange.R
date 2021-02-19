
# 20210115 change maturity data
# size information of each species and convert them into files

wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)
# mature length ################################################
# data from Tu et al.'s study
mature.L <- c(230,540,270,210,220,490,260,120,140)
write.table(mature.L,"./SizeAggregTend_data/compiled/NEW_mature_L.txt")



# size range  ##################################################
# species-specific bin 
sp_size_bin_16 <- c(17.2,55.1,29.4,26.9,24.4,30.1,16.3,7.6,9.4,
                   18.5,53.8,30.7,28.2,23.2,34.4,18.8,6.9,11.3)
sp_min_size <- c(70,90,90,60,60,290,160,40,70,
                 60,50,60,40,120,250,160,50,40)


allsp_length_range.16 <- data.frame(matrix(0,ncol=17,nrow=18))
for(i in 1:18){
  allsp_length_range.16[i,] <- seq(from=sp_min_size[i],length.out = 17,by=sp_size_bin_16[i])
}

write.table(allsp_length_range.16,"./SizeAggregTend_data/compiled/allsp_length_range_16class.txt")


# mid size   #######################################################
allsp_mid_size.16 <- data.frame(matrix(0,ncol=16,nrow=18))
for(i in 1:18){
  allsp_mid_size.16[i,] <- seq(from=(sp_min_size[i]+(sp_size_bin_16[i]/2)),length.out = 16,by=sp_size_bin_16[i])
}
write.table(allsp_mid_size.16,"./SizeAggregTend_data/compiled/allsp_length_mid_16class.txt")

