
# size information of each species and convert them into files


setwd("D:/Ruo_data/2019_Paper_Publish/Publish/SizeAggregTend_data")
# mature length ################################################
mature.L <- c(253,697,335,202,266,554,250,115,190)
write.table(mature.L,"./compiled/mature_L.txt")



# size range  ##################################################
# species-specific bin 
sp_size_bin_16 <- c(17.2,58.8,31.9,26.9,23.8,41.9,15.1,7.6,9.4,
                   18.2,56.3,33.1,28.8,20.7,37.6,16.3,7.2,11.3)
sp_min_size <- c(70,90,90,60,100,190,160,40,70,
                 65,50,60,40,160,250,190,50,40)


allsp_length_range.16 <- data.frame(matrix(0,ncol=17,nrow=18))
for(i in 1:18){
  allsp_length_range.16[i,] <- seq(from=sp_min_size[i],length.out = 17,by=sp_size_bin_16[i])
}

write.table(allsp_length_range.16,"./compiled/allsp_length_range_16class.txt")

# mid size   #######################################################

allsp_mid_size.16 <- data.frame(matrix(0,ncol=16,nrow=18))
for(i in 1:18){
  allsp_mid_size.16[i,] <- seq(from=(sp_min_size[i]+(sp_size_bin_16[i]/2)),length.out = 16,by=sp_size_bin_16[i])
}
write.table(allsp_mid_size.16,"./compiled/allsp_length_mid_16class.txt")

