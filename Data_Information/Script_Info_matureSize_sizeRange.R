
# size information of each species and convert them into files

# mature length ################################################
mature.L <- c(253,697,335,202,266,554,250,115,190)
write.table(mature.L,"D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\mature_L.txt")

###############################################################
plai_size_selec <- as.data.frame(table(sp.2.Q1$LngtClas,sp.2.Q1$Year))
Lngth_selec <- plai_size_selec[which(plai_size_selec$Freq>5),]
Lngth_selec <- unique(as.numeric(as.character(Lngth_selec$Var1)))
Lngth_selec[2]
max(Lngth_selec) # 100-480

plai_size_selec <- as.data.frame(table(sp.2.Q3$LngtClas,sp.2.Q3$Year))
Lngth_selec <- plai_size_selec[which(plai_size_selec$Freq>5),]
Lngth_selec <- unique(as.numeric(as.character(Lngth_selec$Var1)))
Lngth_selec[2]
max(Lngth_selec) # 160-490

# size range  ##################################################
# species-specific bin 
sp_size_bin_8 <- c(35,118,64,54,84,30,15,20,
                   36.5,113,67,58,75,33,15,23)
sp_size_bin_16 <- c(17.2,58.8,31.9,26.9,23.8,41.9,15.1,7.6,9.4,
                   18.2,56.3,33.1,28.8,20.7,37.6,16.3,7.2,11.3)
sp_min_size <- c(70,90,90,60,100,190,160,40,70,
                 65,50,60,40,160,250,190,50,40)

# 8 classes
allsp_length_range.s <- data.frame(matrix(0,ncol=9,nrow=16))
for(i in 1:16){
  allsp_length_range.s[i,] <- seq(from=sp_min_size[i],length.out = 9,by=sp_size_bin_8[i])
}

allsp_length_range.df <- as.data.frame(allsp_length_range.s)
write.csv(allsp_length_range.s,"D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\allsp_length_range_8class.txt")

# 16 classes
allsp_length_range.16 <- data.frame(matrix(0,ncol=17,nrow=18))
for(i in 1:18){
  allsp_length_range.16[i,] <- seq(from=sp_min_size[i],length.out = 17,by=sp_size_bin_16[i])
}

write.table(allsp_length_range.16,"D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\allsp_length_range_16class.txt")

# mid size   #######################################################
# 8 classes
allsp_mid_size <- data.frame(matrix(0,ncol=8,nrow=16))
for(i in 1:16){
  allsp_mid_size[i,] <- seq(from=(sp_min_size[i]+(sp_size_bin_8[i]/2)),length.out = 8,by=sp_size_bin_8[i])
}
write.table(allsp_mid_size,"D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\allsp_length_mid_8class.txt")

# 16 classes
allsp_mid_size.16 <- data.frame(matrix(0,ncol=16,nrow=18))
for(i in 1:18){
  allsp_mid_size.16[i,] <- seq(from=(sp_min_size[i]+(sp_size_bin_16[i]/2)),length.out = 16,by=sp_size_bin_16[i])
}
write.table(allsp_mid_size.16,"D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\allsp_length_mid_16class.txt")

######################################################################
#length to biomass parameters ########################################
######################################################################

GW_par <- data.frame(
  GW_par1 = c(0.00603, 0.01030, 0.0157, 0.0093, 0.0238, 0.00381, 0.002112, 0.047),
  GW_par2 = c(3.0904, 3, 2.8268, 2.9456, 2.7374, 3.21, 3.4746, 3.125)
)

write.table(GW_par,"D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\length2weight.txt")

