
windowsFonts(newrom = windowsFont("Times New Roman"))
# place difference

place_range <- data.frame(cen.lat=rep(NA,256),
                          cen.lon=rep(NA,256),
                          north.bound = rep(NA,256),
                          south.bound = rep(NA,256),
                          east.bound = rep(NA,256),
                          west.bound = rep(NA,256))

library("maps")
distribution_range_fuc <- function(species,y,x,sp.hab.info){
  fish_stage <- species[which(species$LngtClas < x & species$LngtClas >= y),]
  
  stage.each.year.sta <- tapply(fish_stage$CPUE_number_per_hour,
                                list(fish_stage$Year,fish_stage$Subarea),sum)
  
  if(length(sp.hab.info[[3]]$subarea)!=0){
    for(i in 1:length(sp.hab.info[[3]]$subarea)){
      stage.each.year.sta[,which(colnames(stage.each.year.sta)==sp.hab.info[[3]]$subarea[i])] <- NA
    }
  }
  
  # calculate mean density for each subarea from 1991 to 2015
  stage.each.year.sta.2 <- stage.each.year.sta
  stage.each.year.sta.2[which(is.na(stage.each.year.sta.2)==T)] <- 0
  stage.abun.mean <- apply(stage.each.year.sta.2,2,mean)
  
  # the subarea that has at least 3 data through whole station
  stage.habitat_pre<-apply(stage.each.year.sta,2,function(x){length(which(is.na(x)==F))})
  stage <- cbind(subarea_symb,stage.abun.mean)
  stage.habitat.hul <- stage[-which(stage.habitat_pre<=3),]
  
  #calculation for polygon
  hull <- chull(as.matrix(stage.habitat.hul[,-1]))
  hull <- c(hull, hull[1])
  poly_sta <- stage.habitat.hul[hull,]
  
  # record the distribution range of size classes
  sp_size_distribute <- NULL
  sp_size_distribute <- c(sum(stage.habitat.hul$lat*stage.habitat.hul$stage.abun.mean)/sum(stage.habitat.hul$stage.abun.mean),
                          sum(stage.habitat.hul$long*stage.habitat.hul$stage.abun.mean)/sum(stage.habitat.hul$stage.abun.mean),
                          max(poly_sta$lat)+0.5,
                          min(poly_sta$lat)-0.5,
                          max(poly_sta$long)+0.25,
                          min(poly_sta$long)-0.25) 
  return(sp_size_distribute)
}

# for each size class of each species
sp_name <- c("Clupea harengus","Gadus morhua","Melanogrammus aeglefinus",
             "Merlangius merlangus", "Pollachius virens","Scomber scombrus",
             "Sprattus sprattus", "Trisopterus esmarkii")
sp_common_name <- c("Herring","Cod","Haddock","Whiting","Saithe",
                    "Mackerel","Sprat","Norwaypout")

allsp_length_range.s <- as.matrix(read.table("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\allsp_length_range_16class.txt"))
source("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Function_Habitat_information.R")


for(i in 1:8){ 
  sp_s_name <- sp_name[i]
  sp_c_name <- sp_common_name[i]
  sp.2 <- northsea[which(northsea$Species== sp_s_name & northsea$Year>=1991 & northsea$Year<=2015),]
  sp.hab.info <- habitat_info_fuc(sp.2)
  
  sp.2.Q1 <- northsea[which(northsea$Species== sp_s_name & northsea$Year>=1991 & 
                              northsea$Year<=2015 & northsea$Quarter ==1),]
  sp.2.Q3 <- northsea[which(northsea$Species== sp_s_name & northsea$Year>=1991 & 
                              northsea$Year<=2015 & northsea$Quarter ==3),]
  
  
  sp_length_range.1 <- allsp_length_range.s[i,]
  sp_length_range.3 <- allsp_length_range.s[i+8,]
  
  #Q1
  for(s in 1:16){
    place_range[32*(i-1)+s,] <- distribution_range_fuc(sp.2.Q1,sp_length_range.1[s],sp_length_range.1[s+1],sp.hab.info)
  }  
  #Q3
  for(s in 1:16){
    place_range[32*i-16+s,] <- distribution_range_fuc(sp.2.Q3,sp_length_range.3[s],sp_length_range.3[s+1],sp.hab.info)
  }  
}  


# plot #################################################################
size_g_shift_col <- gray.colors(16, start = 0, end = 1, gamma=1)

# sp 1-4

jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\size_distribution_gravity1.jpeg", width=4, height=8, units = "in",res=300)

par(mfrow=c(4,2),mar=c(1,0,5,0),oma=c(1,2,0,2))
for(i in 0:3){
  plot(c(min_lon,min_lat),xlim=c(min_lon-5,max_lon+5),
       ylim=c(min_lat-2,max_lat+2),type="n",xlab = "",ylab="",family=c("newrom"))
  maps::map('world',xlim=c(min_lon-4.8,max_lon+4.8),
            ylim=c(min_lat-1.8,max_lat+1.8),fill=F,add=T,col = "grey40")
  points(place_range$cen.lon[(i*32+1):(i*32+16)],place_range$cen.lat[(i*32+1):(i*32+16)],col=rep(1,16),bg=size_g_shift_col,pch=21,cex=1.5)
  
  text(pos=4,x=-9,y=49,label="Q1",family="newrom")
  
  plot(c(min_lon,min_lat),xlim=c(min_lon-5,max_lon+5),
       ylim=c(min_lat-2,max_lat+2),type="n",xlab = "",ylab="",yaxt="n",family=c("newrom"))
  maps::map('world',xlim=c(min_lon-4.8,max_lon+4.8),
            ylim=c(min_lat-1.8,max_lat+1.8),fill=F,add=T,col = "grey40")
  points(place_range$cen.lon[(i*32+17):(i*32+32)],place_range$cen.lat[(i*32+17):(i*32+32)],col=rep(1,16),bg=size_g_shift_col,pch=21,cex=1.5)
  text(pos=4,x=-9,y=49,label="Q3",family="newrom")
}

dev.off()  

# sp 4-8
jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\size_distribution_gravity2.jpeg", width=4, height=8, units = "in",res=300)

par(mfrow=c(4,2),mar=c(1,0,5,0),oma=c(1,2,0,2))
for(i in 4:7){
  plot(c(min_lon,min_lat),xlim=c(min_lon-5,max_lon+5),
       ylim=c(min_lat-2,max_lat+2),type="n",xlab = "",ylab="",family=c("newrom"))
  maps::map('world',xlim=c(min_lon-4.8,max_lon+4.8),
            ylim=c(min_lat-1.8,max_lat+1.8),fill=F,add=T,col = "grey40")
  points(place_range$cen.lon[(i*32+1):(i*32+16)],place_range$cen.lat[(i*32+1):(i*32+16)],col=rep(1,16),bg=size_g_shift_col,pch=21,cex=1.5)
  
  text(pos=4,x=-9,y=49,label="Q1",family="newrom")
  
  plot(c(min_lon,min_lat),xlim=c(min_lon-5,max_lon+5),
       ylim=c(min_lat-2,max_lat+2),type="n",xlab = "",ylab="",yaxt="n",family=c("newrom"))
  maps::map('world',xlim=c(min_lon-4.8,max_lon+4.8),
            ylim=c(min_lat-1.8,max_lat+1.8),fill=F,add=T,col = "grey40")
  points(place_range$cen.lon[(i*32+17):(i*32+32)],place_range$cen.lat[(i*32+17):(i*32+32)],col=rep(1,16),bg=size_g_shift_col,pch=21,cex=1.5)
  text(pos=4,x=-9,y=49,label="Q3",family="newrom")
  
}
dev.off()

jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\size_distribution_gravity_colorbar.jpeg", width=4.5, height=6, units = "in",res=300)
par(mfrow=c(1,1),mar=c(1,0,1,0))
plot(1,1,ylim=c(0,50),family=c("newrom"))
legend("topleft",legend=c(1:16),
       pch=21,pt.bg=size_g_shift_col,col = rep(1,16),text.font = c("newrom"))
dev.off()      

