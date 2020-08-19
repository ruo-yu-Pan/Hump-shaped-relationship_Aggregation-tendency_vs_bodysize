
windowsFonts(newrom = windowsFont("Times New Roman"))

setwd("D:\\Ruo_data\\2017_Master\\Master_Research\\Data\\NorthSea\\Analysis")
northsea <- read.csv("CPUE per length per subarea_2016-09-21_1965to2016_q1q3.csv", header = T, sep = ",")
tot_sta <-as.data.frame(xtabs(~Subarea,data = northsea))

source("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Function_Habitat_information.R")
source("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Function_Size_distribution.R")
source("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Function_Habitat_information.R")


# make the subarea symbol become true latitude and longtitude
lat <-seq(49.75,61.75,0.5)
long <- seq(-3.5,12.5,1)

subarea_symb <- data.frame(subarea=tot_sta[,1],
                           lat = lat[as.integer(factor(substr(tot_sta[,1],1,2)))],
                           long = long[as.integer(factor(substr(tot_sta[,1],3,4)))])


# plot for size specific distribution of all species ####################

library(maps)
library(mapdata)
library(ggplot2)
library(sp)

min_lat =min(subarea_symb$lat)
max_lat =max(subarea_symb$lat)
min_lon =min(subarea_symb$long)
max_lon =max(subarea_symb$long)


sp_name <- c("Clupea harengus","Gadus morhua","Melanogrammus aeglefinus",
             "Merlangius merlangus","Pleuronectes platessa", 
             "Pollachius virens","Scomber scombrus",
             "Sprattus sprattus", "Trisopterus esmarkii")
sp_common_name <- c("Herring","Cod","Haddock","Whiting","Plaice","Saithe",
                    "Mackerel","Sprat","Norwaypout")
allsp_length_range.s <- as.matrix(read.table("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\allsp_length_range_16class.txt"))



# plot

for(i in 1:9){ 
  sp_s_name <- sp_name[i]
  sp_c_name <- sp_common_name[i]
  sp.2 <- northsea[which(northsea$Species== sp_s_name & northsea$Year>=1991 & northsea$Year<=2015),]
  sp.hab.info <- habitat_info_fuc(sp.2)
  
  sp.2.Q1 <- northsea[which(northsea$Species== sp_s_name & northsea$Year>=1991 & 
                              northsea$Year<=2015 & northsea$Quarter ==1),]
  sp.2.Q3 <- northsea[which(northsea$Species== sp_s_name & northsea$Year>=1991 & 
                              northsea$Year<=2015 & northsea$Quarter ==3),]
  
  
  sp_length_range.1 <- allsp_length_range.s[i,]
  sp_length_range.3 <- allsp_length_range.s[i+9,]
  
  #Q1
  fig.name1 <- paste("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\sp_size_distribution\\size_dist_",sp_c_name,"Q1.png",sep="")
  
  png(fig.name1, width=8, height=8, units = "in",res=300)
  par(mfrow=c(4,4),mar=c(0,0,0,0),oma=c(5,5,1,1))
  for(j in 1:16){
    if(j %in% c(1:12) & j%%4==1){
      size_distr_fuc(sp.2.Q1,sp_length_range.1[j],sp_length_range.1[j+1],sp.hab.info)
      text(x=-10,y=63,pos = 4,paste("(",letters[j],")"),family=c("newrom"))
      axis(side=2)
    }
    if(j %in% c(1:12) & j%%4!=1){
      size_distr_fuc(sp.2.Q1,sp_length_range.1[j],sp_length_range.1[j+1],sp.hab.info)
      text(x=-10,y=63,pos = 4,paste("(",letters[j],")"),family=c("newrom"))
    }
    if(j ==13){
      size_distr_fuc(sp.2.Q1,sp_length_range.1[j],sp_length_range.1[j+1],sp.hab.info)
      text(x=-10,y=63,pos = 4,paste("(",letters[j],")"),family=c("newrom"))
      axis(side=1)
      axis(side=2)
    }
    if(j %in%c(14:16)){
      size_distr_fuc(species=sp.2.Q1,y=sp_length_range.1[j],x=sp_length_range.1[j+1],sp.hab.info)
      text(x=-10,y=63,pos = 4,paste("(",letters[j],")"),family=c("newrom"))
      axis(side=1)
    }
  }
  mtext(side = 1,"longtitude",line = 2.5,outer = T,family=c("newrom"))
  mtext(side = 2,"latitude",line = 2.5,outer = T,family=c("newrom"))
  dev.off()
  
  # Q3
  fig.name2 <- paste("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\sp_size_distribution\\size_dist_",sp_c_name,"Q3.png",sep="")
  
  png(fig.name2, width=8, height=8, units = "in",res=300)
  
  par(mfrow=c(4,4),mar=c(0,0,0,0),oma=c(5,5,1,1))
  for(j in 1:16){
    if(j %in% c(1:12) & j%%4==1){
      size_distr_fuc(sp.2.Q3,sp_length_range.3[j],sp_length_range.3[j+1],sp.hab.info)
      text(x=-10,y=63,pos = 4,paste("(",letters[j],")"),family=c("newrom"))
      axis(side=2)
    }
    if(j %in% c(1:12) & j%%4!=1){
      size_distr_fuc(sp.2.Q3,sp_length_range.3[j],sp_length_range.3[j+1],sp.hab.info)
      text(x=-10,y=63,pos = 4,paste("(",letters[j],")"),family=c("newrom"))
    }
    if(j ==13){
      size_distr_fuc(sp.2.Q3,sp_length_range.3[j],sp_length_range.3[j+1],sp.hab.info)
      text(x=-10,y=63,pos = 4,paste("(",letters[j],")"),family=c("newrom"))
      axis(side=1)
      axis(side=2)
    }
    if(j %in% c(14:16)){
      size_distr_fuc(sp.2.Q3,sp_length_range.3[j],sp_length_range.3[j+1],sp.hab.info)
      text(x=-10,y=63,pos = 4,paste("(",letters[j],")"),family=c("newrom"))
      axis(side=1)
    }
  }
  mtext(side = 1,"longtitude",line = 2.5,outer = T,family=c("newrom"))
  mtext(side = 2,"latitude",line = 2.5,outer = T,family=c("newrom"))
  dev.off()

}

