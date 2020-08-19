
# temperature data import #

# bottom
allsp_bT <- read.csv("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\mean_bottom_Temperature.csv")
# surface
pelsp_sst<- read.csv("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\mean_surface_Temperature_pela.csv")

# compile bT and sst
allsp_T <- allsp_bT
allsp_T[c(1:32,193:256),2] <- pelsp_sst[,2]

allsp_T$sp_ID <- rep(c(1:9),each=32)
allsp_T$quarter <- rep(c(rep("Q1",16),rep("Q3",16)),9)




# plot temperature data #

sp_common_name <- c("Herring","Cod","Haddock","Whiting","Plaice","Saithe",
                    "Mackerel","Sprat","Norwaypout")



jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\size_mean_T_1.jpeg", width=3, height=6.5, units = "in",res=300)
par(mfrow=c(5,2))
for(i in c(1,3,5,7,9)){
  sp_T <- allsp_T[which(allsp_T$sp_ID==i),]
  par(mar=c(2.5,2,1.5,0))
  plot(sp_T[1:16,2],main="",xlim=c(0,17),ylim=c(2,24),pch=19,family=c("newrom"),xlab="")
  title(paste("(",letters[i],") ",sp_common_name[i]),adj=0,family=c("newrom"))
  text(0,22,label="Q1",adj=0,family=c("newrom"))
  
  par(mar=c(2.5,0,1.5,1))
  plot(sp_T[17:32,2],main="",xlim=c(0,17),ylim=c(2,24),yaxt="n",pch=19,family=c("newrom"),xlab="")
  text(0,22,label="Q3",adj=0,family=c("newrom"))
}
dev.off()

jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\size_mean_T_2.jpeg", width=3, height=6.5, units = "in",res=300)
par(mfrow=c(5,2))
for(i in c(2,4,6,8)){
  sp_T <- allsp_T[which(allsp_T$sp_ID==i),]
  par(mar=c(2.5,2,1.5,0))
  plot(sp_T[1:16,2],main="",xlim=c(0,17),ylim=c(2,24),pch=19,family=c("newrom"),xlab="")
  title(paste("(",letters[i],") ",sp_common_name[i]),adj=0,family=c("newrom"))
  text(0,22,label="Q1",adj=0,family=c("newrom"))
  
  par(mar=c(2.5,0,1.5,1))
  plot(sp_T[17:32,2],main="",xlim=c(0,17),ylim=c(2,24),yaxt="n",pch=19,family=c("newrom"),xlab="")
  text(0,22,label="Q3",adj=0,family=c("newrom"))
}
dev.off()
