
windowsFonts(newrom = windowsFont("Times New Roman"))

wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)

# temperature data import #
allsp_T <- read.csv("./SizeAggregTend_data/compiled/Temperature_info.csv")

# plot temperature data #

sp_common_name <- c("Herring","Cod","Haddock","Whiting","Plaice","Saithe",
                    "Mackerel","Sprat","Norwaypout")


# divid by quarter
jpeg("./SizeAggregTend_data/output/fig/Appendix/Fig_size_T_mean.jpeg", width=6, height=5, units = "in",res=300)
par(mfrow=c(3,3),oma=c(4,4,1,1))
for(i in 1:9){
  sp_T <- allsp_T$mean_T[which(allsp_T$species==i)]
  par(mar=c(2.5,3,1.5,1))
  plot(sp_T[1:16],main="",xlim=c(0,17),ylim=c(5,18),pch=19,family=c("newrom"),xlab="")
  points(sp_T[17:32],main="",xlim=c(0,17),pch=1,family=c("newrom"),xlab="")
  mtext(paste("(",letters[i],") ",sp_common_name[i]), side = 3, line = 0.5, outer =F,at =-5,adj=0,family=c("newrom"),cex = 0.9)
}
mtext("Size-specific habitat mean temperature",side=2,line=1,outer = T,family=c("newrom"))
mtext("Size class",side=1,line=1,outer=T,family=c("newrom"))
dev.off()

jpeg("./SizeAggregTend_data/output/fig/Appendix/Fig_size_T_cv.jpeg", width=6, height=5, units = "in",res=300)
par(mfrow=c(3,3),oma=c(4,4,1,1))
for(i in 1:9){
  sp_T_cv <- allsp_T$cv_T[which(allsp_T$species==i)]
  par(mar=c(2.5,3,1.5,1))
  plot(sp_T_cv[1:16],main="",xlim=c(0,17),ylim=c(2,24),pch=19,family=c("newrom"),xlab="")
  points(sp_T_cv[17:32],main="",xlim=c(0,17),yaxt="n",pch=1,family=c("newrom"),xlab="")
  mtext(paste("(",letters[i],") ",sp_common_name[i]), side = 3, line = 0.5, outer =F,at =-5,adj=0,family=c("newrom"),cex = 0.9)
}
mtext("Size-specific habitat temperature CV",side=2,line=1,outer = T,family=c("newrom"))
mtext("Size class",side=1,line=1,outer=T,family=c("newrom"))
dev.off()
