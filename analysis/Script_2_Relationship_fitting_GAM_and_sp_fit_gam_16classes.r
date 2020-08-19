
windowsFonts(newrom = windowsFont("Times New Roman"))


#####################################################################################
######################    arrange data    ###########################################
#####################################################################################

## import data
setwd("D:\\Ruo_data\\2017_Master\\Master_Research\\Data\\NorthSea\\Analysis")
sp_TL.result <- read.csv("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\all_TL_result_16class.csv",stringsAsFactors = F,sep=",",header = T)
sp_TL.result <-sp_TL.result[,-1]
sp.mid.L <- as.matrix(read.table("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\allsp_length_mid_16class.txt"))
mature.L <- as.matrix(read.table("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\mature_L.txt"))
sp_name <-c("Herring","Cod","Haddock","Whiting","Plaice","Saithe","Mackerel","Sprat","Norwaypout")


## calculate the standard length 
std.L <- NULL
for(i in 1:9){
  sp.1 <- (sp.mid.L[i,]-mature.L[i])/(max(sp.mid.L[i,])-min(sp.mid.L[i,]))
  sp.3 <- (sp.mid.L[i+9,]-mature.L[i])/(max(sp.mid.L[i+9,])-min(sp.mid.L[i+9,]))
  std.L <- c(std.L, sp.1, sp.3) 
} 
sp_TL.result$std_L <- std.L


## add species factor
sp_TL.result$sp_ID <- factor(rep(1:9,each=32))


## add quater factor
sp_TL.result$quarter <- as.factor(rep(c(rep("Q1",16),rep("Q3",16)),9))


## add spawning information
sp_TL.result$spwn <- as.factor(rep(c("Y","Y","Y","N","Y","N",
                                     "Y","N","Y","N","Y","N",
                                     "N","N","N","Y","Y","N"),each=16))

## add temperature data

# bottom
allsp_bT  <- read.csv("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\mean_bottom_Temperature.csv")
# surface
pelsp_sst <- read.csv("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\mean_surface_Temperature_pela.csv")

# compile bT and sst
allsp_T <- allsp_bT
allsp_T[c(1:32,193:256),2] <- pelsp_sst[,2]

sp_TL.result$temp <- allsp_T[,2]
sp_TL.result$std_T <- unlist(tapply(sp_TL.result$temp,
                                    list(sp_TL.result$sp_ID,sp_TL.result$quarter),
                                    function(x){(x-median(x))/(max(x)-min(x))}))
## add dummy variable
sp_TL.result$dum <- 1

## export sp_TL.result
write.csv(sp_TL.result,"D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\sp_TL.result_all_16classes.csv")




#####################################################################################
######################   Relationship fitting     ###################################
#####################################################################################

library("lme4")
library("mgcv")
library("itsadug")
library("ggplot2")
#library("rstanarm")
library("blme")
library("optimx")


### GLM ############################################################

## Quarter ##
fit.QART.lin <- glmer(b~std_T*std_L*quarter+(1+std_L|sp_ID),
                     data = sp_TL.result,
                     family = Gamma(link = log),
                     control = glmerControl(optimizer ='bobyqa' )
)
## Spawn ##
fit.SPWN.lin <- glmer(b~std_T*std_L*spwn+(1+std_L|sp_ID),
                      data = sp_TL.result,
                      family = Gamma(link = log),
                      control = glmerControl(optimizer ='bobyqa' )
)

### GAM ############################################################

# std_L smooth, temp linear
#fit.QART.temp.line.inter <- gam(b~std_T*quarter+
#                                  s(std_L,by=quarter,bs="ts")+
#                                  s(std_L,sp_ID,bs="re")+s(sp_ID,bs="re"),
#                                data = sp_TL.result,method = "ML",select = F,
#                                family = Gamma(link = log))
#fit.SPWN.temp.line.inter <- gam(b~std_T*spwn+
#                                  s(std_L,by=spwn,bs="ts")+
#                                  s(std_L,sp_ID,bs="re")+s(sp_ID,bs="re"),
#                                data = sp_TL.result,method = "ML",select = F,
#                                family = Gamma(link = log))
#
# std_L smooth, temp smooth , with smoothing interaction

fit.QART.temp.smooth.inter <- gam(b~quarter+
                                    ti(std_T,by=quarter,bs="ts")+
                                    ti(std_L,by=quarter,bs="ts")+
                                    ti(std_L,std_T,by=quarter,bs="ts")+
                                    s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                                  data = sp_TL.result,
                                  method = "ML",
                                  family = Gamma(link = log)
                                  #select = T
)
fit.SPWN.temp.smooth.inter <- gam(b~spwn+
                                    ti(std_T,by=spwn,bs="ts")+
                                    ti(std_L,by=spwn,bs="ts")+
                                    ti(std_L,std_T,by=spwn,bs="ts")+
                                    s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                                  data = sp_TL.result,
                                  method = "ML",
                                  family = Gamma(link = log)
                                  #select = F
)


# after use shrinkage method, we don't need to do the following part
# std_L smooth, temp smooth , no smoothing interaction #

#fit.QART.temp.smooth <- gam(b~quarter+
#                              s(std_T,by=quarter,bs="ts")+
#                              s(std_L,by=quarter,bs="ts")+
#                              s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
#                            data = sp_TL.result,method = "ML",#select = F,
#                            family = Gamma(link = log))
#fit.SPWN.temp.smooth <- gam(b~spwn+
#                              s(std_T,by=spwn,bs="ts")+
#                              s(std_L,by=spwn,bs="ts")+
#                              s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
#                            data = sp_TL.result,method = "ML",select = F,
#                            family = Gamma(link = log))


# no quarter or spawning #
fit.noQorSPW <- gam(b~s(std_L,bs="ts")+s(std_T,bs="ts")+
                  s(std_L,sp_ID,bs="re")+s(sp_ID,bs="re"),
                data = sp_TL.result,method = "ML",select = F,
                family = Gamma(link = log))


# one factor and no random effect #
fit.size.noRE <- gam(b~s(std_L),
                     data = sp_TL.result,method = "ML",select = F,
                     family = Gamma(link = log))
fit.temp.noRE <- gam(b~s(std_T),
                     data = sp_TL.result,method = "ML",select = F,
                     family = Gamma(link = log))


### Model selection##########################################################

AIC(fit.QART.lin,
    fit.QART.temp.smooth.inter,
    fit.SPWN.lin,
    fit.SPWN.temp.smooth.inter,
    fit.noQorSPW,
    fit.size.noRE,
    fit.temp.noRE
    )

### Result using REML #######################################
fit.QART.temp.smooth.inter_Rslt <- gam(b~quarter+
                                    ti(std_T,by=quarter,bs="ts")+
                                    ti(std_L,by=quarter,bs="ts")+
                                    ti(std_L,std_T,by=quarter,bs="ts")+
                                    s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                                  data = sp_TL.result,
                                  method = "REML",
                                  family = Gamma(link = log)
)


summary(fit.QART.temp.smooth.inter_Rslt)
plot(fit.QART.temp.smooth.inter_Rslt)


### Plotting #################################################

# plot partial effect #

jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\partial_effect.jpeg", width=6, height=5, units = "in",res=300)

par(mfrow=c(2,2),mar=c(4,2,2,2),oma=c(0,3,1,1))

# size effect
plot(fit.QART.temp.smooth.inter_Rslt,select=3,main="Q1",xlab="Standardized length",ylab="",family=c("newrom"))
abline(h=0,col="grey60")
title("(a) ",adj=0,family=c("newrom"))

plot(fit.QART.temp.smooth.inter_Rslt,select=4,main="Q3",xlab="Standardized length",ylab="",family=c("newrom"))
abline(h=0,col="grey60")
title("(b) ",adj=0,family=c("newrom"))

# temperature effect
plot(fit.QART.temp.smooth.inter_Rslt,select=1,main="",xlab="Standardized temperature",ylab="",family=c("newrom"))
abline(h=0,col="grey60")
title("(c)",adj=0,family=c("newrom"))

plot(fit.QART.temp.smooth.inter_Rslt,select=2,main="",xlab="Standardized temperature",ylab="",family=c("newrom"))
abline(h=0,col="grey60")
title("(d)",adj=0,family=c("newrom"))

mtext("Taylor's exponents",side = 2,line=1,family=c("newrom"),outer=T,cex=1.5)

dev.off()

# plot summed effect #

#plot_smooth(fit.QART.temp.smooth.inter_Rslt,
#            view = "std_T", cond = list(quarter="Q1",std_L=-0.0335), 
#            rm.ranef=T,transform = exp,
#            col = c("grey30"),xlim = c(-0.9,0.9),ylim = c(0.5,3.5),
#            xlab = "",ylab = "Taylor's exponents",family=c("newrom"),
#            hide.label = T)

#title("(a) Q1",adj=0,family=c("newrom"))
#points(b~std_T,data=sp_TL.result[which(sp_TL.result$quarter=="Q1"),],
#       pch=19,cex=0.8,xlim = c(-0.9,0.9),ylim = c(0.5,3.5),
#       col=alpha(viridis(9)[rep(1:9,each = 8)],0.4))#col="grey30",)
#abline(v=0,col="grey30",lty=2,lwd=2)

# size
jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\general_relationship_quarter.jpeg", width=5, height=7, units = "in",res=300)

par(mfrow=c(2,1),mar=c(2,4,3,2),oma=c(3,0,0,0),xpd=F)
plot_smooth(fit.QART.temp.smooth.inter_Rslt,
            view = "std_L", cond = list(quarter="Q1",std_T=0), 
            rm.ranef=T,transform = exp,
            col = c("grey30"),xlim = c(-0.9,0.9),ylim = c(0.5,3.5),
            xlab = "",ylab = "Taylor's exponents",family=c("newrom"),
            hide.label = T)

title("(a) Q1",adj=0,family=c("newrom"))
points(b~std_L,data=sp_TL.result[which(sp_TL.result$quarter=="Q1"),],pch=19,cex=0.8,col="grey30",xlim = c(-0.9,0.9),ylim = c(0.5,3.5))
abline(v=0,col="grey30",lty=2,lwd=2)

plot_smooth(fit.QART.temp.smooth.inter_Rslt,
            view = "std_L", cond = list(quarter="Q3",std_T=0), 
            rm.ranef=T,transform = exp,
            col = c("grey30"),xlim = c(-0.9,0.9),ylim = c(0.5,3.5),
            xlab = "",ylab = "Taylor's exponents",family=c("newrom"),
            hide.label = T)

title("(b) Q3",adj=0,family=c("newrom"))
points(b~std_L,data=sp_TL.result[which(sp_TL.result$quarter=="Q3"),],pch=19,cex=0.8,col="grey30",xlim = c(-0.9,0.9),ylim = c(0.5,3.5))
abline(v=0,col="grey30",lty=2,lwd=2)

mtext("Standardized length",side = 1,line=2.5,family=c("newrom"))
dev.off()

# temperature
jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\general_relationship_quarter.jpeg", width=5, height=7, units = "in",res=300)

par(mfrow=c(2,1),mar=c(2,4,3,2),oma=c(3,0,0,0),xpd=F)
plot_smooth(fit.QART.temp.smooth.inter_Rslt,
            view = "std_T", cond = list(quarter="Q1",std_L=0), 
            rm.ranef=T,transform = exp,
            col = c("grey30"),xlim = c(-0.9,0.9),ylim = c(0.5,3.5),
            xlab = "",ylab = "Taylor's exponents",family=c("newrom"),
            hide.label = T)

title("(a) Q1",adj=0,family=c("newrom"))
points(b~std_L,data=sp_TL.result[which(sp_TL.result$quarter=="Q1"),],pch=19,cex=0.8,col="grey30",xlim = c(-0.9,0.9),ylim = c(0.5,3.5))
abline(v=0,col="grey30",lty=2,lwd=2)

plot_smooth(fit.QART.temp.smooth.inter_Rslt,
            view = "std_T", cond = list(quarter="Q3",std_L=0), 
            rm.ranef=T,transform = exp,
            col = c("grey30"),xlim = c(-0.9,0.9),ylim = c(0.5,3.5),
            xlab = "",ylab = "Taylor's exponents",family=c("newrom"),
            hide.label = T)

title("(b) Q3",adj=0,family=c("newrom"))
points(b~std_L,data=sp_TL.result[which(sp_TL.result$quarter=="Q3"),],pch=19,cex=0.8,col="grey30",xlim = c(-0.9,0.9),ylim = c(0.5,3.5))
abline(v=0,col="grey30",lty=2,lwd=2)

mtext("Standardized length",side = 1,line=2.5,family=c("newrom"))
dev.off()



# plot for random intercept and slope for species factor #

jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\relationship_sp_random_byQ.jpeg", width=6.5, height=6.5, units = "in",res=300)

par(mfrow=c(3,3),mar=c(4,3,1,1),oma=c(3,3,2,0),xpd=F)
for(i in 1:9){
  # Q1
  plot_smooth(fit.QART.temp.smooth.inter_Rslt,
              view = "std_L",
              cond = list(sp_ID=factor(i),quarter="Q1",std_T=0), 
              rm.ranef=F,
              transform = exp,
              col = "grey40", lty=2,
              main = "",xlab = "",ylab = "",rug = F,
              family="newrom",
              xlim=c(max(sp_TL.result$std_L[(32*i-31):(32*i)])+0.15,min(sp_TL.result$std_L[(32*i-31):(32*i)])-0.15),
              ylim = c(0.5,3.5),
              hide.label = T)
  points(sp_TL.result$std_L[(32*i-31):(32*i-16)],sp_TL.result$b[(32*i-31):(32*i-16)],col="grey10",cex=0.8)
  
  # Q3
  plot_smooth(fit.QART.temp.smooth.inter_Rslt,
              view = "std_L",
              cond = list(sp_ID=factor(i),quarter="Q3",std_T=0), 
              rm.ranef=F,
              transform = exp,
              col = "grey10",main = "",xlab = "",ylab = "",rug = F,
              family="newrom",
              xlim=c(max(sp_TL.result$std_L[(32*i-31):(32*i)])+0.15,min(sp_TL.result$std_L[(32*i-31):(32*i)])-0.15),
              ylim = c(0.5,3.5),
              hide.label = T,add = T)
  points(sp_TL.result$std_L[(32*i-15):(32*i)],sp_TL.result$b[(32*i-15):(32*i)],pch=19,col="grey10",cex=0.8)
  
  title(paste("(",letters[i],") ",sp_name[i]),adj=0,family="newrom",cex.main=1.5)
  abline(v=0,col="grey20",lty=2)
  }

mtext("Taylor's expnents",outer = T,cex=1,side=2,family="newrom")
mtext("Standardized length",outer = T,cex=1,side=1,family="newrom")

dev.off()

#########################################################################
### Difference between quarters #########################################
#########################################################################

jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\quarter_diff.jpeg", width=6, height=5, units = "in",res=300)
par(mfrow=c(1,1),mar=c(4,4,2,2),oma=c(0,0,0,0),xpd=F)
plot_diff(fit.QART.temp.smooth.inter_Rslt, 
          view=c("std_L"), 
          comp=list(quarter=c("Q1", "Q3")),
          
          print.summary=T, hide.label = T,
          ylab = "Difference in Taylor's exponents",main = "",
          xlab = "Standardized length",
          col.diff = "black",
          family = c("newrom"))

dev.off()


