
windowsFonts(newrom = windowsFont("Times New Roman"))


#####################################################################################
######################    arrange data    ###########################################
#####################################################################################

## import data
wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)

sp_TL.result <- read.csv("./SizeAggregTend_data/output/1_size_TL/all_TL_result_16class.csv",stringsAsFactors = F,sep=",",header = T)
sp_TL.result <-sp_TL.result[,-1]
sp.mid.L <- as.matrix(read.table("./SizeAggregTend_data/compiled/allsp_length_mid_16class.txt"))
mature.L <- as.matrix(read.table("./SizeAggregTend_data/compiled/mature_L.txt"))
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
allsp_bT  <- read.csv("./SizeAggregTend_data/compiled/bottomT/mean_bottom_Temperature_trans.csv")
# surface
pelsp_sst <- read.csv("./SizeAggregTend_data/compiled/sst/size_mean_sst.csv")

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
write.csv(sp_TL.result,"./SizeAggregTend_data/output/1_size_TL/all_TL_result_16_compiled.csv")



#####################################################################################
######################   Relationship fitting     ###################################
#####################################################################################

library("lme4")
library("mgcv")
library("itsadug")
library("ggplot2")
#library("rstanarm")
#library("blme")
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


# plot summed effect #

# size
jpeg("./SizeAggregTend_data/output/fig/Fig3_general_relationship_quarter.jpeg", width=5, height=7, units = "in",res=300)

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



# plot for random intercept and slope for species factor #

#jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\relationship_sp_random_byQ.jpeg", width=6.5, height=6.5, units = "in",res=300)

#par(mfrow=c(3,3),mar=c(4,3,1,1),oma=c(3,3,2,0),xpd=F)
#for(i in 1:9){
#  # Q1
#  plot_smooth(fit.QART.temp.smooth.inter_Rslt,
#              view = "std_L",
#              cond = list(sp_ID=factor(i),quarter="Q1",std_T=0), 
#              rm.ranef=F,
#              transform = exp,
#              col = "grey40", lty=2,
#              main = "",xlab = "",ylab = "",rug = F,
#              family="newrom",
#              xlim=c(max(sp_TL.result$std_L[(32*i-31):(32*i)])+0.15,min(sp_TL.result$std_L[(32*i-31):(32*i)])-0.15),
#              ylim = c(0.5,3.5),
#              hide.label = T)
#  points(sp_TL.result$std_L[(32*i-31):(32*i-16)],sp_TL.result$b[(32*i-31):(32*i-16)],col="grey10",cex=0.8)
  
#  # Q3
#  plot_smooth(fit.QART.temp.smooth.inter_Rslt,
#              view = "std_L",
#              cond = list(sp_ID=factor(i),quarter="Q3",std_T=0), 
#              rm.ranef=F,
#              transform = exp,
#              col = "grey10",main = "",xlab = "",ylab = "",rug = F,
#              family="newrom",
#              xlim=c(max(sp_TL.result$std_L[(32*i-31):(32*i)])+0.15,min(sp_TL.result$std_L[(32*i-31):(32*i)])-0.15),
#              ylim = c(0.5,3.5),
#              hide.label = T,add = T)
#  points(sp_TL.result$std_L[(32*i-15):(32*i)],sp_TL.result$b[(32*i-15):(32*i)],pch=19,col="grey10",cex=0.8)
#  
#  title(paste("(",letters[i],") ",sp_name[i]),adj=0,family="newrom",cex.main=1.5)
#  abline(v=0,col="grey20",lty=2)
#  }

#mtext("Taylor's expnents",outer = T,cex=1,side=2,family="newrom")
#mtext("Standardized length",outer = T,cex=1,side=1,family="newrom")

#dev.off()


#########################################################################
### Difference between quarters #########################################
#########################################################################

jpeg("./SizeAggregTend_data/output/fig/FigS7_quarter_diff.jpeg", width=6, height=5, units = "in",res=300)
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




###########################################################################
#### predicted peak and do the bootstrapping for the sd of peak ###########
###########################################################################

### original ###

pdat.1 <- expand.grid(std_L = seq(-0.852,0.714,length=200),
                      std_T = 0,
                      quarter = c("Q1"),sp_ID = factor(c(1:9)),dum=0)

pred_b.1 <- predict(  fit.QART.temp.smooth.inter_Rslt, 
                      newdata = pdat.1, se.fit = T, type="response")
pred_b_main.1   <- cbind(pdat.1,pred_b.1)
pred_b_main2.1  <- tapply(pred_b_main.1$fit,pred_b_main.1$std_L,mean)
max_sp.resp_b.1 <- max(pred_b_main2.1)
peak_loc.1      <- unique(pred_b_main.1$std_L)[which(pred_b_main2.1==max_sp.resp_b.1)]


pdat.3 <- expand.grid(std_L = seq(-0.852,0.714,length=200),
                      std_T = 0,
                      quarter = c("Q3"),sp_ID = factor(c(1:9)),dum=0)

pred_b.3 <- predict(fit.QART.temp.smooth.inter_Rslt,
                    newdata = pdat.3, se.fit = T, type="response")
pred_b_main.3   <- cbind(pdat.3,pred_b.3)
pred_b_main2.3  <- tapply(pred_b_main.3$fit,pred_b_main.3$std_L,mean)
max_sp.resp_b.3 <- max(pred_b_main2.3)
peak_loc.3 <- unique(pred_b_main.3$std_L)[which(pred_b_main2.3==max_sp.resp_b.3)]


### bootstrapping ###

boot_peak_stdL.1 <- NULL
boot_peak_stdL.3 <- NULL


for(bt in 1:499){
  boot.sizeclass <- NULL
  set.seed(bt)
  for(k in 1:18){
    bbss <- sample((k*16-15):(k*16),10)
    boot.sizeclass <- rbind(boot.sizeclass,bbss)
  }
  
  boot.TL.result <- sp_TL.result[boot.sizeclass,]
  boot.fit.QART.temp <- gam(b~quarter+
                              ti(std_T,by=quarter,bs="ts")+
                              ti(std_L,by=quarter,bs="ts")+
                              ti(std_L,std_T,by=quarter,bs="ts")+
                              s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                            data = boot.TL.result,method = "REML")
  #plot_smooth(boot.fit.tot,view = "std_L", rm.ranef=T,main = "",xlim = c(-0.9,0.9),ylim = c(1,3.5),col = "#DAA520",xlab = "standardized length",ylab = "Taylor's exponents")
  #points(boot.TL.result$std_L,boot.TL.result$b,pch=19,cex=0.8,col="#32A1AD")
  #abline(v=0,col="#DAA520",lty=2,lwd=2)
  
  #Q1
  boot_pdat.1 <- expand.grid(std_L = seq(-0.852,0.714,length=200),
                             std_T = 0,
                             quarter = c("Q1"),
                             sp_ID = factor(c(1:9)),
                             dum=0)
  boot_pred_b.1 <- predict(boot.fit.QART.temp, newdata = boot_pdat.1,
                           se.fit = T, type="response")
  boot_main.1 <- c(max(boot.TL.result$std_L),min(boot.TL.result$std_L))
  boot_main_2.1 <- cbind(boot_pdat.1,boot_pred_b.1)
  bt_pred_b_main.1 <- boot_main_2.1[which(boot_main_2.1$std_L<boot_main.1[1] & 
                                            boot_main_2.1$std_L>boot_main.1[2]),]
  bt_pred_b_main2.1 <- tapply(bt_pred_b_main.1$fit,
                              bt_pred_b_main.1$std_L,mean)
  max_bt_sp.resp_b.1 <- max(bt_pred_b_main2.1)
  bt_peak_loc.1 <- unique(bt_pred_b_main.1$std_L)[which(bt_pred_b_main2.1==max_bt_sp.resp_b.1)]
  
  
  #Q3
  boot_pdat.3 <- expand.grid(std_L = seq(-0.852,0.714,length=200),
                             std_T = 0,
                             quarter = c("Q3"),
                             sp_ID = factor(c(1:9)),
                             dum=0)
  boot_pred_b.3 <- predict(boot.fit.QART.temp, newdata = boot_pdat.3,
                           se.fit = T, type="response")
  boot_main.3 <- c(max(boot.TL.result$std_L),min(boot.TL.result$std_L))
  boot_main_2.3 <- cbind(boot_pdat.3,boot_pred_b.3)
  bt_pred_b_main.3 <- boot_main_2.3[which(boot_main_2.3$std_L<boot_main.3[1] & 
                                            boot_main_2.3$std_L>boot_main.3[2]),]
  bt_pred_b_main2.3 <- tapply(bt_pred_b_main.3$fit,
                              bt_pred_b_main.3$std_L,mean)
  max_bt_sp.resp_b.3 <- max(bt_pred_b_main2.3)
  bt_peak_loc.3 <- unique(bt_pred_b_main.3$std_L)[which(bt_pred_b_main2.3==max_bt_sp.resp_b.3)]
  
  
  boot_peak_stdL.1 <- rbind(boot_peak_stdL.1,c(max_bt_sp.resp_b.1,bt_peak_loc.1))
  boot_peak_stdL.3 <- rbind(boot_peak_stdL.3,c(max_bt_sp.resp_b.3,bt_peak_loc.3))
  
  if(bt%%40==0){print(bt)}
}

write.csv(boot_peak_stdL.1,"./SizeAggregTend_data/output/2_size_TL_peak/bs_peak_stdL_Q1.csv")
write.csv(boot_peak_stdL.3,"./SizeAggregTend_data/output/2_size_TL_peak/bs_peak_stdL_Q3.csv")



### plot distribution of peak location ###
boot_peak_stdL.1 <- read.csv("./SizeAggregTend_data/output/2_size_TL_peak/bs_peak_stdL_Q1.csv")
boot_peak_stdL.3 <- read.csv("./SizeAggregTend_data/output/2_size_TL_peak/bs_peak_stdL_Q3.csv")

boot_peak_stdL.1 <- boot_peak_stdL.1[,2:3]
boot_peak_stdL.3 <- boot_peak_stdL.3[,2:3]

boot_peak_stdL.1 <- rbind(boot_peak_stdL.1,c(max_sp.resp_b.1,peak_loc.1))
boot_peak_stdL.3 <- rbind(boot_peak_stdL.3,c(max_sp.resp_b.3,peak_loc.3))


par(mfrow=c(1,2),mar=c(0,4,3,1),oma=c(4,0,0,0))
hist(boot_peak_stdL.1[,2],breaks=15,col="grey",
     family="newrom",main="",xlab="")
hist(boot_peak_stdL.3[,2],breaks=15,col="grey",xlab="",
     family="newrom",main="")


# remove extreme value
ext_val.1 <- which(boot_peak_stdL.1[,2]< -0.4)
boot_peak_stdL.1 <- boot_peak_stdL.1[-ext_val.1,]
boot_peak_stdL.3 <- boot_peak_stdL.3[-ext_val.1,]

ext_val.3 <- which(boot_peak_stdL.3[,2]>0.6)
boot_peak_stdL.1 <- boot_peak_stdL.1[-ext_val.3,]
boot_peak_stdL.3 <- boot_peak_stdL.3[-ext_val.3,]

library("coxed")
bca_95CI.1 <- bca(boot_peak_stdL.1[,2])
bca_95CI.3 <- bca(boot_peak_stdL.3[,2])


# plot

jpeg("./SizeAggregTend_data/output/fig/FigS1_bs_peak.jpeg", width=6, height=4, units = "in",res=300)

par(mfrow=c(1,2),mar=c(0,4,3,1),oma=c(4,0,0,0))
hist(boot_peak_stdL.1[,2],breaks=15,col="grey",
     family="newrom",main="",xlab="")
title("(a) Q1",family="newrom",adj=0)
abline(v=peak_loc.1,lwd=2)
abline(v=bca_95CI.1,lty=2)

hist(boot_peak_stdL.3[,2],breaks=15,col="grey",xlab="",
     family="newrom",main="")
title("(b) Q3",family="newrom",adj=0)
abline(v=peak_loc.3,lwd=2)
abline(v=bca_95CI.3,lty=2)

mtext("Bootstrapped peak of hump-shaped relationship",outer = T,cex=1,side=1,family="newrom",line = 2.5)
dev.off()

