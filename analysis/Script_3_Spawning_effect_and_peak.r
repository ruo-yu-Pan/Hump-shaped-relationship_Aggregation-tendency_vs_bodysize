
windowsFonts(newrom = windowsFont("Times New Roman"))

# Bootstrapping peak in spawning season and non-spawning season


########################################################################
### Relationship fitted with considering spawning effect ###############
########################################################################


sp_TL.result <- read.csv("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\sp_TL.result_all_16classes.csv",stringsAsFactors = F,sep=",",header = T)
sp_TL.result$spwn <- as.factor(sp_TL.result$spwn)
sp_TL.result$quarter <- as.factor(sp_TL.result$quarter)
sp_TL.result$sp_ID <- as.factor(sp_TL.result$sp_ID)
#sp_TL.result_noherr <- sp_TL.result[-c(1:32),]
#sp_TL.result_noherr$sp_ID <- factor(rep(c(1:7),each=32))

library("mgcv")
library("itsadug")

## GAMM result with Spawn by REML
fit.SPWN.temp.smooth.inter.Rslt <- gam(b~spwn+
                                    ti(std_T,by=spwn,bs="ts")+
                                    ti(std_L,by=spwn,bs="ts")+
                                    ti(std_L,std_T,by=spwn,bs="ts")+
                                    s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                                  data = sp_TL.result,
                                  method = "REML",
                                  family = Gamma(link = log)
                                  #select = F
)

summary(fit.SPWN.temp.smooth.inter.Rslt)
#########################################################################
### Difference between in and out of spawning season ####################
#########################################################################

jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\spawn_diff.jpeg", width=6, height=5, units = "in",res=300)
par(mfrow=c(1,1),mar=c(4,4,2,2),oma=c(0,0,0,0),xpd=F)
plot_diff(fit.SPWN.temp.smooth.inter.Rslt, 
          view=c("std_L"), 
          comp=list(spwn=c("Y", "N")),
          
          print.summary=T, hide.label = T,
          ylab = "Difference in Taylor's exponents",main = "",
          xlab = "Standardized length",
          col.diff = "black",
          family = c("newrom"))

dev.off()



################################################################
####### hump-shape peak ########################################
################################################################


# original

pdat.spwn0 <- expand.grid(std_L = seq(-0.852,0.788,length=200),
                      std_T = 0,
                      spwn = "N",
                      sp_ID = factor(c(1:9)),
                      dum=0)

pred_b.spwn0 <- predict(fit.SPWN.temp.smooth.inter.Rslt, 
                        newdata = pdat.spwn0, 
                        se.fit = T, type="response")
pred_b_main.spwn0   <- cbind(pdat.spwn0,pred_b.spwn0)
pred_b_main2.spwn0  <- tapply(pred_b_main.spwn0$fit,pred_b_main.spwn0$std_L,mean)
max_sp.resp_b.spwn0 <- max(pred_b_main2.spwn0)
peak_loc.spwn0      <- unique(pred_b_main.spwn0$std_L)[which(pred_b_main2.spwn0==max_sp.resp_b.spwn0)]


pdat.spwn1 <- expand.grid(std_L = seq(-0.852,0.788,length=200),
                      std_T = 0,
                      spwn = "Y",
                      sp_ID = factor(c(1:9)),
                      dum=0)

pred_b.spwn1 <- predict(fit.SPWN.temp.smooth.inter.Rslt, 
                        newdata = pdat.spwn1, 
                        se.fit = T, type="response")
pred_b_main.spwn1   <- cbind(pdat.spwn1,pred_b.spwn1)
pred_b_main2.spwn1  <- tapply(pred_b_main.spwn1$fit,pred_b_main.spwn1$std_L,mean)
max_sp.resp_b.spwn1 <- max(pred_b_main2.spwn1)
peak_loc.spwn1      <- unique(pred_b_main.spwn1$std_L)[which(pred_b_main2.spwn1==max_sp.resp_b.spwn1)]


# bootstrapping

boot_peak_stdL.spwn0 <- NULL
boot_peak_stdL.spwn1 <- NULL

for(bt in 1:499){
  boot.sizeclass <- NULL
  set.seed(1000+bt)
  for(k in 1:18){
    bbss <- sample((k*16-15):(k*16),10)
    boot.sizeclass <- rbind(boot.sizeclass,bbss)
  }
  
  boot.TL.result <- sp_TL.result[boot.sizeclass,]
  boot.fit.SPWN.temp <- gam(b~spwn+
                              ti(std_T,by=spwn,bs="ts")+
                              ti(std_L,by=spwn,bs="ts")+
                              ti(std_L,std_T,by=spwn,bs="ts")+
                              s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                            data = boot.TL.result,method = "REML")
  
  #Q1
  boot_pdat.spwn0 <- expand.grid(std_L = seq(-0.852,0.788,length=200),
                             std_T = 0,
                             spwn = "N",
                             sp_ID = factor(c(1:9)),
                             dum=0)
  boot_pred_b.spwn0 <- predict(boot.fit.SPWN.temp, 
                               newdata = boot_pdat.spwn0, 
                               se.fit = T, type="response")  
  boot_size_range.spwn0  <- c(max(boot.TL.result$std_L),min(boot.TL.result$std_L))
  boot_main_2.spwn0      <- cbind(boot_pdat.spwn0,boot_pred_b.spwn0)
  bt_pred_b_main.spwn0   <- boot_main_2.spwn0[which(boot_main_2.spwn0$std_L<boot_size_range.spwn0[1] & 
                                                      boot_main_2.spwn0$std_L>boot_size_range.spwn0[2]),]
  bt_pred_b_main2.spwn0  <- tapply(bt_pred_b_main.spwn0$fit,
                                   bt_pred_b_main.spwn0$std_L,mean)
  max_bt_sp.resp_b.spwn0 <- max(bt_pred_b_main2.spwn0)
  bt_peak_loc.spwn0      <- unique(bt_pred_b_main.spwn0$std_L)[which(bt_pred_b_main2.spwn0==max_bt_sp.resp_b.spwn0)]
  
  #Q3
  boot_pdat.spwn1 <- expand.grid(std_L = seq(-0.852,0.788,length=200),
                                 std_T = 0,
                                 spwn = "Y",
                                 sp_ID = factor(c(1:9)),
                                 dum=0)
  boot_pred_b.spwn1 <- predict(boot.fit.SPWN.temp, 
                               newdata = boot_pdat.spwn1, 
                               se.fit = T, type="response")  
  boot_size_range.spwn1  <- c(max(boot.TL.result$std_L),min(boot.TL.result$std_L))
  boot_main_2.spwn1      <- cbind(boot_pdat.spwn1,boot_pred_b.spwn1)
  bt_pred_b_main.spwn1   <- boot_main_2.spwn1[which(boot_main_2.spwn1$std_L<boot_size_range.spwn1[1] & 
                                                      boot_main_2.spwn1$std_L>boot_size_range.spwn1[2]),]
  bt_pred_b_main2.spwn1  <- tapply(bt_pred_b_main.spwn1$fit,
                                   bt_pred_b_main.spwn1$std_L,mean)
  max_bt_sp.resp_b.spwn1 <- max(bt_pred_b_main2.spwn1)
  bt_peak_loc.spwn1      <- unique(bt_pred_b_main.spwn1$std_L)[which(bt_pred_b_main2.spwn1==max_bt_sp.resp_b.spwn1)]
  
  
  boot_peak_stdL.spwn0 <- rbind(boot_peak_stdL.spwn0,c(max_bt_sp.resp_b.spwn0,bt_peak_loc.spwn0))
  boot_peak_stdL.spwn1 <- rbind(boot_peak_stdL.spwn1,c(max_bt_sp.resp_b.spwn1,bt_peak_loc.spwn1))

  if(bt%%40==0){print(bt)}
}

#write.csv(boot_peak_stdL.spwn1,"D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\boot_peak_stdL_spawnY.csv")
#write.csv(boot_peak_stdL.spwn0,"D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\boot_peak_stdL_spawnN.csv")

boot_peak_stdL.spwn0 <- rbind(boot_peak_stdL.spwn0,c(max_sp.resp_b.spwn0,peak_loc.spwn0))
boot_peak_stdL.spwn1 <- rbind(boot_peak_stdL.spwn1,c(max_sp.resp_b.spwn1,peak_loc.spwn1))


### plot distribution of peak location ###

par(mfrow=c(1,2),mar=c(0,4,3,1),oma=c(4,0,0,0))
hist(boot_peak_stdL.spwn0[,2],breaks=15,col="grey",
     family="newrom",main="",xlab="")
hist(boot_peak_stdL.spwn1[,2],breaks=15,col="grey",xlab="",
     family="newrom",main="")

boot_peak_stdL_diff_SPWN <- boot_peak_stdL.spwn1[,2]-boot_peak_stdL.spwn0[,2]
write.csv(boot_peak_stdL_diff_SPWN,"D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\boot_peak_spawn_diff.csv")

par(mfrow=c(1,1),mar=c(4,4,1,1),oma=c(1,0,0,0))
jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\bs_spawning_peak_diff.jpeg", width=6, height=4, units = "in",res=300)
hist(boot_peak_stdL_diff_SPWN,breaks=20,col="grey",xlab="Bootstrapped difference of peak location",family="newrom",main="")
dev.off()

peak_loc.spwn1 -peak_loc.spwn0
boot_peak_stdL_diff_SPWN <- sort(boot_peak_stdL_diff_SPWN)
(boot_peak_stdL_diff_SPWN[487]+boot_peak_stdL_diff_SPWN[488])/2
(boot_peak_stdL_diff_SPWN[12]+boot_peak_stdL_diff_SPWN[13])/2


shapiro.test(boot_peak_stdL_diff_SPWN) #not follow normal distribution
wilcox.test(boot_peak_stdL_diff_SPWN, mu = 0, alternative = "two.sided")

qqnorm(boot_peak_stdL_diff_SPWN)
qqline(boot_peak_stdL_diff_SPWN)



#
#b_diff_spawn <- sp_TL.result$b[which(sp_TL.result$spwn==1)[-c(1:32)]]-sp_TL.result$b[which(sp_TL.result$spwn==0)]
#qqnorm(b_diff_spawn)
#qqline(b_diff_spawn)
#shapiro.test(b_diff_spawn)
#t.test(b_diff_spawn,mu=0)
