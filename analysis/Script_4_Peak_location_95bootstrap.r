
###########################################################################
#### predicted peak and do the bootstrapping for the sd of peak ###########
###########################################################################

### original ###

pdat.1 <- expand.grid(std_L = seq(-0.852,0.788,length=200),
                      std_T = 0,
                      quarter = c("Q1"),sp_ID = factor(c(1:9)),dum=0)

pred_b.1 <- predict(  fit.QART.temp.smooth.inter_Rslt, 
                      newdata = pdat.1, se.fit = T, type="response")
pred_b_main.1   <- cbind(pdat.1,pred_b.1)
pred_b_main2.1  <- tapply(pred_b_main.1$fit,pred_b_main.1$std_L,mean)
max_sp.resp_b.1 <- max(pred_b_main2.1)
peak_loc.1      <- unique(pred_b_main.1$std_L)[which(pred_b_main2.1==max_sp.resp_b.1)]


pdat.3 <- expand.grid(std_L = seq(-0.852,0.788,length=200),
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
  boot_pdat.1 <- expand.grid(std_L = seq(-0.852,0.788,length=200),
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
  boot_pdat.3 <- expand.grid(std_L = seq(-0.852,0.788,length=200),
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

write.csv(boot_peak_stdL.1,"bs_peak_stdL_Q1.csv")
write.csv(boot_peak_stdL.3,"bs_peak_stdL_Q3.csv")



### plot distribution of peak location ###

boot_peak_stdL.1 <- read.csv("D:\\Ruo_data\\2017_Master\\Master_Research\\Data\\NorthSea\\Analysis\\bs_peak_stdL_Q1.csv")
boot_peak_stdL.3 <- read.csv("D:\\Ruo_data\\2017_Master\\Master_Research\\Data\\NorthSea\\Analysis\\bs_peak_stdL_Q3.csv")

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
ext_val.1 <- which(boot_peak_stdL.1[,2]>0.6)
boot_peak_stdL.1 <- boot_peak_stdL.1[-ext_val.1,]
boot_peak_stdL.3 <- boot_peak_stdL.3[-ext_val.1,]

ext_val.3 <- which(boot_peak_stdL.3[,2]>0.6)
boot_peak_stdL.1 <- boot_peak_stdL.1[-ext_val.3,]
boot_peak_stdL.3 <- boot_peak_stdL.3[-ext_val.3,]

bca(boot_peak_stdL.1)
bca(boot_peak_stdL.3)


# plot

jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\bs_peak.jpeg", width=6, height=4, units = "in",res=300)

par(mfrow=c(1,2),mar=c(0,4,3,1),oma=c(4,0,0,0))
hist(boot_peak_stdL.1[,2],breaks=15,col="grey",
     family="newrom",main="",xlab="")
title("(a) Q1",family="newrom",adj=0)
hist(boot_peak_stdL.3[,2],breaks=15,col="grey",xlab="",
     family="newrom",main="")
title("(b) Q3",family="newrom",adj=0)

mtext("Bootstrapped peak of hump-shaped relationship",outer = T,cex=1,side=1,family="newrom",line = 2.5)
dev.off()



# examine the difference of peak location

library("coxed")
boot_peak_stdL_diff_Q <- boot_peak_stdL.1[,2]-boot_peak_stdL.3[,2]
write.csv(boot_peak_stdL_diff_Q,"D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\boot_peak_quarter_diff.csv")


jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\bs_quarter_peak_diff.jpeg", width=6, height=4, units = "in",res=300)

par(mfrow=c(1,1),mar=c(4,4,1,1),oma=c(1,0,0,0))
hist(boot_peak_stdL_diff_Q,breaks=30,col="grey",xlab="Bootstrapped difference of peak location",family="newrom",main="")

dev.off()

t.test(boot_peak_stdL_diff_Q,alternative = "less",mu=0)

shapiro.test(boot_peak_stdL_diff_Q) # follow normal distribution? no
wilcox.test(boot_peak_stdL_diff_Q, mu = 0, alternative = "less")
