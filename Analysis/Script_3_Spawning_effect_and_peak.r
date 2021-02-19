
# import data

wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)

sp_TL.result <- read.csv("./SizeAggregTend_data/output/1_size_TL/all_TL_result_16_compiled.csv",stringsAsFactors = F,sep=",",header = T)
sp_TL.result$spwn <- as.factor(sp_TL.result$spwn)
sp_TL.result$quarter <- as.factor(sp_TL.result$quarter)
sp_TL.result$sp_ID <- as.factor(sp_TL.result$sp_ID)
sp_TL.result$mat_stage <- as.factor(sp_TL.result$mat_stage)



########################################################################
### Relationship fitted with considering spawning effect ###############
########################################################################

library("mgcv")
library("itsadug")

## GAMM result with Spawn by REML
fit.SPWN.tempCV.smooth.inter.Rslt <- gam(b~spwn+
                                    ti(std_T_cv,by=spwn,bs="ts")+
                                    ti(std_L,by=spwn,bs="ts")+
                                    ti(std_L,std_T_cv,by=spwn,bs="ts")+
                                    s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                                  data = sp_TL.result,
                                  method = "REML",
                                  family = Gamma(link = log)
                                  #select = F
)

summary(fit.SPWN.tempCV.smooth.inter.Rslt)

##################################################
# Bootstrapping

TL_BSdata <- read.csv("./SizeAggregTend_data/output/1_size_TL/resample_TL_bs_data.csv")
TL_BSdata <- TL_BSdata[,-1]

# bootstrap 999 times for model fitting and prediction
bs_fit_para_summary <- NULL
bs_fit_smth_summary <- data.frame(temp_cv_N=NA,temp_cv_Y=NA,
                                  size_N=NA,size_Y=NA,
                                  inter_N=NA,inter_Y=NA,
                                  re_slope=NA,re_intercept=NA)

bs_spawn_diff <- NULL
bs_pred_result_Y <- NULL
bs_pred_result_Y_temp <- NULL
bs_pred_result_N <- NULL
bs_pred_result_N_temp <- NULL

newdat <- expand.grid(std_L = seq(-0.69,0.75,length=200),
                        std_T_cv = 0,
                        spwn = c("Y","N"),
                        sp_ID = factor(c(1:9)),
                        dum=0)
newdat_Y <- expand.grid(std_L = seq(-0.69,0.75,length=200),
                        std_T_cv = 0,
                         spwn = c("Y"),
                         sp_ID = factor(c(1:9)),
                         dum=0)
newdat_N <- expand.grid(std_L = seq(-0.69,0.75,length=200),
                        std_T_cv = 0,
                         spwn = c("N"),
                         sp_ID = factor(c(1:9)),
                         dum=0)
newdat_Y_temp <- expand.grid(std_T_cv = seq(-0.80,0.92,length=200),
                                std_L = 0,
                                spwn = c("Y"),
                                sp_ID = factor(c(1:9)),
                                dum=0)
newdat_N_temp <- expand.grid(std_T_cv = seq(-0.80,0.92,length=200),
                             std_L = 0,
                             spwn = c("N"),
                             sp_ID = factor(c(1:9)),
                             dum=0)

for (bs in 1:999){
  bs_TL_result <- cbind(TL_BSdata[,bs],sp_TL.result[,9:20])
  colnames(bs_TL_result)[1] <- "b"
  bs_fit <- gam(b~spwn+
                  ti(std_T_cv,by=spwn,bs="ts")+
                  ti(std_L,by=spwn,bs="ts")+
                  ti(std_L,std_T_cv,by=spwn,bs="ts")+
                  s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                data = bs_TL_result,
                method = "REML",
                family = Gamma(link = log)
  )
  bs_fit_summary <- summary(bs_fit)
  bs_fit_para_summary = rbind(bs_fit_para_summary,bs_fit_summary$p.table[2,])
  bs_fit_smth_summary = rbind(bs_fit_smth_summary,bs_fit_summary$s.table[,4])
  
  # spawn different
  bs_pred_b <- predict(bs_fit, newdata = newdat, type="lpmatrix")
  c1 <- grepl('Y', colnames(bs_pred_b))
  c3 <- grepl('N', colnames(bs_pred_b))
  r1 <- with(newdat, spwn == 'Y')
  r3 <- with(newdat, spwn == 'N')
  X <- bs_pred_b[r1, ] - bs_pred_b[r3, ]
  X[, ! (c1 | c3)] <- 0
  X[, !grepl('^ti\\(', colnames(bs_pred_b))] <- 0
  spawn_diff <- X %*% coef(bs_fit)
  spawn_diff <- tapply(spawn_diff,newdat_Y$std_L,mean)
  bs_spawn_diff <- cbind(bs_spawn_diff,spawn_diff)
  
  
  # Spawn season prediction
  bs_size_pred_b_Y <- tapply(predict(bs_fit, newdata = newdat_Y, type="response"),newdat_Y$std_L,mean)
  bs_pred_result_Y <- cbind(bs_pred_result_Y,bs_size_pred_b_Y)
  
  bs_temp_pred_b_Y <- tapply(predict(bs_fit, newdata = newdat_Y_temp, type="response"),newdat_Y_temp$std_T,mean)
  bs_pred_result_Y_temp <- cbind(bs_pred_result_Y_temp,bs_temp_pred_b_Y)
  
  # Rest season prediction
  bs_size_pred_b_N <- tapply(predict(bs_fit, newdata = newdat_N, type="response"),newdat_N$std_L,mean)
  bs_pred_result_N <- cbind(bs_pred_result_N,bs_size_pred_b_N)
  
  bs_temp_pred_b_N <- tapply(predict(bs_fit, newdata = newdat_N_temp, type="response"),newdat_N_temp$std_T,mean)
  bs_pred_result_N_temp <- cbind(bs_pred_result_N_temp,bs_temp_pred_b_N)
  
  if(bs%%50==0){print(bs)}
}

bs_fit_smth_summary <- bs_fit_smth_summary[-1,]
write.csv(bs_fit_para_summary,"./SizeAggregTend_data/output/2_spwan_effect/SPWN_bs_fit_para_summary.csv")
write.csv(bs_fit_smth_summary,"./SizeAggregTend_data/output/2_spwan_effect/SPWN_bs_fit_smooth_summary.csv")

write.csv(bs_spawn_diff,"./SizeAggregTend_data/output/2_spwan_effect/bs_fit_spawn_diff.csv")
write.csv(bs_pred_result_Y,"./SizeAggregTend_data/output/2_spwan_effect/bs_fit_predicted_data_Y.csv")
write.csv(bs_pred_result_N,"./SizeAggregTend_data/output/2_spwan_effect/bs_fit_predicted_data_N.csv")
write.csv(bs_pred_result_Y_temp,"./SizeAggregTend_data/output/2_spwan_effect/bs_fit_predicted_data_Y_temp.csv")
write.csv(bs_pred_result_N_temp,"./SizeAggregTend_data/output/2_spwan_effect/bs_fit_predicted_data_N_temp.csv")


# original model prediction
# spawn different
pred_b <- predict(fit.SPWN.tempCV.smooth.inter.Rslt, newdata = newdat, type="lpmatrix")
c1 <- grepl('Y', colnames(pred_b))
c3 <- grepl('N', colnames(pred_b))
r1 <- with(newdat, spwn == 'Y')
r3 <- with(newdat, spwn == 'N')
X <- pred_b[r1, ] - pred_b[r3, ]
X[, ! (c1 | c3)] <- 0
X[, !grepl('^ti\\(', colnames(pred_b))] <- 0
spawn_diff <- X %*% coef(fit.SPWN.tempCV.smooth.inter.Rslt)
spawn_diff <- tapply(spawn_diff,newdat_Y$std_L,mean)


pred_b_Y <- tapply(predict(fit.SPWN.tempCV.smooth.inter.Rslt, newdata = newdat_Y, type="response"),
                   newdat_Y$std_L,mean)
pred_b_N <- tapply(predict(fit.SPWN.tempCV.smooth.inter.Rslt, newdata = newdat_N, type="response"),
                   newdat_N$std_L,mean)
pred_b_Y_temp <- tapply(predict(fit.SPWN.tempCV.smooth.inter.Rslt, newdata = newdat_Y_temp, type="response"),
                        newdat_Y_temp$std_T,mean)
pred_b_N_temp <- tapply(predict(fit.SPWN.tempCV.smooth.inter.Rslt, newdata = newdat_N_temp, type="response"),
                        newdat_N_temp$std_T,mean)

bs_spawn_diff <- cbind(bs_spawn_diff[,-1],spawn_diff)
bs_pred_result_Y <- cbind(bs_pred_result_Y[,-1],pred_b_Y)
bs_pred_result_N <- cbind(bs_pred_result_N[,-1],pred_b_N)
bs_pred_result_Y_temp <- cbind(bs_pred_result_Y_temp[,-1],pred_b_Y_temp)
bs_pred_result_N_temp <- cbind(bs_pred_result_N_temp[,-1],pred_b_N_temp)

library("coxed")
bca_SPWNdiff_95CI <- apply(bs_spawn_diff,1,bca)
size_Y_95CI = apply(bs_pred_result_Y,1,bca)
size_N_95CI = apply(bs_pred_result_N,1,bca)
temp_Y_95CI = apply(bs_pred_result_Y_temp,1,bca)
temp_N_95CI = apply(bs_pred_result_N_temp,1,bca)



# peak location
Max_b_Y <- apply(bs_pred_result_Y,2,max)
peak_locat_order_Y <- apply(bs_pred_result_Y,2,function(x)which(x==max(x)))
peak_locat_Y <- seq(-0.69,0.75,length=200)[peak_locat_order_Y]
bca(peak_locat_Y)
peak_locat_Y[length(peak_locat_Y)]

Max_b_N <- apply(bs_pred_result_N,2,max)
peak_locat_order_N <- apply(bs_pred_result_N,2,function(x)which(x==max(x)))
peak_locat_N <- seq(-0.69,0.75,length=200)[peak_locat_order_N]
bca(peak_locat_N)
peak_locat_N[length(peak_locat_N)]

#######################################################
### Plot difference between spawning and resting season 
jpeg(".SizeAggregTend_data/output/fig/Appendix/Fig_spwan_size_diff.jpeg", width=6, height=5, units = "in",res=600)

plot(unique(newdat$std_L),bs_spawn_diff[,1000],
     type="l",xlim = c(-0.7,0.8),ylim=c(-0.15,0.2),
     xlab="Standardized length",ylab="Difference between spawning and resting season",family=c("newrom"),bty="l")
polygon(c(rev(unique(newdat$std_L)), unique(newdat$std_L)), 
        c(rev(bca_SPWNdiff_95CI[2,]), bca_SPWNdiff_95CI[1,]), 
        col = rgb(50, 50, 50, 40, maxColorValue=255), border = NA)
abline(h=0)
abline(v=range(as.numeric(colnames(bca_SPWNdiff_95CI)[which(bca_SPWNdiff_95CI[1,]>0)])),lty=2)
abline(v=range(as.numeric(colnames(bca_SPWNdiff_95CI)[which(bca_SPWNdiff_95CI[2,]<0)])),lty=2)


dev.off()



