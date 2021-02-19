

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

# add information #
## calculate the standard length 
std.L <- NULL
for(i in 1:9){
  sp.1 <- (sp.mid.L[i,]-mature.L[i,1])/(max(sp.mid.L[i,])-min(sp.mid.L[i,]))
  sp.3 <- (sp.mid.L[i+9,]-mature.L[i,1])/(max(sp.mid.L[i+9,])-min(sp.mid.L[i+9,]))
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
allsp_T  <- read.csv("./SizeAggregTend_data/compiled/Temperature_info.csv")
colnames(allsp_T)[2:3] <- c("mean_T","cv_T")

sp_TL.result <- cbind(sp_TL.result,allsp_T[,2:3])
sp_TL.result$std_T <- unlist(tapply(sp_TL.result$mean_T,
                                    list(sp_TL.result$sp_ID,sp_TL.result$quarter),
                                    function(x){(x-median(x))/(max(x)-min(x))}))
sp_TL.result$std_T_cv <- unlist(tapply(sp_TL.result$cv_T,
                                    list(sp_TL.result$sp_ID,sp_TL.result$quarter),
                                    function(x){(x-median(x))/(max(x)-min(x))}))
## add maturity stage
size_at_maturity_stage = read.csv("./SizeAggregTend_data/compiled/size_at_maturityStage_byQ.csv")
L05 = size_at_maturity_stage$L05
L95 = size_at_maturity_stage$L95

mat_stage <- NULL
for(i in 1:9){
  stage = rep(NA,32)
  stage[which(sp.mid.L[i,]<L05[i])] = 1
  stage[which(sp.mid.L[i+9,]<L05[i+9])+16] = 1
  stage[which(sp.mid.L[i,]>L95[i])] = 3
  stage[which(sp.mid.L[i+9,]>L95[i+9])+16] = 3
  stage[which(is.na(stage))] = 2
  mat_stage = c(mat_stage,stage)
} 
sp_TL.result$mat_stage <- factor(mat_stage)

## add dummy variable
sp_TL.result$dum <- 1

## export sp_TL.result
#write.csv(sp_TL.result,"./SizeAggregTend_data/output/1_size_TL/all_TL_result_16_compiled.csv")



#####################################################################################
######################   Relationship fitting     ###################################
#####################################################################################

library("mgcv")
library("itsadug")
library("ggplot2")
library("dplyr")

### GLMM ############################################################

## Quarter ##
# temp_mean #
fit.QART.temp.lin <- gam(b~std_T*std_L*quarter+s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                     data = sp_TL.result,method = "ML",
                     family = Gamma(link = log))

# temp_cv #
fit.QART.tempCV.lin <- gam(b~std_T_cv*std_L*quarter+s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                    data = sp_TL.result,method = "ML",
                    family = Gamma(link = log))


## Spawn ##
# temp_mean #
fit.SPWN.temp.lin <- gam(b~std_T*std_L*spwn+s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                      data = sp_TL.result,method = "ML",
                      family = Gamma(link = log))

# temp_cv #
fit.SPWN.tempCV.lin <- gam(b~std_T_cv*std_L*spwn+s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                          data = sp_TL.result,method = "ML",
                          family = Gamma(link = log))


### GAMM ############################################################

fit.QART.temp.smooth.inter <- gam(b~quarter+
                                    ti(std_T,by=quarter,bs="ts")+
                                    ti(std_L,by=quarter,bs="ts")+
                                    ti(std_L,std_T,by=quarter,bs="ts")+
                                    s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                                  data = sp_TL.result,
                                  method = "ML",
                                  family = Gamma(link = log))
fit.QART.tempCV.smooth.inter <- gam(b~quarter+
                                    ti(std_T_cv,by=quarter,bs="ts")+
                                    ti(std_L,by=quarter,bs="ts")+
                                    ti(std_L,std_T_cv,by=quarter,bs="ts")+
                                    s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                                  data = sp_TL.result,
                                  method = "ML",
                                  family = Gamma(link = log))


fit.SPWN.temp.smooth.inter <- gam(b~spwn+
                                    ti(std_T,by=spwn,bs="ts")+
                                    ti(std_L,by=spwn,bs="ts")+
                                    ti(std_L,std_T,by=spwn,bs="ts")+
                                    s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                                  data = sp_TL.result,
                                  method = "ML",
                                  family = Gamma(link = log))
fit.SPWN.tempCV.smooth.inter <- gam(b~spwn+
                                    ti(std_T_cv,by=spwn,bs="ts")+
                                    ti(std_L,by=spwn,bs="ts")+
                                    ti(std_L,std_T_cv,by=spwn,bs="ts")+
                                    s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                                  data = sp_TL.result,
                                  method = "ML",
                                  family = Gamma(link = log))


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
fit.temp.noQorSPW <- gam(b~s(std_L,bs="ts")+s(std_T,bs="ts")+
                  s(std_L,sp_ID,bs="re")+s(sp_ID,bs="re"),
                data = sp_TL.result,method = "ML",select = F,
                family = Gamma(link = log))
fit.tempCV.noQorSPW <- gam(b~s(std_L,bs="ts")+s(std_T_cv,bs="ts")+
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
fit.tempCV.noRE <- gam(b~s(std_T_cv),
                     data = sp_TL.result,method = "ML",select = F,
                     family = Gamma(link = log))


### Model selection##########################################################

AIC(fit.QART.temp.lin,
    fit.QART.tempCV.lin,
    fit.QART.temp.smooth.inter,
    fit.QART.tempCV.smooth.inter,
    fit.SPWN.temp.lin,
    fit.SPWN.tempCV.lin,
    fit.SPWN.temp.smooth.inter,
    fit.SPWN.tempCV.smooth.inter,
    fit.temp.noQorSPW,
    fit.tempCV.noQorSPW,
    fit.size.noRE,
    fit.temp.noRE,
    fit.tempCV.noRE
    )

### Result using REML #######################################


fit.QART.tempCV.smooth.inter_Rslt <- gam(b~quarter+
                                    ti(std_T_cv,by=quarter,bs="ts")+
                                    ti(std_L,by=quarter,bs="ts")+
                                    ti(std_L,std_T_cv,by=quarter,bs="ts")+
                                    s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                                  data = sp_TL.result,
                                  method = "REML",
                                  family = Gamma(link = log)
                                  #select = T
)
summary(fit.QART.tempCV.smooth.inter_Rslt)
plot(fit.QART.tempCV.smooth.inter_Rslt)



##############################################################
#### propogate error of estimated TL exponents 
#### to relationship analysis
##############################################################


##########
## NOTE ## To avoid resampling, use the data already save in 1_size_TL
########## 


# TL exponents' bootstrap data -1
#TL_BSdata = apply(sp_TL.result,1,
#                  function(x){sample(seq(x[7],x[8],by=0.01),999,replace = T)})

# TL exponents' bootstrap data -2
#TL_BSdata = NULL

#for(s in 1:9){
#  sp_BS_result_filename =list.files("./SizeAggregTend_data/output/1_size_TL/TL999boot",
#                                    pattern = sp_name[s],
#                                    full.names = T)
#  sp_BS_result_filename_Q1 = sp_BS_result_filename[1:16]
#  size_class_Q1 = substr(sp_BS_result_filename_Q1,nchar(sp_BS_result_filename_Q1)-5,nchar(sp_BS_result_filename_Q1)-4)
#  size_class_Q1 = as.numeric(gsub("_", "", size_class))
  
#  sp_BS_result_filename_Q3 = sp_BS_result_filename[1:16]
#  size_class_Q3 = substr(sp_BS_result_filename_Q3,nchar(sp_BS_result_filename_Q3)-5,nchar(sp_BS_result_filename_Q3)-4)
#  size_class_Q3 = as.numeric(gsub("_", "", size_class))
  
#  for(i in 1:16){
#    size_BS_result_Q1 = read.csv(sp_BS_result_filename[i])
#    BS_resample_data_Q1 = sample(size_BS_result_Q1$V1,999,replace = T)
#    TL_BSdata = rbind(TL_BSdata,c(BS_resample_data_Q1,size_class_Q1[i],1))}
  
  
#  for(i in 1:16){
#    size_BS_result_Q3 = read.csv(sp_BS_result_filename[i+16])
#    BS_resample_data_Q3 = sample(size_BS_result_Q3$V1,999,replace = T)
#    TL_BSdata = rbind(TL_BSdata,c(BS_resample_data_Q3,size_class_Q3[i],3))
#  }
#}

#TL_BSdata = as.data.frame(TL_BSdata)
#colnames(TL_BSdata)[1000] = "size_class"
#colnames(TL_BSdata)[1001] = "quarter"
#TL_BSdata$sp_ID = rep(c(1:9),each=32)
#library("dplyr")
#TL_BSdata = TL_BSdata %>% arrange(sp_ID,quarter,size_class)

# save bs sample data 
#write.csv(TL_BSdata,"./SizeAggregTend_data/output/1_size_TL/resample_TL_bs_data.csv")

TL_BSdata <- read.csv("./SizeAggregTend_data/output/1_size_TL/resample_TL_bs_data.csv")
TL_BSdata <- TL_BSdata[,-1]

# bootstrap 999 times for model fitting and prediction
bs_fit_para_summary <- NULL
bs_fit_smth_summary <- data.frame(temp_cv_Q1=NA,temp_cv_Q3=NA,
                                  size_Q1=NA,size_Q3=NA,
                                  inter_Q1=NA,inter_Q3=NA,
                                  re_slope=NA,re_intercept=NA)
bs_quarter_diff <- NULL
bs_pred_result_Q1 <- NULL
bs_pred_result_Q3 <- NULL
bs_pred_result_Q1_tempCV <- NULL
bs_pred_result_Q3_tempCV <- NULL
bs_pred_result_Q1_inter <- list()
newdat <- expand.grid(std_L = seq(-0.69,0.75,length=200),
                         std_T_cv = 0,
                         quarter = c("Q1","Q3"),
                         sp_ID = factor(c(1:9)),
                         dum=0)

newdat_Q1 <- expand.grid(std_L = seq(-0.69,0.75,length=200),
                      std_T_cv = 0,
                      quarter = c("Q1"),
                      sp_ID = factor(c(1:9)),
                      dum=0)
newdat_Q3 <- expand.grid(std_L = seq(-0.69,0.75,length=200),
                          std_T_cv = 0,
                          quarter = c("Q3"),
                          sp_ID = factor(c(1:9)),
                          dum=0)
newdat_Q3_tempCV <- expand.grid(std_T_cv = seq(-0.80,0.92,length=200),
                         std_L = 0,
                         quarter = c("Q3"),
                         sp_ID = factor(c(1:9)),
                         dum=0)
newdat_Q1_inter <- expand.grid(std_L = seq(-0.69,0.75,length=80),
                               std_T_cv = seq(-0.80,0.92,length=80),
                                quarter = c("Q1"),
                                sp_ID = factor(c(1:9)),
                                dum=0)

for (bs in 1:999){
  bs_TL_result <- cbind(TL_BSdata[,bs],sp_TL.result[,9:19])
  colnames(bs_TL_result)[1] <- "b"
  bs_fit <- gam(b~quarter+
                  ti(std_T_cv,by=quarter,bs="ts")+
                  ti(std_L,by=quarter,bs="ts")+
                  ti(std_L,std_T_cv,by=quarter,bs="ts")+
                  s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                data = bs_TL_result,
                method = "REML",
                family = Gamma(link = log)
  )
  bs_fit_summary <- summary(bs_fit)
  bs_fit_para_summary = rbind(bs_fit_para_summary,bs_fit_summary$p.table[2,])
  bs_fit_smth_summary = rbind(bs_fit_smth_summary,bs_fit_summary$s.table[,4])
  
  # quarter different
  bs_pred_b <- predict(bs_fit, newdata = newdat, type="lpmatrix")
  c1 <- grepl('Q1', colnames(bs_pred_b))
  c3 <- grepl('Q3', colnames(bs_pred_b))
  r1 <- with(newdat, quarter == 'Q1')
  r3 <- with(newdat, quarter == 'Q3')
  X <- bs_pred_b[r1, ] - bs_pred_b[r3, ]
  X[, ! (c1 | c3)] <- 0
  X[, !grepl('^ti\\(', colnames(bs_pred_b))] <- 0
  quarter_diff <- X %*% coef(bs_fit)
  quarter_diff <- tapply(quarter_diff,newdat_Q1$std_L,mean)
  bs_quarter_diff <- cbind(bs_quarter_diff,quarter_diff)
  
  
  # Q1 prediction
  bs_pred_b_Q1 <- tapply(predict(bs_fit, newdata = newdat_Q1, type="response"),newdat_Q1$std_L,mean)
  bs_pred_result_Q1 <- cbind(bs_pred_result_Q1,bs_pred_b_Q1)
  
  bs_pred_b_inter <- tapply(predict(bs_fit, newdata = newdat_Q1_inter, type="response"),list(newdat_Q1_inter$std_L,newdat_Q1_inter$std_T_cv),mean)
  bs_pred_result_Q1_inter[[bs]] <- bs_pred_b_inter
  
  # Q3 prediction
  
  bs_pred_b_Q3 <- tapply(predict(bs_fit, newdata = newdat_Q3, type="response"),newdat_Q3$std_L,mean)
  bs_pred_b_Q3_tempCV <- tapply(predict(bs_fit, newdata = newdat_Q3_tempCV, type="response"),newdat_Q3_tempCV$std_T_cv,mean)
  
  bs_pred_result_Q3 <- cbind(bs_pred_result_Q3,bs_pred_b_Q3)
  bs_pred_result_Q3_tempCV <- cbind(bs_pred_result_Q3_tempCV,bs_pred_b_Q3_tempCV)
  
  
  
  if(bs%%50==0){print(bs)}
}

bs_fit_smth_summary <- bs_fit_smth_summary[-1,]
write.csv(bs_fit_para_summary,"./SizeAggregTend_data/output/1_size_TL/bs_fit_para_summary.csv")
write.csv(bs_fit_smth_summary,"./SizeAggregTend_data/output/1_size_TL/bs_fit_smooth_summary.csv")

write.csv(bs_quarter_diff,"./SizeAggregTend_data/output/1_size_TL/bs_fit_quarter_difference.csv")
write.csv(bs_pred_result_Q1,"./SizeAggregTend_data/output/1_size_TL/bs_fit_predicted_data_Q1_way1.csv")
write.csv(bs_pred_result_Q3,"./SizeAggregTend_data/output/1_size_TL/bs_fit_predicted_data_Q3_way1.csv")
write.csv(bs_pred_result_Q1_tempCV,"./SizeAggregTend_data/output/1_size_TL/bs_fit_predicted_data_Q1_tempCV_way1.csv")
write.csv(bs_pred_result_Q3_tempCV,"./SizeAggregTend_data/output/1_size_TL/bs_fit_predicted_data_Q3_tempCV_way1.csv")
save(bs_pred_result_Q1_inter,file="./SizeAggregTend_data/output/1_size_TL/bs_fit_predicted_data_Q1_inter_way1.RData")


# plot p-value distribution in smooth result
bs_fit_para_summary <- read.csv("./SizeAggregTend_data/output/1_size_TL/bs_fit_para_summary.csv")
bs_fit_smth_summary <- read.csv("./SizeAggregTend_data/output/1_size_TL/bs_fit_smooth_summary.csv")

bs_fit_smth_summary <- bs_fit_smth_summary[-1,-1]
smth_var = c("Temp. CV Q1","Temp. CV Q3","Size Q1","Size Q3","Temp.CV:Size Q1","Temp. CV:Size Q3","Random Slope","Random Intercept")
No_p_sig = apply(bs_fit_smth_summary,2,function(x)length(which(x<=0.05)))

jpeg("./SizeAggregTend_data/output/fig/Appendix/Fig_bs_relationship_pval.jpeg", width=6.5, height=3.5, units = "in",res=500)
par(mfrow=c(2,4),mar=c(3,3,3,1),oma=c(2,2,0,0),cex=0.8)
for(i in 1:8){
  hist(bs_fit_smth_summary[,i],
       main="",
       xlim=c(0,max(bs_fit_smth_summary[,i])+0.002),xlab = "p-value",
       breaks = seq(0,max(bs_fit_smth_summary[,i])+0.001,0.001),
       family=c("newrom"))
  title(paste0(smth_var[i],"\n n=",No_p_sig[i]), adj = 0, line = 1,family=c("newrom"),cex.main=0.9)
  abline(v=0.05,lty=2)
}
mtext("p-value",outer = T,side = 1,family=c("newrom"))
mtext("Frequency",outer = T,side = 2,family=c("newrom"))
dev.off()


######################
# plot result 
######################
bs_quarter_diff <- read.csv("./SizeAggregTend_data/output/1_size_TL/bs_fit_quarter_difference.csv")
bs_pred_result_Q1 <- read.csv("./SizeAggregTend_data/output/1_size_TL/bs_fit_predicted_data_Q1_way1.csv")
bs_pred_result_Q3 <- read.csv("./SizeAggregTend_data/output/1_size_TL/bs_fit_predicted_data_Q3_way1.csv")
bs_pred_result_Q3_tempCV <- read.csv("./SizeAggregTend_data/output/1_size_TL/bs_fit_predicted_data_Q3_tempCV_way1.csv")
load("./SizeAggregTend_data/output/1_size_TL/bs_fit_predicted_data_Q1_inter_way1.RData")


# original model prediction
# quarter different
pred_b <- predict(fit.QART.tempCV.smooth.inter_Rslt, newdata = newdat, type="lpmatrix")
c1 <- grepl('Q1', colnames(pred_b))
c3 <- grepl('Q3', colnames(pred_b))
r1 <- with(newdat, quarter == 'Q1')
r3 <- with(newdat, quarter == 'Q3')
X <- pred_b[r1, ] - pred_b[r3, ]
X[, ! (c1 | c3)] <- 0
X[, !grepl('^ti\\(', colnames(pred_b))] <- 0
quarter_diff <- X %*% coef(fit.QART.tempCV.smooth.inter_Rslt)
quarter_diff <- tapply(quarter_diff,newdat_Q1$std_L,mean)

pred_b_Q1 <- tapply(predict(fit.QART.tempCV.smooth.inter_Rslt, newdata = newdat_Q1, type="response"),
                    newdat_Q1$std_L,mean)
pred_b_Q3 <- tapply(predict(fit.QART.tempCV.smooth.inter_Rslt, newdata = newdat_Q3, type="response"),
                    newdat_Q3$std_L,mean)
pred_b_Q3_tempCV <- tapply(predict(fit.QART.tempCV.smooth.inter_Rslt, newdata = newdat_Q3_tempCV, type="response"),
                    newdat_Q3$std_L,mean)

bs_quarter_diff <- cbind(bs_quarter_diff[,-1],quarter_diff)
bs_pred_result_Q1 <- cbind(bs_pred_result_Q1[,-1],pred_b_Q1)
bs_pred_result_Q3 <- cbind(bs_pred_result_Q3[,-1],pred_b_Q3)
bs_pred_result_Q3_tempCV <- cbind(bs_pred_result_Q3_tempCV[,-1],pred_b_Q3_tempCV)


# bca 95CI
library("coxed")
bca_Qdiff_95CI <- apply(bs_quarter_diff,1,bca)
bca_size_95CI_Q1 <- apply(bs_pred_result_Q1,1,bca)
bca_size_95CI_Q3 <- apply(bs_pred_result_Q3,1,bca)
bca_tempCV_95CI_Q3 <- apply(bs_pred_result_Q3_tempCV,1,bca)


library(dplyr)

# plot #
jpeg("./SizeAggregTend_data/output/fig/Fig_main_effect_with_bs_and_matstage.jpeg", width=1961, height=800, units = "px",res=300)

par(mfrow=c(1,2),oma=c(0,0,1,1),mar=c(4,4,1,1),cex.axis=0.8,cex.lab=0.8)

#Q1
plot(unique(newdat$std_L),pred_b_Q1,
     type="l",xlim = c(-0.8,0.8),ylim=c(0.5,4),
     xlab="Standardized length",
     ylab="Taylor's exponents",bty="l")
polygon(c(rev(unique(newdat$std_L)), unique(newdat$std_L)), 
        c(rev(bca_size_95CI_Q1[2,]), bca_size_95CI_Q1[1,]), 
        col = rgb(50, 50, 50, 40, maxColorValue=255), border = NA)

#for(bs in 1:999){
#  lines(unique(newdat_Q1$std_L),bs_pred_result_Q1[,bs],
#        col=rgb(128, 138, 135, 60, maxColorValue=255),lwd=0.5)}
lines(unique(newdat$std_L),pred_b_Q1,lwd=2)

sp_TL.result %>% filter(quarter=="Q1" & mat_stage==1) %>% 
  with(points(b~std_L,pch=1,cex=0.6,col="grey30",xlim = c(-0.9,0.9),ylim = c(1,3.5)))
sp_TL.result %>% filter(quarter=="Q1" & mat_stage==2) %>% 
  with(points(b~std_L,pch=1,cex=0.6,col="grey80",xlim = c(-0.9,0.9),ylim = c(1,3.5)))
sp_TL.result %>% filter(quarter=="Q1" & mat_stage==3) %>% 
  with(points(b~std_L,pch=1,cex=0.6,col="black",xlim = c(-0.9,0.9),ylim = c(1,3.5)))
legend("topright",c("immature","maturing","matured"),pch=1,col=c("grey30","grey80","black"),horiz = F,cex = 0.5)

#points(b~std_L,data=sp_TL.result[which(sp_TL.result$quarter=="Q1"),],
#       col=rgb(0, 0, 0, 80, maxColorValue=255),pch=19,cex=0.5)
mtext("(a)", side = 3, line = 1, outer =F,at =-1,adj=0,cex = 0.9)


#Q3
plot(unique(newdat$std_L),pred_b_Q3,
     type="l",xlim = c(-0.8,0.8),ylim=c(0.5,4),
     xlab="Standardized length",
     ylab="Taylor's exponents",bty="l")
polygon(c(rev(unique(newdat$std_L)), unique(newdat$std_L)), 
        c(rev(bca_size_95CI_Q3[2,]), bca_size_95CI_Q3[1,]), 
        col = rgb(50, 50, 50, 40, maxColorValue=255), border = NA)


#for(bs in 1:999){
#  lines(unique(newdat_Q3$std_L),bs_pred_result_Q3[,bs],
#        col=rgb(128, 138, 135, 60, maxColorValue=255),lwd=0.5)}
lines(unique(newdat$std_L),pred_b_Q3,lwd=2)
sp_TL.result %>% filter(quarter=="Q3" & mat_stage==1) %>% 
  with(points(b~std_L,pch=1,cex=0.6,col="grey30",xlim = c(-0.9,0.9),ylim = c(1,3.5)))
sp_TL.result %>% filter(quarter=="Q3" & mat_stage==2) %>% 
  with(points(b~std_L,pch=1,cex=0.6,col="grey80",xlim = c(-0.9,0.9),ylim = c(1,3.5)))
sp_TL.result %>% filter(quarter=="Q3" & mat_stage==3) %>% 
  with(points(b~std_L,pch=1,cex=0.6,col="black",xlim = c(-0.9,0.9),ylim = c(1,3.5)))
legend("topright",c("immature","maturing","matured"),pch=1,col=c("grey30","grey80","black"),horiz = F,cex = 0.5)

#points(b~std_L,
#       data=sp_TL.result[which(sp_TL.result$quarter=="Q3"),],
#       col=rgb(0, 0, 0, 80, maxColorValue=255),pch=19,cex=0.5)

mtext("(b)", side = 3, line = 1, outer =F,adj=0,cex = 0.9)
dev.off()



# peak location
Max_b_Q1 <- apply(bs_pred_result_Q1,2,max)
peak_locat_order_Q1 <- apply(bs_pred_result_Q1,2,function(x)which(x==max(x)))
peak_locat_Q1 <- seq(-0.69,0.75,length=200)[peak_locat_order_Q1]
bca(peak_locat_Q1)
peak_locat_Q1[length(peak_locat_Q1)]

Max_b_Q3 <- apply(bs_pred_result_Q3,2,max)
peak_locat_order_Q3 <- apply(bs_pred_result_Q3,2,function(x)which(x==max(x)))
peak_locat_Q3 <- seq(-0.69,0.75,length=200)[peak_locat_order_Q3]
bca(peak_locat_Q3)
peak_locat_Q3[length(peak_locat_Q3)]



#######################################################
### Plot difference between two quarter 
jpeg("./SizeAggregTend_data/output/fig/Appendix/Fig_quarter_size_diff.jpeg", width=6, height=5, units = "in",res=600)

plot(unique(newdat$std_L),bs_quarter_diff[,1000],
     type="l",xlim = c(-0.7,0.8),ylim=c(-0.15,0.2),
     xlab="Standardized length",ylab="Difference between quarter",family=c("newrom"),bty="l")
polygon(c(rev(unique(newdat$std_L)), unique(newdat$std_L)), 
        c(rev(bca_Qdiff_95CI[2,]), bca_Qdiff_95CI[1,]), 
        col = rgb(50, 50, 50, 40, maxColorValue=255), border = NA)
abline(h=0)
abline(v=range(as.numeric(colnames(bca_Qdiff_95CI)[which(bca_Qdiff_95CI[1,]>0)])),lty=2)
abline(v=range(as.numeric(colnames(bca_Qdiff_95CI)[which(bca_Qdiff_95CI[2,]<0)])),lty=2)


dev.off()






#################################################
# mature stage effect on the relationship
#################################################

# dunn-test
library("rstatix")
dunn_matstage_Q1 = sp_TL.result%>%
  filter(quarter=="Q1") %>%
  dunn_test(b~mat_stage, p.adjust.method = "BH")
dunn_matstage_Q1 <- dunn_matstage_Q1 %>% add_xy_position(x = "mat_stage")

dunn_matstage_Q3 = sp_TL.result%>%
  filter(quarter=="Q3") %>%
  dunn_test(b~mat_stage, p.adjust.method = "BH")
dunn_matstage_Q3 <- dunn_matstage_Q3 %>% add_xy_position(x = "mat_stage")

jpeg("./SizeAggregTend_data/output/fig/Fig_matstagebyQ_dunntest_Q1.jpeg", width=980.5, height=700, units = "px",res=300)
library(ggplot2)
library("ggpubr")
ggboxplot(sp_TL.result[which(sp_TL.result$quarter=="Q1"),], 
          x = "mat_stage", y = "b", fill = "mat_stage",
          xlab="",ylab="Taylor's exponents") +
  stat_pvalue_manual(dunn_matstage_Q1, hide.ns = FALSE,y.position = c(3.5,3.6,3.7))+
  scale_x_discrete(labels=c("immature","maturing","matured"))+
  theme(legend.position = "none",
        axis.text.y = element_text(size=rel(0.9),angle = 90),
        axis.text.x = element_text(size=rel(0.9)),
        axis.title=element_text(size=rel(0.9)))+
  ylim(1.5,3.8)

dev.off()

jpeg("./SizeAggregTend_data/output/fig/Fig_matstagebyQ_dunntest_Q3.jpeg", width=980.5, height=700, units = "px",res=300)

ggboxplot(sp_TL.result[which(sp_TL.result$quarter=="Q3"),], 
          x = "mat_stage", y = "b", fill = "mat_stage",
          xlab="",ylab="Taylor's exponents") +
  stat_pvalue_manual(dunn_matstage_Q3, hide.ns = FALSE,y.position = c(3.5,3.6,3.7))+
  scale_x_discrete(labels=c("immature","maturing","matured"))+
  theme(legend.position = "none",
        axis.text.y = element_text(size=rel(0.9),angle = 90),
        axis.text.x = element_text(size=rel(0.9)),
        axis.title=element_text(size=rel(0.8)))+
  ylim(1.5,3.8)

dev.off()


#######
# examine the relationship in each maturity stage
#######

# Q1 

lm1.g = sp_TL.result %>% filter(quarter=="Q1" & mat_stage==1) %>% 
  with(gam(b~std_L+s(sp_ID,bs="re"),family = Gamma(link = "log")))
lm2.g = sp_TL.result %>% filter(quarter=="Q1" & mat_stage==2) %>% 
  with(gam(b~std_L+s(sp_ID,std_L,bs="re")+s(sp_ID,bs="re"),family = Gamma(link = "log")))
lm3.g = sp_TL.result %>% filter(quarter=="Q1" & mat_stage==3) %>% 
  with(gam(b~std_L+s(sp_ID,bs="re"),family = Gamma(link = "log")))

# Q3
lm1.3.g = sp_TL.result %>% filter(quarter=="Q3" & mat_stage==1) %>% 
  with(gam(b~std_L+s(sp_ID,std_L,bs="re")+s(sp_ID,bs="re"),family = Gamma(link = "log")))
lm2.3.g = sp_TL.result %>% filter(quarter=="Q3" & mat_stage==2) %>% 
  with(gam(b~std_L+s(sp_ID,std_L,bs="re")+s(sp_ID,bs="re"),family = Gamma(link = "log")))
lm3.3.g = sp_TL.result %>% filter(quarter=="Q3" & mat_stage==3) %>% 
  with(gam(b~std_L,family = Gamma(link = "log")))


# bootstrap 999 times for model fitting and prediction
bs_matstage_lin_result_Q1 <- NULL
bs_matstage_lin_result_Q3 <- NULL


for (bs in 1:999){
  bs_TL_result <- cbind(TL_BSdata[,bs],sp_TL.result[,9:19])
  colnames(bs_TL_result)[1] <- "b"
  
  #Q1
  for(mat in 1:3){
    bs_glm_result <- bs_TL_result %>% filter(quarter=="Q1" & mat_stage==mat) %>% 
      with(gam(b~std_L+s(sp_ID,std_L,bs="re")+s(sp_ID,bs="re"),family = Gamma(link = "log")))
    bs_matstage_lin_result_Q1 <- rbind(bs_matstage_lin_result_Q1,c(coef(bs_glm_result)[1:2],mat,1))
  }
  
  #Q3
  for(mat in 1:3){
    bs_glm_result <- bs_TL_result %>% filter(quarter=="Q3" & mat_stage==mat) %>% 
      with(gam(b~std_L+s(sp_ID,std_L,bs="re")+s(sp_ID,bs="re"),family = Gamma(link = "log")))
    bs_matstage_lin_result_Q3 <- rbind(bs_matstage_lin_result_Q3,c(coef(bs_glm_result)[1:2],mat,3))
  }
  if(bs%%50==0){print(bs)}
}


bs_matstage_lin_result_Q1 <- as.data.frame(bs_matstage_lin_result_Q1)
bs_matstage_lin_result_Q3 <- as.data.frame(bs_matstage_lin_result_Q3)
colnames(bs_matstage_lin_result_Q1)[2:4] <- c("slope","mat_stage","quarter")
colnames(bs_matstage_lin_result_Q3)[2:4] <- c("slope","mat_stage","quarter")

write.csv(bs_matstage_lin_result_Q1,"./SizeAggregTend_data/output/1_size_TL/bs_lin_matstage_byQ_QART_1.csv")
write.csv(bs_matstage_lin_result_Q3,"./SizeAggregTend_data/output/1_size_TL/bs_lin_matstage_byQ_QART_3.csv")



bs_matstage_lin_result_Q1 <- read.csv("./SizeAggregTend_data/output/1_size_TL/bs_lin_matstage_byQ_QART_1.csv")
bs_matstage_lin_result_Q3 <- read.csv("./SizeAggregTend_data/output/1_size_TL/bs_lin_matstage_byQ_QART_3.csv")

# plot
add_eq_fuc = function(model){
  cf <- round(coef(model), 2) 
  eq <- paste0(" log(y) = ", cf[1],ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), "x")
  return(eq)
}

library("coxed")
glm_fit_result <- list(lm1.g,lm2.g,lm3.g,lm1.3.g,lm2.3.g,lm3.3.g)
mat_stage_name <- c("immature","maturing","matured")

# Q1
jpeg("./SizeAggregTend_data/output/fig/Fig_relationship_in_each_matstagebyQ_Q1.jpeg", width=980.5, height=800, units = "px",res=300)
par(mfrow=c(1,3),mar=c(2,2,2,0),oma=c(3,3,3,1),cex.axis=1.2,cex.lab=0.8) 

for(mat in 1:3){
  sp_TL.result %>% filter(quarter=="Q1" & mat_stage==mat) %>% with(plot(b~std_L,ylim=c(1,4),yaxt="n"))
  if(mat==1){axis(side = 2)}
  
  if(summary(glm_fit_result[[mat]])$p.table[2,4]<=0.05){
  lines(seq(-0.8,0.8,0.1),predict(glm_fit_result[[mat]],newdata = data.frame(expand.grid(std_L=seq(-0.8,0.8,0.1),sp_ID=1)),type = "response"))
  lm.g.eq <- add_eq_fuc(glm_fit_result[[mat]])
  lm.g.ci <- bs_matstage_lin_result_Q1 %>% filter(mat_stage==mat) %>% with(bca(slope)) %>% round(2)
  mtext(lm.g.eq, 3, line=-1.5,cex=0.35)
  mtext(paste0("CI=[",lm.g.ci[1],",",lm.g.ci[2],"]"), 3, line=-2.4,cex=0.35)
  }
  mtext(mat_stage_name[mat],3,line=1,cex = 0.8)
}
mtext("Standardized length",side=1,line=1,outer = T,cex = 0.8)
mtext("Taylor's exponents",side=2,line=1,outer = T,cex = 0.8)
mtext("(c)",side=3,line=1,outer = T,cex = 0.9,adj = 0)

dev.off()

# Q3
jpeg("./SizeAggregTend_data/output/fig/Fig_relationship_in_each_matstagebyQ_Q3.jpeg", width=980.5, height=800, units = "px",res=300)
par(mfrow=c(1,3),mar=c(2,2,2,0),oma=c(3,3,3,1),cex.axis=1.2,cex.lab=0.8) 
for(mat in 1:3){
  sp_TL.result %>% filter(quarter=="Q3" & mat_stage==mat) %>% with(plot(b~std_L,ylim=c(0.5,4),yaxt="n"))
  if(mat==1){axis(side = 2)}
  
  if(summary(glm_fit_result[[mat+3]])$p.table[2,4]<=0.05){
    lines(seq(-0.8,0.8,0.1),predict(glm_fit_result[[mat+3]],newdata = data.frame(expand.grid(std_L=seq(-0.8,0.8,0.1),sp_ID=1)),type = "response"))
    lm.g.eq <- add_eq_fuc(glm_fit_result[[mat+3]])
    lm.g.ci <- bs_matstage_lin_result_Q3 %>% filter(mat_stage==mat) %>% with(bca(slope)) %>% round(2)
    mtext(lm.g.eq, 3, line=-1.5,cex=0.35)
    mtext(paste0("CI=[",lm.g.ci[1],",",lm.g.ci[2],"]"), 3, line=-2.2,cex=0.35)
  }
  mtext(mat_stage_name[mat],3,line=1,cex = 0.8)
}

mtext("Standardized length",side=1,line=1,outer = T,cex = 0.8)
mtext("Taylor's exponents",side=2,line=1,outer = T,cex = 0.8)
mtext("(d)",side=3,line=1,outer = T,cex = 0.9,adj = 0)

dev.off()

