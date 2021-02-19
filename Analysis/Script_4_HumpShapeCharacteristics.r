
# import data

wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)

sp_TL.result <- read.csv("./SizeAggregTend_data/output/1_size_TL/all_TL_result_16_compiled.csv",stringsAsFactors = F,sep=",",header = T)
sp_TL.result$spwn <- as.factor(sp_TL.result$spwn)
sp_TL.result$quarter <- as.factor(sp_TL.result$quarter)
sp_TL.result$sp_ID <- as.factor(sp_TL.result$sp_ID)
sp_TL.result$mat_stage <- as.factor(sp_TL.result$mat_stage)

####### species fitted respectively #################################

library("coxed")
library("mgcv")
library("tidymv")

sp_common_name <- c("Herring","Cod","Haddock","Whiting","Plaice",
                    "Saithe", "Mackerel","Sprat","Norwaypout")

sp_fit_summary <- list()
sp_resp_humppeak <- NULL
sp.resp_slope_Q1 <- NULL
sp.resp_slope_Q3 <- NULL

TL_BSdata <- read.csv("./SizeAggregTend_data/output/1_size_TL/resample_TL_bs_data.csv")
TL_BSdata <- TL_BSdata[,-1]
TL_BSdata <- cbind(sp_TL.result$b,TL_BSdata)

size_at_maturity_stage = read.csv("./SizeAggregTend_data/compiled/size_at_maturityStage_byQ.csv")
sp.mid.L <- as.matrix(read.table("./SizeAggregTend_data/compiled/allsp_length_mid_16class.txt"))
mature.L <- as.matrix(read.table("./SizeAggregTend_data/compiled/mature_L.txt"))


all_sp_peak_slope_95CI <-NULL
all_bs_pred_result_Q1 <- list()
all_bs_pred_result_Q3 <- list()


# bootstrap 999 times for model fitting and prediction

newdat_Q1 <- expand.grid(std_L = seq(-0.69,0.75,length=200),
                         quarter = c("Q1"))
newdat_Q3 <- expand.grid(std_L = seq(-0.69,0.75,length=200),
                         quarter = c("Q3"))



for(i in 1:9){
  
  sp_bs_pred_result_Q1 <- NULL
  sp_bs_pred_result_Q3 <- NULL
  for(bs in 1:1000){
    bs_TL_result <- cbind(TL_BSdata[,bs],sp_TL.result[,9:20])
    colnames(bs_TL_result)[1] <- "b"
    sp_bs_fit <- gam(b~quarter+s(std_L,by=quarter,bs="ts"),
                     data=bs_TL_result[which(bs_TL_result$sp_ID==i),],
                     method = "REML")
    # Q1 prediction
    sp_bs_pred_b_Q1 <- tapply(predict(sp_bs_fit, newdata = newdat_Q1, type="response"),newdat_Q1$std_L,mean)
    sp_bs_pred_result_Q1 <- cbind(sp_bs_pred_result_Q1,sp_bs_pred_b_Q1)
    
    # Q3 prediction
    sp_bs_pred_b_Q3 <- tapply(predict(sp_bs_fit, newdata = newdat_Q3, type="response"),newdat_Q3$std_L,mean)
    sp_bs_pred_result_Q3 <- cbind(sp_bs_pred_result_Q3,sp_bs_pred_b_Q3)
    
  }
  all_bs_pred_result_Q1[[i]] = sp_bs_pred_result_Q1
  all_bs_pred_result_Q3[[i]] = sp_bs_pred_result_Q3
  
  ## hump shape peak ##
  
  main <- c(max(sp_TL.result$std_L[(i*32-31):(i*32)]),min(sp_TL.result$std_L[(i*32-31):(i*32)]))
  
  peak_fuc <- function(x){
    x = rbind(x,newdat_Q1$std_L)
    x = x[,which(newdat_Q1$std_L<main[1] & newdat_Q1$std_L>main[2])]
    max_b = max(x[1,])
    peak_loc = x[2,which(x[1,]==max_b)]
    return(c(max_b,peak_loc))
  }
  
  peak_info_Q1 <- apply(sp_bs_pred_result_Q1,2,peak_fuc)
  peak_info_Q3 <- apply(sp_bs_pred_result_Q3,2,peak_fuc)
  
  sp_resp_humppeak <- rbind(sp_resp_humppeak, peak_info_Q1, peak_info_Q3)
  
  
  ## hump shape slope ##
  sp_TL.result_Q1 <- filter(sp_TL.result,quarter=="Q1" & sp_ID==i)
  sp_TL_BSdata_Q1 <- filter(TL_BSdata,sp_TL.result$quarter=="Q1" & sp_TL.result$sp_ID==i)
  
  sp_TL.result_Q3 <- filter(sp_TL.result,quarter=="Q3" & sp_ID==i)
  sp_TL_BSdata_Q3 <- filter(TL_BSdata,sp_TL.result$quarter=="Q3" & sp_TL.result$sp_ID==i)
  
  std_L05_Q1 = (size_at_maturity_stage$L05[i]-mature.L[i])/(max(sp.mid.L[i,])-min(sp.mid.L[i,]))
  std_L95_Q1 = (size_at_maturity_stage$L95[i]-mature.L[i])/(max(sp.mid.L[i,])-min(sp.mid.L[i,]))
  std_L05_Q3 = (size_at_maturity_stage$L05[i+9]-mature.L[i])/(max(sp.mid.L[i+9,])-min(sp.mid.L[i+9,]))
  std_L95_Q3 = (size_at_maturity_stage$L95[i+9]-mature.L[i])/(max(sp.mid.L[i+9,])-min(sp.mid.L[i+9,]))
  
  for(bs in 1:1000){
    #Q1
    sp_TLBS_result_Q1 <- cbind(sp_TL_BSdata_Q1[,bs],sp_TL.result_Q1[,10:20])
    colnames(sp_TLBS_result_Q1)[1] <-"b"
    mat1_dat_1 = filter(sp_TLBS_result_Q1,std_L<std_L05_Q1)
    mat2_dat_1 = filter(sp_TLBS_result_Q1,std_L>=std_L05_Q1 & std_L<std_L95_Q1)
    mat3_dat_1 = filter(sp_TLBS_result_Q1,std_L>=std_L95_Q1)
    
    if(length(mat1_dat_1[,1])>2) {
      mat1_line_fit_1 <- summary(gam(b~std_L,mat1_dat_1,family = Gamma(link = "log")))
      mat1_line_Rst_1 <- c(mat1_line_fit_1$p.table[2,],mat1_line_fit_1$r.sq,mat1_line_fit_1$dev.expl)
    }else{mat1_line_Rst_1 <- matrix(0,nrow = 1,ncol=5)}
    
    if(length(mat2_dat_1[,1])>2) {
      mat2_line_fit_1 <- summary(gam(b~std_L,mat2_dat_1,family = Gamma(link = "log")))
      mat2_line_Rst_1 <- c(mat2_line_fit_1$p.table[2,],mat2_line_fit_1$r.sq,mat2_line_fit_1$dev.expl)
    }else{mat2_line_Rst_1 <- matrix(0,nrow = 1,ncol=5)}
    
    if(length(mat3_dat_1[,1])>2) {
      mat3_line_fit_1 <- summary(gam(b~std_L,mat3_dat_1,family = Gamma(link = "log")))
      mat3_line_Rst_1 <- c(mat3_line_fit_1$p.table[2,],mat3_line_fit_1$r.sq,mat3_line_fit_1$dev.expl)
    }else{mat3_line_Rst_1 <- matrix(0,nrow = 1,ncol=5)}
    
    #Q3
    sp_TLBS_result_Q3 <- cbind(sp_TL_BSdata_Q3[,bs],sp_TL.result_Q3[,10:20])
    colnames(sp_TLBS_result_Q3)[1] <-"b"
    mat1_dat_3 = filter(sp_TLBS_result_Q3,std_L<std_L05_Q3)
    mat2_dat_3 = filter(sp_TLBS_result_Q3,std_L>=std_L05_Q3 & std_L<std_L95_Q3)
    mat3_dat_3 = filter(sp_TLBS_result_Q3,std_L>=std_L95_Q3)
    
    if(length(mat1_dat_3[,1])>2) {
      mat1_line_fit_3 <- summary(gam(b~std_L,mat1_dat_3,family = Gamma(link = "log")))
      mat1_line_Rst_3 <- c(mat1_line_fit_3$p.table[2,],mat1_line_fit_3$r.sq,mat1_line_fit_3$dev.expl)
    }else{mat1_line_Rst_3 <- matrix(0,nrow = 1,ncol=5)}
    
    if(length(mat2_dat_3[,1])>2) {
      mat2_line_fit_3 <- summary(gam(b~std_L,mat2_dat_3,family = Gamma(link = "log")))
      mat2_line_Rst_3 <- c(mat2_line_fit_3$p.table[2,],mat2_line_fit_3$r.sq,mat2_line_fit_3$dev.expl)
    }else{mat2_line_Rst_3 <- matrix(0,nrow = 1,ncol=5)}
    
    if(length(mat3_dat_3[,1])>2) {
      mat3_line_fit_3 <- summary(gam(b~std_L,mat3_dat_3,family = Gamma(link = "log")))
      mat3_line_Rst_3 <- c(mat3_line_fit_3$p.table[2,],mat3_line_fit_3$r.sq,mat3_line_fit_3$dev.expl)
    }else{mat3_line_Rst_3 <- matrix(0,nrow = 1,ncol=6)}
    
    sp.resp_slope_Q1 <- rbind(sp.resp_slope_Q1,
                           #Q1
                          c(mat1_line_Rst_1,length(mat1_dat_1[,1]),
                            mat2_line_Rst_1,length(mat2_dat_1[,1]),
                            mat3_line_Rst_1,length(mat3_dat_1[,1])))
    sp.resp_slope_Q3 <- rbind(sp.resp_slope_Q3,
                              #Q3
                              c(mat1_line_Rst_3,length(mat1_dat_3[,1]),
                                mat2_line_Rst_3,length(mat2_dat_3[,1]),
                                mat3_line_Rst_3,length(mat3_dat_3[,1])))
  }
  
  all_same_logic_fuc = function(x=peak_info_Q3[2,]){
    not_same_lengt = length(which(x[2:length(x)]!=x[1]))
    if(not_same_lengt<100){logit=c(T,T)}else{logit=c(F,F)}
    return(logit)
  }
  
  max_b_95CI = rbind(ifelse(all_same_logic_fuc(peak_info_Q1[1,]),c(peak_info_Q1[1,1],peak_info_Q1[1,1]),bca(peak_info_Q1[1,])),
                     ifelse(all_same_logic_fuc(peak_info_Q3[1,]),c(peak_info_Q3[1,1],peak_info_Q3[1,1]),bca(peak_info_Q3[1,])))
  peak_loc_95CI = rbind(ifelse(all_same_logic_fuc(peak_info_Q1[2,]),c(peak_info_Q1[2,1],peak_info_Q1[2,1]),bca(peak_info_Q1[2,])),
                        ifelse(all_same_logic_fuc(peak_info_Q3[2,]),c(peak_info_Q3[2,1],peak_info_Q3[2,1]),bca(peak_info_Q3[2,])))
  mat1_slope_95CI = rbind(bca(sp.resp_slope_Q1[,1]),bca(sp.resp_slope_Q3[,1]))
  mat2_slope_95CI = rbind(bca(sp.resp_slope_Q1[,8]),bca(sp.resp_slope_Q3[,8]))
  mat3_slope_95CI = rbind(bca(sp.resp_slope_Q1[,15]),bca(sp.resp_slope_Q3[,15]))
  
  all_sp_peak_slope_95CI = rbind(all_sp_peak_slope_95CI,
                                 max_b_95CI, peak_loc_95CI,
                                 mat1_slope_95CI,mat2_slope_95CI,mat3_slope_95CI)
  #all_sp_resp_humppeak[[i]] = sp_resp_humppeak
  #all_sp.resp_slope_Q1[[i]] = sp.resp_slope_Q1
  #all_sp.resp_slope_Q3[[i]] = sp.resp_slope_Q3
  
  print(i)
}

all_sp_peak_slope_95CI <- as.data.frame(all_sp_peak_slope_95CI)
all_sp_peak_slope_95CI$sp_ID <- rep(1:9,each=10)
all_sp_peak_slope_95CI$quarter <- rep(c("Q1","Q3"),45)
all_sp_peak_slope_95CI$hump_char <- rep(rep(c("max_b","peak_loc","mat1_slope","mat2_slope","mat3_slope"),each=2),9)


write.csv(sp_resp_humppeak,"./SizeAggregTend_data/output/3_humpshape_characteristics/sp_resp_humppeak.csv")
write.csv(sp.resp_slope_Q1,"./SizeAggregTend_data/output/3_humpshape_characteristics/sp_resp_slope_Q1.csv")
write.csv(sp.resp_slope_Q3,"./SizeAggregTend_data/output/3_humpshape_characteristics/sp_resp_slope_Q3.csv")
write.csv(all_sp_peak_slope_95CI,"./SizeAggregTend_data/output/3_humpshape_characteristics/sp_resp_peak_slope_95CI.csv")

save(all_bs_pred_result_Q1,file = "./SizeAggregTend_data/output/3_humpshape_characteristics/sp_bs_pred_Q1.RData")
save(all_bs_pred_result_Q3,file = "./SizeAggregTend_data/output/3_humpshape_characteristics/sp_bs_pred_Q3.RData")

##########################################
# plot species fit
##########################################
sp_resp_humppeak <- read.csv("./SizeAggregTend_data/output/3_humpshape_characteristics/sp_resp_humppeak.csv")
sp.resp_slope_Q1 <- read.csv("./SizeAggregTend_data/output/3_humpshape_characteristics/sp_resp_slope_Q1.csv")
sp.resp_slope_Q3 <- read.csv("./SizeAggregTend_data/output/3_humpshape_characteristics/sp_resp_slope_Q3.csv")
all_sp_peak_slope_95CI <- read.csv("./SizeAggregTend_data/output/3_humpshape_characteristics/sp_resp_peak_slope_95CI.csv")

load("./SizeAggregTend_data/output/3_humpshape_characteristics/sp_bs_pred_Q1.RData")
load("./SizeAggregTend_data/output/3_humpshape_characteristics/sp_bs_pred_Q3.RData")


jpeg("./SizeAggregTend_data/output/fig/Fig_sp_resp_fit_byQ_bs_matstagebyQ.jpeg", width=1476, height=1476, units = "px",res=300)

par(mfrow=c(3,3),mar=c(3,2,3,1),oma=c(3,3,1,0),xpd=F)
for(i in 1:9){
  
  sp_bs_pred_b_main_Q1 = as.data.frame(cbind(all_bs_pred_result_Q1[[i]],newdat_Q1$std_L))
  sp_bs_pred_b_main_Q3 = as.data.frame(cbind(all_bs_pred_result_Q3[[i]],newdat_Q3$std_L))
  
  main <- c(max(sp_TL.result$std_L[(i*32-31):(i*32)]),min(sp_TL.result$std_L[(i*32-31):(i*32)]))
  sp_bs_pred_b_main_Q1 = sp_bs_pred_b_main_Q1[which(sp_bs_pred_b_main_Q1$V1001<main[1] & 
                                                      sp_bs_pred_b_main_Q1$V1001>main[2]),]
  sp_bs_pred_b_main_Q3 = sp_bs_pred_b_main_Q3[which(sp_bs_pred_b_main_Q3$V1001<main[1] & 
                                                      sp_bs_pred_b_main_Q3$V1001>main[2]),]
  
  sp_fit_95CI_Q1 = apply(sp_bs_pred_b_main_Q1[,1:1000],1,bca)
  sp_fit_95CI_Q3 = apply(sp_bs_pred_b_main_Q3[,1:1000],1,bca)
  
  plot(sp_bs_pred_b_main_Q1$V1001,sp_bs_pred_b_main_Q1[,1],
       col = c("grey40"),lty = 1,type="l",lwd=2,
       xlim = c(-0.8,0.8),
       ylim = c(0.5,3.5),
       family="newrom",xlab = "",ylab="",main = sp_common_name[i])
  polygon(c(rev(sp_bs_pred_b_main_Q1$V1001), sp_bs_pred_b_main_Q1$V1001), 
          c(rev(sp_fit_95CI_Q1[ 2,]), sp_fit_95CI_Q1[ 1,]), 
          col = rgb(150, 150, 150, 80, maxColorValue=255), border = NA)
  points(sp_TL.result$std_L[(32*i-31):(32*i-16)],
         sp_TL.result$b[(32*i-31):(32*i-16)],col="grey10",cex=0.8)
  
  lines(sp_bs_pred_b_main_Q3$V1001,sp_bs_pred_b_main_Q3[,1],
        col = rgb(220, 34, 34, 100, maxColorValue=255),lwd=2,
        xlim = c(-0.8,0.8),
        ylim = c(0.5,3.5))
  polygon(c(rev(sp_bs_pred_b_main_Q3$V1001), sp_bs_pred_b_main_Q3$V1001), 
          c(rev(sp_fit_95CI_Q3[ 2,]), sp_fit_95CI_Q3[ 1,]), 
          col = rgb(220, 34, 34, 80, maxColorValue=255), border = NA)
  points(sp_TL.result$std_L[(32*i-15):(32*i)],
         sp_TL.result$b[(32*i-15):(32*i)],pch=1,col=2,cex=0.8)
  
  abline(v=((size_at_maturity_stage$L05[i]-mature.L[i])/(max(sp.mid.L[i,])-min(sp.mid.L[i,]))),lty=2)
  abline(v=((size_at_maturity_stage$L95[i]-mature.L[i])/(max(sp.mid.L[i,])-min(sp.mid.L[i,]))),lty=2)
  abline(v=((size_at_maturity_stage$L05[i+9]-mature.L[i])/(max(sp.mid.L[i+9,])-min(sp.mid.L[i+9,]))),lty=2,col=2)
  abline(v=((size_at_maturity_stage$L95[i+9]-mature.L[i])/(max(sp.mid.L[i+9,])-min(sp.mid.L[i+9,]))),lty=2,col=2)
  
}

mtext("Standardized length",side=1,line=1,outer = T,cex = 1)
mtext("Taylor's exponents",side=2,line=1,outer = T,cex = 1)

dev.off()


###########################################################################
# check bootstrapped result
###########################################################################

# peak
sp_resp_humppeak = sp_resp_humppeak[,-1]
par(mfrow=c(3,3))
for(i in 1:9) {
  hist(as.numeric(sp_resp_humppeak[4*(i-1)+1,]),main=sp_common_name[i])
}

jpeg("./SizeAggregTend_data/output/fig/Appendix/Fig_bs_peaklocat_Q1.jpeg", width=6.5, height=5, units = "in",res=300)
par(mfrow=c(3,3),mar=c(2,2,4,2),oma=c(3,3,1,1))
for(i in 1:9) {
  no_not_hump = length(which(sp_resp_humppeak[4*(i-1)+2,]==max(sp_resp_humppeak[4*(i-1)+2,]) | sp_resp_humppeak[4*(i-1)+2,]==min(sp_resp_humppeak[4*(i-1)+2,])))
  hist(as.numeric(sp_resp_humppeak[4*(i-1)+2,]),main=paste0(sp_common_name[i]," \n not humped shape = ",no_not_hump),xlab ="",ylab="")
}
mtext("Peak location (mm)", side =1,outer= T,line=1)
mtext("Frequency", side =2,outer=T,line=1)
dev.off()

jpeg("./SizeAggregTend_data/output/fig/Appendix/Fig_bs_peaklocat_Q3.jpeg", width=6.5, height=5, units = "in",res=300)
par(mfrow=c(3,3),mar=c(2,2,4,2),oma=c(3,3,1,1))
for(i in 1:9) {
  no_not_hump = length(which(sp_resp_humppeak[4*(i-1)+4,]==max(sp_resp_humppeak[4*(i-1)+4,]) | sp_resp_humppeak[4*(i-1)+4,]==min(sp_resp_humppeak[4*(i-1)+2,])))
  hist(as.numeric(sp_resp_humppeak[4*(i-1)+4,]),main=paste0(sp_common_name[i]," \n not humped shape = ",no_not_hump),xlab ="",ylab="")
}
mtext("Peak location (mm)", side =1,outer= T,line=1)
mtext("Frequency", side =2,outer=T,line=1)
dev.off()


# slope

sp.resp_slope_Q1 = sp.resp_slope_Q1[,-1]
sp.resp_slope_est_Q1 = sp.resp_slope_Q1[,c(1,8,15)]
par(mfrow=c(3,3))
for(i in 1:9) {
  hist(sp.resp_slope_est_Q1[1000*(i-1)+1:1000*i,1],main=sp_common_name[i])
}
for(i in 1:9) {
  hist(sp.resp_slope_est_Q1[1000*(i-1)+1:1000*i,2],main=sp_common_name[i])
}
for(i in 1:9) {
  hist(sp.resp_slope_est_Q1[1000*(i-1)+1:1000*i,3],main=sp_common_name[i])
}

sp.resp_slope_Q3 = sp.resp_slope_Q3[,-1]
sp.resp_slope_est_Q3 = sp.resp_slope_Q3[,c(1,8,15)]
par(mfrow=c(3,3))
for(i in 1:9) {
  hist(sp.resp_slope_est_Q3[1000*(i-1)+1:1000*i,1],main=sp_common_name[i])
}
for(i in 1:9) {
  hist(sp.resp_slope_est_Q3[1000*(i-1)+1:1000*i,2],main=sp_common_name[i])
}
for(i in 1:9) {
  hist(sp.resp_slope_est_Q3[1000*(i-1)+1:1000*i,3],main=sp_common_name[i])
}
