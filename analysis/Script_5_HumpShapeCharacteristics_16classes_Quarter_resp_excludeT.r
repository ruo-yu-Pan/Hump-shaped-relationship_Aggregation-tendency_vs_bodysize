
##########################################################################
# Examine the characteristics of hump shape for each species :
# peak, peak location, uphill slope, downhill slope 
##########################################################################
setwd("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis")
sp_TL.result <- read.csv("sp_TL.result_all_16classes.csv",header = T)
sp_TL.result$sp_ID <- as.factor(sp_TL.result$sp_ID)

library("mgcv")
library("itsadug")
library("tidymv")


####### species fitted respectively #################################
sp_common_name <- c("Herring","Cod","Haddock","Whiting","Plaice",
                    "Saithe", "Mackerel","Sprat","Norwaypout")

sp_fit_summary <- list()
sp_resp_humppeak <- NULL
sp.resp_slope <- NULL



jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\sp_resp_fit_byQ.jpeg", width=6, height=5, units = "in",res=300)

par(mfrow=c(3,3),mar=c(4,3,3,1),oma=c(2,2,1,0),xpd=F)
for(i in 1:9){
    sp_fit <- gam(b~quarter+
                    s(std_L,by=quarter,bs="ts"),
                  data=sp_TL.result[which(sp_TL.result$sp_ID==i),],
                  method = "REML")
    
    # collect fitting result
    sp_fit_summary[[i]] <- rbind(summary(sp_fit)$p.table,summary(sp_fit)$s.table)
  
    # plot the fitting of each species
    plot_smooth(sp_fit,view = "std_L",cond = list(quarter="Q1"),
                col = c("grey40"),lty = 2,
                xlim = c(max(sp_TL.result$std_L[(32*i-31):(32*i-16)])+0.15,min(sp_TL.result$std_L[(32*i-31):(32*i-16)])-0.15),
                ylim = c(0.5,3.5),
                family="newrom",xlab = "",
                hide.label = T)
    plot_smooth(sp_fit,view = "std_L",cond = list(quarter="Q3"),
                col = c("black"),
                xlim = c(max(sp_TL.result$std_L[(32*i-15):(32*i)])+0.15,min(sp_TL.result$std_L[(32*i-15):(32*i)])-0.15),
                ylim = c(0.5,3.5),
                family="newrom",xlab = "",add=T,
                hide.label = T)
    points(sp_TL.result$std_L[(32*i-31):(32*i-16)],
           sp_TL.result$b[(32*i-31):(32*i-16)],col="grey10",cex=0.8)
    points(sp_TL.result$std_L[(32*i-15):(32*i)],
           sp_TL.result$b[(32*i-15):(32*i)],pch=19,col="grey10",cex=0.8)
    abline(v=0,lty=2,col="grey30")
    title(paste("(",letters[i],") ",sp_common_name[i]),adj=0,family="newrom")

    
    ## hump shape peak ##
    
    p_nd_Q1 <- expand.grid(std_L = seq(-0.852,0.788,length=500),
                            quarter = factor(c("Q1")))
    pred_b_Q1 <- predict(sp_fit, newdata=p_nd_Q1,type="response")
    p_nd_Q1$fit <- pred_b_Q1

    p_nd_Q3 <- expand.grid(std_L = seq(-0.852,0.788,length=500),
                         quarter = factor(c("Q3")))
    pred_b_Q3 <- predict(sp_fit, newdata=p_nd_Q3,type="response")
    p_nd_Q3$fit <- pred_b_Q3
  
    main <- c(max(sp_TL.result$std_L[(i*32-31):(i*32)]),min(sp_TL.result$std_L[(i*32-31):(i*32)]))
  
    pred_b_main_Q1 <- pred_b_Q1[which(p_nd_Q1$std_L<main[1] & p_nd_Q1$std_L>main[2])]
    pred_b_main_Q3 <- pred_b_Q3[which(p_nd_Q3$std_L<main[1] & p_nd_Q3$std_L>main[2])]
  
    p_nd_main_Q1 <- p_nd_Q1$std_L[which(p_nd_Q1$std_L<main[1] & p_nd_Q1$std_L>main[2])]
    p_nd_main_Q3 <- p_nd_Q3$std_L[which(p_nd_Q3$std_L<main[1] & p_nd_Q3$std_L>main[2])]
  
    max_sp.resp_b_Q1 <- max(pred_b_main_Q1)
    max_sp.resp_b_Q3 <- max(pred_b_main_Q3)
    peak_loc_Q1 <- p_nd_main_Q1[which(pred_b_main_Q1==max_sp.resp_b_Q1)]
    peak_loc_Q3 <- p_nd_main_Q3[which(pred_b_main_Q3==max_sp.resp_b_Q3)]
  
    sp_resp_humppeak <- rbind(sp_resp_humppeak,
                              c(max_sp.resp_b_Q1,peak_loc_Q1),
                              c(max_sp.resp_b_Q3,peak_loc_Q3))
    
    
    ## hump shape slope ##
    
    # Q1
    sp_TL.1 <- sp_TL.result[which(sp_TL.result$sp_ID==i & sp_TL.result$quarter=="Q1"),]
    sm_pred_1 <- sp_TL.1[which(sp_TL.1$std_L<peak_loc_Q1 ),]
    lar_pred_1 <- sp_TL.1[which(sp_TL.1$std_L>=peak_loc_Q1),]
    
    if(length(sm_pred_1[,1])>1) {
      sm_line_fit_1 <- summary(lm(b~std_L,sm_pred_1))
      sm_line_Rst_1 <- c(sm_line_fit_1$coefficients[2,],sm_line_fit_1$r.squared)
      }else{sm_line_Rst_1 <- matrix(0,nrow = 1,ncol=5)}
    
    if(length(lar_pred_1[,1])>1){
      lar_line_fit_1 <- summary(lm(b~std_L,lar_pred_1))
      lar_line_Rst_1 <- c(lar_line_fit_1$coefficients[2,],lar_line_fit_1$r.squared)
      }else{lar_line_Rst_1 <- matrix(0,nrow = 1,ncol=5)}
    
    
    # Q3
    sp_TL.3 <- sp_TL.result[which(sp_TL.result$sp_ID==i & sp_TL.result$quarter=="Q3"),]
    sm_pred_3 <- sp_TL.3[which(sp_TL.3$std_L<peak_loc_Q3),]
    lar_pred_3 <- sp_TL.3[which(sp_TL.3$std_L>=peak_loc_Q3),]
    
    if(length(sm_pred_3[,1])>1) {
      sm_line_fit_3 <- summary(lm(b~std_L,sm_pred_3))
      sm_line_Rst_3 <- c(sm_line_fit_3$coefficients[2,],sm_line_fit_3$r.squared)
      }else{sm_line_Rst_3 <- matrix(0,nrow = 1,ncol=5)}
    
    if(length(lar_pred_3[,1])>1){
      lar_line_fit_3 <- summary(lm(b~std_L,lar_pred_3))
      lar_line_Rst_3 <- c(lar_line_fit_1$coefficients[2,],lar_line_fit_3$r.squared)
      }else{lar_line_Rst_3 <- matrix(0,nrow = 1,ncol=5)}
    
    
    
    
    
    sp.resp_slope <- rbind(sp.resp_slope,
                           #Q1
                           cbind(
                             rbind(sm_line_Rst_1,
                                   lar_line_Rst_1),
                             c(length(sm_pred_1[,1]),
                               length(lar_pred_1[,1]))),
                           #Q3
                           cbind(
                             rbind(sm_line_Rst_3,
                                   lar_line_Rst_3),
                             c(length(sm_pred_3[,1]),
                               length(lar_pred_3[,1])))
                           )
    
    
}

mtext("Taylor's exponents",outer = T,cex=1,side=2,family="newrom")
mtext("Standardized length",outer = T,cex=1,side=1,family="newrom")

dev.off()

sp_resp_humppeak <- as.data.frame(sp_resp_humppeak)
sp_resp_humppeak$quarter <- rep(c("Q1","Q3"),9)

sp.resp_slope <- as.data.frame(sp.resp_slope)
sp.resp_slope$sp <- rep(c(1:9),each=4)

write.csv(sp_fit_summary,"sp_fit_summary_16class_byQ_excludeT.csv")
write.csv(sp_resp_humppeak,"sp_resp_humppeak_16class_byQ_excludeT.csv")
write.csv(sp.resp_slope,"sp.resp_slope_16class_byQ_excludeT.csv")


