
windowsFonts(newrom = windowsFont("Times New Roman"))

library(dplyr)
library(mgcv)

setwd("D:/Ruo_data/2019_Paper_Publish/Publish")

IBTS_SMALK = read.csv("./SizeAggregTend_data/raw_data/Maturity/SMALK_data.csv",stringsAsFactors = F)
sp_name <- c("Clupea harengus","Gadus morhua","Melanogrammus aeglefinus",
             "Merlangius merlangus","Pleuronectes platessa", 
             "Pollachius virens","Scomber scombrus",
             "Sprattus sprattus", "Trisopterus esmarkii")
sp_common_name <- c("Herring","Cod","Haddock","Whiting","Plaice","Saithe",
                    "Mackerel","Sprat","Norwaypout")



IBTS_SMALK_avail_maturity = IBTS_SMALK %>% filter(Maturity!=-9)

L5 = NULL
L50 = NULL
L95 = NULL

par(mfrow=c(3,3),mar=c(2,2,3,1))
for(i in 1:9){
  sp_mature_data = IBTS_SMALK_avail_maturity %>% 
    filter(Species == sp_name[i]) %>%
    dplyr::select(Year, Quarter,LngtClass,Maturity,NoAtLngt) 
  
  sp_mature_data$Converted_maturity = NA
  sp_mature_data$Converted_maturity[sp_mature_data$Maturity>60] = sp_mature_data$Maturity[sp_mature_data$Maturity>60]%%10
  sp_mature_data$Converted_maturity[sp_mature_data$Maturity<60] = sp_mature_data$Maturity[sp_mature_data$Maturity<60]
  
  sp_mature_data$Converted_maturity[sp_mature_data$Converted_maturity>=3] = 3
  
  
  # flatten the NoAtLngt (if NoAtLngt=3, repeat the row three times)
  sp_mature_data_extend <- sp_mature_data[rep(seq_len(nrow(sp_mature_data)), sp_mature_data[,5]), ]
  
  # glm
  
  # logistic
  sp_mature_data_logistic = sp_mature_data_extend %>%
    filter(Converted_maturity==1|Converted_maturity==3)
  sp_mature_data_logistic$Converted_maturity[sp_mature_data_logistic$Converted_maturity==1]=0
  sp_mature_data_logistic$Converted_maturity[sp_mature_data_logistic$Converted_maturity==3]=1
  
  maturity_logistic_model = glm(Converted_maturity~LngtClass, 
                                data= sp_mature_data_logistic[sp_mature_data_logistic$Quarter==1,],family = binomial)
  
  xLngt = sort(unique(sp_mature_data_logistic$LngtClass[sp_mature_data_logistic$Quarter==1]))
  ymaturity = predict(maturity_logistic_model, list(LngtClass = xLngt),type="response")
  
  plot(Converted_maturity~LngtClass,data=sp_mature_data_extend[sp_mature_data_extend$Quarter==1,],main=sp_common_name[i],col="grey")
  lines(xLngt,ymaturity*2+1,lwd=2)
  
  # plot
  #plot(Converted_maturity~LngtClass,data=sp_mature_data_extend[sp_mature_data_extend$Quarter==1,],main=sp_name[i])
  
  size_at_maturity_fuc = function(p,model){(log(p/(1-p)) - coef(model)[1])/coef(model)[2]}
  L5 = c(L5,size_at_maturity_fuc(0.05,maturity_logistic_model))
  # average length at first maturity
  L50 = c(L50,size_at_maturity_fuc(0.5,maturity_logistic_model))
  # length at mass maturity
  L95 = c(L95,size_at_maturity_fuc(0.95,maturity_logistic_model))
}

# Q3
L5_Q3 = NULL
L50_Q3 = NULL
L95_Q3 = NULL

par(mfrow=c(3,3),mar=c(2,2,3,1))
for(i in 1:9){
  sp_mature_data = IBTS_SMALK_avail_maturity %>% 
    filter(Species == sp_name[i]) %>%
    dplyr::select(Year, Quarter,LngtClass,Maturity,NoAtLngt) 
  
  sp_mature_data$Converted_maturity = NA
  sp_mature_data$Converted_maturity[sp_mature_data$Maturity>60] = sp_mature_data$Maturity[sp_mature_data$Maturity>60]%%10
  sp_mature_data$Converted_maturity[sp_mature_data$Maturity<60] = sp_mature_data$Maturity[sp_mature_data$Maturity<60]
  
  sp_mature_data$Converted_maturity[sp_mature_data$Converted_maturity>=3] = 3
  
  
  # flatten the NoAtLngt (if NoAtLngt=3, repeat the row three times)
  sp_mature_data_extend <- sp_mature_data[rep(seq_len(nrow(sp_mature_data)), sp_mature_data[,5]), ]
  
  # glm
  
  # logistic
  sp_mature_data_logistic = sp_mature_data_extend %>%
    filter(Converted_maturity==1|Converted_maturity==3)
  sp_mature_data_logistic$Converted_maturity[sp_mature_data_logistic$Converted_maturity==1]=0
  sp_mature_data_logistic$Converted_maturity[sp_mature_data_logistic$Converted_maturity==3]=1
  
  maturity_logistic_model = glm(Converted_maturity~LngtClass, 
                                data= sp_mature_data_logistic[sp_mature_data_logistic$Quarter==3,],family = binomial)
  
  xLngt = sort(unique(sp_mature_data_logistic$LngtClass[sp_mature_data_logistic$Quarter==3]))
  ymaturity = predict(maturity_logistic_model, list(LngtClass = xLngt),type="response")
  
  plot(Converted_maturity~LngtClass,data=sp_mature_data_extend[sp_mature_data_extend$Quarter==3,],main=sp_common_name[i],col="grey")
  lines(xLngt,ymaturity*2+1,lwd=2)
  
  # plot
  #plot(Converted_maturity~LngtClass,data=sp_mature_data_extend[sp_mature_data_extend$Quarter==1,],main=sp_name[i])
  
  size_at_maturity_fuc = function(p,model){(log(p/(1-p)) - coef(model)[1])/coef(model)[2]}
  L5_Q3 = c(L5_Q3,size_at_maturity_fuc(0.05,maturity_logistic_model))
  # average length at first maturity
  L50_Q3 = c(L50_Q3,size_at_maturity_fuc(0.5,maturity_logistic_model))
  # length at mass maturity
  L95_Q3 = c(L95_Q3,size_at_maturity_fuc(0.95,maturity_logistic_model))
}

size_at_maturity_stage = as.data.frame(cbind(c(L5,L5_Q3),c(L50,L50_Q3),c(L95,L95_Q3)))
size_at_maturity_stage$species = rep(c(1:9),2)
size_at_maturity_stage$quarter = rep(c("Q1","Q3"),each=9)
colnames(size_at_maturity_stage) = c("L05","L50","L95")

write.csv(size_at_maturity_stage,"D:/Ruo_data/2019_Paper_Publish/Publish/SizeAggregTend_data/compiled/size_at_maturityStage_byQ.csv")




# plot Q1 and Q3 fitted line together ###
jpeg("./SizeAggregTend_data/output/fig/Appendix/Fig_maturation_schedule_byQ.jpeg", width=6.5, height=5, units = "in",res=300)

par(mfrow=c(3,3),mar=c(2,2,3,1),oma=c(3,3,0,0),xpd=F)

for(i in 1:9){
  sp_mature_data = IBTS_SMALK_avail_maturity %>% 
    filter(Species == sp_name[i]) %>%
    dplyr::select(Year, Quarter,LngtClass,Maturity,NoAtLngt) 
  
  sp_mature_data$Converted_maturity = NA
  sp_mature_data$Converted_maturity[sp_mature_data$Maturity>60] = sp_mature_data$Maturity[sp_mature_data$Maturity>60]%%10
  sp_mature_data$Converted_maturity[sp_mature_data$Maturity<60] = sp_mature_data$Maturity[sp_mature_data$Maturity<60]
  
  sp_mature_data$Converted_maturity[sp_mature_data$Converted_maturity>=3] = 3
  
  
  # flatten the NoAtLngt (if NoAtLngt=3, repeat the row three times)
  sp_mature_data_extend <- sp_mature_data[rep(seq_len(nrow(sp_mature_data)), sp_mature_data[,5]), ]
  
  # glm
  
  # logistic
  sp_mature_data_logistic = sp_mature_data_extend %>%
    filter(Converted_maturity==1|Converted_maturity==3)
  sp_mature_data_logistic$Converted_maturity[sp_mature_data_logistic$Converted_maturity==1]=0
  sp_mature_data_logistic$Converted_maturity[sp_mature_data_logistic$Converted_maturity==3]=1
  
  maturity_logistic_model_Q1 = glm(Converted_maturity~LngtClass, 
                                data= sp_mature_data_logistic[sp_mature_data_logistic$Quarter==1,],family = binomial)
  
  xLngt_Q1 = sort(unique(sp_mature_data_logistic$LngtClass[sp_mature_data_logistic$Quarter==1]))
  ymaturity_Q1 = predict(maturity_logistic_model_Q1, list(LngtClass = xLngt_Q1),se.fit = T)

  
  maturity_logistic_model_Q3 = glm(Converted_maturity~LngtClass, 
                                   data= sp_mature_data_logistic[sp_mature_data_logistic$Quarter==3,],family = binomial)
  
  xLngt_Q3 = sort(unique(sp_mature_data_logistic$LngtClass[sp_mature_data_logistic$Quarter==3]))
  ymaturity_Q3 = predict(maturity_logistic_model_Q3, list(LngtClass = xLngt_Q3),se.fit = T)
  
  # plot
  xLngt_lim = c(min(sp_mature_data_logistic$LngtClass),max(sp_mature_data_logistic$LngtClass))
  
  plot(xLngt_Q1,(exp(ymaturity_Q1$fit)/(1+exp(ymaturity_Q1$fit))),main=sp_common_name[i],col="grey50",type="l",xlim = xLngt_lim)
  polygon(c(rev(xLngt_Q1), xLngt_Q1), 
          with(ymaturity_Q1,
               c(rev((exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)))), 
                 (exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit))))), 
          col = rgb(120, 120, 120, 80, maxColorValue=255), border = NA)
  
  
  lines(xLngt_Q3,(exp(ymaturity_Q3$fit)/(1+exp(ymaturity_Q3$fit))),xlim = xLngt_lim)
  polygon(c(rev(xLngt_Q3), xLngt_Q3), 
          with(ymaturity_Q3,
               c(rev((exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)))), 
                 (exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit))))), 
          col = rgb(120, 120, 120, 80, maxColorValue=255), border = NA)
  
  abline(h = 0.05,lty=2)
  abline(h = 0.95,lty=2)
}
mtext("Length Class",side=1,outer=T,line=1)
mtext("% mature",side=2,outer=T,line=1)

dev.off() 
