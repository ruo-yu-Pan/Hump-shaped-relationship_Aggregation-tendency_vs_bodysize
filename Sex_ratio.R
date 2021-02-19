library(dplyr)
library("reshape2")

setwd("D:/Ruo_data/2019_Paper_Publish/Publish")

# import exchange data
IBTS_exchange_data_Q1 = read.csv("./SizeAggregTend_data/raw_data/SexRatio/Exchange_Data_Q1_9sp.csv",stringsAsFactors = F)
IBTS_exchange_data_Q3 = read.csv("./SizeAggregTend_data/raw_data/SexRatio/Exchange_Data_Q3_9sp.csv",stringsAsFactors = F)

sp_name <- c("Clupea harengus","Gadus morhua","Melanogrammus aeglefinus",
             "Merlangius merlangus","Pleuronectes platessa", 
             "Pollachius virens","Scomber scombrus",
             "Sprattus sprattus", "Trisopterus esmarkii")
sp_name_new <- c("Clupea harengus","Gadus morhua","Melanogrammus aeglefinus",
             "Merlangius merlangus","Pleuronectes platessa", 
             "Pollachius virens","Scomber scombrus","Trisopterus esmarkii") ##no sprat
sp_c_name_new <- c("Herring","Cod","Haddock","Whiting","Plaice","Saithe",
                   "Mackerel","Norwaypout") # no sprat

# Q1
sp_sex_foreachyear_1 = list()
sp_size_year_detail_1 = list()

for(i in c(1:7,9)){
  sp_data = IBTS_exchange_data_Q1 %>%
    filter(ScientificName_WoRMS == sp_name[[i]],Sex!="U")%>%
    group_by(Year,Sex,LngtClass) %>%
    select(Year,Sex,LngtCode,LngtClass,HLNoAtLngt)
  
  sp_data$LngtClass[which(sp_data$LngtCode=="1")]= sp_data$LngtClass[which(sp_data$LngtCode=="1")]*10
  sp_w_data = dcast(sp_data,Year+LngtClass~Sex,value.var=c('HLNoAtLngt'), sum)
  colnames(sp_w_data)[3:4]=c("Female","Male")
  sp_w_data$LngtClass[which(sp_w_data$LngtClass%/%100==0)]=sp_w_data$LngtClass[which(sp_w_data$LngtClass%/%100==0)]*10
  sp_sex_foreachyear_1[[i]] = sp_w_data 
  
  sp_size_year_detail_1[[i]] = dcast(sp_w_data,Year~LngtClass)
}


lacking_sex_data_1 = NULL
avail_sex_data_detail_1 = NULL

for(i in c(1:7,9)){
  sp_lack_sex_data = sp_sex_foreachyear_1[[i]]%>% filter(Female==0| Male==0)
  lacking_sex_data_1 = rbind(lacking_sex_data_1, cbind(rep(sp_name[i],nrow(sp_lack_sex_data)),sp_lack_sex_data))
  
  sp_avail_sex_data = sp_sex_foreachyear_1[[i]]%>% filter(Female!=0, Male!=0)
  detail = cbind(rep(sp_name[i],length(unique(sp_avail_sex_data$Year))),
                 tapply(sp_avail_sex_data$LngtClass,sp_avail_sex_data$Year,max),
                 tapply(sp_avail_sex_data$LngtClass,sp_avail_sex_data$Year,min))
  avail_sex_data_detail_1 = rbind(avail_sex_data_detail_1,detail)
}



# Q3
sp_sex_foreachyear_3 = list()
sp_size_year_detail_3= list()

for(i in c(1:7,9)){
  sp_data = IBTS_exchange_data_Q3 %>%
    filter(ScientificName_WoRMS == sp_name[i],Sex!="U")%>%
    group_by(Year,Sex,LngtClass) %>%
    select(Year,Sex,LngtCode,LngtClass,HLNoAtLngt)
  
  sp_data$LngtClass[which(sp_data$LngtCode=="1")]= sp_data$LngtClass[which(sp_data$LngtCode=="1")]*10
  sp_w_data = dcast(sp_data,Year+LngtClass~Sex,value.var=c('HLNoAtLngt'), sum)
  colnames(sp_w_data)[3:4]=c("Female","Male")
  sp_w_data$LngtClass[which(sp_w_data$LngtClass%/%100==0)]=sp_w_data$LngtClass[which(sp_w_data$LngtClass%/%100==0)]*10
  sp_sex_foreachyear_3[[i]] = sp_w_data 
  
  sp_size_year_detail_3[[i]] = dcast(sp_w_data,Year~LngtClass)
}


lacking_sex_data_3 = NULL
avail_sex_data_detail_3 = NULL

for(i in c(1:7,9)){
  sp_lack_sex_data = sp_sex_foreachyear_3[[i]]%>% filter(Female==0| Male==0)
  lacking_sex_data_3 = rbind(lacking_sex_data_3, cbind(rep(sp_name[i],nrow(sp_lack_sex_data)),sp_lack_sex_data))
  
  sp_avail_sex_data = sp_sex_foreachyear_3[[i]]%>% filter(Female!=0, Male!=0)
  detail = cbind(rep(sp_name[i],length(unique(sp_avail_sex_data$Year))),
                 tapply(sp_avail_sex_data$LngtClass,sp_avail_sex_data$Year,max),
                 tapply(sp_avail_sex_data$LngtClass,sp_avail_sex_data$Year,min))
  avail_sex_data_detail_3 = rbind(avail_sex_data_detail_3,detail)
}

# save data
save(sp_sex_foreachyear_1,   sp_sex_foreachyear_3,
     sp_size_year_detail_1,  sp_size_year_detail_3,
     lacking_sex_data_1,     lacking_sex_data_3,
     avail_sex_data_detail_1,avail_sex_data_detail_3, file="./sex ratio data/sex_data_detail.RData")


############## sex ratio ##############

# Calculate Sex ratio

calc_sexratio_func = function(x){x %>% filter(Female!=0, Male!=0)%>% mutate(SexRatio=Female/Male)}

sp_sex_foreachyear_1 = sp_sex_foreachyear_1[-8]
sp_sexratio_foreachyear_1 = lapply(sp_sex_foreachyear_1,calc_sexratio_func)

sp_sex_foreachyear_3 = sp_sex_foreachyear_3[-8]
sp_sexratio_foreachyear_3 = lapply(sp_sex_foreachyear_3,calc_sexratio_func)


# transfer to wide table (Year vs. LngtClass)
wtable_func = function(x){x %>% dcast(Year~LngtClass,value.var=c('SexRatio'))}
sexratio_byLandY_1 = lapply(sp_sexratio_foreachyear_1, wtable_func)
sexratio_byLandY_3 = lapply(sp_sexratio_foreachyear_3, wtable_func)

# Plot the scatter plot for SexRatio vs. LngtClass
# investigating SexRatio vs. LngtClass trend
Sexratio_vs_size_trend = NULL

jpeg("./SizeAggregTend_data/output/fig/Appendix/Fig_sexratio_vs_lengthclass_Q1.jpeg", width=6, height=5, units = "in",res=300)
par(mfrow=c(3,3),mar=c(2,2,3,1),oma=c(3,3,1,1))
for(i in 1:8){
  
  sp_sex_size_trend = lm(log(SexRatio)~LngtClass, data=sp_sexratio_foreachyear_1[[i]])
  
  new_length = seq(min(sp_sexratio_foreachyear_1[[i]]$LngtClass),
                   max(sp_sexratio_foreachyear_1[[i]]$LngtClass),by = 1)
  trend_CI <- predict(sp_sex_size_trend, 
                      newdata=data.frame(LngtClass=new_length), 
                      interval="confidence",
                      level = 0.95)
  
  plot(log(SexRatio)~LngtClass,data=sp_sexratio_foreachyear_1[[i]],
       main=sp_c_name_new[i],col=rgb(120, 120, 120, 60, maxColorValue=255))
  
  #points(log(SexRatio)~LngtClass,data=sp_sexratio_foreachyear_1[[i]],
  #     main=sp_c_name_new[i],col=rgb(20, 20, 20, 60, maxColorValue=255))
  abline(h=0,col=2)
  
  polygon(c(rev(new_length), new_length), 
          c(rev(trend_CI[ ,3]), trend_CI[ ,2]), 
          col = rgb(150, 150, 150, 80, maxColorValue=255), border = NA)
  if(summary(sp_sex_size_trend)$coefficients[2,4]<=0.05){abline(sp_sex_size_trend)
  }else{abline(sp_sex_size_trend,lty=2)}
  
  Sexratio_vs_size_trend = rbind(Sexratio_vs_size_trend,summary(sp_sex_size_trend)$coefficient)
}

mtext("Length class",side=1,outer=T,line=1)
mtext("log(Sex ratio)",side=2,outer=T,line=1)
dev.off()

jpeg("./SizeAggregTend_data/output/fig/Appendix/Fig_sexratio_vs_lengthclass_Q3.jpeg", width=6, height=5, units = "in",res=300)
par(mfrow=c(3,3),mar=c(2,2,3,1),oma=c(3,3,1,1))
for(i in 1:8){
  
  sp_sex_size_trend = lm(log(SexRatio)~LngtClass, data=sp_sexratio_foreachyear_3[[i]])
  
  new_length = seq(min(sp_sexratio_foreachyear_3[[i]]$LngtClass),
                   max(sp_sexratio_foreachyear_3[[i]]$LngtClass),by = 1)
  trend_CI <- predict(sp_sex_size_trend, 
                      newdata=data.frame(LngtClass=new_length), 
                      interval="confidence",
                      level = 0.95)
  
  plot(log(SexRatio)~LngtClass,data=sp_sexratio_foreachyear_3[[i]],
       main=sp_c_name_new[i],col=rgb(120, 120, 120, 60, maxColorValue=255))
  
  #points(log(SexRatio)~LngtClass,data=sp_sexratio_foreachyear_1[[i]],
  #     main=sp_c_name_new[i],col=rgb(20, 20, 20, 60, maxColorValue=255))
  abline(h=0,col=2)
  
  polygon(c(rev(new_length), new_length), 
          c(rev(trend_CI[ ,3]), trend_CI[ ,2]), 
          col = rgb(150, 150, 150, 80, maxColorValue=255), border = NA)
  if(summary(sp_sex_size_trend)$coefficients[2,4]<=0.05){abline(sp_sex_size_trend)
  }else{abline(sp_sex_size_trend,lty=2)}
  
  Sexratio_vs_size_trend = rbind(Sexratio_vs_size_trend,summary(sp_sex_size_trend)$coefficient)
}
mtext("Length class",side=1,outer=T,line=1)
mtext("log(Sex ratio)",side=2,outer=T,line=1)
dev.off()

write.csv(Sexratio_vs_size_trend,"./SizeAggregTend_data/output/sexratio_vs_lengthclass.csv")
