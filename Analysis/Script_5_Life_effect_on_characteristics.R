
library(dplyr)

### import data

wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)
sp_common_name <- c("Herring","Cod","Haddock","Whiting","Plaice",
                    "Saithe", "Mackerel","Sprat","Norwaypout")
size_at_maturity_stage = read.csv("./SizeAggregTend_data/compiled/size_at_maturityStage_byQ.csv")
sp.mid.L <- as.matrix(read.table("./SizeAggregTend_data/compiled/allsp_length_mid_16class.txt"))
mature.L <- as.matrix(read.table("./SizeAggregTend_data/compiled/mature_L.txt"))

# TL result
sp_TL.result <- read.csv("./SizeAggregTend_data/output/1_size_TL/all_TL_result_16_compiled.csv",stringsAsFactors = F,sep=",",header = T)
sp_TL.result$spwn <- as.factor(sp_TL.result$spwn)
sp_TL.result$quarter <- as.factor(sp_TL.result$quarter)
sp_TL.result$sp_ID <- as.factor(sp_TL.result$sp_ID)
sp_TL.result$mat_stage <- as.factor(sp_TL.result$mat_stage)

# TL humpshape characteristics
sp_resp_humppeak <- read.csv("./SizeAggregTend_data/output/3_humpshape_characteristics/sp_resp_humppeak.csv",header=T)
sp_resp_slope_Q1    <- read.csv("./SizeAggregTend_data/output/3_humpshape_characteristics/sp_resp_slope_Q1.csv",header=T)
sp_resp_slope_Q3    <- read.csv("./SizeAggregTend_data/output/3_humpshape_characteristics/sp_resp_slope_Q3.csv",header=T)
all_sp_peak_slope_95CI <- read.csv("./SizeAggregTend_data/output/3_humpshape_characteristics/sp_resp_peak_slope_95CI.csv")
load("./SizeAggregTend_data/output/3_humpshape_characteristics/sp_bs_pred_Q1.RData")
load("./SizeAggregTend_data/output/3_humpshape_characteristics/sp_bs_pred_Q3.RData")

# Life history/ life style/ fishing mortality data
Life_data <- read.csv("./SizeAggregTend_data/compiled/Thorpe2015_Life_history_traits.csv",header = T)
L.history <- Life_data[1:9,c(3,4,5)]
L.history$Linf <- L.history$Linf*10
L.history$L50 <- L.history$L50*10
L.history2 <- as.data.frame(apply(L.history,2,function(x)rep(x,each=2)))

ts <- as.character(Life_data$life_style)
ts[which(ts=="pelagic")]="P"
ts[which(ts=="benthopelagic")]="B"
ts[which(ts=="demersal")]="D"
L.history$life_style <- as.factor(ts[1:9])
ls <- as.factor(rep(ts,each=2))

fish_M <- read.csv("./SizeAggregTend_data/compiled/fishingM.csv")
mean_fish_M <- tapply(fish_M$F,fish_M$Species,mean)
mean_F_2 <- rep(mean_fish_M,each=2)


# species-specific relationship hump shaped characteristics

sp_resp_humppeak$quarter <- rep(c("Q1","Q1","Q3","Q3"),9)
sp_resp_humppeak$charc <- factor(rep(c("max_b","peak_loc"),18))
sp_resp_humppeak_Q1 <- sp_resp_humppeak %>% filter(quarter=="Q1")
sp_resp_humppeak_Q3 <- sp_resp_humppeak %>% filter(quarter=="Q3")

newdat_Q1 <- expand.grid(std_L = seq(-0.69,0.75,length=200),
                         quarter = c("Q1"))
newdat_Q3 <- expand.grid(std_L = seq(-0.69,0.75,length=200),
                         quarter = c("Q3"))


### prepare data 
# record whether there are data for examing the slope

# in Q1--> sp5 matured/sp8 matured/sp9 matured
# in Q3--> sp2 matured/sp5 immature/sp8 matured/sp9 matured

sp_origin_slope <- NULL
for(i in 1:9){
  sp_origin_slope <- rbind(sp_origin_slope,sp_resp_slope_Q1[1000*(i-1)+1,c(2,9,16)],sp_resp_slope_Q3[1000*(i-1)+1,c(2,9,16)])
}
sp_origin_slope[sp_origin_slope==0]<- NA

par(mfrow=c(3,3),mar=c(2,2,1,1),oma=c(3,3,0,0))

plot(L.history2$Linf,sp_origin_slope$Estimate,ylim=c(-2,1))
plot(L.history2$K,sp_origin_slope$Estimate,ylim=c(-2,1))
plot(L.history2$L50,sp_origin_slope$Estimate,ylim=c(-2,1))

plot(L.history2$Linf,sp_origin_slope$Estimate.1,ylim=c(-2,1))
plot(L.history2$K,sp_origin_slope$Estimate.1,ylim=c(-2,1))
plot(L.history2$L50,sp_origin_slope$Estimate.1,ylim=c(-2,1))

plot(L.history2$Linf,sp_origin_slope$Estimate.2,ylim=c(-2,1))
plot(L.history2$K,sp_origin_slope$Estimate.2,ylim=c(-2,1))
plot(L.history2$L50,sp_origin_slope$Estimate.2,ylim=c(-2,1))


# bootstrapping
bs_potentialFactors_slope_COR_result <- NULL

for(bs in 1:1000){
  #sp_bs_slope <- NULL
  sp_bs_slope_Q1 <- NULL
  sp_bs_slope_Q3 <- NULL
  for(i in 1:9){
    #sp_bs_slope <- rbind(sp_bs_slope,sp_resp_slope_Q1[1000*(i-1)+bs,c(2,9,16)],sp_resp_slope_Q3[1000*(i-1)+bs,c(2,9,16)])
    sp_bs_slope_Q1 <- rbind(sp_bs_slope_Q1,sp_resp_slope_Q1[1000*(i-1)+bs,c(2,9,16)])
    sp_bs_slope_Q3 <- rbind(sp_bs_slope_Q3,sp_resp_slope_Q3[1000*(i-1)+bs,c(2,9,16)])
  }
  #sp_bs_slope[sp_bs_slope==0]<- NA
  sp_bs_slope_Q1[sp_bs_slope_Q1==0]<- NA
  sp_bs_slope_Q3[sp_bs_slope_Q3==0]<- NA
  
  for(m in 1:3){
    
    # life history traits effect
    for(l in 1:3){
      #bs_lifehis_slope_COR <- cor(L.history2[,l],sp_bs_slope[,m],method = "spearman", use="complete.obs")
      #bs_lifehis_slope_COR_result <- c(bs_lifehis_slope_COR_result,bs_lifehis_slope_COR)
      
      bs_lifehis_slope_COR_Q1 <- cor.test(L.history[,l],sp_bs_slope_Q1[,m],method = "spearman", use="complete.obs",exact = F)
      bs_lifehis_slope_COR_Q3 <- cor.test(L.history[,l],sp_bs_slope_Q3[,m],method = "spearman", use="complete.obs",exact = F)
      
      bs_potentialFactors_slope_COR_result <- rbind(bs_potentialFactors_slope_COR_result,
                                                    c(bs_lifehis_slope_COR_Q1$estimate,bs_lifehis_slope_COR_Q1$p.value),
                                                    c(bs_lifehis_slope_COR_Q3$estimate,bs_lifehis_slope_COR_Q3$p.value))
    }
    
    # fishing mortality effect
      bs_fishM_slope_COR_Q1 <- cor.test(mean_fish_M,sp_bs_slope_Q1[,m],method = "spearman", use="complete.obs",exact = F)
      bs_fishM_slope_COR_Q3 <- cor.test(mean_fish_M,sp_bs_slope_Q3[,m],method = "spearman", use="complete.obs",exact = F)
      
      bs_potentialFactors_slope_COR_result <- rbind(bs_potentialFactors_slope_COR_result,
                                                    c(bs_fishM_slope_COR_Q1$estimate,bs_fishM_slope_COR_Q1$p.value),
                                                    c(bs_fishM_slope_COR_Q3$estimate,bs_fishM_slope_COR_Q3$p.value))
    
    
    # life style effect
      bs_lifesty_slope_diff_Q1 <- wilcox.test(sp_bs_slope_Q1[-5,m]~ts[-5])
      bs_lifesty_slope_diff_Q3 <- wilcox.test(sp_bs_slope_Q3[-5,m]~ts[-5])
      bs_potentialFactors_slope_COR_result <- rbind(bs_potentialFactors_slope_COR_result,
                                                    c(bs_lifesty_slope_diff_Q1$statistic[1],bs_lifesty_slope_diff_Q1$p.value),
                                                    c(bs_lifesty_slope_diff_Q3$statistic[1],bs_lifesty_slope_diff_Q3$p.value))
  }
}

range(sp_bs_slope_Q1[,1],na.rm=T)
range(sp_bs_slope_Q1[,2],na.rm=T)
range(sp_bs_slope_Q1[,3],na.rm=T)
range(sp_bs_slope_Q3[,1],na.rm=T)
range(sp_bs_slope_Q3[,2],na.rm=T)
range(sp_bs_slope_Q3[,3],na.rm=T)

# correlation is not significant
No_p_sig_Q1 = length(which(bs_potentialFactors_slope_COR_result[seq(1,nrow(bs_potentialFactors_slope_COR_result),2),2]<=0.05))
No_p_sig_Q3 = length(which(bs_potentialFactors_slope_COR_result[seq(2,nrow(bs_potentialFactors_slope_COR_result),2),2]<=0.05))

bs_potentialFactors_slope_COR_result <- as.data.frame(bs_potentialFactors_slope_COR_result)
colnames(bs_potentialFactors_slope_COR_result)=c("statis_val","p-value")
bs_potentialFactors_slope_COR_result$potenFactor <- rep(rep(rep(c("Linf","K","L50","fish_M","lifestyle"),each=2),3),1000)
bs_potentialFactors_slope_COR_result$mat_stage <- rep(c(1:3),each=10000)
bs_potentialFactors_slope_COR_result$quarter <- rep(c(1:2),15000)

write.csv(bs_potentialFactors_slope_COR_result,
          "./SizeAggregTend_data/output/4_potential_factor_effect_on_humpshape/Potential_factor_effect_result.csv")

potenFactor_name <- c("Linf","K","L50","fish_M","lifestyle")
potenFactor_slope_statistic_95CI = NULL
for(q in 1:2){
  for(m in 1:3){
    for(p in 1:5){
      statis_95CI = 
        bs_potentialFactors_slope_COR_result %>% 
        filter(potenFactor==potenFactor_name[p]&mat_stage==m&quarter==q) %>%
        with(bca(statis_val))
      pvalue_95CI = 
        bs_potentialFactors_slope_COR_result %>% 
        filter(potenFactor==potenFactor_name[p]&mat_stage==m&quarter==q) %>%
        with(bca(`p-value`))
      potenFactor_slope_statistic_95CI = rbind(potenFactor_slope_statistic_95CI,
                                               c(statis_95CI,pvalue_95CI))
    }
  }
}
potenFactor_slope_statistic_95CI = as.data.frame(potenFactor_slope_statistic_95CI)
potenFactor_slope_statistic_95CI$potenFactor = rep(potenFactor_name,6)
potenFactor_slope_statistic_95CI$mat_stage = rep(rep(1:3,each=5),2)
potenFactor_slope_statistic_95CI$quarter = rep(1:2,each=15)

write.csv(potenFactor_slope_statistic_95CI,
          "./SizeAggregTend_data/output/4_potential_factor_effect_on_humpshape/Potential_factor_effect_95CI.csv")
