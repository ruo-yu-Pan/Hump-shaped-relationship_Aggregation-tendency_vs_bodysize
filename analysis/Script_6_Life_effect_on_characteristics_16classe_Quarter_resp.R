windowsFonts(newrom = "Times New Roman")

##########################################################################
# Examine the relationship between life history, life style, 
# fishery mortality and the characteristics of hump-shaped relationship
##########################################################################
setwd("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis")
sp_resp_humppeak <- read.csv("sp_resp_humppeak_16class_byQ_excludeT.csv",header=T)
sp_resp_slope <- read.csv("sp.resp_slope_16class_byQ_excludeT.csv",header=T)
sp_resp_slope$quarter <- rep(c("Q1","Q1","Q3","Q3"),9)

sp_TL.result <- read.csv("sp_TL.result_all_16classes.csv",header = T)
sp_TL.result$sp_ID <- as.factor(sp_TL.result$sp_ID)

# label whether the relationship is humped shape
sp_resp_humppeak$h_shape <- 1
sp_resp_humppeak$h_shape[c(2,10,13,16,18)] <-0

sp_resp_slope $h_shape <- 1
sp_resp_slope $h_shape[c(3,4,19,20,25,26,31,32,35,36)] <- 0


# prepare data #
uphil.seq <- seq(1,36,2)
dwhil.seq <- seq(2,36,2)

sp_resp_slope_up <- sp_resp_slope[uphil.seq,]
sp_resp_slope_dw <- sp_resp_slope[dwhil.seq,]

h_peak <- subset(sp_resp_humppeak, h_shape==1)
h_peak.1 <- subset(sp_resp_humppeak, h_shape==1&quarter=="Q1")
h_peak.3 <- subset(sp_resp_humppeak, h_shape==1&quarter=="Q3")

h_slope.up <- subset(sp_resp_slope_up, h_shape==1)
h_slope.dw <- subset(sp_resp_slope_dw, h_shape==1)
h_slope.up.1 <- subset(sp_resp_slope_up, h_shape==1&quarter=="Q1")
h_slope.dw.1 <- subset(sp_resp_slope_dw, h_shape==1&quarter=="Q1")
h_slope.up.3 <- subset(sp_resp_slope_up, h_shape==1&quarter=="Q3")
h_slope.dw.3 <- subset(sp_resp_slope_dw, h_shape==1&quarter=="Q3")



#########################################################################
### life history traits 
#########################################################################

Life_data <- read.csv("Life_history_traits.csv",header = T)
L.history <- Life_data[1:9,c(3,4,6)]
L.history$Linf <- L.history$Linf*10
L.history2 <- as.data.frame(apply(L.history,2,function(x)rep(x,each=2))) 
L.history2 <- L.history2[-c(2,10,13,16,18),]

lifehistory_effect <- NULL


for(i in 1:3){
  L <- L.history[,i]
  L2 <- rep(L,each=2)
  L2 <- L2[-c(2,10,13,16,18)]
  
  effect.p   <- cor.test(L2,h_peak[,2],method = "spearman", exact = F)
  effect.pl  <- cor.test(L2,h_peak[,3],method = "spearman", exact = F)
  effect.sm  <- cor.test(L2,h_slope.up[,2],method = "spearman", exact = F)
  effect.lar <- cor.test(L2,h_slope.dw[,2],method = "spearman", exact = F)
  
  effect.p.1   <- cor.test(L[-7],h_peak.1[,2],method = "spearman", exact = F)
  effect.pl.1  <- cor.test(L[-7],h_peak.1[,3],method = "spearman", exact = F)
  effect.sm.1  <- cor.test(L[-7],h_slope.up.1[,2],method = "spearman", exact = F)
  effect.lar.1 <- cor.test(L[-7],h_slope.dw.1[,2],method = "spearman", exact = F)
  
  effect.p.3   <- cor.test(L[-c(1,5,8,9)],h_peak.3[,2],method = "spearman", exact = F)
  effect.pl.3  <- cor.test(L[-c(1,5,8,9)],h_peak.3[,3],method = "spearman", exact = F)
  effect.sm.3  <- cor.test(L[-c(1,5,8,9)],h_slope.up.3[,2],method = "spearman", exact = F)
  effect.lar.3 <- cor.test(L[-c(1,5,8,9)],h_slope.dw.3[,2],method = "spearman", exact = F)
  
  
  
  lifehistory_effect <-
    rbind(lifehistory_effect,
          c(effect.p$estimate,effect.p$p.value),
          c(effect.pl$estimate,effect.pl$p.value),
          c(effect.sm$estimate,effect.sm$p.value),
          c(effect.lar$estimate,effect.lar$p.value),
          c(effect.p.1$estimate,effect.p.1$p.value),
          c(effect.pl.1$estimate,effect.pl.1$p.value),
          c(effect.sm.1$estimate,effect.sm.1$p.value),
          c(effect.lar.1$estimate,effect.lar.1$p.value),
          c(effect.p.3$estimate,effect.p.3$p.value),
          c(effect.pl.3$estimate,effect.pl.3$p.value),
          c(effect.sm.3$estimate,effect.sm.3$p.value),
          c(effect.lar.3$estimate,effect.lar.3$p.value))
  }

lifehistory_effect <- as.data.frame(lifehistory_effect)
colnames(lifehistory_effect)<-c("estimate","p-value")
lifehistory_effect$var <- rep(c("Linf","K","Lmtur"),each=12)
lifehistory_effect$quarter <- rep(rep(c("all","Q1","Q3"),each=4),3)

# plotting ################################################################
### all ###
jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\L_history_effect\\Ltrait_effect_on_humpshape.jpeg", width=6, height=5, units = "in",res=300)
par(mfrow=c(4,3),mar=c(0,2,1,1),oma=c(4,0,0,0))

# plot_max b
plot(L.history2$Linf, h_peak[,2],  xlab="",ylim = c(1.5,3.7),xlim=c(-50,2000),xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(2000,3.5,paste("p-value=",round(lifehistory_effect$`p-value`[1],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history2$K,    h_peak[,2],  xlab="",ylim = c(1.5,3.7),xlim=c(0,0.6),xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(0.6,3.5,paste("p-value=",round(lifehistory_effect$`p-value`[13],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history2$L50,  h_peak[,2],  xlab="",ylim = c(1.5,3.7),xlim=c(0,80),xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(80,3.5,paste("p-value=",round(lifehistory_effect$`p-value`[25],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


# plot_peaklocation
plot(L.history2$Linf, h_peak[,3],  xlab="",ylim = c(-0.5,0.7),xlim=c(-50,2000),xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(2000,0.6,paste("p-value=",round(lifehistory_effect$`p-value`[2],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history2$K,    h_peak[,3],  xlab="",ylim = c(-0.5,0.7),xlim=c(0,0.6),xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(0.6,0.6,paste("p-value=",round(lifehistory_effect$`p-value`[14],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history2$L50,  h_peak[,3],  xlab="",ylim = c(-0.5,0.7),xlim=c(0,80),xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(80,0.6,paste("p-value=",round(lifehistory_effect$`p-value`[26],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


# plot_uphill slope
plot(L.history2$Linf, h_slope.up[,2], xlab="",ylim = c(0,2.4),xlim=c(-50,2000),xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(2000,2.2,paste("p-value=",round(lifehistory_effect$`p-value`[3],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history2$K,    h_slope.up[,2], xlab="",ylim = c(0,2.4),xlim=c(0,0.6),xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(0.6,2.2,paste("p-value=",round(lifehistory_effect$`p-value`[15],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history2$L50,  h_slope.up[,2], xlab="",ylim = c(0,2.4),xlim=c(0,80),xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(80,2.2,paste("p-value=",round(lifehistory_effect$`p-value`[27],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


# plot_downhill slope
plot(L.history2$Linf, h_slope.dw[,2], ylim = c(-5,2),xlim=c(-50,2000),family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(2000,1.5,paste("p-value=",round(lifehistory_effect$`p-value`[4],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history2$K,    h_slope.dw[,2], ylim = c(-5,2),xlim=c(0,0.6),family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(0.6,1.5,paste("p-value=",round(lifehistory_effect$`p-value`[26],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history2$L50,  h_slope.dw[,2], ylim = c(-5,2),xlim=c(0,80),family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(80,1.5,paste("p-value=",round(lifehistory_effect$`p-value`[28],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


dev.off()




### Q1 ###
jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\L_history_effect\\Ltrait_effect_on_humpshape_byQ1.jpeg", width=6, height=5, units = "in",res=300)
par(mfrow=c(4,3),mar=c(0,2,1,1),oma=c(4,0,0,0))

# plot_max b
plot(L.history$Linf[-7], h_peak.1[,2],  xlab="",ylim = c(1.5,3.7),xlim=c(-50,2000),xaxt="n",pch=19,family=c("newrom"))
text(2000,3.5,paste("p-value=",round(lifehistory_effect$`p-value`[5],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history$K[-7],    h_peak.1[,2],  xlab="",ylim = c(1.5,3.7),xlim=c(0,0.6),xaxt="n",pch=19,family=c("newrom"))
text(0.6,3.5,paste("p-value=",round(lifehistory_effect$`p-value`[17],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history$L50[-7],  h_peak.1[,2],  xlab="",ylim = c(1.5,3.7),xlim=c(0,80),xaxt="n",pch=19,family=c("newrom"))
text(80,3.5,paste("p-value=",round(lifehistory_effect$`p-value`[29],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


# plot_peaklocation
plot(L.history$Linf[-7], h_peak.1[,3],  xlab="",ylim = c(-0.5,0.7),xlim=c(-50,2000),xaxt="n",pch=19,family=c("newrom"))
text(2000,0.6,paste("p-value=",round(lifehistory_effect$`p-value`[6],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history$K[-7],    h_peak.1[,3],  xlab="",ylim = c(-0.5,0.7),xlim=c(0,0.6),xaxt="n",pch=19,family=c("newrom"))
text(0.6,0.6,paste("p-value=",round(lifehistory_effect$`p-value`[18],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history$L50[-7],  h_peak.1[,3],  xlab="",ylim = c(-0.5,0.7),xlim=c(0,80),xaxt="n",pch=19,family=c("newrom"))
text(80,0.6,paste("p-value=",round(lifehistory_effect$`p-value`[30],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


# plot_uphill slope
plot(L.history$Linf[-7], h_slope.up.1[,2], xlab="",ylim = c(0,2.4),xlim=c(-50,2000),xaxt="n",pch=19,family=c("newrom"))
text(2000,2.2,paste("p-value=",round(lifehistory_effect$`p-value`[7],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history$K[-7],    h_slope.up.1[,2], xlab="",ylim = c(0,2.4),xlim=c(0,0.6),xaxt="n",pch=19,family=c("newrom"))
text(0.6,2.2,paste("p-value=",round(lifehistory_effect$`p-value`[19],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history$L50[-7],  h_slope.up.1[,2], xlab="",ylim = c(0,2.4),xlim=c(0,80),xaxt="n",pch=19,family=c("newrom"))
text(80,2.2,paste("p-value=",round(lifehistory_effect$`p-value`[31],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


# plot_downhill slope
plot(L.history$Linf[-7], h_slope.dw.1[,2], ylim = c(-5,2),xlim=c(-50,2000),pch=19,family=c("newrom"))
text(2000,1.5,paste("p-value=",round(lifehistory_effect$`p-value`[8],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history$K[-7],    h_slope.dw.1[,2], ylim = c(-5,2),xlim=c(0,0.6),pch=19,family=c("newrom"))
text(0.6,1.5,paste("p-value=",round(lifehistory_effect$`p-value`[20],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history$L50[-7],  h_slope.dw.1[,2], ylim = c(-5,2),xlim=c(0,80),pch=19,family=c("newrom"))
text(80,1.5,paste("p-value=",round(lifehistory_effect$`p-value`[32],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


dev.off()


### Q3 ###
jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\L_history_effect\\Ltrait_effect_on_humpshape_byQ3.jpeg", width=6, height=5, units = "in",res=300)
par(mfrow=c(4,3),mar=c(0,2,1,1),oma=c(4,0,0,0))

# plot_max b
plot(L.history$Linf[-c(1,5,8,9)], h_peak.3[,2],  xlab="",ylim = c(1.5,3.7),xlim=c(-50,2000),xaxt="n",pch=19,family=c("newrom"))
text(2000,3.5,paste("p-value=",round(lifehistory_effect$`p-value`[9],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history$K[-c(1,5,8,9)],    h_peak.3[,2],  xlab="",ylim = c(1.5,3.7),xlim=c(0,0.6),xaxt="n",pch=19,family=c("newrom"))
text(0.6,3.5,paste("p-value=",round(lifehistory_effect$`p-value`[21],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history$L50[-c(1,5,8,9)],  h_peak.3[,2],  xlab="",ylim = c(1.5,3.7),xlim=c(0,80),xaxt="n",pch=19,family=c("newrom"))
text(80,3.5,paste("p-value=",round(lifehistory_effect$`p-value`[33],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


# plot_peaklocation
plot(L.history$Linf[-c(1,5,8,9)], h_peak.3[,3],  xlab="",ylim = c(-0.5,0.7),xlim=c(-50,2000),xaxt="n",pch=19,family=c("newrom"))
text(2000,0.6,paste("p-value=",round(lifehistory_effect$`p-value`[10],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history$K[-c(1,5,8,9)],    h_peak.3[,3],  xlab="",ylim = c(-0.5,0.7),xlim=c(0,0.6),xaxt="n",pch=19,family=c("newrom"))
text(0.6,0.6,paste("p-value=",round(lifehistory_effect$`p-value`[22],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history$L50[-c(1,5,8,9)],  h_peak.3[,3],  xlab="",ylim = c(-0.5,0.7),xlim=c(0,80),xaxt="n",pch=19,family=c("newrom"))
text(80,0.6,paste("p-value=",round(lifehistory_effect$`p-value`[34],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


# plot_uphill slope
plot(L.history$Linf[-c(1,5,8,9)], h_slope.up.3[,2], xlab="",ylim = c(0,2.4),xlim=c(-50,2000),xaxt="n",pch=19,family=c("newrom"))
text(2000,2.2,paste("p-value=",round(lifehistory_effect$`p-value`[11],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history$K[-c(1,5,8,9)],    h_slope.up.3[,2], xlab="",ylim = c(0,2.4),xlim=c(0,0.6),xaxt="n",pch=19,family=c("newrom"))
text(0.6,2.2,paste("p-value=",round(lifehistory_effect$`p-value`[23],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history$L50[-c(1,5,8,9)],  h_slope.up.3[,2], xlab="",ylim = c(0,2.4),xlim=c(0,80),xaxt="n",pch=19,family=c("newrom"))
text(80,2.2,paste("p-value=",round(lifehistory_effect$`p-value`[35],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


# plot_downhill slope
plot(L.history$Linf[-c(1,5,8,9)], h_slope.dw.3[,2], ylim = c(-5,2),xlim=c(-50,2000),pch=19,family=c("newrom"))
text(2000,1.5,paste("p-value=",round(lifehistory_effect$`p-value`[12],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history$K[-c(1,5,8,9)],    h_slope.dw.3[,2], ylim = c(-5,2),xlim=c(0,0.6),pch=19,family=c("newrom"))
text(0.6,1.5,paste("p-value=",round(lifehistory_effect$`p-value`[24],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history$L50[-c(1,5,8,9)],  h_slope.dw.3[,2], ylim = c(-5,2),xlim=c(0,80),pch=19,family=c("newrom"))
text(80,1.5,paste("p-value=",round(lifehistory_effect$`p-value`[36],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


dev.off()



######################################################################
# life style effect
######################################################################

ts <- as.character(Life_data$life_style)
ts[which(ts=="pelagic")]="P"
ts[which(ts=="benthopelagic")]="B"
ts[which(ts=="demersal")]="D"
L.history$life_style <- as.factor(ts[1:9])

ls <- rep(ts,each=2)
ls.p <- ls[which(sp_resp_humppeak$h_shape==1)]
ls.s.up <- ls[which(sp_resp_slope_up$h_shape==1)]
ls.s.dw <- ls[which(sp_resp_slope_dw$h_shape==1)]

# all
lifestyle.p   <- wilcox.test(h_peak[-8,2]~factor(ls.p[-8]))
lifestyle.pl  <- wilcox.test(h_peak[-8,3]~factor(ls.p[-8]))
lifestyle.sm  <- wilcox.test(h_slope.up[-8,2]~factor(ls.s.up[-8]))
lifestyle.lar <- wilcox.test(h_slope.dw[-8,2]~factor(ls.s.dw[-8]))

lifehistory_effect<-
  rbind(lifehistory_effect,
        c(lifestyle.p$statistic[1],lifestyle.p$p.value,"lifestyle","all"),
        c(lifestyle.pl$statistic[1],lifestyle.pl$p.value,"lifestyle","all"),
        c(lifestyle.sm$statistic[1],lifestyle.sm$p.value,"lifestyle","all"),
        c(lifestyle.lar$statistic[1],lifestyle.lar$p.value,"lifestyle","all"))
# Q1
lifestyle.p.1   <- wilcox.test(h_peak.1[-5,2]~L.history$life_style[-c(5,7)])
lifestyle.pl.1  <- wilcox.test(h_peak.1[-5,3]~L.history$life_style[-c(5,7)])
lifestyle.sm.1  <- wilcox.test(h_slope.up.1[-5,2]~L.history$life_style[-c(5,7)])
lifestyle.lar.1 <- wilcox.test(h_slope.dw.1[-5,2]~L.history$life_style[-c(5,7)])

lifehistory_effect<-
  rbind(lifehistory_effect,
        c(lifestyle.p.1$statistic[1],lifestyle.p.1$p.value,"lifestyle","Q1"),
        c(lifestyle.pl.1$statistic[1],lifestyle.pl.1$p.value,"lifestyle","Q1"),
        c(lifestyle.sm.1$statistic[1],lifestyle.sm.1$p.value,"lifestyle","Q1"),
        c(lifestyle.lar.1$statistic[1],lifestyle.lar.1$p.value,"lifestyle","Q1"))

# Q3
lifestyle.p.3   <- wilcox.test(h_peak.3[,2]~L.history$life_style[-c(1,5,8,9)])
lifestyle.pl.3  <- wilcox.test(h_peak.3[,3]~L.history$life_style[-c(1,5,8,9)])
lifestyle.sm.3  <- wilcox.test(h_slope.up.3[,2]~L.history$life_style[-c(1,5,8,9)])
lifestyle.lar.3 <- wilcox.test(h_slope.dw.3[,2]~L.history$life_style[-c(1,5,8,9)])

lifehistory_effect<-
  rbind(lifehistory_effect,
        c(lifestyle.p.3$statistic[1],lifestyle.p.3$p.value,"lifestyle","Q3"),
        c(lifestyle.pl.3$statistic[1],lifestyle.pl.3$p.value,"lifestyle","Q3"),
        c(lifestyle.sm.3$statistic[1],lifestyle.sm.3$p.value,"lifestyle","Q3"),
        c(lifestyle.lar.3$statistic[1],lifestyle.lar.3$p.value,"lifestyle","Q3"))



# plotting ################################################################
jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\L_history_effect\\Lstyle_effect_on_humpshape.jpeg", width=6, height=4, units = "in",res=300)

par(mfrow=c(1,4),mar=c(4,5,2,1),oma=c(2,0,0,0),family="newrom")

boxplot(h_peak[-8,2]~factor(ls.p[-8]),
        ylab="Maximum b value",xlab="",
        cex.lab=1.5,medlwd=1,outline=FALSE,notch=F,
        ylim=c(min(h_peak[-8,2])-0.1,max(h_peak[-8,2])+0.1))
points(h_peak[-8,2]~jitter(as.numeric(as.factor(ls.p[-8]))),pch=as.numeric(h_peak$quarter))
title(paste("(",letters[1],")",sep=""),adj=0,family=c("newrom"))

boxplot(h_peak[-8,3]~factor(ls.p[-8]),
        ylab="Peak location",xlab="",
        cex.lab=1.5,medlwd=1,outline=FALSE,notch=F,
        ylim=c(min(h_peak[-8,3])-0.1,max(h_peak[-8,3])+0.1))
points(h_peak[-8,3]~jitter(as.numeric(as.factor(ls.p[-8]))),pch=as.numeric(h_peak$quarter))
title(paste("(",letters[2],")",sep=""),adj=0,family=c("newrom"))

boxplot(h_slope.up[-8,2]~factor(ls.p[-8]),
        ylab="Uphill slope",xlab="",
        cex.lab=1.5,medlwd=1,outline=FALSE,notch=F,
        ylim=c(min(h_slope.up[-8,2])-0.1,max(h_slope.up[-8,2])+0.1))
points(h_slope.up[-8,2]~jitter(as.numeric(as.factor(ls.p[-8]))),pch=as.numeric(h_peak$quarter))
title(paste("(",letters[3],")",sep=""),adj=0,family=c("newrom"))

boxplot(h_slope.dw[-8,2]~factor(ls.p[-8]),
        ylab="Downhill slope",xlab="",
        cex.lab=1.5,medlwd=1,outline=FALSE,notch=F,
        ylim=c(min(h_slope.dw[-8,2])-0.1,max(h_slope.dw[-8,2])+0.1))
points(h_slope.dw[-8,2]~jitter(as.numeric(as.factor(ls.p[-8]))),pch=as.numeric(h_peak$quarter))
title(paste("(",letters[4],")",sep=""),adj=0,family=c("newrom"))

mtext("Life style",side=1,outer=T,cex = 1,line = 0)
dev.off()

par(mfrow=c(1,1))
boxplot(b~sp_ID,data=sp_TL.result)
kruskal.test(b~sp_ID,data=sp_TL.result)
library("DescTools")
DunnTest(sp_TL.result$b,sp_TL.result$sp_ID,method="BH",alternative = "greater")





#####################################################################
# fishing mortaltiy 
#####################################################################

fish_M <- read.csv("fishingM.csv")
mean_fish_M <- tapply(fish_M$F,fish_M$Species,mean)
mean_F_2 <- rep(mean_fish_M,each=2)
mean_F_2 <- mean_F_2[-c(2,10,13,16,18)]

sp_TL.result$fish_M <- rep(mean_fish_M,each=32)

# all
fishM_p   <-cor.test(mean_F_2,h_peak[,2],method="spearman",exact = F)
fishM_pl  <-cor.test(mean_F_2,h_peak[,3],method="spearman",exact = F)
fishM_sm  <-cor.test(mean_F_2,h_slope.up[,2],method="spearman",exact = F)
fishM_lar <-cor.test(mean_F_2,h_slope.dw[,2],method="spearman",exact = F)

lifehistory_effect<-
  rbind(lifehistory_effect,
        c(fishM_p$estimate,fishM_p$p.value,"fish_M","all"),
        c(fishM_pl$estimate,fishM_pl$p.value,"fish_M","all"),
        c(fishM_sm$estimate,fishM_sm$p.value,"fish_M","all"),
        c(fishM_lar$estimate,fishM_lar$p.value,"fish_M","all"))


# Q1
fishM_p.1   <-cor.test(mean_fish_M[-7],h_peak.1[,2],method="spearman",exact = F)
fishM_pl.1  <-cor.test(mean_fish_M[-7],h_peak.1[,3],method="spearman",exact = F)
fishM_sm.1  <-cor.test(mean_fish_M[-7],h_slope.up.1[,2],method="spearman",exact = F)
fishM_lar.1 <-cor.test(mean_fish_M[-7],h_slope.dw.1[,2],method="spearman",exact = F)

lifehistory_effect<-
  rbind(lifehistory_effect,
        c(fishM_p.1$estimate,fishM_p.1$p.value,"fish_M","Q1"),
        c(fishM_pl.1$estimate,fishM_pl.1$p.value,"fish_M","Q1"),
        c(fishM_sm.1$estimate,fishM_sm.1$p.value,"fish_M","Q1"),
        c(fishM_lar.1$estimate,fishM_lar.1$p.value,"fish_M","Q1"))


# Q3
fishM_p.3   <-cor.test(mean_fish_M[-c(1,5,8,9)],h_peak.3[,2],method="spearman",exact = F)
fishM_pl.3  <-cor.test(mean_fish_M[-c(1,5,8,9)],h_peak.3[,3],method="spearman",exact = F)
fishM_sm.3  <-cor.test(mean_fish_M[-c(1,5,8,9)],h_slope.up.3[,2],method="spearman",exact = F)
fishM_lar.3 <-cor.test(mean_fish_M[-c(1,5,8,9)],h_slope.dw.3[,2],method="spearman",exact = F)

lifehistory_effect<-
  rbind(lifehistory_effect,
        c(fishM_p.3$estimate,fishM_p.3$p.value,"fish_M","Q3"),
        c(fishM_pl.3$estimate,fishM_pl.3$p.value,"fish_M","Q3"),
        c(fishM_sm.3$estimate,fishM_sm.3$p.value,"fish_M","Q3"),
        c(fishM_lar.3$estimate,fishM_lar.3$p.value,"fish_M","Q3"))



# plotting ################################################################

### plot all ###
jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\L_history_effect/Fmorality_effect_on_humpshape.jpeg", width=3, height=7.5, units = "in",res=300)

par(mfrow=c(4,1),mar=c(0,3,1,1),oma=c(5,0,1,0))

# plot_max b
plot(mean_F_2,h_peak[,2],
     xlab="",ylab="",xlim=c(0.2,1.2),ylim = c(1.5,3.7),
     xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(1.2,3.45,paste("p-value=",round(fishM_p$p.value,2),sep = ""),pos = 2,family=c("newrom"),cex = 1)

# plot_peaklocation
plot(mean_F_2,h_peak[,3],
     xlab="",ylab="",xlim=c(0.2,1.2),ylim = c(-0.5,0.7),
     xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter),cex.lab=2)
text(1.2,0.55,paste("p-value=",round(fishM_pl$p.value,2),sep = ""),pos = 2,family=c("newrom"),cex = 1)


# plot_uphill slope
plot(mean_F_2,h_slope.up[,2],
     xlab="",ylab="",xlim=c(0.2,1.2),ylim = c(0,2.4),
     xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter),cex.lab=2)
text(1.2,2.1,paste("p-value=",round(fishM_sm$p.value,2),sep = ""),pos = 2,family=c("newrom"),cex = 1)


# plot_downhill slope
plot(mean_F_2,h_slope.dw[,2],
     xlab="",ylab="",xlim=c(0.2,1.2),ylim = c(-5,2),
     family=c("newrom"),pch=as.numeric(h_peak$quarter),cex.lab=2)
text(1.2,1.1,paste("p-value=",round(fishM_lar$p.value,2),sep = ""),pos = 2,family=c("newrom"),cex = 1)


mtext("Fishery mortality",side = 1,outer = T,cex = 1,line = 3, family=c("newrom"))
dev.off()



### by Quarter ###
jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\L_history_effect\\Fmorality_effect_on_humpshape_byQ.jpeg", width=5, height=6, units = "in",res=300)

par(mfrow=c(4,2),mar=c(0,3,1,1),oma=c(4,0,1,0))

# plot_max b
plot(mean_fish_M[-7],h_peak.1[,2],
     xlab="",ylab="Maximum b value",xlim=c(0.2,1.2),ylim = c(1.5,3.7),
     xaxt="n",family=c("newrom"),pch=19,cex.lab=2)
text(1.2,3.5,paste("p-value=",round(fishM_p.1$p.value,2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)

plot(mean_fish_M[-c(1,5,8,9)],h_peak.3[,2],
     xlab="",ylab="",xlim=c(0.2,1.2),ylim = c(1.5,3.7),
     xaxt="n",family=c("newrom"),pch=19)
text(1.2,3.5,paste("p-value=",round(fishM_p.3$p.value,2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


# plot_peaklocation
plot(mean_fish_M[-7],h_peak.1[,3],
     xlab="",ylab="Maximum b value",xlim=c(0.2,1.2),ylim = c(-0.5,1),
     xaxt="n",family=c("newrom"),pch=19,cex.lab=2)
text(1.2,0.8,paste("p-value=",round(fishM_pl.1$p.value,2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)

plot(mean_fish_M[-c(1,5,8,9)],h_peak.3[,3],
     xlab="",ylab="",xlim=c(0.2,1.2),ylim = c(-0.5,1),
     xaxt="n",family=c("newrom"),pch=19)
text(1.2,0.8,paste("p-value=",round(fishM_pl.3$p.value,2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


# plot_uphill slope
plot(mean_fish_M[-7],h_slope.up.1[,2],
     xlab="",ylab="Maximum b value",xlim=c(0.2,1.2),ylim = c(0,1.7),
     xaxt="n",family=c("newrom"),pch=19,cex.lab=2)
text(1.2,1.5,paste("p-value=",round(fishM_sm.1$p.value,2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)

plot(mean_fish_M[-c(1,5,8,9)],h_slope.up.3[,2],
     xlab="",ylab="",xlim=c(0.2,1.2),ylim = c(0,1.7),
     xaxt="n",family=c("newrom"),pch=19)
text(1.2,1.5,paste("p-value=",round(fishM_sm.3$p.value,2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


# plot_downhill slope
plot(mean_fish_M[-7],h_slope.dw.1[,2],
     xlab="",ylab="Maximum b value",xlim=c(0.2,1.2),ylim = c(-4,2),
     xaxt="n",family=c("newrom"),pch=19,cex.lab=2)
text(1.2,1.5,paste("p-value=",round(fishM_lar.1$p.value,2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)

plot(mean_fish_M[-c(1,5,8,9)],h_slope.dw.3[,2],
     xlab="",ylab="",xlim=c(0.2,1.2),ylim = c(-4,2),
     xaxt="n",family=c("newrom"),pch=19)
text(1.2,1.5,paste("p-value=",round(fishM_lar.3$p.value,2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


dev.off()



write.csv(lifehistory_effect, "D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\Life_history_effect_result.csv")
###########################################################################
# fishing mortality CV
#Fcv_effect.sm <- lm(sp.resp_slope[seq(2,30,4),1]~L.history$CV_F_M)
#Fcv_effect.lar <- lm(sp.resp_slope[seq(4,32,4),1]~L.history$CV_F_M)
#Fcv_effect.sm <- cor.test(L.history$CV_F_M,sp.resp_slope[seq(2,30,4),1],method="spearman")
#Fcv_effect.lar <- cor.test(L.history$CV_F_M,sp.resp_slope[seq(4,32,4),1],method="spearman")


#plot(sp_resp_humppeak[-c(1,8),1]~L.history$CV_F_M[-c(1,8)],xlim=c(0.1,0.6),ylab="slope",xlab="mean fishing mortality")
#plot(sp_resp_humppeak[-c(1,8),2]~L.history$CV_F_M[-c(1,8)],xlim=c(0.1,0.6))
#plot(sm_slope[,1]~L.history$CV_F_M,xlim=c(0.1,0.6))
#plot(lar_slope[,1]~L.history$CV_F_M,xlim=c(0.1,0.6))


# b values difference among sp
#library("nortest")
#library("FSA")
#for(i in 1:8){lillie.test(sp_TL.result$b[which(sp_TL.result$sp_ID==8)])}

#boxplot(b~sp_ID,sp_TL.result)
#sp_diff_b <- kruskal.test(b~sp_ID,sp_TL.result)
#sp_2_diff <- dunnTest(b~sp_ID,data = sp_TL.result,method = "bh")
#summary(sp_diff_b)
#TukeyHSD(sp_diff_b)

#boxplot(b~quarter,sp_TL.result)
#t.test(sp_TL.result$b[which(sp_TL.result$quarter=="Q1")],sp_TL.result$b[which(sp_TL.result$quarter=="Q3")],paired = T)
