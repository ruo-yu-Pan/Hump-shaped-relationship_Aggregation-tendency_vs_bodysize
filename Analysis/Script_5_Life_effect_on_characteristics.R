windowsFonts(newrom = "Times New Roman")

### import data

wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)

# TL result
sp_TL.result <- read.csv("./SizeAggregTend_data/output/1_size_TL/all_TL_result_16_compiled.csv",stringsAsFactors = F,sep=",",header = T)
sp_TL.result$spwn <- as.factor(sp_TL.result$spwn)
sp_TL.result$quarter <- as.factor(sp_TL.result$quarter)
sp_TL.result$sp_ID <- as.factor(sp_TL.result$sp_ID)

# TL humpshape characteristics
sp_resp_humppeak <- read.csv("./SizeAggregTend_data/output/3_humpshape_characteristics/sp_resp_humppeak.csv",header=T)
sp_resp_slope    <- read.csv("./SizeAggregTend_data/output/3_humpshape_characteristics/sp_resp_slope.csv",header=T)
sp_resp_slope$quarter <- rep(c("Q1","Q1","Q3","Q3"),9)


### prepare data 
# label whether the relationship is humped shape
sp_resp_humppeak$h_shape <- 1
sp_resp_humppeak$h_shape[c(2,4,10,13,16,18)] <-0

sp_resp_slope $h_shape <- 1
sp_resp_slope $h_shape[c(3,4,7,8,19,20,25,26,31,32,35,36)] <- 0


#
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

##########################################################################
# Examine the relationship between life history, life style, 
# fishery mortality and the characteristics of hump-shaped relationship
##########################################################################


#########################################################################
### life history traits 
#########################################################################

Life_data <- read.csv("./SizeAggregTend_data/compiled/Life_history_traits.csv",header = T)
L.history <- Life_data[1:9,c(3,4,6)]
L.history$Linf <- L.history$Linf*10
L.history$L50 <- L.history$L50*10
L.history2 <- as.data.frame(apply(L.history,2,function(x)rep(x,each=2))) 
L.history2 <- L.history2[-c(2,4,10,13,16,18),]

lifehistory_effect <- NULL


for(i in 1:3){
  L <- L.history[,i]
  L2 <- rep(L,each=2)
  L2 <- L2[-c(2,4,10,13,16,18)]
  
  effect.p   <- cor.test(L2,h_peak[,2],method = "spearman", exact = F)
  effect.pl  <- cor.test(L2,h_peak[,3],method = "spearman", exact = F)
  effect.sm  <- cor.test(L2,h_slope.up[,2],method = "spearman", exact = F)
  effect.lar <- cor.test(L2,h_slope.dw[,2],method = "spearman", exact = F)
  
  lifehistory_effect <-
    rbind(lifehistory_effect,
          c(effect.p$estimate,effect.p$p.value),
          c(effect.pl$estimate,effect.pl$p.value),
          c(effect.sm$estimate,effect.sm$p.value),
          c(effect.lar$estimate,effect.lar$p.value))
  }

lifehistory_effect <- as.data.frame(lifehistory_effect)
colnames(lifehistory_effect)<-c("estimate","p-value")
lifehistory_effect$var <- rep(c("Linf","K","Lmtur"),each=4)
lifehistory_effect$quarter <- rep("all",12)


# plotting ################################################################
### all ###
jpeg("./SizeAggregTend_data/output/fig/FigS4_Ltrait_effect_on_humpshape.jpeg", width=7, height=6, units = "in",res=600)

par(mfrow=c(4,3),mar=c(0,2,1,1),oma=c(6,8,0,0))

# plot_max b
plot(L.history2$Linf, h_peak[,2],  xlab="",ylim = c(1.5,3.7),xlim=c(-50,2000),xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(2000,3.5,paste("p-value=",round(lifehistory_effect$`p-value`[1],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history2$K,    h_peak[,2],  xlab="",ylim = c(1.5,3.7),xlim=c(0,0.6),xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(0.6,3.5,paste("p-value=",round(lifehistory_effect$`p-value`[5],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history2$L50,  h_peak[,2],  xlab="",ylim = c(1.5,3.7),xlim=c(80,750),xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(750,3.5,paste("p-value=",round(lifehistory_effect$`p-value`[9],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


# plot_peaklocation
plot(L.history2$Linf, h_peak[,3],  xlab="",ylim = c(-0.65,0.7),xlim=c(-50,2000),xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(2000,0.6,paste("p-value=",round(lifehistory_effect$`p-value`[2],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history2$K,    h_peak[,3],  xlab="",ylim = c(-0.65,0.7),xlim=c(0,0.6),xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(0.6,0.6,paste("p-value=",round(lifehistory_effect$`p-value`[6],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history2$L50,  h_peak[,3],  xlab="",ylim = c(-0.65,0.7),xlim=c(80,750),xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(750,0.6,paste("p-value=",round(lifehistory_effect$`p-value`[10],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


# plot_uphill slope
plot(L.history2$Linf, h_slope.up[,2], xlab="",ylim = c(0,2.4),xlim=c(-50,2000),xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(2000,2.2,paste("p-value=",round(lifehistory_effect$`p-value`[3],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history2$K,    h_slope.up[,2], xlab="",ylim = c(0,2.4),xlim=c(0,0.6),xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(0.6,2.2,paste("p-value=",round(lifehistory_effect$`p-value`[7],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history2$L50,  h_slope.up[,2], xlab="",ylim = c(0,2.4),xlim=c(80,750),xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(750,2.2,paste("p-value=",round(lifehistory_effect$`p-value`[11],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


# plot_downhill slope
plot(L.history2$Linf, h_slope.dw[,2], ylim = c(-5,2),xlim=c(-50,2000),family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(2000,1.5,paste("p-value=",round(lifehistory_effect$`p-value`[4],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history2$K,    h_slope.dw[,2], ylim = c(-5,2),xlim=c(0,0.6),family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(0.6,1.5,paste("p-value=",round(lifehistory_effect$`p-value`[8],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)
plot(L.history2$L50,  h_slope.dw[,2], ylim = c(-5,2),xlim=c(80,750),family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(750,1.5,paste("p-value=",round(lifehistory_effect$`p-value`[12],2),sep = ""),pos = 2,family=c("newrom"),cex = 1.4)


ylab1 <- list( bquote( paste( "Maximum " ) ) ,
               bquote( paste( italic(b), " value" ) ) )
ylab2 <- list( bquote( paste( "Peak " ) ) ,
               bquote( paste( "Location" ) ) )
ylab3 <- list( bquote( paste( "Uphill " ) ) ,
               bquote( paste( "slope" ) ) )
ylab4 <- list( bquote( paste( "Downhill" ) ) ,
               bquote( paste( "slope" ) ) )

mtext(do.call(expression, ylab1 ),side = 2,padj = c(-16,-14),adj=0.5,las=1,line = 4,outer = T,family=c("newrom"))
mtext(do.call(expression, ylab2 ),side = 2,padj = c(-5,-3),adj=0.5,las=1,line = 4,outer = T,family=c("newrom"))
mtext(do.call(expression, ylab3 ),side = 2,padj = c(5,7),adj=0.5,las=1,line = 4,outer = T,family=c("newrom"))
mtext(do.call(expression, ylab4 ),side = 2,padj = c(16,14.5),adj=0.5,las=1,line = 4,outer = T,family=c("newrom"))

mtext(expression(L["inf"]),side = 1,at=0.175,adj=0.5,las=1,line = 4,outer = T,family=c("newrom"))
mtext("K",side = 1,at=0.5,adj=0.475,las=1,line = 4,outer = T,family=c("newrom"))
mtext(expression(L[50]),side = 1,at=0.85,adj=0.5,las=1,line = 4,outer = T,family=c("newrom"))

dev.off()


######################################################################
# life style effect
######################################################################

ts <- as.character(Life_data$life_style)
ts[which(ts=="pelagic")]="P"
ts[which(ts=="benthopelagic")]="B"
ts[which(ts=="demersal")]="D"
L.history$life_style <- as.factor(ts[1:9])

ls <- as.factor(rep(ts,each=2))
ls.p <- ls[which(sp_resp_humppeak$h_shape==1)][-7]
ls.p <- droplevels(ls.p)
ls.s.up <- droplevels(ls[which(sp_resp_slope_up$h_shape==1)][-7])
ls.s.dw <- droplevels(ls[which(sp_resp_slope_dw$h_shape==1)][-7])

# all
lifestyle.p   <- wilcox.test(h_peak[-7,2]~factor(ls.p))
lifestyle.pl  <- wilcox.test(h_peak[-7,3]~factor(ls.p))
lifestyle.sm  <- wilcox.test(h_slope.up[-7,2]~factor(ls.s.up))
lifestyle.lar <- wilcox.test(h_slope.dw[-7,2]~factor(ls.s.dw))

lifehistory_effect<-
  rbind(lifehistory_effect,
        c(lifestyle.p$statistic[1],lifestyle.p$p.value,"lifestyle","all"),
        c(lifestyle.pl$statistic[1],lifestyle.pl$p.value,"lifestyle","all"),
        c(lifestyle.sm$statistic[1],lifestyle.sm$p.value,"lifestyle","all"),
        c(lifestyle.lar$statistic[1],lifestyle.lar$p.value,"lifestyle","all"))


# plotting ################################################################
jpeg("./SizeAggregTend_data/output/fig/FigS5_Lstyle_effect_on_humpshape.jpeg", width=6, height=6, units = "in",res=500)

par(mfrow=c(2,2),mar=c(2,5,2,1),oma=c(2,0,0,0),family="newrom")

boxplot(h_peak[-7,2]~factor(ls.p),
        ylab="Maximum b value",xlab="",
        cex.lab=1.2,medlwd=1,outline=FALSE,notch=F,
        ylim=c(min(h_peak[-7,2])-0.1,max(h_peak[-7,2])+0.4))
points(h_peak[-7,2]~jitter(as.numeric(as.factor(ls.p))),pch=as.numeric(h_peak$quarter))
text(2.55,3.43,paste("p-value=",round(as.numeric(lifehistory_effect$`p-value`[13]),2),sep = ""),pos = 2,family=c("newrom"),cex = 1)
title(paste("(",letters[1],")",sep=""),adj=0,family=c("newrom"))

boxplot(h_peak[-7,3]~factor(ls.p),
        ylab="Peak location",xlab="",
        cex.lab=1.2,medlwd=1,outline=FALSE,notch=F,
        ylim=c(min(h_peak[-7,3])-0.1,max(h_peak[-7,3])+0.4))
points(h_peak[-7,3]~jitter(as.numeric(as.factor(ls.p))),pch=as.numeric(h_peak$quarter))
text(2.55,0.675,paste("p-value=",round(as.numeric(lifehistory_effect$`p-value`[14]),2),sep = ""),pos = 2,family=c("newrom"),cex = 1)
title(paste("(",letters[2],")",sep=""),adj=0,family=c("newrom"))

boxplot(h_slope.up[-7,2]~factor(ls.p),
        ylab="Uphill slope",xlab="",
        cex.lab=1.2,medlwd=1,outline=FALSE,notch=F,
        ylim=c(min(h_slope.up[-7,2])-0.1,max(h_slope.up[-7,2])+0.6))
points(h_slope.up[-7,2]~jitter(as.numeric(as.factor(ls.p))),pch=as.numeric(h_peak$quarter))
text(2.6,2.32,paste("p-value=",round(as.numeric(lifehistory_effect$`p-value`[15]),2),sep = ""),pos = 2,family=c("newrom"),cex = 1)
title(paste("(",letters[3],")",sep=""),adj=0,family=c("newrom"))

boxplot(h_slope.dw[-7,2]~factor(ls.p),
        ylab="Downhill slope",xlab="",
        cex.lab=1.2,medlwd=1,outline=FALSE,notch=F,
        ylim=c(min(h_slope.dw[-7,2])-0.1,max(h_slope.dw[-7,2])+1.5))
points(h_slope.dw[-7,2]~jitter(as.numeric(as.factor(ls.p))),pch=as.numeric(h_peak$quarter))
text(2.6,1.177,paste("p-value=",round(as.numeric(lifehistory_effect$`p-value`[16]),2),sep = ""),pos = 2,family=c("newrom"),cex = 1)
title(paste("(",letters[4],")",sep=""),adj=0,family=c("newrom"))

mtext("Life style",adj=0.3,side=1,outer=T,cex = 1,line = 0)
mtext("Life style",adj=0.85,side=1,outer=T,cex = 1,line = 0)

dev.off()



#####################################################################
# fishing mortaltiy 
#####################################################################

fish_M <- read.csv("./SizeAggregTend_data/compiled/fishingM.csv")
mean_fish_M <- tapply(fish_M$F,fish_M$Species,mean)
mean_F_2 <- rep(mean_fish_M,each=2)
mean_F_2 <- mean_F_2[-c(2,4,10,13,16,18)]


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



# plotting ################################################################

jpeg("./SizeAggregTend_data/output/fig/FigS6_Fmorality_effect_on_humpshape.jpeg", width=4, height=7.5, units = "in",res=300)

par(mfrow=c(4,1),mar=c(0,3,1,1),oma=c(5,7,1,0))

# plot_max b
plot(mean_F_2,h_peak[,2],
     xlab="",ylab="",xlim=c(0.2,1.2),ylim = c(1.5,3.7),
     xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter))
text(1.2,3.45,paste("p-value=",round(fishM_p$p.value,2),sep = ""),pos = 2,family=c("newrom"),cex = 1.5)

# plot_peaklocation
plot(mean_F_2,h_peak[,3],
     xlab="",ylab="",xlim=c(0.2,1.2),ylim = c(-0.65,0.7),
     xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter),cex.lab=2)
text(1.2,0.55,paste("p-value=",round(fishM_pl$p.value,2),sep = ""),pos = 2,family=c("newrom"),cex = 1.5)


# plot_uphill slope
plot(mean_F_2,h_slope.up[,2],
     xlab="",ylab="",xlim=c(0.2,1.2),ylim = c(0,2.4),
     xaxt="n",family=c("newrom"),pch=as.numeric(h_peak$quarter),cex.lab=2)
text(1.2,2.1,paste("p-value=",round(fishM_sm$p.value,2),sep = ""),pos = 2,family=c("newrom"),cex = 1.5)


# plot_downhill slope
plot(mean_F_2,h_slope.dw[,2],
     xlab="",ylab="",xlim=c(0.2,1.2),ylim = c(-5,2),
     family=c("newrom"),pch=as.numeric(h_peak$quarter),cex.lab=2)
text(1.2,1.1,paste("p-value=",round(fishM_lar$p.value,2),sep = ""),pos = 2,family=c("newrom"),cex = 1.5)

mtext("Fishery mortality",side = 1,outer = T,cex = 1,line = 3, family=c("newrom"))

mtext(do.call(expression, ylab1 ),side = 2,padj = c(-20.5,-18.5),adj=0.5,las=1,line = 4,outer = T,family=c("newrom"))
mtext(do.call(expression, ylab2 ),side = 2,padj = c(-6,-4.5),adj=0.5,las=1,line = 4,outer = T,family=c("newrom"))
mtext(do.call(expression, ylab3 ),side = 2,padj = c(6.25,7.75),adj=0.5,las=1,line = 4,outer = T,family=c("newrom"))
mtext(do.call(expression, ylab4 ),side = 2,padj = c(21.2,18.5),adj=0.5,las=1,line = 4,outer = T,family=c("newrom"))

dev.off()


### output result summary

write.csv(lifehistory_effect, "./SizeAggregTend_data/output/4_potential_factor_effect_on_humpshape/Potential_factor_effect_result.csv")
