
windowsFonts(newrom = windowsFont("Times New Roman"))

# import data

wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)

sp_TL.result <- read.csv("./SizeAggregTend_data/output/1_size_TL/all_TL_result_16_compiled.csv",stringsAsFactors = F,sep=",",header = T)
sp_TL.result$spwn <- as.factor(sp_TL.result$spwn)
sp_TL.result$quarter <- as.factor(sp_TL.result$quarter)
sp_TL.result$sp_ID <- as.factor(sp_TL.result$sp_ID)



########################################################################
### Relationship fitted with considering spawning effect ###############
########################################################################

library("mgcv")
library("itsadug")

## GAMM result with Spawn by REML
fit.SPWN.temp.smooth.inter.Rslt <- gam(b~spwn+
                                    ti(std_T,by=spwn,bs="ts")+
                                    ti(std_L,by=spwn,bs="ts")+
                                    ti(std_L,std_T,by=spwn,bs="ts")+
                                    s(std_L,sp_ID,bs="re",by=dum)+s(sp_ID,bs="re",by=dum),
                                  data = sp_TL.result,
                                  method = "REML",
                                  family = Gamma(link = log)
                                  #select = F
)

summary(fit.SPWN.temp.smooth.inter.Rslt)

#########################################################################
### Difference between in and out of spawning season ####################
#########################################################################

jpeg("./SizeAggregTend_data/output/fig/FigS8_spawn_diff.jpeg", width=6, height=5, units = "in",res=300)
par(mfrow=c(1,1),mar=c(4,4,2,2),oma=c(0,0,0,0),xpd=F)
plot_diff(fit.SPWN.temp.smooth.inter.Rslt, 
          view=c("std_L"), 
          comp=list(spwn=c("Y", "N")),
          
          print.summary=T, hide.label = T,
          ylab = "Difference in Taylor's exponents",main = "",
          xlab = "Standardized length",
          col.diff = "black",
          family = c("newrom"))

dev.off()
