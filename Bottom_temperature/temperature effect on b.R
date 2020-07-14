
setwd("D:\\Ruo_data\\2019_Paper_Publish\\Data\\environment_data\\Temperature")
windowsFonts(newrom = windowsFont("Times New Roman"))

#Temp_data <- read.csv("1991.csv",header = T)
#for(year in 1:24){
#  temp <- read.csv(paste(as.character(year+1991),".csv",sep=""),header = T)
#  Temp_data <- rbind(Temp_data,temp)
#  print("save")
#}

# YOU CAN DIRECTLY MOVE TO ANALYSIS PART

Temp_data <- read.csv("Temp_1991_2015.csv",header = T)

Temp.Q1 <- rbind(Temp_data[which(substr(Temp_data$yyyy.mm.ddThh.mm,6,7)=="01"),],
                 Temp_data[which(substr(Temp_data$yyyy.mm.ddThh.mm,6,7)=="02"),],
                 Temp_data[which(substr(Temp_data$yyyy.mm.ddThh.mm,6,7)=="03"),])

Temp.Q3 <- rbind(Temp_data[which(substr(Temp_data$yyyy.mm.ddThh.mm,6,7)=="07"),],
                 Temp_data[which(substr(Temp_data$yyyy.mm.ddThh.mm,6,7)=="08"),],
                 Temp_data[which(substr(Temp_data$yyyy.mm.ddThh.mm,6,7)=="09"),])
#Temp_need <- rbind(Temp.Q1,Temp.Q3)
#write.csv(Temp_need,"Temp_1991_2015.csv")


##################################################################
#### check which station have temperature data ###################
##################################################################

northsea <- read.csv("D:\\Ruo_data\\2019_Paper_Publish\\Data\\CPUE per length per subarea_2016-09-21_1965to2016_q1q3.csv", header = T, sep = ",")
tot_sta <-as.data.frame(xtabs(~Subarea,data = northsea))
allsp_length_range.s <- as.matrix(read.table("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\allsp_length_range_16class.txt"))

lat <-seq(49.75,61.75,0.5)
long <- seq(-3.5,12.5,1)

# tot_sta --> from "Script_Size_based_TL_16classes.R"
subarea_symb <- data.frame(subarea=tot_sta[,1],
                           lat = lat[as.integer(factor(substr(tot_sta[,1],1,2)))],
                           long = long[as.integer(factor(substr(tot_sta[,1],3,4)))])
subarea_symb$lat_lab <- as.integer(factor(subarea_symb$lat))
subarea_symb$long_lab <- as.integer(factor(subarea_symb$long))

#
min_lat =min(subarea_symb$lat)
max_lat =max(subarea_symb$lat)
min_lon =min(subarea_symb$long)
max_lon =max(subarea_symb$long)

# grid line

grid_size_lat = 0.5
grid_size_lon = 1

lat_vec =  seq(min_lat-0.25,max_lat+0.25,grid_size_lat)
lon_vec =  seq(min_lon-0.5,max_lon+0.5,grid_size_lon)

# the function for plot the station that have temp data
source("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Bottom_temperature_effect\\Function_station_have_temperature.R")

########################################################
# get bottom temperature function
########################################################

source("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Bottom_temperature_effect\\Function_weight_prefer_bottom_temperature.R")



########################################################
# each sp
########################################################

source("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Function_Habitat_information.R")

sp_name <- c("Clupea harengus","Gadus morhua","Melanogrammus aeglefinus",
             "Merlangius merlangus", "Pollachius virens","Scomber scombrus",
             "Sprattus sprattus", "Trisopterus esmarkii")

sp_common_name <- c("Herring","Cod","Haddock","Whiting","Saithe",
                    "Mackerel","Sprat","Norwaypout")


allsp_bT <- NULL

for(i in 1:8){
  bT_1 <- bottom_T_fuc(Temp.Q1,sp_name[i],1,allsp_length_range.s[i,],i)
  allsp_bT <- rbind(allsp_bT,bT_1)
}

for(i in 1:8){
  bT_3 <- bottom_T_fuc(Temp.Q3,sp_name[i],3,allsp_length_range.s[i+8,],i)
  allsp_bT <- rbind(allsp_bT,bT_3)
}

#plot

jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\size_prefer_bT.jpeg", width=3, height=6, units = "in",res=300)
par(mfrow=c(4,2),mar=c(3.5,2,1,0))
for(i in c(1,3,5,7)){
  plot(allsp_bT[i,],main="",xlim=c(0,17),ylim=c(2,22),pch=19,family=c("newrom"),xlab="")
  plot(allsp_bT[i+8,],main="",xlim=c(0,17),ylim=c(2,22),yaxt="n",pch=19,family=c("newrom"),xlab="")
}
dev.off()

jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\size_prefer_bT2.jpeg", width=3, height=6, units = "in",res=300)
par(mfrow=c(4,2),mar=c(3.5,0,1,0))
for(i in c(2,4,6,8)){
  plot(allsp_bT[i,],main="",xlim=c(0,17),ylim=c(2,22),pch=19,family=c("newrom"),xlab="")
  plot(allsp_bT[i+8,],main="",xlim=c(0,17),ylim=c(2,22),yaxt="n",pch=19,family=c("newrom"),xlab="")
}
dev.off()

# analysis ########################################################
allsp_bT_tras <- NULL
for(j in 1:8){
  allsp_bT_tras <- c(allsp_bT_tras,allsp_bT[j,],allsp_bT[j+8,])
}

write.csv(allsp_bT_tras,"D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\preferable_bottom_Temperature.csv")


#### use code in "thin-plate spline GAM and sp_fit_gam 190711.R"
allsp_bT_tras <- read.csv("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\preferable_bottom_Temperature.csv")
sp_TL.result <- read.csv("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\sp_TL.result_all_16classes.csv")

library("nlme")
library("mgcv")
library("itsadug")
library("blme")
library("optimx")


sp_TL.result$temp  <- allsp_bT_tras[,2]
sp_TL.result$sp_ID <- as.factor(sp_TL.result$sp_ID)
sp_TL.result$spwn  <- as.factor(sp_TL.result$spwn)
write.csv(sp_TL.result,"D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\sp_TL.result_all_16classes.csv")


# fit with body size

fit.temp.size.line.Q.inter <- gam(b~temp*quarter+s(std_L,by=quarter)+s(std_L,sp_ID,bs="re",by=quarter)+s(sp_ID,bs="re",by=quarter),data = sp_TL.result,method = "ML")
fit.temp.size.line.Q <- gam(b~temp+quarter+s(std_L,by=quarter)+s(std_L,sp_ID,bs="re",by=quarter)+s(sp_ID,bs="re",by=quarter),data = sp_TL.result,method = "ML")
fit.temp.size.gam <- gam(b~quarter+s(temp,by=quarter)+s(std_L,by=quarter)+s(std_L,sp_ID,bs="re",by=quarter)+s(sp_ID,bs="re",by=quarter),data = sp_TL.result,method = "ML")
fit.temp.size.gam.inter <- gam(b~quarter+te(temp,std_L,by=quarter)+s(std_L,sp_ID,bs="re",by=quarter)+s(sp_ID,bs="re",by=quarter),data = sp_TL.result,method = "ML")
fit.temp.size.gam.inter.spwn <- gam(b~spwn+te(temp,std_L,by=spwn)+s(std_L,sp_ID,bs="re",by=spwn)+s(sp_ID,bs="re",by=spwn),data = sp_TL.result,method = "ML")

fit.temp.size.gam.inter2 <- gam(b~quarter+s(temp,by=quarter)+s(std_L,by=quarter)+ti(temp,std_L,by=quarter)+s(std_L,sp_ID,bs="re",by=quarter)+s(sp_ID,bs="re",by=quarter),data = sp_TL.result,method = "ML")
fit.temp.size.gam.inter2.spwn <- gam(b~spwn+s(temp,by=spwn)+s(std_L,by=spwn)+ti(temp,std_L,by=spwn)+s(std_L,sp_ID,bs="re",by=spwn)+s(sp_ID,bs="re",by=spwn),data = sp_TL.result,method = "ML")

fit.temp.size.gam.inter3 <- gam(b~quarter+s(std_L,by=quarter)+ti(temp,std_L,by=quarter)+s(std_L,sp_ID,bs="re",by=quarter)+s(sp_ID,bs="re",by=quarter),data = sp_TL.result,method = "ML")


fit.temp.size.gam.inter2.Q1 <- gam(b~s(temp)+s(std_L)+ti(temp,std_L)+s(std_L,sp_ID,bs="re")+s(sp_ID,bs="re"),data = sp_TL.result[which(sp_TL.result$quarter=="Q1"),],method = "ML")
fit.temp.size.gam.inter2.Q3 <- gam(b~s(temp)+s(std_L)+ti(temp,std_L)+s(std_L,sp_ID,bs="re")+s(sp_ID,bs="re"),data = sp_TL.result[which(sp_TL.result$quarter=="Q3"),],method = "ML")

AIC(
    fit.temp.size.line.Q.inter,fit.temp.size.line.Q,
    fit.temp.size.gam,fit.temp.size.gam.inter,
    fit.temp.size.gam.inter.spwn,
    fit.temp.size.gam.inter2,fit.temp.size.gam.inter2.spwn,
    fit.temp.size.gam.inter3)



plot_smooth(fit.temp.size.gam.inter3,view = "std_L",cond = list(quarter="Q1"),rm.ranef = T)
plot_smooth(fit.temp.size.gam.inter3,view = "std_L",cond = list(quarter="Q3"),rm.ranef = T)



plot_smooth(fit.temp.size.gam.inter3,view = "temp",rm.ranef = T,cond=list(quarter="Q1"),lty=2,col="grey60",ylim=c(0.5,3.5))
plot_smooth(fit.temp.size.gam.inter3,view = "temp",rm.ranef = T,cond=list(quarter="Q3"),add=T,ylim=c(0.5,3.5))

library("tidymv")
plot_difference(
  fit.temp.size.gam.inter,
  series = temp,
  difference = list(quarter = c("Q3", "Q1"))
)

plot_smooths(
  model = fit.temp.size.gam,
  series = std_L,
  comparison = quarter
) 

par(mfrow=c(1,2),mar=c(1,1,1,1))
vis.gam(fit.temp.size.gam.inter3,view=c("temp","std_L"),cond=list(quarter="Q1"), plot.type = "terrian", color = "terrain",theta=-60,main="Q1",phi=20,too.far = 0.1)
vis.gam(fit.temp.size.gam.inter3,view=c("temp","std_L"),cond=list(quarter="Q3"), plot.type = "terrian", color = "terrain",theta=150,main="Q3",phi=60,too.far = 0.1)

par(mfrow=c(1,1))
plot_smooth(fit.temp.size.gam.inter,view=c("temp"),cond=list(quarter="Q1"))
plot_smooth(fit.temp.size.gam.inter,view=c("temp"),cond=list(quarter="Q3"),add=T)

par(mfrow=c(1,2),mar=c(4,4,2,1))
vis.gam(fit.temp.size.gam.inter3,view=c("temp","std_L"),cond=list(quarter="Q1"), plot.type = "contour", color = "bw",main="Q1",too.far = 0.2,xlim=c(5,9))
points(sp_TL.result$temp[which(sp_TL.result$quarter=="Q1")],
       sp_TL.result$std_L[which(sp_TL.result$quarter=="Q1")],col=viridis(8)[sp_TL.result$sp_ID[which(sp_TL.result$quarter=="Q1")]])
vis.gam(fit.temp.size.gam.inter3,view=c("temp","std_L"),cond=list(quarter="Q3"), plot.type = "contour", color = "bw",main="Q3",too.far = 0.2)
points(sp_TL.result$temp[which(sp_TL.result$quarter=="Q3")],
       sp_TL.result$std_L[which(sp_TL.result$quarter=="Q3")],col=viridis(8)[sp_TL.result$sp_ID[which(sp_TL.result$quarter=="Q3")]])


plot()

fit.temp.size.gam.inter.2 <- gam(b~te(temp,by=quarter)+te(std_L)+s(std_L,sp_ID,bs="re",by=quarter)+s(sp_ID,bs="re",by=quarter),data = sp_TL.result,method = "ML")
vis.gam(fit.temp.size.gam.inter.2,theta=135)



             
               
jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\general_relationship_with_temp.jpeg", width=5, height=5, units = "in",res=300)

par(mfrow=c(1,1),mar=c(4,4,2,2),oma=c(0,0,0,0),xpd=F)
plot_smooth(fit.tot,view = "std_L", rm.ranef=T,main = "",xlim = c(-0.9,0.9),ylim = c(0.5,3.5),col = "grey40",xlab = "Standardized length",ylab = "Taylor's exponents",family="newrom")
abline(v=0,col="grey30",lty=2,lwd=2)
points(sp_TL.result$std_L,sp_TL.result$b,pch=19,cex=0.8,col="grey30")

dev.off()



plot_smooth(fit.temp.size.,view = "temp",ylim=c(1.5,3))
points(sp_TL.result$temp,sp_TL.result$b,col=viridis(8)[sp_TL.result$ageclass])

fho <- lm(b~temp,data=sp_TL.result[which(sp_TL.result$quarter=="Q3"),])

# linear - plot

library("ggeffects")

pr <- ggpredict(fit.temp.lin, "temp")

plot(pr)+
  geom_point(data =  sp_TL.result, 
             mapping =  aes(x=temp, y=b),)+
  xlab("temperature")+
  ylab("Taylor's exponents")+
  theme_classic()+
  theme(text = element_text(family = "newrom"))

pr.1 <- ggpredict(fit.temp.lin.1, "temp")

plot(pr.1)+
  geom_point(data =  sp_TL.result[which(sp_TL.result$quarter=="Q1"),], 
                    mapping =  aes(x=temp, y=b),)+
  xlab("temperature")+
  ylab("Taylor's exponents")+
  theme_classic()+
  theme(text = element_text(family = "newrom"))
  
pr.3 <- ggpredict(fit.temp.lin.3, "temp")

plot(pr.3)+
  geom_point(data =  sp_TL.result[which(sp_TL.result$quarter=="Q3"),], 
             mapping =  aes(x=temp, y=b),)+
  xlab("temperature")+
  ylab("Taylor's exponents")+
  theme_classic()+
  theme(text = element_text(family = "newrom"))
#
plot_smooth(fit.temp.size.,view = "std_L",rm.ranef = T,ylim = c(0.5,3.5),xlim = c(-1,1))
points(sp_TL.result$std_L,sp_TL.result$b)


#general
par(mfrow=c(1,1),mar=c(4,4,2,2),oma=c(0,0,0,0),xpd=F)
plot_smooth(fit.temp.qua.sp.re,view = "temp", rm.ranef=T,main = "",ylim = c(1,3.5),col = "grey40",xlab = "bottom temperature",ylab = "Taylor's exponents",family="newrom",family=c("newrom"))
points(sp_TL.result$temp,sp_TL.result$b,pch=19,cex=0.8,col="grey30")

#seasonal
jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\appendix\\bottom_temperature_effect.jpeg", width=5, height=4, units = "in",res=300)

par(mfrow=c(1,1),mar=c(4,4,2,2),oma=c(0,0,0,0),xpd=F)
plot_smooth(fit.temp.qua.sp.re,view = "temp",cond = list(quarter="Q1"), rm.ranef=T,col = c("grey40"),xlim = c(2,18),ylim = c(1,3),xlab = "bottom temperature",ylab = "Taylor's exponents",rug=F,lty=2,family=c("newrom"))
plot_smooth(fit.temp.qua.sp.re,view = "temp",cond = list(quarter="Q3"), rm.ranef=T,col = c("grey10"),xlim = c(2,18),ylim = c(1,3),xlab = "bottom temperature",ylab = "Taylor's exponents",rug=F,lty=1,family=c("newrom"),add=T)
points(sp_TL.result$temp[which(sp_TL.result$quarter=="Q1")],
       sp_TL.result$b[which(sp_TL.result$quarter=="Q1")],
       pch=1,cex=0.8,col="grey30")
points(sp_TL.result$temp[which(sp_TL.result$quarter=="Q3")],
       sp_TL.result$b[which(sp_TL.result$quarter=="Q3")],
       pch=2,cex=0.8,col="grey10")
dev.off()

#each sp
par(mfrow=c(3,3),mar=c(4,3,2,1),oma=c(3,3,2,0),xpd=T)
for(i in 1:8){
  plot_smooth(fit.temp,view = "temp",cond = list(sp_ID=factor(i)), rm.ranef=F,col = "#DAA520",xlim=c(0,13),ylim = c(0.5,3),main = sp_common_name[i],xlab = "",ylab = "",rug = F)
  points(sp_TL.result$temp[(16*i-15):(16*i)],sp_TL.result$b[(16*i-15):(16*i)],pch=19,col="#32A1AD")
}
mtext("Taylor's expnents",outer = T,cex=1.5,side=2)
mtext("Standardized length",outer = T,cex=1.5,side=1)

############################################################
############################################################

## sea surface temperature
library ("ncdf4")
surface_T_nc <- nc_open("surface/surface_mon_mean.nc")
attributes(surface_T_nc$var)$names

ncatt_get(surface_T_nc, attributes(surface_T_nc$var)$names[1])


surface_T <- ncvar_get (surface_T_nc,"sst")


