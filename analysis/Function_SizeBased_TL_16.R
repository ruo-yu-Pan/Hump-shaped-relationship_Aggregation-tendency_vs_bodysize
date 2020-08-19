
#########################################################################
###########   function for size-based Taylor's power law   ##############
#########################################################################

colo <- c("red","darkorange","chartreuse3","springgreen3","cyan3","darkblue","darkorchid2","deeppink1")
library("rgr")
library("boot")

##
# spec_data    : the northsea data of species in different quarters
# stage_length : the length range of each size classes
# spe_n_hab    : the subarea that is not habitat
# spe.name     : species name that we want to show on the plot
##

sb_TL_fuc <- function(spec_data=sp.2.Q1,stage_length_range=sp_length_range.1,spe_n_hab=sp.hab.info,spe.name=paste(sp_c_name," Q1")){
  
  fish <- spec_data 
  # exclude the subarea data which is not in habitat
  for(i in 1:length(spe_n_hab[[3]]$subarea)){
    if(length(which(fish$Subarea==spe_n_hab[[3]]$subarea[i]))!=0){
      fish <- fish[-which(fish$Subarea==spe_n_hab[[3]]$subarea[i]),] 
    }
  }
  
  # set function for calculate the mean and variance of abundance
  fish.length_range <- stage_length_range
  mean_var <-function(x,y){
    fish_stage <- fish[which(fish$LngtClas < x & fish$LngtClas >= y),]
    fish_totalcount <- tapply(fish_stage$CPUE_number_per_hour,
                              list(fish_stage$Year,fish_stage$Subarea),sum)#calculate the density of fish whose size is within certain length for each year and station.
    
    fish_totalcount[which(is.na(fish_totalcount)==T)]=0
    station_n <- 194-length(spe_n_hab[[3]]$subarea)
    mean <- apply(fish_totalcount,1,sum)/station_n
    var <- apply(fish_totalcount,1,function(x){sum((x-mean(x)/station_n)^2)/(station_n-1)})
    m_v <- cbind(mean,var)
    return(m_v)
  }
  
  # make function for calculating slope and intercept of Taylor's power law
  alpha_beta <- function(u,l,sizeclas){
    stage <- as.data.frame(mean_var(u,l))
    stage.log <-as.data.frame(log10(stage))
    
    tls <- gx.rma(stage.log$mean,stage.log$var)
    slope <- tls$a1
    intercept <- tls$a0
    r_squared <- tls$corr
    # bootstrap for 95% CI
    TL.bs_func <- function(data,i){
      d <- data[i,]
      tls <- gx.rma(d$mean,d$var)
      return(tls$a1)
    }
    TL.bs_func.2 <- function(data,i){
      d <- data[i,]
      tls <- gx.rma(d$mean,d$var)
      return(tls$a0)
    }
    set.seed(199)
    b.bs <- boot(stage.log,TL.bs_func,R=999)
    b.bca.CI <- boot.ci(b.bs,conf = 0.95,type = "bca")
    a.bs <- boot(stage.log,TL.bs_func.2,R=999)
    a.bca.CI <- boot.ci(a.bs,conf = 0.95,type = "bca")
    Path <- "D:\\Ruo_data\\2019_Paper_Publish\\Code\\Result_data_for_analysis\\TL_boot_999bvalue\\"
    write.csv(b.bs$t,paste(Path,spe.name,"_",as.character(sizeclas),".csv",sep = ""))
    
    # compile data
    TL <- c(slope,intercept,r_squared,mean(stage$mean),a.bca.CI$bca[4:5],b.bca.CI$bca[4:5])
    #plot(stage.log,main=paste(stage_length_range[i],stage_length_range[i+1],sep="~"),xlab=paste("log(mean)",round(TL[3],digits = 4),sep = "  "),ylab="log(var)")
    #abline(intercept,slope)
    return(TL)
  }
  
  TL_result <- NULL
  TL.tot <- NULL
  for (i in 1: (length(stage_length_range)-1)){  
    stage <- as.data.frame(log10(mean_var(stage_length_range[i+1],stage_length_range[i])))
    stage <- cbind(stage,rep(i,length(stage[,1])))
    TL<-alpha_beta(stage_length_range[i+1],stage_length_range[i],i)
    TL_result <-as.data.frame(rbind(TL_result,TL))
    TL.tot <- as.data.frame(rbind(TL.tot,stage))
  }
  
  colnames(TL_result) <- c("b","a","r_squared","mean.25y","a.low.95CI","a.upp.95CI","b.low.95CI","b.upp.95CI")
  TL_result$ageclass <-c(1:(length(stage_length_range)-1))
  colnames(TL.tot)[3] <-"ageclass"
  
  #par(mfrow=c(1,1),mar=c(4,4,3,1),oma=c(0,0,0,0))
  #plot(c(0,0),xlim=c(min(TL.tot$mean)-1,max(TL.tot$mean)+1),
  #     ylim=c(min(TL.tot$var)-1,max(TL.tot$var)+1),type="n",xlab = "log(mean)",ylab="log(variance)",main=spe.name,family=c("newrom"))
  #points(TL.tot$mean,TL.tot$var,col=colo[TL.tot$ageclass])
  #for(i in 1:(length(stage_length_range)-1)){
  #  abline(b=TL_result[i,1],a = TL_result[i,2],col=colo[i])
  #}
  #legend("bottomright",legend = c(1:16),col = colo,pch=1)
  
  par(mfrow=c(4,4),mar=c(2,2,2,1),oma=c(3,3,0,0))
  for(i in 1:(length(stage_length_range)-1)){
    plot(c(0,0),xlim=c(min(TL.tot$mean)-1,max(TL.tot$mean)+1),
         ylim=c(min(TL.tot$var)-1,max(TL.tot$var)+1),
         xlab = "log(mean)",ylab="log(variance)",type="n",
         main=paste(stage_length_range[i],stage_length_range[i+1],sep="-"),
         family=c("newrom"))
    points(TL.tot$mean[which(TL.tot$ageclass==i)],TL.tot$var[which(TL.tot$ageclass==i)])
    abline(b=TL_result[i,1],a = TL_result[i,2])
    text(x=(max(TL.tot$mean)-min(TL.tot$mean))*0.8+min(TL.tot$mean),y=(max(TL.tot$var)-min(TL.tot$var))/12+min(TL.tot$var),paste("R^2=",round(TL_result$r_squared[i],2)),family=c("newrom"))
  }
  mtext(side = 1,"log(mean)",line = 1,outer = T,family=c("newrom"))
  mtext(side = 2,"log(variance)",line = 1,outer = T,family=c("newrom"))
  
  return(TL_result)
}
